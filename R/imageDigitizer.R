## vim:textwidth=128:expandtab:shiftwidth=2:softtabstop=2


##> ## Testing for shearing transform
##> library(magick)
##> logo <- magick::image_read("logo:")
##> S <- magick::image_shear(logo,"5x0")
##> plot(0:1,0:1);rasterImage(S,0,0,1,1)
##>> ii<-image_read("~/friehe_schmitt_1976_fig2.png")
##>> par(mar=c(1.5,2,2,2))
##>> plot(0:1,0:1,xlab="",ylab="",type="n")
##>> rasterImage(image_rotate(image_shear(ii,"1x0"),-0.5),0,0,1,1)

## https://cran.r-project.org/web/packages/magick/vignettes/intro.html#Transformations
## http://alumni.media.mit.edu/~maov/classes/comp_photo_vision08f/lect/08_image_warps.pdf
## http://www.imagemagick.org/Usage/distorts/#perspective
## https://github.com/ropensci/magick/issues/273

library(shiny)
library(png)

#options(shiny.error=browser) # this does not seem to do anything useful, for errors I get

default <- list(fourCorners=TRUE)

## corners() returns a list with the corners in x and y
## corners(list(x=A,y=B)) adds a corner
corners <- local({
  rval <- list(x=NULL, y=NULL)
  function(xy) {
    if (missing(xy) || length(rval$x) > 4) {
      rval
    } else {
      rval$x <<- c(rval$x, xy$x)
      rval$y <<- c(rval$y, xy$y)
      rval
    }
  }
})

stageMeanings <- c("Input file",          # 1
                   "Four Corners",        # 2 (OLD "2"="Rotate image",)
                   "Name x axis",         # 3
                   "Define low x value",  # 4
                   "Click low x value",   # 5
                   "Define high x value", # 6
                   "Click high x value",  # 7
                   "Name y axis",         # 8
                   "Define low y value",  # 9
                   "Click low y value",   # 10
                   "Define high y value", # 11
                   "Click high y value",  # 12
                   "Digitize Points")     # 13

col <- list(axes="magenta", grid="blue")

version <- "0.1.5"
keypressHelp <- "
<i>Keystroke interpretation</i>
<ul>
<!--
<li><b>4</b> select 4 corners</li>
<li> BROKEN '<b>+</b>': zoom in, centred on mouse location</li>
<li> BROKEN '<b>-</b>': zoom out</li>
<li> BROKEN '<b>0</b>': unzoom</li>
-->
<li> '<b>?</b>': show this message</li>
</ul>
"

debugFlag <- TRUE                      # For console messages that trace control flow.
dmsg <- function(...)
{
  if (debugFlag)
    cat(file=stderr(), ...)
}

fileLoaded <- FALSE

ui <- shiny::fluidPage(tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
                       style="margin-left:2ex",
                       shiny::uiOutput(outputId="title"),
                       shiny::conditionalPanel(condition="output.stage == '1'",
                                               shiny::fluidRow(shiny::uiOutput(outputId="loadFile"))),
                       shiny::conditionalPanel(condition="output.stage == '92'",
                                               shiny::fluidRow(shiny::p(paste("Dan 1, Click on the 4 corners of the plot axes")),
                                                               shiny::plotOutput("plot", click="click", hover="plotHover", height=600))),
                       ##OLD shiny::conditionalPanel(condition="output.stage == '2'",
                       ##OLD                         shiny::fluidRow(shiny::column(4, shiny::sliderInput("rotate",
                       ##OLD                                                                             shiny::h5("Rotate Image [degrees]"),
                       ##OLD                                                                             min=-10, max=10, value=0, step=0.1)),
                       ##OLD                                         shiny::column(4, shiny::sliderInput("shearx",
                       ##OLD                                                                             shiny::h5("Shear x"),
                       ##OLD                                                                             min=0, max=30, value=0, step=0.1)),
                       ##OLD                                         shiny::column(4, shiny::sliderInput("sheary",
                       ##OLD                                                                             shiny::h5("Shear y"),
                       ##OLD                                                                             min=0, max=30, value=0, step=0.1))),
                       ##OLD                         shiny::fluidRow(shiny::column(3, shiny::actionButton("stage2button", "Next"))),
                       ##OLD                         shiny::fluidRow(shiny::radioButtons("grid", label=shiny::h5("Grid"),
                       ##OLD                                                             choices=c("None"="off", "Fine"="fine",
                       ##OLD                                                                       "Medium"="medium", "Coarse"="coarse"),
                       ##OLD                                                             selected="medium", inline=TRUE))),
                       shiny::conditionalPanel(condition="output.stage == '3'",
                                               shiny::fluidRow(shiny::column(4, shiny::textInput("xname", shiny::h5("X name"))),
                                                               shiny::column(3, shiny::textInput("xmin", shiny::h5("X min"))),
                                                               shiny::column(3, shiny::textInput("xmax", shiny::h5("X max")))),
                                               shiny::fluidRow(shiny::column(4, shiny::textInput("yname", shiny::h5("Y name"))),
                                                               shiny::column(3, shiny::textInput("ymin", shiny::h5("Y min"))),
                                                               shiny::column(3, shiny::textInput("ymax", shiny::h5("Y max")))),
                                               shiny::fluidRow(shiny::actionButton("stage3Abutton", "Next"))),
                       shiny::conditionalPanel(condition="output.stage == 'OLD3'",
                                               shiny::fluidRow(shiny::column(6, shiny::textInput("xname", shiny::h5("Name x axis"))),
                                                               shiny::column(6, shiny::actionButton("stage3button",
                                                                                                    "Next",
                                                                                                    style="margin-top: 50px;")))),
                       shiny::conditionalPanel(condition="output.stage == 'OLD4'",
                                               shiny::fluidRow(shiny::column(6, shiny::textInput("xmin", shiny::h5("A low x value you will click in a moment"))),
                                                               shiny::column(6, shiny::actionButton("stage4button",
                                                                                                    "Next (then click on that value)",
                                                                                                    style="margin-top: 50px;")))),
                       shiny::conditionalPanel(condition="output.stage == 'OLD6'",
                                               shiny::fluidRow(shiny::column(6, shiny::textInput("xmax", shiny::h5("A high x value you will click in a moment"))),
                                                               shiny::column(6, shiny::actionButton("stage6button",
                                                                                                    "Next (then click on that value)",
                                                                                                    style="margin-top: 50px;")))),
                       shiny::conditionalPanel(condition="output.stage == 'OLD8'",
                                               shiny::fluidRow(shiny::column(6, shiny::textInput("yname", shiny::h5("Name y axis"))),
                                                               shiny::column(6, shiny::actionButton("stage8button",
                                                                                                    "Next",
                                                                                                    style="margin-top: 50px;")))),
                       shiny::conditionalPanel(condition="output.stage == 'OLD9'",
                                               shiny::fluidRow(shiny::column(6, shiny::textInput("ymin", shiny::h5("A low y value you will click in a moment"))),
                                                               shiny::column(6, shiny::actionButton("stage9button",
                                                                                                    "Next (then click on that value)",
                                                                                                    style="margin-top: 50px;")))),
                                               ## This button increases stage to 9, and then the click increases it to 10.
                       shiny::conditionalPanel(condition="output.stage == 'OLD11'",
                                               shiny::fluidRow(shiny::column(6, shiny::textInput("ymax", shiny::h5("A high y value you will click in a moment"))),
                                                               shiny::column(6, shiny::actionButton("stage11button",
                                                                                                    "Next (then click on that value)",
                                                                                                    style="margin-top: 50px;")))),
                       shiny::conditionalPanel(condition="output.stage == '13'",
                                               shiny::fluidRow(shiny::radioButtons("code", label="Symbol code",
                                                                                   choices=1:10, selected=1, inline=TRUE)),
                                               shiny::fluidRow(shiny::column(2, shiny::actionButton("undo", "Undo")),
                                                               shiny::column(2, shiny::actionButton("save", "Save Data")),
                                                               shiny::column(2, shiny::actionButton("Rcode", "R Code")),
                                                               shiny::column(2, shiny::actionButton("quit", "Quit"))),
                                               shiny::fluidRow(shiny::column(2, shiny::htmlOutput("status"))),
                                               shiny::fluidRow(shiny::plotOutput("plot", click="click", hover="plotHover", height=600))))

server <- function(input, output)
{
  state <- shiny::reactiveValues(step=1,
                                 stage=1,
                                 shearx=0,
                                 sheary=0,
                                 rotate=0,
                                 inputFile=NULL,
                                 image=NULL,
                                 xname="x",
                                 yname="x",
                                 x=list(device=NULL),
                                 y=list(device=NULL),
                                 code=NULL,
                                 xaxis=list(user=NULL, device=NULL),
                                 yaxis=list(user=NULL, device=NULL),
                                 xaxisModel=NULL,
                                 yaxisModel=NULL,
                                 xhover=NULL,
                                 yhover=NULL)

  saveFile <- function()
  {
    file <- paste(gsub(".png$", "", state$inputFile$name), "_imageDigitizer.dat", sep="")
    cat(paste("# imageDigitizer version ", version, "\n", sep = ""), file=file)
    cat(paste("# file: ", state$inputFile$name, "\n", sep=""), file=file, append=TRUE)
    cat(paste("# rotation: ", state$rotate, "\n", sep=""), file=file, append=TRUE)
    if (length(state$xaxis$device)) {
      cat(paste("# x axis device: ", paste(state$xaxis$device, collapse=" "), "\n", sep=""), file=file, append=TRUE)
      cat(paste("# x axis user:   ", paste(state$xaxis$user,   collapse=" "), "\n", sep=""), file=file, append=TRUE)
      cat(paste("# y axis device: ", paste(state$yaxis$device, collapse=" "), "\n", sep=""), file=file, append=TRUE)
      cat(paste("# y axis user:   ", paste(state$yaxis$user,   collapse=" "), "\n", sep=""), file=file, append=TRUE)
    }
    if (nchar(state$xname) < 1)
      state$xname <- "x"
    if (nchar(state$yname) < 1)
      state$yname <- "y"
    cat(sprintf("i,devicex,devicey,%s,%s,code\n", state$xname, state$yname), file=file, append=TRUE)
    xuser <- predict(state$xaxisModel, data.frame(device=state$x$device))
    yuser <- predict(state$yaxisModel, data.frame(device=state$y$device))
    for (i in seq_along(state$x$device)) {
      cat(sprintf("%3d,%10.3f,%10.3f,%20g,%20g,%d\n", i, state$x$device[i], state$y$device[i], xuser[i], yuser[i],state$code[i]), file=file, append=TRUE)
    }
   }

  shiny::observeEvent(input$Rcode, {
                      ofile <- paste(gsub(".png$", "", state$inputFile$name), "_imageDigitizer.dat", sep="")
                      msg <- "# Sample code to read and plot the saved data file<br>\n"
                      msg <- paste0(msg, "data <- read.csv(file=\"", ofile, "\", skip=7, header=TRUE)<br>\n")
                      msg <- paste0(msg, "plot(",
                                    "data[[\"", state$xname, "\"]],",
                                    "data[[\"", state$yname, "\"]],",
                                    "xlab=\"", state$xname, "\",",
                                    "ylab=\"", state$yname, "\",",
                                    "pch=data$code)<br>\n")
                      dmsg(msg)
                      shiny::showModal(shiny::modalDialog(shiny::HTML(msg), title="R code", size="l"))
  })

  shiny::observeEvent(input$quit, {
                      saveFile()
                      shiny::stopApp()
  })


  ##> shiny::observeEvent(input$loadFile, {
  ##>                     state$stage <<- if (default$fourCorners) "2A" else "2B"
  ##>                     shiny::insertUI("loadAFile", ui=shiny::fileInput("inputFile", shiny::h5("Input file"), accept=c("image/png")))
  ##> })

  shiny::observeEvent(input$loadFile, {
                      shiny::insertUI("loadAFile", ui=shiny::fileInput("inputFile", shiny::h5("Input file"), accept=c("image/png")))
                      if (!is.null(state$inputFile)) {
                        ##state$stage <<- 2
                      } else {
                        shiny::showNotification("Select a file first, then click 'Proceed to Stage 2'")
                      }
  })

  shiny::observeEvent(input$stage1button, {
                      dmsg("clicked stage1button\n")
                      ##? state$stage <<- if (default$fourCorners) "2A" else "2B"
                      ##? state$stage <<- 2
  })

  shiny::observeEvent(input$stage2button, {
                      dmsg("clicked stage2button\n")
                      ##? state$stage <<- 3
  })

  shiny::observeEvent(input$stage3button, {
                      dmsg("clicked stage3button\n")
                      state$xname <- input$xname
                      state$xaxis$user <<- c(input$xmin, input$xmax)
                      state$yname <- input$yname
                      state$yaxis$user <<- c(input$ymin, input$ymax)
                      state$stage <<- 12
  })

  ##OLD shiny::observeEvent(input$stage3button, {
  ##OLD                     dmsg("clicked stage3button\n")
  ##OLD                     state$xname <<- input$xname
  ##OLD                     state$stage <<- "4"
  ##OLD })

  shiny::observeEvent(input$stage4button, {
                      dmsg("clicked stage4button\n")
                      state$xaxis$user <<- as.numeric(input$xmin)
                      ## The click will increase state$stage
                      shiny::showNotification(paste0("Click the mouse once, at x=", input$xmin), type="message")
                      state$stage <<- 5
  })

  shiny::observeEvent(input$stage6button, {
                      dmsg("clicked stage6button\n")
                      state$xaxis$user <<- c(state$xaxis$user, as.numeric(input$xmax))
                      ## The click will increase state$stage
                      shiny::showNotification(paste0("Click the mouse once, at x=", input$xmax), type="message")
                      state$stage <<- 7
  })

  shiny::observeEvent(input$stage8button, {
                      dmsg("clicked stage8button\n")
                      state$yname <<- input$yname
                      state$stage <<- 9
  })

  shiny::observeEvent(input$stage9button, {
                      dmsg("clicked stage9button\n")
                      state$yaxis$user <<- as.numeric(input$ymin)
                      ## The click will increase state$stage
                      shiny::showNotification(paste0("Click the mouse once, at y=", input$ymin), type="message")
                      state$stage <<- 10
  })

  shiny::observeEvent(input$stage11button, {
                      dmsg("clicked stage11button\n")
                      state$yaxis$user <<- c(state$yaxis$user, as.numeric(input$ymax))
                      ## The click will increase state$stage
                      shiny::showNotification(paste0("Click the mouse once, at y=", input$ymax), type="message")
                      state$stage <<- 12
  })


  #'
  #' @importFrom shiny HTML modalDialog showModal
  shiny::observeEvent(input$keypressTrigger,
                      {
                        #if (state$step == 6) {
                          key <- intToUtf8(input$keypress)
                          dmsg("keypress '", input$keypress, "' or '", key, "'\n", sep="")
                          if (key == '?') {
                            shiny::showModal(shiny::modalDialog( title="", shiny::HTML(keypressHelp), easyClose=TRUE))
                          }
                        #}
                      })

  xAxisModal <- function(failed=FALSE)
  {
    modalDialog(
      shiny::textInput("xAxisValue", "Enter x at last mouse click"),
      footer=shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("xAxisButtonOk", "OK"))
    )
  }
  xAxisNameModal <- function(failed=FALSE)
  {
    modalDialog(
      shiny::textInput("xAxisName", "Enter name of x axis"),
      footer=shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("xAxisNameButtonOk", "OK")
      )
    )
  }
  ##OLD yAxisModal <- function(failed=FALSE)
  ##OLD {
  ##OLD   shiny::modalDialog(
  ##OLD     shiny::textInput("yAxisValue", "Enter y at last mouse click"),
  ##OLD     footer=shiny::tagList(
  ##OLD       shiny::modalButton("Cancel"),
  ##OLD       shiny::actionButton("yAxisButtonOk", "OK")
  ##OLD     )
  ##OLD   )
  ##OLD }
  yAxisNameModal <- function(failed=FALSE)
  {
    shiny::modalDialog(
      shiny::textInput("yAxisName", "Enter name of y axis"),
      footer=shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("yAxisNameButtonOk", "OK")
      )
    )
  }


  output$title <- shiny::renderUI({
    dmsg("in $title\n")
    shiny::h5(paste0("imageDigitizer ", version,
                     ifelse(is.null(state$inputFile), "", paste0(" | File '", state$inputFile, "'")),
                     " | Processing stage ", state$stage, " (", stageMeanings[state$stage], ")"))
  })

  output$loadFile <- shiny::renderUI({
    if (state$stage == 1) {
      shiny::fileInput("inputFile", shiny::h5("Input file"), accept=c("image/png"))
      ## state$stage <<- 2
    }
  })

  #' @importFrom graphics abline box mtext par points rasterImage text
  #' @importFrom magick image_shear
  output$plot <- shiny::renderPlot({
    par(mar=rep(1, 4))
    ##DAN<<-state$image
    idim <- dim(state$image[[1]])
    asp <- if (is.null(state$image)) 1 else idim[3] / idim[2] # FIXME: or reciprocal?
    dmsg("idim=c(", idim[1], ",", idim[2], "); asp=", asp, "\n")
    plot(0:1, 0:1, type='n', asp=asp, xaxs="i", yaxs="i", axes=FALSE)
    box()
    if (is.null(state$image)) {
      text(0.2, 0.5, "Please select a png file.")
    } else {
      I <- state$image
      if (state$shearx > 0) {
        dmsg("Performing shearx\n")
        I <- magick::image_shear(I, sprintf("%.0fx0", round(state$shearx)))
      }
      if (state$sheary > 0) {
        dmsg("Performing sheary\n")
        I <- magick::image_shear(I, sprintf("0x%.0f", round(state$sheary)))
      }
      if (state$rotate != 0) {
        dmsg("Performing rotate\n")
        I <- magick::image_rotate(I, state$rotate)
      }
      rasterImage(I, 0, 0, 1, 1, interpolate=FALSE)
      ##rasterImage(state$image, 0, 0, 1, 1, angle=-state$rotate, interpolate=FALSE)
      dmsg("rotate=", state$rotate, ", shearx=", state$shearx, ", sheary=", state$sheary, "\n")
      if (input$grid == "fine") {
        abline(h=seq(-3, 3, 0.05), col=col$grid, lty="dotted")
        abline(v=seq(-3, 3, 0.05 * asp), col=col$grid, lty="dotted")
      } else if (input$grid == "medium") {
        abline(h=seq(-3, 3, 0.1), col=col$grid, lty="dotted")
        abline(v=seq(-3, 3, 0.1 * asp), col=col$grid, lty="dotted")
      } else if (input$grid == "coarse") {
        abline(h=seq(-3, 3, 0.2), col=col$grid, lty="dotted")
        abline(v=seq(-3, 3, 0.2 * asp), col=col$grid, lty="dotted")
      }
      if (length(state$xaxis$device) != 0) {
        abline(v=state$xaxis$device[1], col=col$axes)
        mtext(state$xaxis$user[1], side=3, at=state$xaxis$device[1], col=col$axes)
        if (length(state$xaxis$device) == 2) {
          abline(v=state$xaxis$device[2], col=col$axes)
          mtext(state$xaxis$user[2], side=3, at=state$xaxis$device[2], col=col$axes)
        }
      }
      if (length(state$yaxis$device) != 0) {
        abline(h=state$yaxis$device[1], col=col$axes)
        mtext(state$yaxis$user[1], side=4, at=state$yaxis$device[1], col=col$axes)
        if (length(state$yaxis$device) == 2) {
          abline(h=state$yaxis$device[2], col=col$axes)
          mtext(state$yaxis$user[2], side=4, at=state$yaxis$device[2], col=col$axes)
        }
      }
      if (length(state$x$device)) {
        points(state$x$device, state$y$device, pch=state$code, col="red")
      }
    }
  })

  #' @importFrom utils head
  shiny::observeEvent(input$undo, {
    if (length(state$x$device) > 0) {
      state$x$device <- head(state$x$device, -1)
      state$y$device <- head(state$y$device, -1)
      state$code <- head(state$code, -1)
    }
  })

  shiny::observeEvent(input$save, {
                      saveFile()
                      shiny::showNotification(paste0("File '", state$inputFile$name, "' saved"), type="message")
  })

  shiny::observeEvent(input$plotHover, {
                      state$xhover <- input$plotHover$x
                      state$yhover <- input$plotHover$y
  })

  #' @importFrom stats lm predict
  shiny::observeEvent(input$click, {
                      dmsg("click with state$stage =", state$stage, "\n")
                      if (state$stage == 2) {
                        dmsg("at stage 2 with click at x=", input$click$x, ", y=", input$click$y, "\n")
                        if (default$fourCorners) {
                          corners(input$click)
                          print(file=stderr(), corners())
                          DANcorners<<-corners()
                          DANimage<<-state$image
                          C <- corners()
                          if (length(C$x) == 4) {
                            ## Convert to image coords (note: first dim is channel)
                            idim <- dim(state$image[[1]])[-1]
                            ## FIXME: perhaps we need to trim, in case of a click within a pixel
                            C$x <- round(idim[1] * C$x)
                            C$y <- round(idim[2] * C$y)
                            ## Put corners into a known order
                            centre <- list(x=mean(C$x), y=mean(C$y))
                            DANcentre<<-centre
                            LL <- C$x < centre$x & C$y < centre$y
                            LR <- C$x > centre$x & C$y < centre$y
                            UL <- C$x < centre$x & C$y > centre$y
                            UR <- C$x > centre$x & C$y > centre$y
                            DANLL<<-LL
                            DANLR<<-LR
                            DANUR<<-UR
                            DANUL<<-UL
                            ## Original corners, CCW from bottom left
                            O <- list(x=c(C$x[LL], C$x[LR], C$x[UR], C$x[UL]),
                                      y=c(C$y[LL], C$y[LR], C$y[UR], C$y[UL]))
                            DANO <<- O
                            ## Distorted corners (so output rect contains input rect)
                            Rx <- range(C$x)
                            Ry <- range(C$y)
                            D <- list(x=c(Rx[1], Rx[2], Rx[2], Rx[1]),
                                      y=c(Ry[1], Ry[1], Ry[2], Ry[2]))
                            DAND <<- D
                            ##? P <- c(O$x[1], O$y[1], D$x[1], D$y[1],
                            ##?        O$x[2], O$y[2], D$x[2], D$y[2],
                            ##?        O$x[3], O$y[3], D$x[3], D$y[3],
                            ##?        O$x[4], O$y[4], D$x[4], D$y[4])
                            P <- c(D$x[1], D$y[1], O$x[1], O$y[1],
                                   D$x[2], D$y[2], O$x[2], O$y[2],
                                   D$x[3], D$y[3], O$x[3], O$y[3],
                                   D$x[4], D$y[4], O$x[4], O$y[4])
                            DANP <<- P
                            ##DANimaged<<-image_distort(state$image, 'perspective', P)
                            ##state$image <<- image_distort(state$image, 'perspective', P)
                            state$stage <<- 3
                          }
                        }
                      } else if (state$stage == 2) {
                        dmsg("NOT HANDLED\n")
                      } else if (state$stage == 5) {
                        state$xaxis$device <<- input$click$x
                        state$stage <- 6
                        dmsg("  defined state$xaxis$device[1] as ", state$xaxis$device[1], "\n")
                      } else if (state$stage == 7) {
                        state$xaxis$device <<- c(state$xaxis$device, input$click$x)
                        dmsg("about to do lm() with state$xaxis as follows\n")
                        if (debugFlag) print(file=stderr(), state$xaxis)
                        state$xaxisModel <<- lm(user ~ device, data=state$xaxis)
                        dmsg("next is state$xaxisModel\n")
                        if (debugFlag) print(file=stderr(), state$xaxisModel)
                        state$stage <- 8
                        dmsg("  defined state$xaxis$device[2] as ", state$xaxis$device[2], "\n")
                      } else if (state$stage == 10) {
                        state$yaxis$device <<- input$click$y
                        state$stage <- 11
                        dmsg("  defined state$yaxis$device[1] as ", state$yaxis$device[1], "\n")
                      } else if (state$stage == 12) {
                        state$yaxis$device <<- c(state$yaxis$device, input$click$y)
                        dmsg("about to do lm() with state$yaxis as follows\n")
                        if (debugFlag) print(file=stderr(), state$yaxis)
                        state$yaxisModel <<- lm(user ~ device, data=state$yaxis)
                        dmsg("next is state$yaxisModel\n")
                        if (debugFlag) print(file=stderr(), state$yaxisModel)
                        state$stage <- "13"
                        dmsg("  defined state$yaxis$device[2] as ", state$yaxis$device[2], "\n")
                        shiny::showNotification("Now, start clicking points to digitize them.")
                      } else if (state$stage == 13) { # digitizing points
                        state$x$device <<- c(state$x$device, input$click$x)
                        state$y$device <<- c(state$y$device, input$click$y)
                        state$code <<- c(state$code, as.numeric(input$code))
                        ##dmsg("Next is input$code\n")
                        ##if (debugFlag) print(file=stderr(), input$code)
                        n <- length(state$x$device)
                        dmsg("  defined i-th point as c(", state$x$device[n], ",", state$y$device[n], ")\n")
                      ### } else {
                      ###   stop("programming error in click (unknown state$stage=", state$stage, ")")
                      }
  })

  output$status <- shiny::renderText({
    ## if (is.null(state$inputFile)) {
    ##   state$step <- 1
    ##   return(paste0("<b>SETUP 1</b><br>Select a file, and possibly rotate it. (state$stage=", state$stage, ")"))
    ## }
    ##.. if (length(state$xaxis$device) != 2) {
    ##..   state$step <- 2
    ##..   if (length(state$xaxis$device) == 0)
    ##..     return(paste("Click a known point on the X axis."))
    ##..   else
    ##..     return(paste("Click a second known point on the X axis."))
    ##.. }
    ## if (is.null(state$xname)) {
    ##   state$step <- 3
    ##   return(paste("<b>", state$inputFile$name, "</b><br><b>SETUP 2C</b><br>Enter name of x axis."))
    ## }
    ##.. if (length(state$yaxis$device) != 2) {
    ##..   state$step <- 3
    ##..   if (length(state$yaxis$device) == 0)
    ##..     return(paste("Click a known point on the Y axis."))
    ##..   else
    ##..     return(paste("Click a second known point on the Y axis."))
    ##.. }
    ## if (is.null(state$yname)) {
    ##   state$step <- 5
    ##   return(paste("<b>", state$inputFile$name, "</b><br><b>SETUP 3C</b><br>Enter name of y axis."))
    ## }
    state$step <- 4 # FIXME?
    res <- paste("<b>", state$inputFile$name, "</b><br>", length(state$x$device), "points digitized")
    if (!is.null(state$xaxisModel) && !is.null(state$yaxisModel)) {
      xh <- predict(state$xaxisModel, data.frame(device=state$xhover))
      yh <- predict(state$yaxisModel, data.frame(device=state$yhover))
      res <- paste(res, sprintf("<br><br><i>%.3f %.3f</i>", xh, yh))
    }
    res
  })

  ## Image transformations chosen by user to establish orthogonal x and y axes
  shiny::observeEvent(input$rotate, { state$rotate <- input$rotate })
  shiny::observeEvent(input$shearx, { state$shearx <- input$shearx })
  shiny::observeEvent(input$sheary, { state$sheary <- input$sheary })

  ## @importFrom png readPNG
  #' @importFrom magick image_read
  shiny::observeEvent(input$inputFile, {
    state$inputFile <<- input$inputFile
    ##state$image <- png::readPNG(state$inputFile$datapath)
    state$image <<- magick::image_read(state$inputFile$datapath)
    state$imageExists <<- TRUE
    ## state$stage <- if (default$fourCorners) "2A" else "2B"
    dmsg("state$imageExists=", state$imageExists, "\n")
  })

  shiny::observeEvent(input$xname, {
    state$xname <- input$xname
  })

  shiny::observeEvent(input$yname, {
    state$yname <- input$yname
  })

  ##OLD #' @importFrom stats lm predict
  ##OLD shiny::observeEvent(input$xAxisButtonOk, {
  ##OLD   state$xaxis$user <- c(state$xaxis$user, as.numeric(input$xAxisValue))
  ##OLD   if (length(state$xaxis$user) > 1) {
  ##OLD     state$xaxisModel <- lm(user ~ device, data=state$xaxis)
  ##OLD     state$step <- 3
  ##OLD   }
  ##OLD   shiny::removeModal()
  ##OLD })

  ##OLD shiny::observeEvent(input$xAxisNameButtonOk, {
  ##OLD   state$xname <- input$xAxisName
  ##OLD   shiny::removeModal()
  ##OLD })

  ##OLD shiny::observeEvent(input$yAxisButtonOk, {
  ##OLD   state$yaxis$user <-
  ##OLD     c(state$yaxis$user, as.numeric(input$yAxisValue))
  ##OLD   if (length(state$yaxis$user) > 1) {
  ##OLD     state$yaxisModel <- lm(user ~ device, data=state$yaxis)
  ##OLD     state$step <- 5
  ##OLD   }
  ##OLD   shiny::removeModal()
  ##OLD })

  ##OLD shiny::observeEvent(input$yAxisNameButtonOk, {
  ##OLD   state$yname <- input$yAxisName
  ##OLD   shiny::removeModal()
  ##OLD })

  output$stage <- shiny::reactive({
    dmsg("output$stage is returning ", state$stage, " based on state$stage\n", sep="")
    as.integer(state$stage)
  })

  shiny::outputOptions(output, "stage", suspendWhenHidden=FALSE)

  output$readImage <- shiny::renderUI({
    shiny::fileInput("inputFile", shiny::h5("Input file"), accept=c("image/png"))
  })

}

#' R/shiny app for digitizing points in images.
#'
#' A shiny graphical user interface (GUI) for digitizing points in images, by
#' means of mouse clicks. The GUI is meant to be reasonably self-explanatory.
#'
#' @param fourCorners logical value indicating (FIXME: describe this ... if we keep it!)
#' @export
imageDigitizer <- function(fourCorners=TRUE)
{
  default$fourCorners <- fourCorners
  options(shiny.trace=TRUE)
  shiny::shinyApp(ui, server)
}

