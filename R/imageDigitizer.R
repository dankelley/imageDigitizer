## vim:textwidth=128:expandtab:shiftwidth=2:softtabstop=2

library(shiny)
library(png)

options(shiny.error=browser)
stageMeanings <- c("Input file",          #  1  stage1button increases stage to  2
                   "Rotate image",        #  2  stage2button increases stage to  3
                   "Name x axis",         #  3  stage3button increases stage to  4
                   "Define low x value",  #  4  stage4button increases stage to  5
                   "Click low x value",   #  5   mouse click increases stage to  6
                   "Define high x value", #  6  stage6button increases stage to  7
                   "Click high x value",  #  7   mouse click increases stage to  8
                   "Name y axis",         #  8  stage8button increases stage to  9
                   "Define low y value",  #  9  stage9button increases stage to 10
                   "Click low y value",   # 10   mouse click increases stage to 11
                   "Define high y value", # 11 stage11button increases stage to 12
                   "Click high y value",  # 12   mouse click increases stage to 13
                   "Digitize Points")     # 13 final stage (FIXME: what about a 'quit' stage?)

col <- list(axes="magenta", grid="blue")

debugFlag <- !TRUE                     # For console messages that trace control flow.
version <- "0.1.5"
keypressHelp <- "
<i>Keystroke interpretation</i>
<ul>
<li> '<b>+</b>': zoom in, centred on mouse location
<li> '<b>-</b>': zoom out
<li> '<b>0</b>': unzoom
</ul>

<i>Developer plans</i>
<ul>
<li> Allow single-scale (like for a map)
<li> Zooming
<li> Read analysis file
</ul>
"

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
                                               ##shiny::fluidRow(shiny::actionButton("stage1button", "Proceed to Stage 2 (rotating image)"))),
                       shiny::conditionalPanel(condition="output.stage == '2'",
                                               shiny::fluidRow(shiny::column(6, shiny::sliderInput("rotate", shiny::h5("Rotate Image [deg]"),
                                                                                                   min=-10, max=10, value=0, step=0.1)),
                                                               shiny::column(6, shiny::actionButton("stage2button",
                                                                                                    "Proceed to next step",
                                                                                                    style="margin-top: 50px;"))),
                                               shiny::fluidRow(shiny::radioButtons("grid", label=shiny::h5("Grid"),
                                                                                   choices=c("None"="off", "Fine"="fine",
                                                                                             "Medium"="medium", "Coarse"="coarse"),
                                                                                   selected="medium", inline=TRUE))),
                       shiny::conditionalPanel(condition="output.stage == '3'",
                                               shiny::fluidRow(shiny::column(6, shiny::textInput("xname", shiny::h5("Name x axis"))),
                                                               shiny::column(6, shiny::actionButton("stage3button",
                                                                                                    "Poceed to next step",
                                                                                                    style="margin-top: 50px;")))),
                       shiny::conditionalPanel(condition="output.stage == '4'",
                                               shiny::fluidRow(shiny::column(6, shiny::textInput("xmin", shiny::h5("A low x value you will click in a moment"))),
                                                               shiny::column(6, shiny::actionButton("stage4button",
                                                                                                    "Proceed to clicking on that value",
                                                                                                    style="margin-top: 50px;")))),
                       shiny::conditionalPanel(condition="output.stage == '6'",
                                               shiny::fluidRow(shiny::column(6, shiny::textInput("xmax", shiny::h5("A high x value you will click in a moment"))),
                                                               shiny::column(6, shiny::actionButton("stage6button",
                                                                                                    "Proceed to clicking on that value",
                                                                                                    style="margin-top: 50px;")))),
                       shiny::conditionalPanel(condition="output.stage == '8'",
                                               shiny::fluidRow(shiny::column(6, shiny::textInput("yname", shiny::h5("Name y axis"))),
                                                               shiny::column(6, shiny::actionButton("stage8button",
                                                                                                    "Proceed to next step",
                                                                                                    style="margin-top: 50px;")))),
                       shiny::conditionalPanel(condition="output.stage == '9'",
                                               shiny::fluidRow(shiny::column(6, shiny::textInput("ymin", shiny::h5("A low y value you will click in a moment"))),
                                                               shiny::column(6, shiny::actionButton("stage9button",
                                                                                                    "Proceed to clicking on that value",
                                                                                                    style="margin-top: 50px;")))),
                                               ## This button increases stage to 9, and then the click increases it to 10.
                       shiny::conditionalPanel(condition="output.stage == '11'",
                                               shiny::fluidRow(shiny::column(6, shiny::textInput("ymax", shiny::h5("A high y value you will click in a moment"))),
                                                               shiny::column(6, shiny::actionButton("stage11button",
                                                                                                    "Proceed to clicking on that value",
                                                                                                    style="margin-top: 50px;")))),
                       shiny::conditionalPanel(condition="output.stage > '12'",
                                               shiny::fluidRow(shiny::radioButtons("code", label="Symbol code",
                                                                                   choices=1:10, selected=1, inline=TRUE)),
                                               shiny::fluidRow(shiny::column(2, shiny::actionButton("undo", "Undo")),
                                                               shiny::column(2, shiny::actionButton("save", "Save Data")),
                                                               shiny::column(2, shiny::actionButton("Rcode", "R Code")),
                                                               shiny::column(2, shiny::actionButton("quit", "Quit"))),
                                               shiny::fluidRow(shiny::column(2, shiny::htmlOutput("status")))),
                       shiny::conditionalPanel(condition="output.stage > '1'",
                                               shiny::fluidRow(shiny::plotOutput("plot", click="click", hover="plotHover", height=600))))

server <- function(input, output)
{
  state <- shiny::reactiveValues(
    step=1, # 1: need to click to define x axis; 2: need to click to define y axis; 3: acquire data
    stage=1,
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
    yhover=NULL
  )

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


  shiny::observeEvent(input$loadFile, {
                      shiny::insertUI("loadAFile", ui=shiny::fileInput("inputFile", shiny::h5("Input file"), accept=c("image/png")))
  })

  shiny::observeEvent(input$stage1button, {
                      dmsg("clicked stage1button\n")
                      if (!is.null(state$inputFile)) {
                        state$stage <<- 2
                      } else {
                        shiny::showNotification("Select a file first, then click 'Proceed to Stage 2'")
                      }
  })

  shiny::observeEvent(input$stage2button, {
                      dmsg("clicked stage2button\n")
                      state$stage <<- 3
  })

  shiny::observeEvent(input$stage3button, {
                      dmsg("clicked stage3button\n")
                      state$xname <<- input$xname
                      state$stage <<- 4
  })

  shiny::observeEvent(input$stage4button, {
                      dmsg("clicked stage4button\n")
                      state$xaxis$user <<- as.numeric(input$xmin)
                      ## The click will increase state$stage
                      shiny::showNotification(paste0("Click the mouse once, at x=", input$xmin))
                      state$stage <<- 5
  })

  shiny::observeEvent(input$stage6button, {
                      dmsg("clicked stage6button\n")
                      state$xaxis$user <<- c(state$xaxis$user, as.numeric(input$xmax))
                      ## The click will increase state$stage
                      shiny::showNotification(paste0("Click the mouse once, at x=", input$xmax))
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
                      shiny::showNotification(paste0("Click the mouse once, at y=", input$ymin))
                      state$stage <<- 10
  })

  shiny::observeEvent(input$stage11button, {
                      dmsg("clicked stage11button\n")
                      state$yaxis$user <<- c(state$yaxis$user, as.numeric(input$ymax))
                      ## The click will increase state$stage
                      shiny::showNotification(paste0("Click the mouse once, at y=", input$ymax))
                      state$stage <<- 12
  })




  #'
  #' @importFrom shiny HTML modalDialog showModal
  shiny::observeEvent(input$keypressTrigger,
                      {
                        if (state$step == 6) {
                          key <- intToUtf8(input$keypress)
                          dmsg("keypress numerical value ",
                              input$keypress,
                              ", i.e. key='",
                              key,
                              "'\n",
                              sep="")
                          if (key == '+') {
                            dmsg("should zoom in now\n")
                          } else if (key == '-') {
                            dmsg("should zoom out now\n")
                          } else if (key == '0') {
                            dmsg("should unzoom now\n")
                          } else if (key == '?') {
                            shiny::showModal(shiny::modalDialog( title="", shiny::HTML(keypressHelp), easyClose=TRUE))
                          }
                        }
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
    shiny::h5(paste0("imageDigitizer ", version,
                     ifelse(is.null(state$inputFile), "", paste0(" | File '", state$inputFile, "'")),
                     " | Processing stage ", state$stage, " (", stageMeanings[state$stage], ")"))
  })

  output$loadFile <- shiny::renderUI({
    if (state$stage == 1) {
      shiny::fileInput("inputFile", shiny::h5("Input file"), accept=c("image/png"))
      #state$stage <<- 2
    }
  })

  #' @importFrom graphics abline box mtext par points rasterImage text
  output$plot <- shiny::renderPlot({
    par(mar=rep(1, 4))
    asp <- if (is.null(state$image)) 1 else dim(state$image)[1] / dim(state$image)[2]
    plot(0:1, 0:1, type='n', asp=asp, xaxs="i", yaxs="i", axes=FALSE)
    box()
    if (is.null(state$image)) {
      text(0.2, 0.5, "No .png file has been selected yet.")
    } else {
      rasterImage(state$image, 0, 0, 1, 1, angle=-state$rotate, interpolate=FALSE)
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
                      shiny::showNotification(paste0("File '", state$inputFile$name, "' saved"))
  })

  shiny::observeEvent(input$plotHover, {
                      state$xhover <- input$plotHover$x
                      state$yhover <- input$plotHover$y
  })

  #' @importFrom stats lm predict
  shiny::observeEvent(input$click, {
                      dmsg("click with state$stage =", state$stage, "\n")
                      if (state$stage == 5) {
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
                        state$stage <- 13
                        dmsg("  defined state$yaxis$device[2] as ", state$yaxis$device[2], "\n")
                        shiny::showNotification("Now, start clicking points to digitize them.")
                      } else if (state$stage == 13) { # digitizing points
                        state$x$device <<- c(state$x$device, input$click$x)
                        state$y$device <<- c(state$y$device, input$click$y)
                        state$code <<- c(state$code, as.numeric(input$code))
                        dmsg("Next is input$code\n")
                        if (debugFlag) print(file=stderr(), input$code)
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

  shiny::observeEvent(input$rotate, {
    state$rotate <- input$rotate
  })

  #' @importFrom png readPNG
  shiny::observeEvent(input$inputFile, {
    state$inputFile <- input$inputFile
    state$image <- png::readPNG(state$inputFile$datapath)
    state$imageExists <- TRUE
    state$stage <- 2
    dmsg("state$imageExists=", state$imageExists)
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
    as.numeric(state$stage)
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
#' @export
imageDigitizer <- function()
{
    shiny::shinyApp(ui, server)        #) #options=list(test.mode=TRUE))
}

