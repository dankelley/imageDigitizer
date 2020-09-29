## vim:textwidth=128:expandtab:shiftwidth=2:softtabstop=2

library(shiny)
library(png)

## options(shiny.error=browser)
debugFlag <- TRUE                      # For console messages that trace control flow.
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

msg <- function(...)
{
  if (debugFlag)
    cat(file=stderr(), ...)
}

ui <- shiny::fluidPage(tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
                shiny::uiOutput(outputId="title"),
                #h5(paste("imager", version)),
                #fluidRow(column(1, checkboxInput("debug", h6("Debug"), value=!FALSE))),
                shiny::fluidRow(conditionalPanel(condition="output.stage == 1",
                                          column(3, shiny::uiOutput(outputId="readImage")),
                                          column(3, shiny::sliderInput("rotate", shiny::h5("Rotate [deg]"),
                                                                min=-10, max=10, value=0, step=0.2)),
                                          column(3, shiny::radioButtons("grid", label=shiny::h5("Grid"),
                                                                 choices=c("None"="off", "Fine"="fine", "Medium"="medium", "Coarse"="coarse"),
                                                                 selected="medium", inline=TRUE)),
                                          column(2, shiny::actionButton("stage1button", "Proceed to next stage")))),
                fluidRow(conditionalPanel(condition="output.stage == 3",
                                          column(3, shiny::textInput("xname", shiny::h5("Name horiz. axis"))),
                                          column(3, shiny::textInput("yname", shiny::h5("Name vert. axis"))))),
                fluidRow(column(2, shiny::actionButton("undo", "Undo")),
                         column(2, shiny::actionButton("save", "Save results"))),
                fluidRow(column(2, shiny::htmlOutput("status")),
                         column(10, shiny::plotOutput("plot", click="plotClick", hover="plotHover", height=600)))
                )

server <- function(input, output)
{
  state <- shiny::reactiveValues(step=1,
                          stage=1,
                          rotate=0,
                          inputFile=NULL,
                          image=NULL,
                          xname="x", yname="x",
                          x=list(device=NULL),
                          y=list(device=NULL),
                          xaxis=list(user=NULL, device=NULL),
                          yaxis=list(user=NULL, device=NULL),
                          xaxisModel=NULL, yaxisModel=NULL,
                          xhover=NULL, yhover=NULL)

  shiny::observeEvent(input$stage1button,
               {
                 state$stage <- 2
               }
  )
  
#  observeEvent(input$debug,
#               {
#                 if (!is.null(input$debug))
#                   debugFlag <- input$debug
#               }
#  )

  #'
  #' @importFrom shiny HTML modalDialog showModal
  shiny::observeEvent(input$keypressTrigger,
               {
                 if (state$step == 6) {
                   key <- intToUtf8(input$keypress)
                   msg("keypress numerical value ", input$keypress, ", i.e. key='", key, "'\n", sep="")
                   if (key == '+') {
                     msg("should zoom in now\n")
                   } else if (key == '-') {
                     msg("should zoom out now\n")
                   } else if (key == '0') {
                     msg("should unzoom now\n")
                   } else if (key == '?') {
                     shiny::showModal(shiny::modalDialog(title="", shiny::HTML(keypressHelp), easyClose=TRUE))
                   }
                 }
               }
  )




  xAxisModal <- function(failed=FALSE)
  {
    modalDialog(shiny::textInput("xAxisValue", "Enter x at last mouse click"),
                footer=shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("xAxisButtonOk", "OK")))
  }
  xAxisNameModal <- function(failed=FALSE)
  {
    modalDialog(shiny::textInput("xAxisName", "Enter name of x axis"),
                footer=shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("xAxisNameButtonOk", "OK")))
  }
  yAxisModal <- function(failed=FALSE)
  {
    shiny::modalDialog(shiny::textInput("yAxisValue", "Enter y at last mouse click"),
                footer=shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("yAxisButtonOk", "OK")))
  }
  yAxisNameModal <- function(failed=FALSE)
  {
    shiny::modalDialog(shiny::textInput("yAxisName", "Enter name of y axis"),
                footer=shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("yAxisNameButtonOk", "OK")))
  }

  output$title <- shiny::renderUI({
    shiny::h5(paste0("imager ", version, ifelse(is.null(state$inputFile), "", paste0(" (", state$inputFile, ")")))) 
  })

  output$readImage <- shiny::renderUI({
    shiny::fileInput("inputFile", shiny::h5("Input file"), accept=c("image/png"))
  })

  #' @importFrom graphics abline box par points rasterImage text
  output$plot <- shiny::renderPlot({
    par(mar=rep(1, 4))
    asp <- if (is.null(state$image)) 1 else dim(state$image)[1] / dim(state$image)[2]
    plot(0:1, 0:1, type='n', asp=asp, xaxs="i", yaxs="i", axes=FALSE)
    box()
    if (is.null(state$image)) {
      text(0.2, 0.5, "No .png file has been selected yet.")
    } else {
      rasterImage(state$image, 0, 0, 1, 1, angle=-state$rotate)
      if (input$grid == "fine") {
        abline(h=seq(-3, 3, 0.05), col='magenta', lty="dotted")
        abline(v=seq(-3, 3, 0.05*asp), col='magenta', lty="dotted")
      } else if (input$grid == "medium") {
        abline(h=seq(-3, 3, 0.1), col='magenta', lty="dotted")
        abline(v=seq(-3, 3, 0.1*asp), col='magenta', lty="dotted")
      } else if (input$grid == "coarse") {
        abline(h=seq(-3, 3, 0.2), col='magenta', lty="dotted")
        abline(v=seq(-3, 3, 0.2*asp), col='magenta', lty="dotted")
      }
      ## if (input$guides == "On") {
      ##   if (length(state$xaxis$user)) {
      ##     abline(v=state$xaxis$device, col='blue')
      ##     mtext(state$xaxis$user, side=1, col='blue', at=state$xaxis$device, line=0)
      ##   }
      ##   if (length(state$yaxis$user)) {
      ##     abline(h=state$yaxis$device, col='blue')
      ##     mtext(state$yaxis$user, side=2, col='blue', at=state$yaxis$device, line=0)
      ##   }
      ## }
      if (length(state$x$device)) {
        points(state$x$device, state$y$device, pch=20, col="red")
      }
    }
  })

  #' @importFrom utils head
  shiny::observeEvent(input$undo, {
               if (length(state$x$device) > 0) {
                 state$x$device <- head(state$x$device, -1)
                 state$y$device <- head(state$y$device, -1)
               }
  })

  shiny::observeEvent(input$save, {
               file <- paste(gsub(".png$", "", state$inputFile$name), "_imager.dat", sep="")
               cat(paste("# imager version ", version, "\n", sep=""), file=file)
               cat(paste("# file:", state$inputFile$name, "\n", sep=""), file=file, append=TRUE)
               cat(paste("# rotation: ", state$rotate, "\n", sep=""), file=file, append=TRUE)
               if (length(state$xaxis$device)) {
                 cat(paste("# x axis device: ", paste(state$xaxis$device, collapse=" "), "\n", sep=""), file=file, append=TRUE)
                 cat(paste("# x axis user: ", paste(state$xaxis$user, collapse=" "), "\n", sep=""), file=file, append=TRUE)
                 cat(paste("# y axis device: ", paste(state$yaxis$device, collapse=" "), "\n", sep=""), file=file, append=TRUE)
                 cat(paste("# y axis user: ", paste(state$yaxis$user, collapse=" "), "\n", sep=""), file=file, append=TRUE)
               }
               if (nchar(state$xname) < 1)
                 state$xname <- "x"
               if (nchar(state$yname) < 1)
                 state$yname <- "y"
               cat(sprintf("i,devicex,devicey,%s,%s\n", state$xname, state$yname), file=file, append=TRUE)
               xuser <- predict(state$xaxisModel, data.frame(device=state$x$device))
               yuser <- predict(state$yaxisModel, data.frame(device=state$y$device))
               for (i in seq_along(state$x$device)) {
                 cat(sprintf("%3d,%10.3f,%10.3f,%20g,%20g\n",
                             i, state$x$device[i], state$y$device[i], xuser[i], yuser[i]),
                     file=file, append=TRUE)
               }
               shiny::showNotification(paste0("File '", file, "' saved"))
  })

  shiny::observeEvent(input$plotHover, {
               state$xhover <- input$plotHover$x
               state$yhover <- input$plotHover$y
  })

  shiny::observeEvent(input$plotClick, {
               if (state$step == 2) {
                 state$xaxis$device <- c(state$xaxis$device, input$plotClick$x)
                 shiny::showModal(xAxisModal())
               } else if (state$step == 3) {
                 shiny::showModal(xAxisNameModal())
               } else if (state$step == 4) {
                 state$yaxis$device <- c(state$yaxis$device, input$plotClick$y)
                 shiny::showModal(yAxisModal())
               } else if (state$step == 5) {
                 shiny::showModal(yAxisNameModal())
               } else if (state$step == 6) {
                 state$x$device <- c(state$x$device, input$plotClick$x)
                 state$y$device <- c(state$y$device, input$plotClick$y)
               } else {
                 stop("programming error in plotClick (unknown state$step)")
               }
  })

  output$status <- shiny::renderText({
    if (is.null(state$inputFile)) {
      state$step <- 1
      return(paste0("<b>SETUP 1</b><br>Select a file, and possibly rotate it. (state$stage=", state$stage, ")"))
    }
    if (length(state$xaxis$device) != 2) {
      state$step <- 2
      if (length(state$xaxis$device) == 0)
        return(paste("<b>SETUP 2A</b><br>Click a known point on the X axis."))
      else
        return(paste("<br><b>SETUP 2B</b><br>Click a second known point on the X axis."))
    }
    if (is.null(state$xname)) {
      state$step <- 3
      return(paste("<b>", state$inputFile$name, "</b><br><b>SETUP 2C</b><br>Enter name of x axis."))
    }
    if (length(state$yaxis$device) != 2) {
      state$step <- 4
      if (length(state$yaxis$device) == 0)
        return(paste("<b>", state$inputFile$name, "</b><br><b>SETUP 3A</b><br>Click a known point on the Y axis."))
      else
        return(paste("<b>", state$inputFile$name, "</b><br><b>SETUP 3B</b><br>Click a second known point on the Y axis."))
    }
    if (is.null(state$yname)) {
      state$step <- 5
      return(paste("<b>", state$inputFile$name, "</b><br><b>SETUP 3C</b><br>Enter name of y axis."))
    }
    state$step <- 6
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
               msg("state$imageExists=", state$imageExists)
  })

  shiny::observeEvent(input$xname, { state$xname <- input$xname} )

  shiny::observeEvent(input$yname, { state$yname <- input$yname} )

  #' @importFrom stats lm predict
  shiny::observeEvent(input$xAxisButtonOk, {
               state$xaxis$user <- c(state$xaxis$user, as.numeric(input$xAxisValue))
               if (length(state$xaxis$user) > 1) {
                 state$xaxisModel <- lm(user ~ device, data=state$xaxis)
                 state$step <- 3
               }
               shiny::removeModal()
  })

  shiny::observeEvent(input$xAxisNameButtonOk, {
               state$xname <- input$xAxisName
               shiny::removeModal()
  })

  shiny::observeEvent(input$yAxisButtonOk, {
               state$yaxis$user <- c(state$yaxis$user, as.numeric(input$yAxisValue))
               if (length(state$yaxis$user) > 1) {
                 state$yaxisModel <- lm(user ~ device, data=state$yaxis)
                 state$step <- 5
               }
               shiny::removeModal()
  })

  shiny::observeEvent(input$yAxisNameButtonOk, {
               state$yname <- input$yAxisName
               shiny::removeModal()
  })

  output$stage <- shiny::reactive({
    msg("in output$stage reactive; returning ", state$stage, " (",ifelse(is.numeric(state$stage),"numeric","not numeric"), ")\n", sep="")
    state$stage
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
  shiny::shinyApp(ui, server) #options=list(test.mode=TRUE))
}

