## vim:textwidth=128:expandtab:shiftwidth=2:softtabstop=2

library(shiny)

library(png)

## options(shiny.error=browser)

ui <- fluidPage(h5("imager 0.1"),
                fluidRow(column(1, fileInput("inputFile", h5("Input file"), accept=c("image/png", ".png"))),
                         column(4, sliderInput("rotate", h5("Rotate [deg]"), min=-10, max=10, value=0, step=0.2),
                                offset=3),
                         column(1, radioButtons("grid", label=h5("Grid"),
                                                choices=c("Off"="off", "Fine"="fine", "Medium"="medium", "Coarse"="coarse"),
                                                selected="medium", inline=TRUE)),
                         column(1, actionButton("undo", "Undo")),
                         column(1, actionButton("save", "Save results"))),
                fluidRow(column(2, htmlOutput("status")),
                         column(10, plotOutput("plot", click="plotClick", hover="plotHover", height=600)))
                )

server <- function(input, output)
{
  state <- reactiveValues(rotate=0, inputFile=NULL, image=NULL, step=1,
                          x=list(device=NULL), y=list(device=NULL),
                          xaxis=list(user=NULL, device=NULL), yaxis=list(user=NULL, device=NULL),
                          xaxisModel=NULL, yaxisModel=NULL,
                          xhover=NULL, yhover=NULL)

  xAxisModal <- function(failed=FALSE)
  {
    modalDialog(textInput("xAxisValue", "Enter X at last mouse click"),
                footer=tagList(modalButton("Cancel"), actionButton("xAxisButtonOk", "OK")))
  }

  yAxisModal <- function(failed=FALSE)
  {
    modalDialog(textInput("yAxisValue", "Enter Y at last mouse click"),
                footer=tagList(modalButton("Cancel"), actionButton("yAxisButtonOk", "OK")))
  }

  output$plot <- renderPlot({
    par(mar=rep(0, 4))
    asp <- dim(p)[1] / dim(p)[2]
    plot(0:1, 0:1, type='n', asp=asp, xaxs="i", yaxs="i")
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
      if (length(state$x$device)) {
        points(state$x$device, state$y$device, pch=20, col="red")
      }
    }
  })

  observeEvent(input$undo, {
               if (length(state$x$device) > 0) {
                 state$x$device <- head(state$x$device, -1)
                 state$y$device <- head(state$y$device, -1)
               }
  })

  observeEvent(input$save, {
               file <- paste(gsub(".png$", "", state$inputFile$name), "_imager.dat", sep="")
               cat(paste("# file:", state$inputFile$name, "\n"), file=file)
               cat(paste("# rotation: ", state$rotate, "\n"), file=file, append=TRUE)
               cat(paste("# x axis device: ", paste(state$xaxis$device, collapse=" "), "\n"), file=file, append=TRUE)
               cat(paste("# x axis user: ", paste(state$xaxis$user, collapse=" "), "\n"), file=file, append=TRUE)
               cat(paste("# y axis device: ", paste(state$yaxis$device, collapse=" "), "\n"), file=file, append=TRUE)
               cat(paste("# y axis user: ", paste(state$yaxis$user, collapse=" "), "\n"), file=file, append=TRUE)
               cat("# i devicex devicey userx usery\n", file=file)
               xuser <- predict(state$xaxisModel, data.frame(device=state$x$device))
               yuser <- predict(state$yaxisModel, data.frame(device=state$y$device))
               for (i in seq_along(state$x$device)) {
                 cat(sprintf("%3d %10.3f %10.3f %20g %20g\n",
                             i, state$x$device[i], state$y$device[i], xuser[i], yuser[i]),
                     file=file, append=TRUE)
               }
  })

  observeEvent(input$plotHover, {
               state$xhover <- input$plotHover$x
               state$yhover <- input$plotHover$y
  })

  observeEvent(input$plotClick, {
               if (state$step == 2) {
                 state$xaxis$device <- c(state$xaxis$device, input$plotClick$x)
                 showModal(xAxisModal())
               } else if (state$step == 3) {
                 state$yaxis$device <- c(state$yaxis$device, input$plotClick$y)
                 showModal(yAxisModal())
               } else if (state$step == 4) {
                 state$x$device <- c(state$x$device, input$plotClick$x)
                 state$y$device <- c(state$y$device, input$plotClick$y)
               } else {
                 stop("programming error in plotClick (unknown state$step)")
               }
  })

  output$status <- renderText({
    if (is.null(state$inputFile)) {
      state$step <- 1
      return("<b>SETUP 1</b><br>Select a file, and possibly rotate it.")
    }
    if (length(state$xaxis$device) != 2) {
      state$step <- 2
      if (length(state$xaxis$device) == 0)
        return(paste("<b>", state$inputFile$name, "</b><br><b>SETUP 2A</b><br>Click a known point on the X axis."))
      else
        return(paste("<b>", state$inputFile$name, "</b><br><b>SETUP 2B</b><br>Click a second known point on the X axis."))
    }
    if (length(state$yaxis$device) != 2) {
      state$step <- 3
      if (length(state$yaxis$device) == 0)
        return(paste("<b>", state$inputFile$name, "</b><br><b>SETUP 3A</b><br>Click a known point on the Y axis."))
      else
        return(paste("<b>", state$inputFile$name, "</b><br><b>SETUP 3B</b><br>Click a second known point on the Y axis."))
    }
    state$step <- 4
    res <- paste("<b>", state$inputFile$name, "</b><br>", length(state$x$device), "points digitized")
    if (!is.null(state$xaxisModel) && !is.null(state$yaxisModel)) {
      xh <- predict(state$xaxisModel, data.frame(device=state$xhover))
      yh <- predict(state$yaxisModel, data.frame(device=state$yhover))
      res <- paste(res, sprintf("<br><br><i>%.3f %.3f</i>", xh, yh))
    }
    res
  })

  observeEvent(input$rotate, {
               state$rotate <- input$rotate
  })

  observeEvent(input$inputFile, {
               state$inputFile <- input$inputFile
               state$image <- readPNG(state$inputFile$datapath)
  })

  observeEvent(input$xAxisButtonOk, {
               state$xaxis$user <- c(state$xaxis$user, as.numeric(input$xAxisValue))
               if (length(state$xaxis$user) > 1)
                 state$xaxisModel <- lm(user ~ device, data=state$xaxis)
               removeModal()
  })

  observeEvent(input$yAxisButtonOk, {
               state$yaxis$user <- c(state$yaxis$user, as.numeric(input$yAxisValue))
               if (length(state$yaxis$user) > 1)
                 state$yaxisModel <- lm(user ~ device, data=state$yaxis)
               removeModal()
  })
}

shinyApp(ui, server) #options=list(test.mode=TRUE))
