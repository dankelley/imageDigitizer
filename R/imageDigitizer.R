# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(png)

options(shiny.error=browser)
stageMeanings <- c("Input file",       # stage  1
    "Rotate image",                    # stage  2
    "Enter axis names",                # stage  3
    "Enter x limits",                  # stage  4
    "Click lower y limit",             # stage  5 (recognized during click processing)
    "Click upper y limit",             # stage  6 (recognized during click processing)
    "Enter y limits",                  # stage  7
    "Click lower y limit",             # stage  8 (recognized during click processing)
    "Click upper y limit",             # stage  9 (recognized during click processing)
    "Digitize Points")                 # stage 10

col <- list(axes="magenta", grid="blue")

version <- "0.1.6"
keypressHelp <- "
<i>Keystroke interpretation</i>
<ul>
<li> '<b>p</b>': toggle printing of debugging information to the R console
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

debugFlag <- TRUE                      # For console messages that trace control flow.

dmsg <- function(...)
{
    if (debugFlag)
        cat(file=stderr(), ...)
}

fileLoaded <- FALSE

ui <- fluidPage(tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
    style="margin-left:2ex",
    uiOutput(outputId="title"),
    uiOutput(outputId="status"),
    uiOutput(outputId="loadFile"),
    uiOutput(outputId="grid"),
    uiOutput(outputId="rotateImage"),
    uiOutput(outputId="enterAxisNames"),
    uiOutput(outputId="enterXLimits"),
    uiOutput(outputId="enterYLimits"),
    uiOutput(outputId="showImage"))

server <- function(input, output)
{
    state <- shiny::reactiveValues(
        stage=1,
        shearx=0,
        sheary=0,
        rotate=0,
        inputFile=NULL,
        image=NULL,
        xname="x",
        yname="y",
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

    observeEvent(input$Rcode, {
        ofile <- paste(gsub(".png$", "", state$inputFile$name), "_imageDigitizer.dat", sep="")
        msg <- "# Sample code to read and plot the saved data file<br>\n"
        msg <- paste0(msg, "data <- read.csv(file=\"", ofile, "\", skip=7, header=TRUE)<br>\n")
        msg <- paste0(msg, "plot(", "data[[\"", state$xname, "\"]],", "data[[\"", state$yname, "\"]],", "xlab=\"", state$xname, "\",", "ylab=\"", state$yname, "\",", "pch=data$code)<br>\n")
        showModal(modalDialog(HTML(msg), title="R code", size="l"))
        })

    observeEvent(input$quit, {
        saveFile()
        stopApp()
        })

    output$loadFile <- renderUI({
        if (state$stage == 1L)
            insertUI("loadAFile", ui=fileInput("inputFile", h5("Input file"), accept=c("image/png")))
    })

    output$showImage <- renderUI({
        if (state$stage > 1L)
            fluidRow(plotOutput("plot", click="click", hover="plotHover", height=600))
    })

    output$grid <- renderUI({
        if (state$stage > 1L)
            fluidRow(radioButtons("grid", label=h5("Grid"),
                    choices=c("None"="off", "Fine"="fine", "Medium"="medium", "Coarse"="coarse"),
                    selected="medium", inline=TRUE))
    })

    output$rotateImage <- renderUI({
        if (state$stage == 2L) {
            fluidRow(
                column(10, sliderInput("rotate", h5("Rotate Image [degrees]"), min=-20, max=20, value=0, step=0.05)),
                column(2, fluidRow(actionButton("finishedRotation", "Done"))))
        }
    })

    observeEvent(input$finishedRotation, {
        state$stage <- 3L            # prepare for next
        dmsg("clicked finishedRotation button, so setting state$stage to ", state$stage, ". Note: state$rotate=", state$rotate, " deg\n", sep="")
    })

    output$enterAxisNames<- renderUI({
        dmsg("in output$enterAxisNames (state$stage=", state$stage, ")\n", sep="")
        if (state$stage == 3L) {
            fluidRow(
                column(4, textInput("xname", h5("Name x axis"))),
                column(4, textInput("yname", h5("Name y axis"))),
                actionButton("finishedGetAxisNames", "Done"))
        }
    })

    observeEvent(input$finishedGetAxisNames, { # at stage 4 (invisible to user)
        state$xname <- input$xname
        state$yname <- input$yname
        state$stage <- 4L            # prepare for next
        dmsg("clicked finishedGetAxisNames button  (state$xname=\"", state$xname, "\" and state$yname=\"", state$yname, "\"; set state$stage=", state$stage, ")\n", sep="")
        #showNotification(paste0("Please click the mouse where x=", state$xaxis$user[1], ", then where x=", state$xaxis$user[2]))
    })

    output$enterXLimits <- renderUI({
        if (state$stage == 4L) {
            dmsg("in output$enterXLimits (state$stage=", state$stage, ")\n", sep="")
            fluidRow(
                column(4, textInput("xlow", h5(paste(state$xname, "low")))),
                column(4, textInput("xhigh", h5(paste(state$xname, "high")))),
                actionButton("finishedGetXLimits", "Done"))
        }
    })
 
    observeEvent(input$finishedGetXLimits, { # at stage 5 (which is noticed by output$click, which also catches stage 6)
        state$xaxis$user <- as.numeric(c(input$xlow, input$xhigh))
        dmsg("clicked finishedGetXLimits button  (state$stage=", state$stage, ")\n", sep="")
        state$stage <- 5L
        showNotification(paste0("Please click the mouse where x=", state$xaxis$user[1], ", and then where x=", state$xaxis$user[2]))
    })

    output$enterYLimits <- renderUI({
        if (state$stage == 7L) {
            dmsg("in output$enterYLimits (state$stage=", state$stage, ")\n", sep="")
            fluidRow(
                column(4, textInput("ylow", h5(paste(state$yname, "low")))),
                column(4, textInput("yhigh", h5(paste(state$yname, "high")))),
                actionButton("finishedGetYLimits", "Done"))
        }
    })
 
    observeEvent(input$finishedGetYLimits, { # sets state$stage to 8 (which is noticed by output$click, which also forms stage 9)
        state$yaxis$user <- as.numeric(c(input$ylow, input$yhigh))
        dmsg("clicked finishedGetYLimits button  (state$stage=", state$stage, ")\n", sep="")
        state$stage <- 8L
        showNotification(paste0("Please click the mouse where y=", state$yaxis$user[1], ", and then where y=", state$yaxis$user[2]))
    })


  #> output$defineXScale <- renderUI({
  #>     if (state$stage == 4L) {
  #>         fluidRow(
  #>             column(4, textInput("xleft", shiny::h5("Value at left"))),
  #>             column(4, textInput("xright", shiny::h5("Value at right"))),
  #>             column(2, fluidRow(actionButton("finishedXLimits", "Done"))))
  #>     }
  #> })

  #> shiny::observeEvent(input$finishedXLimits, {
  #>     state$xaxis$user <- as.numeric(c(input$xleft, input$xright))
  #>     state$stage <- 5L
  #>     dmsg("clicked finishedXLimits button, so setting state$stage to ", state$stage, ". Note: state$xaxis$user=c(", state$xaxis$user[1], ", ", state$xaxis$user[2], ")\n", sep="")
  #> })


    observeEvent(input$keypressTrigger, {
        if (key == 'd') {
            debugFlag <- !debugFlag
        } else if (state$stage > 9L) {
            key <- intToUtf8(input$keypress)
            dmsg("keypress numerical value ", input$keypress, ", i.e. key='", key, "'\n", sep="")
            if (key == 'd') {
                debugFlag <- !debugFlag
            } else if (key == '+') {
                dmsg("should zoom in now\n")
            } else if (key == '-') {
                dmsg("should zoom out now\n")
            } else if (key == '0') {
                dmsg("should unzoom now\n")
            } else if (key == '?') {
                showModal(modalDialog( title="", HTML(keypressHelp), easyClose=TRUE))
            }
        }
    })

    #<old> xAxisModal <- function(failed=FALSE) # FIXME: unused (but looks useful)
    #<old> {
    #<old>     modalDialog(
    #<old>         textInput("xAxisValue", "Enter x at last mouse click"),
    #<old>         footer=tagList(modalButton("Cancel"), actionButton("xAxisButtonOk", "OK"))
    #<old>         )
    #<old> }

    #<old>xAxisNameModal <- function(failed=FALSE)
    #<old>{
    #<old>  modalDialog(
    #<old>    shiny::textInput("xAxisName", "Enter name of x axis"),
    #<old>    footer=shiny::tagList(
    #<old>      shiny::modalButton("Cancel"),
    #<old>      shiny::actionButton("xAxisNameButtonOk", "OK")
    #<old>    )
    #<old>  )
    #<old>}
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
    #<old> yAxisNameModal <- function(failed=FALSE)
    #<old> {
    #<old>   shiny::modalDialog(
    #<old>     shiny::textInput("yAxisName", "Enter name of y axis"),
    #<old>     footer=shiny::tagList(
    #<old>       shiny::modalButton("Cancel"),
    #<old>       shiny::actionButton("yAxisNameButtonOk", "OK")
    #<old>     )
    #<old>   )
    #<old> }

    output$title <- shiny::renderUI({
        h5(paste0("imageDigitizer ", version,
                ifelse(is.null(state$inputFile), "", paste0(" | File '", state$inputFile, "'")),
                " | Processing stage ", state$stage, " (", stageMeanings[state$stage], ")"))
    })

    #> output$status <- shiny::renderUI({
    #>     if (state$stage > 1L)
    #>         shiny::h5(sprintf("<%.3f %.3f>", input$plotHover$x, input$plotHover$y))
    #> })


    output$loadFile <- shiny::renderUI({
        if (state$stage == 1) {
            fileInput("inputFile", h5("Input file"), accept=c("image/png"))
        }
    })

    #' @importFrom graphics abline box mtext par points rasterImage text
    #' @importFrom magick image_shear
    output$plot <- renderPlot({
        par(mar=rep(1, 4))
        idim <- dim(state$image[[1]])
        asp <- if (is.null(state$image)) 1 else idim[3] / idim[2] # FIXME: or reciprocal?
        dmsg("image dimension=", idim[2], "x", idim[3], "; plot aspect ratio=", asp, "\n", sep="")
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
                I <- magick::image_rotate(I, state$rotate)
            }
            rasterImage(I, 0, 0, 1, 1, interpolate=FALSE)
            ##rasterImage(state$image, 0, 0, 1, 1, angle=-state$rotate, interpolate=FALSE)
            #dmsg("rotate=", state$rotate, ", shearx=", state$shearx, ", sheary=", state$sheary, "\n", sep="")
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
    observeEvent(input$undo, {
        if (length(state$x$device) > 0) {
            state$x$device <- head(state$x$device, -1)
            state$y$device <- head(state$y$device, -1)
            state$code <- head(state$code, -1)
        }
    })

    observeEvent(input$save, {
        saveFile()
        showNotification(paste0("File '", state$inputFile$name, "' saved"), type="message")
    })

    observeEvent(input$plotHover, {
        state$xhover <- input$plotHover$x
        state$yhover <- input$plotHover$y
    })

    #' @importFrom stats lm predict
    observeEvent(input$click, {
        dmsg("click with state$stage =", state$stage, "\n")
        if (state$stage == 5L) {
            state$xaxis$device[1] <- input$click$x
            state$stage <- 6L            # prepare for next
            dmsg("  defined state$xaxis$device[1] as ", state$xaxis$device[1], "\n")
        } else if (state$stage == 6L) {
            state$xaxis$device[2] <- input$click$x
            state$stage <- 7L            # prepare for next
            dmsg("  defined state$xaxis$device[2] as ", state$xaxis$device[2], "\n")
        } else if (state$stage == 8L) {
            state$yaxis$device[1] <- input$click$y
            state$stage <- 9L            # prepare for next
            dmsg("  defined state$yaxis$device[1] as ", state$yaxis$device[1], "\n")
        } else if (state$stage == 9L) {
            state$yaxis$device[2] <- input$click$y
            state$stage <- 10L           # prepare for next: digitize points on graph
            dmsg("  defined state$yaxis$device[2] as ", state$yaxis$device[2], "\n")
            dmsg("**FIXME: do lm() here**\n")
            #> } else if (state$stage == 7) {
            #>     state$xaxis$device <<- c(state$xaxis$device, input$click$x)
            #>     dmsg("about to do lm() with state$xaxis as follows\n")
            #>     if (debugFlag) print(file=stderr(), state$xaxis)
            #>     state$xaxisModel <<- lm(user ~ device, data=state$xaxis)
            #>     dmsg("next is state$xaxisModel\n")
            #>     if (debugFlag) print(file=stderr(), state$xaxisModel)
            #>     state$stage <- 8
            #>     dmsg("  defined state$xaxis$device[2] as ", state$xaxis$device[2], "\n")
            #> } else if (state$stage == 10) {
            #>     state$yaxis$device <<- input$click$y
            #>     state$stage <- 11
            #>     dmsg("  defined state$yaxis$device[1] as ", state$yaxis$device[1], "\n")
            #> } else if (state$stage == 12) {
            #>     state$yaxis$device <<- c(state$yaxis$device, input$click$y)
            #>     dmsg("about to do lm() with state$yaxis as follows\n")
            #>     if (debugFlag) print(file=stderr(), state$yaxis)
            #>     state$yaxisModel <<- lm(user ~ device, data=state$yaxis)
            #>     dmsg("next is state$yaxisModel\n")
            #>     if (debugFlag) print(file=stderr(), state$yaxisModel)
            #>     state$stage <- 13
            #>     dmsg("  defined state$yaxis$device[2] as ", state$yaxis$device[2], "\n")
            #>     shiny::showNotification("Now, start clicking points to digitize them.")
        } else if (state$stage == 10L) { # digitizing points
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

    output$status <- renderText({
        if (!is.null(state$inputFile)) {
            res <- paste("<b>", state$inputFile$name, "</b><br>", length(state$x$device), "points digitized")
            if (!is.null(state$xaxisModel) && !is.null(state$yaxisModel)) {
                xh <- predict(state$xaxisModel, data.frame(device=state$xhover))
                yh <- predict(state$yaxisModel, data.frame(device=state$yhover))
                res <- paste(res, sprintf("<br><br><i>%.3f %.3f</i>", xh, yh))
            }
            res
        }
        NULL
    })

    ## Image transformations chosen by user to establish orthogonal x and y axes
    observeEvent(input$rotate, { state$rotate <- input$rotate })
    observeEvent(input$shearx, { state$shearx <- input$shearx })
    observeEvent(input$sheary, { state$sheary <- input$sheary })

    ## @importFrom png readPNG
    #' @importFrom magick image_read
    observeEvent(input$inputFile, {
        state$inputFile <- input$inputFile
        ##state$image <- png::readPNG(state$inputFile$datapath)
        state$image <- magick::image_read(state$inputFile$datapath)
        # Make the 'File' GUI element disappear, and make 'Rotation' appear
        state$stage <- 2 # means stop displaying 'File' UI, and instead display 'Rotate' UI
    })

    observeEvent(input$xname, {
        state$xname <- input$xname
    })

    observeEvent(input$yname, {
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

    #<old>output$stage <- shiny::reactive({
    #<old>  dmsg("output$stage is returning ", state$stage, " based on state$stage\n", sep="")
    #<old>  as.numeric(state$stage)
    #<old>})

    outputOptions(output, "stage", suspendWhenHidden=FALSE)

    output$readImage <- shiny::renderUI({
        fileInput("inputFile", h5("Please select an input file"), accept=c("image/png"))
    })

}

#' R/shiny app for digitizing points in images.
#'
#' A shiny graphical user interface (GUI) for digitizing points in images, by
#' means of mouse clicks. The GUI is meant to be reasonably self-explanatory.
#' @importFrom shiny actionButton column fileInput fluidRow h5 HTML insertUI modalDialog observeEvent outputOptions plotOutput radioButtons
#' renderPlot renderText renderUI showModal showNotification sliderInput stopApp textInput
#' 
#' @export
imageDigitizer <- function()
{
    shiny::shinyApp(ui, server)        #) #options=list(test.mode=TRUE))
}

