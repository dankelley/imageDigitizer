# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(png)

debugFlagDefault <- FALSE              # For console messages that trace control flow.
colPoints <- 2                         # FIXME: let user specify this
cexPoints <- 1                         # FIXME: let user specify this

options(shiny.error=browser)
stageMeanings <- c("Input file",       # stage  1
    "Rotate image",                    # stage  2
    "Enter axis names",                # stage  3
    "Enter x limits",                  # stage  4
    "Click lower x limit",             # stage  5 (recognized during click processing)
    "Click upper x limit",             # stage  6 (recognized during click processing)
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
<li> '<b>+</b>': zoom in, centred on mouse location [FIXME: implement this]
<li> '<b>-</b>': zoom out [FIXME: implement this]
<li> '<b>0</b>': unzoom [FIXME: implement this]
</ul>
"

fileLoaded <- FALSE

ui <- fluidPage(tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
    style="margin-left:2ex",
    uiOutput(outputId="title"),
    uiOutput(outputId="loadFile"),
    uiOutput(outputId="grid"),
    uiOutput(outputId="rotateImage"),
    uiOutput(outputId="enterAxisNames"),
    uiOutput(outputId="enterXLimits"),
    uiOutput(outputId="enterYLimits"),
    uiOutput(outputId="undoSaveCodeQuit"),
    uiOutput(outputId="showImage"))

server <- function(input, output)
{
    debugFlag <- debugFlagDefault
    dmsg <- function(...)
    {
        if (debugFlag)
            cat(file=stderr(), ...)
    }
    state <- reactiveValues(
        stage=1,
        shearx=0,
        sheary=0,
        rotate=0,
        inputFile=NULL,
        image=NULL,
        xname="x",
        yname="y",
        xdevice=NULL,
        ydevice=NULL,
        code=NULL,
        xaxis=list(user=NULL, device=NULL, slope=NULL, user0=NULL, device0=NULL),
        yaxis=list(user=NULL, device=NULL, slope=NULL, user0=NULL, device0=NULL))

    saveFile <- function()
    {
        file <- paste(gsub(".png$", "", state$inputFile$name), "_imageDigitizer.dat", sep="")
        cat(paste("# imageDigitizer: ", version,              "\n", sep=""), file=file)
        cat(paste("# file:           ", state$inputFile$name, "\n", sep=""), file=file, append=TRUE)
        cat(paste("# rotation:       ", state$rotate,         "\n", sep=""), file=file, append=TRUE)
        cat(paste("# xaxis$user0:    ", state$xaxis$user0,    "\n", sep=""), file=file, append=TRUE)
        cat(paste("# xaxis$device0:  ", state$xaxis$device0,  "\n", sep=""), file=file, append=TRUE)
        cat(paste("# xaxis$slope:    ", state$xaxis$slope,    "\n", sep=""), file=file, append=TRUE)
        cat(paste("# yaxis$user0:    ", state$yaxis$user0,    "\n", sep=""), file=file, append=TRUE)
        cat(paste("# yaxis$device0:  ", state$yaxis$device0,  "\n", sep=""), file=file, append=TRUE)
        cat(paste("# yaxis$slope:    ", state$yaxis$slope,    "\n", sep=""), file=file, append=TRUE)
        cat(sprintf("i,devicex,devicey,%s,%s,code\n", state$xname, state$yname), file=file, append=TRUE)
        if (length(state$xaxis$device)) {
            x <- state$xaxis$user0 + state$xaxis$slope * (state$xdevice - state$xaxis$device0)
            y <- state$yaxis$user0 + state$yaxis$slope * (state$ydevice - state$yaxis$device0)
            for (i in seq_along(state$xdevice)) {
                cat(sprintf("%d,%.4f,%.4f,%.4g,%.4g,%d\n",
                        i, state$xdevice[i], state$ydevice[i], x[i], y[i],state$code[i]), file=file, append=TRUE)
            }
        }
        file
    }

    output$loadFile <- renderUI({
        if (state$stage == 1L)
            insertUI("loadAFile", ui=fileInput("inputFile", h5("Input file"), accept=c("image/png")))
    })

    output$showImage <- renderUI({
        if (state$stage > 1L)
            fluidRow(plotOutput("plot", click="click", hover="plotHover", height=600))
    })

    output$grid <- renderUI({
        if (state$stage == 2L)
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
        if (state$stage == 3L) {
            dmsg("in output$enterAxisNames (state$stage=", state$stage, ")\n", sep="")
            fluidRow(
                column(4, textInput("xname", h5("Name x axis"), state$xname)),
                column(4, textInput("yname", h5("Name y axis"), state$yname)),
                actionButton("finishedGetAxisNames", "Done"))
        }
    })

    observeEvent(input$finishedGetAxisNames, { # at stage 4 (invisible to user)
        # refuse to accept zero-length names, retaining defaults ('x' and 'y') if so
        if (nchar(input$xname))
            state$xname <- input$xname
        if (nchar(input$yname))
            state$yname <- input$yname
        dmsg("clicked finishedGetAxisNames button  (state$xname=\"", state$xname, "\" and state$yname=\"", state$yname, "\"; set state$stage=", state$stage, ")\n", sep="")
        state$stage <- 4L            # prepare for next
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

    # FIXME: move the pch choices to an item of its own (for clarity and in case we want to put it in a submenu)
    output$undoSaveCodeQuit <- renderUI({
        if (state$stage == 1L) { # 10L
            dmsg("in output$undoSaveCodeQuit (state$stage=", state$stage, ")\n", sep="")
            # See https://github.com/dankelley/imageDigitizer/issues/8 for the pch-selector method.
            pchChoices <- paste(sapply(0:25, function(i)
                    {
                        if (i == 1L) {
                            sprintf('<label class="radio-inline">
                                <input type="radio" name="pch" value="%d" checked="checked"/>
                                <span> <img src="/pch_%02d.png" alt="%d"/></span>
                                </label>',i, i, i)
                        } else {
                            sprintf('<label class="radio-inline">
                                <input type="radio" name="pch" value="%d"/>
                                <span> <img src="/pch_%02d.png" alt="%d"/></span>
                                </label>',i, i, i)
                        }
                    }),
                collapse="\n")
            fluidRow(
                actionButton("undoButton", "Undo"),
                actionButton("saveButton", "Save"),
                actionButton("codeButton", "Code"),
                actionButton("quitButton", "Quit"),
                column(width=12,
                    tags$div(HTML(paste('<div id="pch" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline"> <label class="control-label" for="pch">Plot symbol</label> <div class="shiny-options-group">', pchChoices, '</div> </div>')),
                        br(),
                        h3(textOutput('selected'))
                        )
                    )
                )
        }
    })

    #' @importFrom utils head
    observeEvent(input$undoButton, {
        if (length(state$xdevice) > 0) {
            state$xdevice <- head(state$xdevice, -1)
            state$ydevice <- head(state$ydevice, -1)
            state$code <- head(state$code, -1)
        }
    })

    observeEvent(input$saveButton, {
        name <- saveFile()
        showNotification(paste0("File '", name, "' saved"), type="message", duration=1)
    })

    observeEvent(input$codeButton, {
        ofile <- paste(gsub(".png$", "", state$inputFile$name), "_imageDigitizer.dat", sep="")
        msg <- "# Sample code to read and plot the saved data file<br>\n"
        msg <- paste0(msg, "data <- read.csv(file=\"", ofile, "\", skip=9, header=TRUE)<br>\n")
        msg <- paste0(msg, "plot(", "data[[\"", state$xname, "\"]],", "data[[\"", state$yname, "\"]],", "xlab=\"", state$xname, "\",", "ylab=\"", state$yname, "\",", "pch=data$code)<br>\n")
        showModal(modalDialog(HTML(msg), title="R code", size="l"))
    })

    observeEvent(input$quitButton, {
        saveFile()
        stopApp()
    })

    observeEvent(input$keypressTrigger, {
        key <- intToUtf8(input$keypress)
        if (key == 'd') {
            debugFlag <<- !debugFlag
        } else if (state$stage > 9L) {
            #dmsg("keypress numerical value ", input$keypress, ", i.e. key='", key, "'\n", sep="")
            if (key == 'd') {
                debugFlag <- !debugFlag
                cat(file=stderr(), "now, debugFlag=", debugFlag, "\n")
            } else if (key == '+') {
                dmsg("should zoom in now\n")
            } else if (key == '-') {
                dmsg("should zoom out now\n")
            } else if (key == '0') {
                dmsg("should unzoom now\n")
            } else if (key == '?') {
                showModal(modalDialog(title="", HTML(keypressHelp), easyClose=TRUE))
            }
        }
    })

    output$title <- renderUI({
        msg <- paste0("imageDigitizer ", version)
        msg <- paste0(msg, "(", input$pch, ")")
        if (!is.null(state$inputFile)) {
            msg <- paste0(msg, " | File '", state$inputFile$name, "'")
            if (state$stage < 10) {
                msg <- paste0(msg, " | Step ", state$stage, " (", stageMeanings[state$stage], ")")
            } else {
                npts <- length(state$xdevice)
                msg <- paste0(msg, " | Digitized ", npts, if (npts!=1L) " points" else " point")
                if (!is.null(input$plotHover$x)) {
                    msg <- paste0(msg,
                        sprintf(" | Hover: %s=%.3g, %s=%.3g",
                            state$xname,
                            with(state$xaxis, user0+slope*(input$plotHover$x-device0)),
                            state$yname,
                            with(state$yaxis, user0+slope*(input$plotHover$y-device0))))
                }
            }
        }
        return(msg)
    })

    output$loadFile <- renderUI({
        if (state$stage == 1) {
            fileInput("inputFile", h5("Input file"), accept=c("image/png"))
        }
    })

    #' @importFrom graphics abline box mtext par points rasterImage text
    #' @importFrom magick image_rotate image_shear
    output$plot <- renderPlot({
        par(mar=rep(1, 4))
        idim <- dim(state$image[[1]])
        asp <- if (is.null(state$image)) 1 else idim[3] / idim[2] # FIXME: or reciprocal?
        #dmsg("image dimension=", idim[2], "x", idim[3], "; plot aspect ratio=", asp, "\n", sep="")
        plot(0:1, 0:1, type='n', asp=asp, xaxs="i", yaxs="i", axes=FALSE)
        box()
        if (is.null(state$image)) {
            text(0.2, 0.5, "Please select a png file.")
        } else {
            I <- state$image
            #<shear> if (state$shearx > 0) {
            #<shear>     dmsg("Performing shearx\n")
            #<shear>     I <- magick::image_shear(I, sprintf("%.0fx0", round(state$shearx)))
            #<shear> }
            #<shear> if (state$sheary > 0) {
            #<shear>     dmsg("Performing sheary\n")
            #<shear>     I <- magick::image_shear(I, sprintf("0x%.0f", round(state$sheary)))
            #<shear> }
            if (state$rotate != 0) {
                I <- image_rotate(I, state$rotate)
            }
            rasterImage(I, 0, 0, 1, 1, interpolate=FALSE)
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
            if (length(state$xdevice)) {
                points(state$xdevice, state$ydevice, pch=state$code, col=colPoints, cex=cexPoints)
            }
        }
    })

    observeEvent(input$save, {
        saveFile()
    })

    observeEvent(input$click, {
        dmsg("click with state$stage =", state$stage, "\n")
        if (state$stage == 5L) {
            state$xaxis$device[1] <- input$click$x
            state$stage <- 6L            # prepare for next
            dmsg("defined state$xaxis$device[1] as ", state$xaxis$device[1], "\n")
        } else if (state$stage == 6L) {
            state$xaxis$device[2] <- input$click$x
            state$stage <- 7L            # prepare for next
            dmsg("defined state$xaxis$device[2] as ", state$xaxis$device[2], "\n")
        } else if (state$stage == 8L) {
            state$yaxis$device[1] <- input$click$y
            state$stage <- 9L            # prepare for next
            dmsg("defined state$yaxis$device[1] as ", state$yaxis$device[1], "\n")
        } else if (state$stage == 9L) {
            state$yaxis$device[2] <- input$click$y
            dmsg("defined state$yaxis$device[2] as ", state$yaxis$device[2], "\n")
            # Save 3 items (built up from 2) to make it easier to code equal-scale cases
            # We will later use e.g.
            # x <- with(state$xaxis, user0+slope*(input$mouse$x-device0))
            if (all(is.finite(state$xaxis$user))) {
                state$xaxis$user0 <- with(state$xaxis, user[1])
                state$xaxis$device0 <- with(state$xaxis, device[1])
                state$xaxis$slope <- with(state$xaxis, (user[2]-user[1])/(device[2]-device[1]))
            } else {
                stop("cannot handle single-point x models yet")
            }
            if (all(is.finite(state$yaxis$user))) {
                state$yaxis$user0 <- with(state$yaxis, user[1])
                state$yaxis$device0 <- with(state$yaxis, device[1])
                state$yaxis$slope <- with(state$yaxis, (user[2]-user[1])/(device[2]-device[1]))
            } else {
                stop("cannot handle single-point y models yet")
            }
            state$stage <- 10L           # prepare for next: digitize points on graph
            showNotification("Click on points to digitize them", type="message", closeButton=TRUE)
        } else if (state$stage == 10L) { # digitizing points
            state$xdevice <- c(state$xdevice, input$click$x)
            state$ydevice <- c(state$ydevice, input$click$y)
            state$code <- c(state$code, as.integer(input$pch))
            n <- length(state$xdevice)
            dmsg("  defined ", n, "-th point as c(", state$xdevice[n], ",", state$ydevice[n], ") in device coordinates\n", sep="")
        }
    })

    ## Image transformations chosen by user to establish orthogonal x and y axes
    observeEvent(input$rotate, { state$rotate <- input$rotate })
    #<idea> observeEvent(input$shearx, { state$shearx <- input$shearx })
    #<idea> observeEvent(input$sheary, { state$sheary <- input$sheary })

    ## @importFrom png readPNG
    #' @importFrom magick image_read
    observeEvent(input$inputFile, {
        state$inputFile <- input$inputFile
        state$image <- magick::image_read(state$inputFile$datapath)
        state$stage <- 2               # prepare for next
    })

    output$readImage <- renderUI({
        fileInput("inputFile", h5("Please select an input file"), accept=c("image/png"))
    })

}

shinyApp(ui=ui, server=server)

