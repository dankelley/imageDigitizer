# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(png)

debugFlagDefault <- !FALSE              # For console messages that trace control flow.

options(shiny.error=browser)
stepMeanings <- c("Input file",        # step  1
    "Rotate image",                    # step  2
    "Enter axis names",                # step  3
    "Enter x limits",                  # step  4
    "Click lower x limit",             # step  5 (recognized during click processing)
    "Click upper x limit",             # step  6 (recognized during click processing)
    "Enter y limits",                  # step  7
    "Click lower y limit",             # step  8 (recognized during click processing)
    "Click upper y limit",             # step  9 (recognized during click processing)
    "Digitize Points")                 # step 10

col <- list(axes="magenta", grid="blue")

version <- "0.1.6"
keypressHelp <- "
<i>Keystroke interpretation</i>
<ul>
<li> <b>p</b>: toggle printing of debugging information to the R console
<li> <b>u</b>: remove the last-digitized point
<li> <b>+</b>: zoom in, centred on mouse location [FIXME: implement this]
<li> <b>-</b>: zoom out [FIXME: implement this]
<li> <b>0</b>: unzoom [FIXME: implement this]
</ul>
"

fileLoaded <- FALSE

ui <- fluidPage(tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
    #style="margin-left:2ex",
    style="text-indent:1em; background:#e6f3ff",
    uiOutput(outputId="title"),
    uiOutput(outputId="loadFile"),
    uiOutput(outputId="grid"),
    uiOutput(outputId="rotateImage"),
    uiOutput(outputId="enterAxisNames"),
    uiOutput(outputId="enterXLimits"),
    uiOutput(outputId="enterYLimits"),
    uiOutput(outputId="undoSaveCodeQuit"),
    uiOutput(outputId="customizeSymbols"),
    #uiOutput(outputId="choosePch"),
    uiOutput(outputId="showStatus"),
    uiOutput(outputId="showImage"))

server <- function(input, output)
{
    debugFlag <- debugFlagDefault
    dmsg <- function(..., sep="")
    {
        if (debugFlag)
            cat(file=stderr(), ..., sep=sep)
    }
    state <- reactiveValues(
        step=1,
        #<shear> shearx=0,
        #<shear> sheary=0,
        rotate=0,
        inputFile=NULL,
        image=NULL,
        xname="x",
        yname="y",
        x=NULL, xdevice=NULL,
        y=NULL, ydevice=NULL,
        cex=NULL, pch=NULL, col=NULL, # symbol characteristics
        xaxis=list(user=rep(NA,2), device=rep(NA,2), slope=NULL, user0=NULL, device0=NULL),
        yaxis=list(user=rep(NA,2), device=rep(NA,2), slope=NULL, user0=NULL, device0=NULL))

    saveFile <- function()
    {
        file <- paste(path_home(), "/", gsub(".png$", "", state$inputFile$name), "_imageDigitizer.dat", sep="")
        cat(paste("# imageDigitizer: ", version,              "\n", sep=""), file=file)
        cat(paste("# file:           ", state$inputFile$name, "\n", sep=""), file=file, append=TRUE)
        cat(paste("# rotation:       ", state$rotate,         "\n", sep=""), file=file, append=TRUE)
        cat(paste("# xaxis$user0:    ", state$xaxis$user0,    "\n", sep=""), file=file, append=TRUE)
        cat(paste("# xaxis$device0:  ", state$xaxis$device0,  "\n", sep=""), file=file, append=TRUE)
        cat(paste("# xaxis$slope:    ", state$xaxis$slope,    "\n", sep=""), file=file, append=TRUE)
        cat(paste("# yaxis$user0:    ", state$yaxis$user0,    "\n", sep=""), file=file, append=TRUE)
        cat(paste("# yaxis$device0:  ", state$yaxis$device0,  "\n", sep=""), file=file, append=TRUE)
        cat(paste("# yaxis$slope:    ", state$yaxis$slope,    "\n", sep=""), file=file, append=TRUE)
        cat(sprintf("i,devicex,devicey,%s,%s,cex,pch,col\n", state$xname, state$yname), file=file, append=TRUE)
        if (length(state$xaxis$device)) {
            #<old> x <- state$xaxis$user0 + state$xaxis$slope * (state$xdevice - state$xaxis$device0)
            #<old> y <- state$yaxis$user0 + state$yaxis$slope * (state$ydevice - state$yaxis$device0)
            for (i in seq_along(state$xdevice)) {
                cat(sprintf("%d,%.4f,%.4f,%.4g,%.4g,%.2g,%d,\"%s\"\n",
                        i, state$xdevice[i], state$ydevice[i], state$x[i], state$y[i], state$cex[i], state$pch[i], state$col[i]),
                    file=file, append=TRUE)
            }
        }
        file
    }

    output$loadFile <- renderUI({
        if (state$step == 1L)
            insertUI("loadAFile", ui=fileInput("inputFile", h5("Input file"), accept=c("image/png")))
    })

    output$showStatus <- renderUI({
        msg <- " "
        npts <- length(state$xdevice)
        if (npts > 0L) {
            msg <- paste0("Digitized ", npts, if (npts!=1L) " points" else " point")
            if (!is.null(input$plotHover$x)) {
                msg <- paste0(msg,
                    sprintf(" | Hovering at %s=%.3g (%.0f px), %s=%.3g (%.0f px)",
                        state$xname,
                        with(state$xaxis, user0+slope*(input$plotHover$x-device0)),
                        input$plotHover$x,
                        state$yname,
                        with(state$yaxis, user0+slope*(input$plotHover$y-device0)),
                        input$plotHover$y))
            }
        }
        msg
    })

    output$showImage <- renderUI({
        if (state$step > 1L)
            fluidRow(plotOutput("plot", click="click", hover="plotHover", width="100%"))
    })

    output$grid <- renderUI({
        if (state$step > 1L)
            fluidRow(radioButtons("grid", label=h5("Grid"),
                    choices=c("None"="off", "Fine"="fine", "Medium"="medium", "Coarse"="coarse"),
                    selected="medium", inline=TRUE))
    })

    output$rotateImage <- renderUI({
        if (state$step == 2L) {
            fluidRow(
                column(10, sliderInput("rotate", h5("Rotate Image [degrees]"), min=-20, max=20, value=0, step=0.05)),
                column(2, fluidRow(actionButton("finishedRotation", "Done"))))
        }
    })

    observeEvent(input$finishedRotation, {
        state$step <- 3L            # prepare for next
        dmsg("clicked finishedRotation button, so setting state$step to ", state$step, ". Note: state$rotate=", state$rotate, " deg\n", sep="")
    })

    output$enterAxisNames<- renderUI({
        if (state$step == 3L) {
            dmsg("in output$enterAxisNames (state$step=", state$step, ")\n", sep="")
            fluidRow(
                column(4, textInput("xname", h5("Name x axis"), state$xname)),
                column(4, textInput("yname", h5("Name y axis"), state$yname)),
                actionButton("finishedGetAxisNames", "Done"))
        }
    })

    observeEvent(input$finishedGetAxisNames, { # at step 4 (invisible to user)
        # refuse to accept zero-length names, retaining defaults ('x' and 'y') if so
        if (nchar(input$xname))
            state$xname <- input$xname
        if (nchar(input$yname))
            state$yname <- input$yname
        dmsg("clicked finishedGetAxisNames button  (state$xname=\"", state$xname, "\" and state$yname=\"", state$yname, "\"; set state$step=", state$step, ")\n", sep="")
        state$step <- 4L            # prepare for next
    })

    output$enterXLimits <- renderUI({
        if (state$step == 4L) {
            dmsg("in output$enterXLimits (state$step=", state$step, ")\n", sep="")
            fluidRow(
                column(4, textInput("xlow", h5(paste(state$xname, "low")))),
                column(4, textInput("xhigh", h5(paste(state$xname, "high")))),
                actionButton("finishedGetXLimits", "Done"))
        }
    })

    observeEvent(input$finishedGetXLimits, { # at step 5 (which is noticed by output$click, which also catches step 6)
        dmsg("clicked finishedGetXLimits button (state$step=", state$step, ")\n", sep="")
        owarning <- options("warning")$warning
        options(warning=0) # turn off warning for NAs (one of which is permitted)
        state$xaxis$user <- as.numeric(c(input$xlow, input$xhigh))
        options(warning=owarning)
        if (sum(is.finite(state$xaxis$user)) < 1L)
            stop("Must give at least 1 non-NA value for x")
        state$step <- if (is.finite(state$xaxis$user[1])) 5L else 6L
        showNotification(paste0("Click the mouse at x =",
                paste(state$xaxis$user[is.finite(state$xaxis$user)], collapse=" and "), "\n"))
    })

    output$enterYLimits <- renderUI({
        if (state$step == 7L) {
            dmsg("in output$enterYLimits (state$step=", state$step, ")\n", sep="")
            fluidRow(
                column(4, textInput("ylow", h5(paste(state$yname, "low")))),
                column(4, textInput("yhigh", h5(paste(state$yname, "high")))),
                actionButton("finishedGetYLimits", "Done"))
        }
    })

    observeEvent(input$finishedGetYLimits, { # sets state$step to 8 (which is noticed by output$click, which also forms step 9)
        dmsg("clicked finishedGetYLimits button (state$step=", state$step, ")\n", sep="")
        owarning <- options("warning")$warning
        options(warning=0) # turn off warning for NAs (one of which is permitted)
        state$yaxis$user <- as.numeric(c(input$ylow, input$yhigh))
        options(warning=owarning)
        if (sum(is.finite(state$yaxis$user)) < 1L)
            stop("Must give at least 1 non-NA value for y")
        state$step <- if (is.finite(state$yaxis$user[1])) 8L else 9L
        showNotification(paste0("Click the mouse at y =",
                paste(state$yaxis$user[is.finite(state$yaxis$user)], collapse=" and "), "\n"))
    })

    # FIXME: add 'Help' here.
    output$undoSaveCodeQuit <- renderUI({
        if (state$step == 10L) {
            dmsg("in output$undoSaveCodeQuit (state$step=", state$step, ")\n", sep="")
            fluidRow(
                actionButton("undoButton", "Undo"),
                actionButton("saveButton", "Save"),
                actionButton("codeButton", "Code"),
                actionButton("quitButton", "Quit"))
        }
    })

    # Icon-based pch selector (defaulting to 5, a diamond).
    # See https://github.com/dankelley/imageDigitizer/issues/8
    output$choosePch <- renderUI({
        if (state$step == 10L) {
            dmsg("in output$choosePch (state$step=", state$step, ")\n", sep="")
            pchChoices <- paste(sapply(0:25, function(i)
                    {
                        sprintf('<label class="radio-inline">
                            <input type="radio" name="pch" value="%d" %s/>
                            <span> <img src="/pch_%02d.png" alt="%d"/> </span>
                            </label>', i, if (i == 5L) 'checked="checked"' else '', i, i)
                    }),
                collapse="\n")
            fluidRow(
                column(width=12,
                    tags$div(HTML(paste('<div id="pch" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline"> <label class="control-label" for="pch">Symbol Type</label> <div class="shiny-options-group">', pchChoices, '</div> </div>')))))
        }
    })

    #' @importFrom colourpicker colourInput
    output$customizeSymbols <- renderUI({
        if (state$step == 10L)
            fluidRow(
                column(3, colourpicker::colourInput("col", "Symbol Colour", "#B632C7", allowTransparent=TRUE)),
                column(2, selectInput("cex", "Symbol Size", seq(0.5, 4, 0.5), selected=2)),
                column(2, selectInput("pch", "Symbol Code", seq(0L, 25L), selected=5)),
                column(2, fluidRow(actionButton("symbolHelp", "Help"))))
    })

    observeEvent(input$symbolHelp, {
        showModal(modalDialog(img(src="/pch_choices.png"), title="Symbol Codes", size="m", easyClose=TRUE))
    })

    undo <- function(n=1L)
    {
        if (n > 0L && length(state$xdevice) > (n-1L)) {
            state$cex <- head(state$cex, -n)
            state$col <- head(state$col, -n)
            state$pch <- head(state$pch, -n)
            state$x <- head(state$x, -n)
            state$y <- head(state$y, -n)
            state$xdevice <- head(state$xdevice, -n)
            state$ydevice <- head(state$ydevice, -n)
        }
    }

    #' @importFrom utils head
    observeEvent(input$undoButton, {
        undo()
    })

    observeEvent(input$saveButton, {
        name <- saveFile()
        showNotification(paste0("File '", name, "' saved"), type="message", duration=3)
    })

    observeEvent(input$codeButton, {
        ofile <- paste(gsub(".png$", "", state$inputFile$name), "_imageDigitizer.dat", sep="")
        msg <- "# Sample code to read and plot the saved data file<br>\n"
        msg <- paste0(msg, "data <- read.csv(file=\"~/", ofile, "\", skip=9, header=TRUE)<br>\n")
        msg <- paste0(msg, "plot(", "data[[\"", state$xname, "\"]],", "data[[\"", state$yname, "\"]],", "xlab=\"", state$xname, "\",", "ylab=\"", state$yname, "\",", "cex=data$cex, pch=data$pch,col=data$col)<br>\n")
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
        } else if (state$step > 3L) {
            dmsg("clicked '", key, "'\n")
            if (key == 'd') {
                debugFlag <- !debugFlag
                cat(file=stderr(), "now, debugFlag=", debugFlag, "\n")
            }
        } else if (state$step == 10L) { # Don't allow zooming until scales are defined.  FIXME: relax this?
            if (key == 'u') {
                undo(2L)               # FIXME: does 2 work?
            } else if (key == '+') {
                dmsg("FIXME: should zoom in now\n")
            } else if (key == '-') {
                dmsg("FIXME: should zoom out now\n")
            } else if (key == '0') {
                dmsg("FIXME: should unzoom now\n")
            } else if (key == '?') {
                showModal(modalDialog(title="", HTML(keypressHelp), easyClose=TRUE))
            }
        }
    })

    output$title <- renderUI({
        msg <- paste0("imageDigitizer ", version)
        if (!is.null(state$inputFile)) {
            msg <- paste0(msg, " | File '", state$inputFile$name, "'")
            if (state$step < 10L)
                msg <- paste0(msg, " | Step ", state$step, " (", stepMeanings[state$step], ")")
        }
        return(msg)
    })

    output$loadFile <- renderUI({
        if (state$step == 1) {
            fileInput("inputFile", h5("Input file"), accept=c("image/png"))
        }
    })

    #' @importFrom graphics abline box mtext par points rasterImage text
    #' @importFrom magick image_rotate image_shear
    output$plot <- renderPlot({
        par(mar=rep(1, 4))
        idim <- dim(state$image[[1]])
        #asp <- if (is.null(state$image)) 1 else idim[3] / idim[2] # FIXME: or reciprocal?
        dmsg("image dim[2]=", idim[2], ", idim[3]=", idim[3], "\n")
        plot(c(1,idim[2]), c(1,idim[3]), type='n', asp=1, xaxs="i", yaxs="i", axes=FALSE)
        box()
        if (!is.null(state$image)) {
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
            rasterImage(I, 1, 1, idim[2], idim[3], interpolate=FALSE)
            # Draw guiding grid.
            if (input$grid != "off") {
                dg <- as.integer(1L + min(idim[2:3])/25) * switch(input$grid, fine=1, medium=2, coarse=5)
                usr <- par("usr")
                for (xg in seq(usr[1], usr[2], dg))
                    lines(rep(xg, 2), usr[2:3], col=col$grid, lty="dotted")
                for (yg in seq(usr[3], usr[4], dg))
                    lines(usr[1:2], rep(yg, 2), col=col$grid, lty="dotted")
            }
            for (i in seq_along(state$xaxis$user)) {
                if (is.finite(state$xaxis$user[i]) && is.finite(state$xaxis$device[i])) {
                    abline(v=state$xaxis$device[i], col=col$axes)
                    mtext(state$xaxis$user[i], side=1, at=state$xaxis$device[i], col=col$axes)
                }
            }
            for (i in seq_along(state$yaxis$user)) {
                if (is.finite(state$yaxis$user[i]) && is.finite(state$yaxis$device[i])) {
                    abline(h=state$yaxis$device[i], col=col$axes)
                    mtext(state$yaxis$user[i], side=2, at=state$yaxis$device[i], col=col$axes)
                }
            }
            if (length(state$xdevice)) {
                #dmsg("plotting points??? next are the points\n")
                #print(file=stderr(), data.frame(xdevice=state$xdevice,ydevice=state$ydevice))
                points(state$xdevice, state$ydevice, pch=state$pch, col=state$col, cex=state$cex)
            }
        }
    })

    observeEvent(input$save, {
        saveFile()
    })

    observeEvent(input$click, {
        dmsg("click with state$step=", state$step, " (", stepMeanings[state$step], ")\n")
        if (state$step == 5L) {
            state$xaxis$device[1] <- input$click$x
            state$step <- if (is.finite(state$xaxis$user[2])) 6L else 7L # prepare for next click (possibly jumping)
            dmsg("step 5: set state$xaxis$device = c(", paste(state$xaxis$device,collapse=", "), ")\n")
            dmsg("step 5: set state$step =", state$step, "\n")
        } else if (state$step == 6L) {
            if (is.finite(state$xaxis$user[2]))
                state$xaxis$device[2] <- input$click$x
            dmsg("step 6: set state$xaxis$device = c(", paste(state$xaxis$device,collapse=","), ")\n")
            state$step <- 7L            # prepare for next
        } else if (state$step == 8L) {
            state$yaxis$device[1] <- input$click$y
            state$step <- if (is.finite(state$yaxis$user[2])) 9L else 10L # prepare for next click (possibly jumping)
            dmsg("step 8: set state$yaxis$device = c(", paste(state$yaxis$device,collapse=","), ")\n")
            dmsg("step 8: set state$step =", state$step, "\n")
        } else if (state$step == 9L) {
            if (is.finite(state$yaxis$user[2]))
                state$yaxis$device[2] <- input$click$y
            state$step <- 10L          # prepare for next
        } else if (state$step == 10L) {
            # We need to set up scales, but only once.
            if (is.null(state$xaxis$slope)) {
                dmsg("step 10 setup -- define state$xaxis and state$yaxis\n")
                # Save 3 items (built up from 2) to make it easier to code equal-scale cases
                # We will later use e.g.
                # x <- with(state$xaxis, user0+slope*(input$mouse$x-device0))
                xn <- sum(is.finite(state$xaxis$user))
                yn <- sum(is.finite(state$yaxis$user))
                if (xn < 1L)
                    stop("must give 1 or 2 two reference points for x axis")
                if (yn < 1L)
                    stop("must give 1 or 2 two reference points for y axis")
                if ((xn + yn) < 3L)
                    stop("must give 2 reference points for either x or y axis (or both)")
                if (xn == 2) {
                    # Determine xaxis$slope from the 2 provided x values.
                    state$xaxis$user0 <- with(state$xaxis, user[1])
                    state$xaxis$device0 <- with(state$xaxis, device[1])
                    state$xaxis$slope <- with(state$xaxis, (user[2]-user[1])/(device[2]-device[1]))
                    if (yn == 2) {
                        dmsg("step 10(setup): xn=2, yn=2\n")
                        # Determine yaxis$slope from the 2 provided y values.
                        state$yaxis$user0 <- with(state$yaxis, user[1])
                        state$yaxis$device0 <- with(state$yaxis, device[1])
                        state$yaxis$slope <- with(state$yaxis, (user[2]-user[1])/(device[2]-device[1]))
                    } else {
                        dmsg("step 10(setup): xn=2, yn=1\n")
                        # Use whichever y value was provided, as the base, and then copy the x slope
                        state$yaxis$user0 <- with(state$yaxis, user[is.finite(user)])
                        state$yaxis$device0 <- with(state$yaxis, device[is.finite(user)])
                        state$yaxis$slope <- state$xaxis$slope
                    }
                } else {
                    dmsg("step 10(setup): xn=1, yn=2\n")
                    # From the above, we know that there are 2 y values.
                    state$yaxis$user0 <- with(state$yaxis, user[1])
                    state$yaxis$device0 <- with(state$yaxis, device[1])
                    state$yaxis$slope <- with(state$yaxis, (user[2]-user[1])/(device[2]-device[1]))
                    # Use whichever x value was provided, as the base, and then copy the y slope
                    state$xaxis$user0 <- with(state$xaxis, user[is.finite(user)])
                    state$xaxis$device0 <- with(state$xaxis, device[is.finite(user)])
                    state$xaxis$slope <- state$yaxis$slope
                }
                with(state$xaxis, dmsg("step 10(setup): set state$xaxis$user0=", user0, ", device0=", device0,", slope=", slope, "\n"))
                with(state$yaxis, dmsg("step 10(setup): set state$yaxis$user0=", user0, ", device0=", device0,", slope=", slope, "\n"))
                showNotification("Click on points to digitize them", type="message", duration=3)
            }
            n <- 1L + length(state$x)
            state$cex[n] <- as.integer(input$cex)
            state$col[n] <- input$col
            state$pch[n] <- as.integer(input$pch)
            state$xdevice[n] <- input$click$x
            state$ydevice[n] <- input$click$y
            state$x[n] <- state$xaxis$user0 + state$xaxis$slope * (state$xdevice[n] - state$xaxis$device0)
            state$y[n] <- state$yaxis$user0 + state$yaxis$slope * (state$ydevice[n] - state$yaxis$device0)
            dmsg("step 10: defined ", n, "-th point as c(", state$x[n], ",", state$y[n], ")\n")
        }
    })

    ## Image transformations chosen by user to establish orthogonal x and y axes
    observeEvent(input$rotate, {
        state$rotate <- input$rotate
    })

    #<shear> observeEvent(input$shearx, { state$shearx <- input$shearx })
    #<shear> observeEvent(input$sheary, { state$sheary <- input$sheary })

    ## @importFrom png readPNG
    #' @importFrom magick image_read
    observeEvent(input$inputFile, {
        state$inputFile <- input$inputFile
        state$image <- magick::image_read(state$inputFile$datapath)
        state$step <- 2               # prepare for next
    })

    output$readImage <- renderUI({
        fileInput("inputFile", h5("Please select an input file"), accept=c("image/png"))
    })

}

shinyApp(ui=ui, server=server)

