# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' R/shiny app for digitizing points in images.
#'
#' Sets up a "shiny" graphical user interface (GUI) for digitizing points in images, by
#' means of mouse clicks. The interface is reasonably self-explanatory.
#'
#' @section Developer plans:
#'
#' 1. Handle single-scale cases by not requiring 2 clicks on each axis.
#' 2. Provide controls for zooming in and out.
#' 3. Provide a way to read files created by previous uses, to extend an
#' analysis beyond a single session.
#'
#' @export
#' @importFrom shiny runApp
#' @importFrom magick image_rotate image_shear
#' @importFrom fs path_home
imageDigitizer <- function()
{
    dir <- system.file("shiny", "imageDigitizer/app.R", package="imageDigitizer")
    if (!nchar(dir))
        stop("The app could not be located.", call.=FALSE)
    runApp(dir, display.mode="normal")
}

