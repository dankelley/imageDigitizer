# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' R/shiny app for digitizing points in images.
#'
#' Sets up a "shiny" graphical user interface (GUI) for digitizing points in images, by
#' means of mouse clicks. The interface is intended to be reasonably self-explanatory.
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

