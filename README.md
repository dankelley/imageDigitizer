imageDigitizer is an R shiny app that can be used to digitize points in a PNG
image.

# Installation

Since imageDigitizer is not on CRAN, it is installed by typing

    remotes::install_github("dankelley/imageDigitizer")

in an R console.

# Usage

Run the main function by typing

    library(imageDigitizer)
    imageDigitizer()

in an R console.  The GUI has elements to guide you through the process. If you
make an error, use the `Undo` button to remove the most recently added point.
Use the `Save` button to save the results to a text file that has a name
patterned on the name of the PNG file.  Use the `Code` button to view a snippet
of R code that can read this file, and plot the results.

