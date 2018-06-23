`imager` is an R shiny app that can be used to digitize points on a graph that
is contained in a PNG image. Here's how to get started:

1. Copy a `.png` file here. It should contain a graph that uses linear axes.

2. Launch RStudio and open the `imager.R` file.

3. follow the directions you see in a panel on the left-hand side. In summary,
the steps are as follows:

    a. Load the `.png` file.
    b. Use the slidebar to rotate it, if necessary. The guidelines may help.
    c. As instructed, click a point on the x axis that has a known value. Typically
       this will be a labelled tick on a graph. It is not important that you get the
       y value correct in this; only the x value is used.
    d. A dialog box will open, asking you to type in the numerical value at this
       point on the x axis.
    e. Repeat c and d a second point on the x axis.
    f. Repeat c, d, and e for the y axis.
    g. Click on points in the graph. The points will turn red on the graph.
       If you make a mistake, click the "Undo" button before proceeding to 
       the next point.
    h. When you've digitized all the ponts, click on the "Save" button, which
       saves to a file with the name name as the `.png` file, except that it
       has suffix `.imager`.

