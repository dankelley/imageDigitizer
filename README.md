`imager` is an R shiny app that can be used to digitize points on a graph that
is contained in a PNG image. Here's how to get started:

1. Copy a `.png` file here. It should contain a graph that uses linear axes.

2. Launch RStudio and open the `imager.R` file, or type `runApp("imager.R")` in
   an R console.

3. Follow the directions you see in the control panels. In summary, the steps
   are as follows:

    1. Load the `.png` file.
    2. Use the slidebar to rotate it, if necessary. The grid may help.
    3. As instructed, click a point on the `x` axis that has a known value. This
       may be a point on an axis, for digitizing a plot, or a grid point, for
       digitizing an image that has no plotted scale. It is not important that
       you get the y value correct in this; only the x value is used.
    4. A dialog box will open, asking you to type in the numerical value at this
       point on the x axis.
    5. Repeat the two previous steps for a second point on the x axis.
    6. Repeat the three previous steps for two points on the y axis.
    7. Now it's time to digitize data in the graph. Do this by clicking on
       the desired points in the diagram. Each point you click gets coloured
       red, so you can see if you made a mistake. You may click the
       "Undo" button to correct the last point, or set of points.
    8. When you've digitized all the points of interest, click on the "Save"
       button, which saves to a text file with the name name as the image file,
       except that the suffix `.png` is switched to `_imager.dat`. A header in
       the output file explains the data format and summarizes the mapping from
       pixel location to x-y value.

