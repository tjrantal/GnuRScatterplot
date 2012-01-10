GnuRScatterplot

marrowCorrelationsReAna.r is an R-script for plotting scatterplots for a manuscript I'm working with.
The script uses two helper functions getPlotLimits.r and getTickMarkLabels.r to set the axis limits
and tickmarks at appropriate positions with appropriate number of significant numbers. Appropriate in my opinion, that is...

The scripts reads a comma separated datasheet, looks for headings on the first row of data and selects the
ones given on variables xVariable and yVariables. yVariables variables will be plotted against xVariable.
Saves the plot as .png to the path defined in marrowCorrelationsReAna.r.

For things to modify to use the script as is, look at marrowCorrelationsReAna.r.

Project is mostly created as a backup for myself, but there might be some useful stuff for others as well.
