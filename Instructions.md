# ImputeREEapp

This a an app based on the [imputeREE package for
R](https://github.com/cicarrascog/imputeREE). It allows the imputation
of missing REE data and calculate Ce and Eu anomalies on zircon based on
an empirical method derived from lattice strain theory of [Blundy and
Wood (1994)](https://www.nature.com/articles/372452a0).

On the left side of the sidebar is possible to access to the different
modules of the app and a general description of each section is included
here.

## Uploads

A module to upload your data. The module automatically load the example
data, which are zircons from [Ballard et al.
(2002)](http://link.springer.com/10.1007/s00410-002-0402-5).

To upload your data, please use the upload button in the **upload
panel**. The column names of the REE **should include only the element
name** (eg. `La`, not `La_final, final_corrected` or
`this_La is the good one`) in **ppm**. **Large files will take longer to
process**. Additional columns can be included. A column with the name
`rowid` (row index used for relational data) is likely to give an error,
please rename or exclude this column.

The chondrite values selector allows to choose the reference of those
values. Only those from [Palme and O'Neill
(2014)](http://www.sciencedirect.com/science/article/pii/B9780080959757002011)
and [McDonough and Sun
(1995)](http://www.sciencedirect.com/science/article/pii/0009254194001404)
are included at this version.

The **model variables** panel allows the selection of the REE that will
be used for the model fitting. La, Eu, Ce and Y, are automatically
selected. This elements do fit the regression and their addition would
be detrimental to the fittings. Using only light REE, only heavy REE or
less than 4 REE is not recomended.

The **R^2^** plot show the distribution of the R squared statistic for
each model (one model per zircon grain). It is expected to be high (over
0.9-0.95). The option button (top right corner) allows to modify some
elements on the plot (e.g. use adjusted R^2^ instead).

The **boxplot** show the ratios between the calculated and the measured
values for each element. The horizontal line is a reference for ratio
= 1. The option button allows to correct for deviations of Yb, Lu and Y.
**This corrections are passed into the data, so the output file will
have this values corrected**. A slider allows to filter the data
according the R^2^ value which is only applyed to the plot.

The last panel is a table with summary statistics of the ratio between
calculated and measured data.

## Data

This panel allows to explore the data individually. At the bottom is a
table with the uploaded data, as well as the calculated values and model
measurements (Rsquared, slope, intercept), calculated concentrations and
Ration between. Additional columns during upload are added at the end of
this table.

Clicking any row in these table, shows the REE pattern and its values in
the top panels. The **REE plot** panel shows the REE pattern for the
calculate (NormalizedCalc) and measured data (Normalized). The option
button has several options, such as change the x scale for the lattice
strain expression used for the linear regression. The **grain data** box
shows the chondrite normalised and calculated values and ratio between
them for the selected row.

## Download

Download the calculated data. Impute option will impute the missing REE
and Y data for those models with an R^2^ higher than 0.9 (custom range
selection to be added). Normalized data option will include the condrite
normalized values calculated during the model fitting.

## 
