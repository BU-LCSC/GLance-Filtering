This repo includes a set of R scripts for filtering training data from the GLanCE land cover and land cover change mapping project.

There are four files:

0.GlanceFunctionsDefs.R includes a set of functions used by the scripts
1.GlanceTrainingSetup.R reads the training data and sets up the dataframes for processing
2.GlanceFfiltering.R filters the data based on relationships among features and classes at continental scale
3.GlanceECfilter.R filters the data based on relationships among features and classes within ecoregions

Note that 3 can be run after 2, or not at all.
