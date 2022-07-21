# R-functions-and-wrappers

This repo includes some Wrappers I wrote in R for my own use.

1. Dimensionality() : The function rely on psych and nFactor packages and operationalises difference aspects of the investigation of contruct dimensionality for a given scale.
   - executes Parallel analysis with non-graphical  solution of scree test (Raiche 2013)
   - executes PCA with several options for rotations
   - plots the scree test showing the results of parallel analysis
   - plots the data on the first two components of a scale 
   - prints the factor loading
   - prints the variance explained by each component
1. reliability() : The function rely on boot and operationalise different aspects of the investigation of reliability for a given scale:
   - executes inter-item (Person) correlations
   - Plots inter-item correlations around the estimated mean interitem correlation
   - estimates Chrombach's alpha
   - estimates (via bootstrap) a 95% conf.int. for the mean interitem cor
   - estimates (via bootstrap) a 95% conf.int. for Chrombach's alpha
