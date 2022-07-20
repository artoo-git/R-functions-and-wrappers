# R-functions-and-wrappers

This repo includes some Wrappers I wrote in R for my own use.

1. ConstrValidity() : The function rely on psych and nFactor packages and operationalises difference aspects of the investigation of contruct validity for a given scale.
   - executes Parallel analysis with non-graphical  solution of scree test (Raiche 2013)
   - executes PCA with several options for rotations
   - plots the scree test showing the results of parallel analysis
   - plots the data on the first two components of a scale 
   - prints the factor loading
   - prints the variance explained by each component
