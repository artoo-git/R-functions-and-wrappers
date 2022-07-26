######################################################################
################ BEGIN FUNCTION CONTRUCT DIMENSIONALITY
####
####         CONTRUCT DIMENSIONALITY: Dimensionality()
####
####  Author: Diego Vitali (d.vitali@ucl.ac.uk)
####
#### The function below conducts a PCA  for a given scale, 
####  . plots the scree test showing the results of parallel analysis
####  . plots the data on the first two components of a scale 
####  . prints the factor loading
####  . prints the variance explained by each component*
####
####  AVAILABLE ROTATIONS VIA psych::principal()
####
####  "none", "varimax", "quartimax", "promax", "oblimin", "simplimax", 
####  and "cluster" are possible rotations/transformations of the solution.
####
######################################################################


Dimensionality <- function(x, 
                           # Default parameters
                           scale = "scale name", 
                           rep = 5000, 
                           rotation = "none", 
                           loadCutoff = .3, 
                           nFact = 2, 
                           cor="cor") {
  
  require(nFactors, psych)
  
  #take only complete scorings
  X <- x %>% na.omit 
  
  ########################  
  ####### BEGIN PARALLEL ANALYSIS AND NON-GRAPHICAL SOLUTION OF SCREE TEST
  ########################

      # initial PCA to be used for parallel analysis
      # Method with Principal: allows easy implementation of promax and 
      # oblimin rotations for components that are assumed to be correlated
      res.pca <- X %>% psych::principal(rotate = rotation, cor = cor)
      
      ## Method with principal: principal returns the eigenvalues directly
      eigenValues<-res.pca$values
      
       
      # Parallel: distribution of the eigenvalues of correlation matrices of 
      # random uncorrelated standardized normal variables.
      ap <- nFactors::parallel(subject=nrow(X),var=ncol(X), rep = rep, quantile = .05) 
      nS <- nFactors::nScree(x = eigenValues, aparallel=ap$eigen$qevpea, cor = T)
      
      # pull the number of components based on parallel analysis 
      # (see raiche (2013) (https://ppw.kuleuven.be/okp/_pdf/Raiche2013NGSFC.pdf) for 
      # non graphical solution of the scree test)
      numComp<- nS$Components$nparallel 
      
      plot1 <- nFactors::plotnScree(nS, 
                legend = TRUE,
                ylab   = "Eigenvalues",
                xlab   = "Components",
                main   = paste(scale, ": Non Graphical Solutions to Scree Test")
                )
  ########################  
  ####### END OF PARALLEL ANALYSIS AND NON-GRAPHICAL SOLUTION OF SCREE TEST
  ########################

  ########################  
  ####### BEGIN PCA AND BIPLOTS VIA principal() 
  ########################

      # preparing the result object for plotting by declaring the number of 
      res.pca <- X %>% psych::principal(nfactors = nFact, rotate = rotation, cor = cor) 
      # nFact default is 2 for the sake of having a biplot
      res.plot.pca <- X %>% psych::principal(nfactors = 2, rotate = rotation, cor = cor) 
      plot2<-biplot.psych(res.plot.pca,
                   #scale = 0, # ensures that arrows are scaled to represent the loadings
                   main = paste(scale,": PCA - Biplot"),
                   sub=paste("Note that the optiman number of dimension identified 
                              via parallel analysis was: ", numComp),
                   #xlab=paste("Comp. 1: ",round((varExp[1])*100,2),"%"),
                   #ylab=paste("Comp. 2: ",round((varExp[2])*100,2),"%")
                    )
  ########################  
  ####### END OFPCA AND BIPLOTS VIA principal() 
  ########################
      
  ########################  
  ####### BEGIN PoV EXPLAINED BY COMPONENTS
  ########################  
    # Method 1
      #standard dev explained by each Principal component of the eigen-values
      #r<-length(eigenValues)
      #ev.SD<- sqrt(eigenValues/(nrow(r)-1))
      
    # Method 2
      ## var explained by each comp
      #varExp<- cumsum(ev.SD^2)/sum(ev.SD^2)
      
    # Method 3
      #varExp<-eigenValues/sum(eigenValues)
      #
      # via "principal" is the preferred method because it 
      # allows for rotation options more easily
      varExp<-res.pca$Vaccounted
   
  ########################  
  ####### END OF PoV EXPLAINED BY COMPONENTS
  ########################  
  
      
  # List of the percentage of variance explained by each component 
  # ( note that it will only store the components decided in parallel)
  if(nFact == 1){
     Pvars <- round(varExp[2,1]*100,2)
  } else{
    Pvars <- round(varExp[2,1:numComp]*100,2)
    
  }
  
    
  res.list<-list(`parallel analysis` = plot1, `PCA` = res.pca, `pca biplot` = plot2)
  
  return(res.list)
  
}
######################################################################
################ END of FUNCTION: CONTRUCT DIMENSIONALITY
######################################################################
