###################################
####
#### INTERNAL CONSISTENCY of a scale. 
####
#### reliability() function
####
#### Author: Diego Vitali (d.vitali@ucl.ac.uk)
###################################

# x<-rawsetB
# replicates<-500
# scale<-"antani"
internalConsistency <- function(x,scale,boot=F,replicates=5000, extraPlots=FALSE, binWidth=NULL) {
  
  require(ggplot2,dplyr,boot)
  # Requires "corrr": correlate() uses pairwise.complete.obs by default and retunrs na instead of 1 in the diagonal
  #inter_item <- x %>% correlate() %>% dplyr::select(-rowname) %>% colMeans(na.rm = TRUE) 
  r<-x %>% na.omit %>% cor  # calculate correlation matrix
  
  # calculate mean inter-item correlation by item, the mean of this is the mean inter-item correlation
  # note that I substitue NA to 1 in the diagonal to allow for the applycation of colmean() without those 1s
  #inter_item<- ifelse(r==1,NA,r) %>%  colMeans(na.rm = TRUE)
  
  inter_item <- ifelse(r==1,NA,r) %>%  colMeans(na.rm = TRUE) %>% cbind
  
  inter_item<- data.frame(MeanCor = inter_item[,1], Item = row.names(inter_item), row.names = NULL)
  # calculating crombach alpha - Use bootstrap if selected 
  # I have duplicated the ggplot function to add the bootstrapped alpha and CI
  if (boot==TRUE){
    
    set.seed(123456);# replicable results
    # originally I did create a bootstrap function because I did not know alpha() had one built in !!!
    # so the following speeds up the code significantly
    result<-alpha(x,na.rm = T, title = scale, n.iter = replicates)
    
    BootT<-result$boot[,2] # take the standardized alpha coefficient distribution from bootstrap (second colum) _
    
    c.alpha <- result$total$std.alpha
    
    ci<-quantile(BootT,c(.025,.975))
    
    fiftyeth<-quantile(BootT,.5)
    
    qp<-data.frame(t=BootT) %>% ggplot(aes(sample=t))+
                                         stat_qq()+ 
                                         stat_qq_line()+
                                         ggtitle(paste(scale, "- Q-Q plot"))
    
    #out<-paste(scale, " Alpha(t0)=", round(c.alpha,2), " (95CI=",round(ci[1],2),";",round(ci[2],2),")")
    
    alpha.out<-list(`Alpha(t0)` = round(c.alpha,2), `Boostrap 95CI`= c(round(ci[1],2),round(ci[2],2)))

    
    
    bp<- data.frame(t=BootT) %>% ggplot(aes(x=t))+
                                          geom_histogram(aes(y=..density..), color = "black", fill="white")+
                                          geom_vline(aes(xintercept=c.alpha), linetype = "dashed")+
                                          geom_vline(aes(xintercept=fiftyeth), linetype = "dotted", color = "blue")+
                                          annotate("text",x=c.alpha,y=30,label=paste("Crombach alpha (t0)=",round(c.alpha,3)),hjust=0)+
                                          annotate("text",x=fiftyeth,y=25,label=paste("50th Crombach alpha =", round(fiftyeth,3)),colour = "blue",hjust=0)+
                                          ggtitle(paste(scale, "- boostrap of crombach alphas"))
              
  
   
    
    p <- inter_item %>% 
        ggplot(aes(x = MeanCor, fill=Item)) + 
          geom_histogram(alpha = .5, bins = binWidth) +
          geom_vline(xintercept = mean(inter_item$MeanCor), color = "red") +
          annotate("text",x=mean(inter_item$MeanCor),y = length(inter_item$MeanCor)/2,  label=round(mean(inter_item$MeanCor),3),hjust=0)+
          annotate("text",x=min(inter_item$MeanCor),y = length(inter_item$MeanCor)/2,  label=paste("Crombach alpha (t0) =",round(c.alpha,3)),hjust=0)+
          annotate("text",x=min(inter_item$MeanCor),y = (length(inter_item$MeanCor)/2-length(inter_item$MeanCor)/30),label=paste("Bootstrap 95 CI: ",round(ci[1],3), " ", round(ci[2],3)),hjust=0)+
          ggtitle(paste(scale, "- N. complete obs: ",(x %>% na.omit%>% nrow), "/120"))+
          xlab(paste(scale,"Mean inter-item correlation by item")) +
          theme_bw()
    
    
    #pbp<-grid.arrange(bp,p,ncol=2)
    if (extraPlots==TRUE){      
      return(list(`Items correlations` = r,`mean inter-item plot` = p, `Crombach Alpha` = alpha.out,`bootstrapped alpha plot` = bp, `Q-Q plot for bootstraped alphas` = qp))
    }else{
      return(list(`Items correlations` = r,`mean inter-item plot` = p, `Crombach Alpha` = alpha.out))
    }
          
  }else{
    alpha.ex<- psych::alpha(x)
    c.alpha<- alpha.ex$total$std.alpha
    
    p <- data.frame(inter_item) %>% 
          ggplot(aes(x = MeanCor, fill=Item)) + 
            geom_histogram(alpha = .5) +
            geom_vline(xintercept = mean(inter_item$MeanCor), color = "red") +
            annotate("text",x=mean(inter_item$MeanCor),y = length(inter_item$MeanCor)/2,label=round(mean(inter_item$MeanCor),3),hjust=0)+
            annotate("text",x=min(inter_item$MeanCor),y = length(inter_item$MeanCor)/2,label=paste("Crombach alpha =",round(c.alpha,3)),hjust=0)+
            ggtitle(scale)
            xlab(paste(scale,"Mean inter-item correlation by item")) +
            theme_bw()
   
    return(list(`Items correlations` = r,`mean inter-item plot` = p,`Crombach Alpha` = c.alpha))
  }
}
###################################
####   END
###################################
