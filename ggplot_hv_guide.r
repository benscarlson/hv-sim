

options(rgl.useNULL = TRUE)

library(hypervolume)
library(MASS)
library(tidyverse)
library(ggConvexHull)

#source('src/funs/fortify_hv.r')
source('src/funs/funs.r')
source('src/funs/theme.r')
theme_set(themepdf)

#set.seed(1504)

.figP <- 'analysis'

sig <- matrix(c(1,0,0,1),ncol=2)
n <- 200 #recommended obsevations should be > exp(number of axes). exp(2) = 7.39
mu <- c(0,0)


dat <- mvrnorm(n,mu,sig)

hv <- hypervolume_svm(dat,verbose=FALSE)

plot(hv,show.density=TRUE,show.data=FALSE, show.centroid = FALSE,
     show.random=TRUE,show.contour=TRUE, show.legend=FALSE,
     contour.type='alphahull',contour.alphahull.alpha=1) #data and contour

ggplot(mapping=aes(x=X1,y=X2)) +
  #geom_convexhull(data=fortify_hv(hv,'RandomPoints'),fill=NA,color='blue') +
  #geom_point(data=fortify_hv(hv,'Data')) +
  #geom_point(data=fortify_hv(hv,'RandomPoints')) +
  geom_density_2d(data=fortify_hv(hv,'RandomPoints'),bins=1)
  
