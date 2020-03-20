options(rgl.useNULL = TRUE)

library(hypervolume)
library(MASS)
library(tidyverse)

source('src/funs.r')
theme_set(themepdf)

#set.seed(1504)

.figP <- 'analysis'

sig <- matrix(c(1,0,0,1),ncol=2)
n <- 4000 #recommended obsevations should be > exp(number of axes). exp(2) = 7.39
mu <- c(0,0)

dat <- mvrnorm(n,mu,sig)

hv <- hypervolume_svm(dat,verbose=FALSE)

#Contour type can be:
# Type of contour boundary: any of "alphahull" (alpha hull), "ball" (experimental ball covering), 
# "kde" (2D KDE smoothing), or "raster" (grid-based rasterization).
# alphahull with high alpha is similar to convex hull

plot(hv,show.density=TRUE,show.data=FALSE, show.centroid = FALSE,
     show.random=TRUE,show.contour=TRUE, show.legend=FALSE,
     contour.type='alphahull',contour.alphahull.alpha=100) #data and contour

ggplot(hv@RandomPoints,aes(x=X1,y=X2)) +
  geom_con

#Testing other contour types
plot(hv,show.density=TRUE,show.data=FALSE, show.centroid = FALSE,
     show.random=TRUE,show.contour=TRUE, show.legend=FALSE) #data and contour



plot(hv,show.density=FALSE,show.data=TRUE, show.centroid = FALSE,
     show.random=FALSE,show.contour=TRUE, show.legend=FALSE,
     contour.type='ball',contour.ball.radius.factor=2) #data and contour

plot(hv,show.density=FALSE,show.data=TRUE, show.centroid = FALSE,
     show.random=FALSE,show.contour=TRUE, show.legend=FALSE,
     contour.type='raster') #data and contour. #,contour.raster.resolution=1
