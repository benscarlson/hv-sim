options(rgl.useNULL = TRUE)

library(hypervolume)
library(MASS)
library(tidyverse)

source('src/funs/fortify_hv.r')
source('src/funs/funs.r')
source('src/funs/themes.r')
theme_set(themepdf)

#set.seed(1504)

.figP <- 'analysis'
.n <- 500 #recommended obsevations should be > exp(number of axes). exp(2) = 7.39
.method <- 'gaussian'

sig <- matrix(c(1,0,0,1),ncol=2)

mu <- c(0,0)

dat <- mvrnorm(.n,mu,sig)
colnames(dat) <- c('V1','V2')

hv <- hypervolume(dat,method=.method,verbose=FALSE)

#Area of 95% ellipse
poly <- dat %>% ellipsedf %>% toPoly
st_area(poly)

#Area of hypervolume
hv@Volume

#Can't figure out how this works
#hv95 <- hypervolume_threshold(hv,quantile.requested=0.2,plot=FALSE,quantile.requested.type='probability')

#---- figures ----#

hvdf <- fortify_hv(hv)

ggplot(mapping=aes(x=V1,y=V2)) +
  geom_point(data=hvdf,aes(x=V1,y=V2,color=slot)) +
  stat_ellipse(data=filter(hvdf,slot=='Data'),type='norm') +
  coord_fixed() +
  labs(title=.method)

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
