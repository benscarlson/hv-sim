

options(rgl.useNULL = TRUE)

library(hypervolume)
library(MASS)
library(tidyverse)

source('src/funs.r')
theme_set(theme_pdf)

set.seed(1504)

sig <- matrix(c(1,0,0,1),ncol=2)
n <- 400 #recommended obsevations should be > exp(number of axes). exp(2) = 7.39
mu <- c(0,0)

n1 <- mvrnorm(n,mu,sig) %>% as.data.frame %>% mutate(niche_name='n1')
n2 <- mvrnorm(n,mu,sig) %>% as.data.frame %>% mutate(niche_name='n2')

dat <- bind_rows(n1,n2)

ggplot(dat,aes(x=V1,y=V2,color=niche_name)) +
  geom_point() +
  stat_ellipse(type='norm') #

#bw <- estimate_bandwidth(dat %>% select(-niche_name))

#ceiling((10^(3 + sqrt(ncol(dat %>% select(-niche_name)))))/nrow(dat)) #default for samples.per.point in hypervolume call?

#Try changing chunk size and requesting more memory

# hv1 <- dat %>% 
#   filter(niche_name=='n1') %>%
#   select(-niche_name) %>%
#   expectation_convex(verbose=FALSE,check.memory=FALSE)
# 
# hv2 <- dat %>% 
#   filter(niche_name=='n2') %>%
#   select(-niche_name) %>%
#   expectation_convex(verbose=FALSE,check.memory=FALSE)

hvdf <- dat %>%
  group_by(niche_name) %>%
  nest %>%
  mutate(
    #hv=map(data,hypervolume_gaussian,verbose=FALSE),
    hv=map(data,hypervolume_svm,verbose=FALSE)
    #hv=map(data,expectation_convex,verbose=FALSE,check.memory=FALSE)#,
    #hv=map(data,expectation_ball)#,
    #hv=map(data,expectation_box)#,
    #vol=map_dbl(hv,get_volume)) #, ,samples.per.point=1e4
  )


#hvdf <- hvdf %>% mutate(vol=map_dbl(hv,get_volume))

#10^(3+sqrt(ncol(dat %>% select(-niche_name)))) #default num.points.max in hypervolume_set
hvset <- hypervolume_set(hvdf$hv[[1]],hvdf$hv[[2]],
  distance.factor=4, check.memory = FALSE) #, num.points.max = 1e6
hvset@HVList$Intersection@Volume / hvset@HVList$Union@Volume #Jaccard

hvset2 <- hypervolume_set(hvdf$hv[[1]],hvdf$hv[[1]],check.memory = FALSE) #, num.points.max = 1e6
hvset2@HVList$Intersection@Volume / hvset2@HVList$Union@Volume #Jaccard

hvl <- hypervolume_join(hvdf$hv)

# hvg <- hypervolume(dat,'gaussian',verbose=FALSE)
# hvg <- hypervolume(dat,'gaussian',quantile.requested=0.99, verbose=FALSE)
# hvg <- hypervolume(dat,'gaussian',samples.per.point=400,verbose=FALSE)
# hvg <- hypervolume(dat,'gaussian',kde.bandwidth=bw,verbose=FALSE)

plot(hvl,show.density=FALSE)
plot(hvl,show.density=FALSE,show.data=FALSE,show.contour=FALSE) #shows random points
plot(hvl,show.density=FALSE,show.random=FALSE,show.contour=FALSE) #shows data
plot(hvl,show.density=TRUE,show.data=TRUE,show.random=FALSE,show.contour=TRUE) #density and data
plot(hvl,show.density=FALSE,show.data=FALSE,show.random=FALSE,show.contour=TRUE)
plot(hvl,show.density=FALSE,show.data=TRUE, show.centroid = FALSE,
     show.random=FALSE,show.contour=TRUE, show.legend=FALSE) #data and contour
plot(hvl,show.density=FALSE,show.data=FALSE,show.random=FALSE,show.contour=TRUE,
     contour.kde.level=0.05)
plot(hvl,show.density=FALSE,show.data=FALSE,show.random=FALSE,show.contour=TRUE,
  contour.kde.level=0.0001)

10^(3+sqrt(ncol(dat %>% select(-niche_name)))) #default num.points.max in hypervolume_set
25954.55, 50000
#changing num.points.max does not seem to do anything
hvset <- hypervolume_set(hvdf$hv[[1]],hvdf$hv[[2]],check.memory = FALSE, num.points.max = 1e6)
hvset@HVList$Intersection@Volume / hvset@HVList$Union@Volume #Jaccard

hvc <- expectation_convex(mvrnorm(n,mu,sig), check.memory=FALSE)
plot(hvc,show.density=FALSE,show.data=TRUE,show.random=FALSE,show.contour=TRUE,
     show.legend=FALSE, contour.kde.level=0.001) #data and contour

#--- old code ---#
hvs <- hypervolume(dat,'svm',verbose=FALSE)
plot(hvs)
plot(hvs,show.density=FALSE,show.data=FALSE,show.contour=FALSE)

cor(hvg@RandomPoints)
cor(hvs@RandomPoints)

#nice code from here: https://stackoverflow.com/questions/48690755/adding-convex-hull-to-ggplot-map
ndat <- hvg@RandomPoints %>% as.data.frame
hull <- dat %>%
  slice(chull(V1, V2)) #chull returns row numbers, slice selects rows based on numbers
nhull <- ndat %>%
  slice(chull(V1, V2))

cchull <- concaveman(hvg@RandomPoints,concavity=4) %>% as.data.frame

# Define the scatterplot
ggplot(ndat, aes(V1, V2)) + 
  geom_point(shape = 21, color='grey',alpha=0.3) +
  geom_polygon(data = nhull, color='grey',alpha = 0.2, fill=NA) +
  geom_polygon(data = cchull, color='blue',alpha = 0.2, fill=NA ) +
  geom_point(data=dat, color='red',alpha=0.3) +
  geom_polygon(data = hull, color='red',alpha = 0.2) +
  theme_classic()
  
# It seems that 99% contour on hv points is still too aggressive. Visually, lots of false "holes" Perhaps try doing convex hull, or myabe LoCoH to do 95% convex hull?
# How does correlation affect volume estimates? 
#   Try doing volume with correlated variables, then doing volume after pca
# When building hypervolume, seems points are already thresholded at "95%". 
#   So, all points are in the "95% threshold" so it is weird to draw another 95% contour on top of that
#   This might be justification for doing a convex hull around the hv random points.
# Try comparing the 95% ellipse on the data to a convex hull around the hypervolume
#   Should be very similar if points are drawn from normal distribution.
#   Also try drawing a convex hull around the data. Is there any difference?