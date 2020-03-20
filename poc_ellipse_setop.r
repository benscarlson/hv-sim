options(rgl.useNULL = TRUE)

library(hypervolume)
library(MASS)
library(tidyverse)
library(sf)

source('src/funs.r')
theme_set(themepdf)

#set.seed(1504)

sig <- matrix(c(1,0,0,1),ncol=2)
n <- 1000 #recommended obsevations should be > exp(number of axes). exp(2) = 7.39

n1 <- mvrnorm(n,c(-0.5,0),sig) %>% as.data.frame %>% mutate(niche_name='niche1')
n2 <- mvrnorm(n,c(0.5,0),sig) %>% as.data.frame %>% mutate(niche_name='niche2')

dat <- bind_rows(n1,n2)

ggplot(dat,aes(x=V1,y=V2,color=niche_name)) +
  geom_point() +
  stat_ellipse(type='norm') +
  coord_fixed()

poldf <- dat %>%
  group_by(niche_name) %>%
  nest %>%
  mutate(
    eldat=map(data,ellipsedf),
    poly=map(eldat,toPoly),
    pvol=map_dbl(poly,st_area),
    hv=map(data,hypervolume_svm,verbose=FALSE))

hvset <- hypervolume_set(poldf$hv[[1]],poldf$hv[[2]],
                distance.factor=3, check.memory = FALSE)

hvset@HVList$Intersection@Volume
hvset@HVList$Union@Volume

st_area(st_intersection(poldf$poly[[1]],poldf$poly[[2]]))
st_area(st_union(poldf$poly[[1]],poldf$poly[[2]]))

ovdf <- poldf %>%
  group_by(group,rep,n) %>%
  nest %>%
  mutate(
    hov=map_dbl(data,~{
      hvset <- hypervolume_set(.$hv[[1]],.$hv[[2]],
                               distance.factor=.distFact, check.memory = FALSE) #, num.points.max = 1e6
      hvset@HVList$Intersection@Volume / hvset@HVList$Union@Volume #Jaccard
    }),
    pov=map_dbl(data,~{
      st_area(st_intersection(.$poly[[1]],.$poly[[2]])) / st_area(st_union(.$poly[[1]],.$poly[[2]]))
    }))
