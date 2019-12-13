options(rgl.useNULL = TRUE)

library(glue)
library(hypervolume)
library(MASS)
library(tidyverse)

source('src/funs.r')
theme_set(theme_pdf)

.figP <- 'analysis'
.resP <- 'analysis'

set.seed(1504)

#-- This setup saved as overlapdat.rds
ns <- 2^(4:12)
sig <- matrix(c(1,0,0,1),ncol=2)
mu <- c(0,0)

dists <- tibble(
  n=rep(ns,each=2),
  sig=rep(list(sig),2*length(ns)),
  mu=rep(list(mu),2*length(ns)))

#----
#---- create the data
#----

dat <- dists %>% 
  mutate(ndat=pmap(list(n,sig,mu),function(n,sig,mu){
    mvrnorm(n,mu,sig) %>% as.data.frame
  }))

#----
#---- create the hypervolumes
#----

t1 <- Sys.time()

#create the hvs
hvdf <- dat %>%
  mutate(
    hv=map(ndat,hypervolume_gaussian,verbose=FALSE),
    hvol=map_dbl(hv,get_volume))

t2 <- Sys.time()

message(glue('Complete in {diffmin(t1,t2)} minutes'))

#create the 95% ellipses
poldf <- hvdf %>%
  mutate(
    eldat=map(ndat,ellipsedf),
    poly=map(eldat,toPoly),
    pvol=map_dbl(poly,st_area))

t1 <- Sys.time()

ovdf <- poldf %>%
  group_by(n) %>%
  nest %>%
  mutate(
    hov=map_dbl(data,~{
      hvset <- hypervolume_set(.$hv[[1]],.$hv[[2]],check.memory = FALSE) #, num.points.max = 1e6
      hvset@HVList$Intersection@Volume / hvset@HVList$Union@Volume #Jaccard
    }),
    pov=map_dbl(data,~{
      st_area(st_intersection(.$poly[[1]],.$poly[[2]])) / st_area(st_union(.$poly[[1]],.$poly[[2]]))
    }))
message(glue('Complete in {diffmin(t1)} minutes'))

saveRDS(ovdf,file.path(.figP,'overlapdat.rds'))

gdat <- ovdf %>%
  select(n,hov,pov) %>%
  gather(source,overlap,-n)

p <- ggplot(gdat,aes(x=log2(n),y=overlap,color=source)) +
  geom_line(size=1) +
  geom_hline(yintercept=0.95,linetype='dashed',size=0.5) +
  scale_x_continuous(breaks=4:12,labels=2^(4:12)) +
  scale_color_manual(name=NULL,values=c('#0073C2FF','#FC4E07'),
                     labels=c(hov='Hypervolume',pov='95% ellipse')) +
  lims(y=0:1) +
  labs(x='n',y='Overlap (Jaccard)') +
  theme(legend.position = c(.8, .2)); print(p)

ggsave(file.path(.figP,'overlap.pdf'),plot=p,height=4,width=6,device=cairo_pdf) #6,
