options(rgl.useNULL = TRUE)

library(glue)
library(hypervolume)
library(MASS)
library(multidplyr)
library(sf)
library(tidyverse)

source('src/funs/themes.r')
theme_set(theme_eda)

.figP <- 'analysis'
.resP <- 'analysis'
.seed <- 1540 #rnorm(1)

set.seed(.seed)


.hvalg <- 'hypervolume_svm'
.distFact <- 3

.simName <- glue('hvsvm_ellipse_overlap_df{.distFact}')
.cap <- glue('Hypervolume algorithm: {.hvalg}.')
.title <- glue('Distance factor: {.distFact}')

ex <- 4:12
ns <- 2^ex
sig <- matrix(c(1,0,0,1),ncol=2)
mu <- c(0,0)
reps <- 30

dists <- tibble(
    group=rep(seq_along(ns),each=2), #all niches of size n are in the same group
    n=rep(ns,each=2),
    sig=rep(list(sig),2*length(ns)),
    mu=rep(list(mu),2*length(ns))) %>%  
  slice(rep(seq_len(n()), reps)) %>% #this is the number of replicate pairs
  arrange(n) %>%
  mutate(rep=rep(seq_along(1:(n()/2)),each=2)) #each pair within a group is a rep

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
    hv=map(ndat,hypervolume_svm,verbose=FALSE),
    #hv=map(ndat,hypervolume_gaussian,verbose=FALSE),
    #hv=map(ndat,expectation_ball),
    hvol=map_dbl(hv,get_volume))

message(glue('Complete in {diffmin(t1)} minutes'))

#create the 95% ellipses
poldf <- hvdf %>%
  mutate(
    eldat=map(ndat,ellipsedf),
    poly=map(eldat,toPoly),
    pvol=map_dbl(poly,st_area))

t1 <- Sys.time()

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
message(glue('Overlap complete in {diffmin(t1)} minutes'))

#saveRDS(ovdf,file.path(.figP,'overlapdat.rds'))

gdat <- ovdf %>%
  select(group,rep,n,hov,pov) %>%
  gather(source,overlap,-c(group,rep,n)) %>%
  arrange(group,rep) %>%
  group_by(group,n,source) %>%
  summarize(mean=mean(overlap),max=max(overlap),min=min(overlap))

p <- ggplot(gdat,aes(x=log2(n),y=mean,color=source,fill=source)) +
  geom_hline(yintercept=0.95,linetype='dashed',size=0.5) +
  geom_ribbon(aes(ymin=min,ymax=max),alpha=0.6,color=NA) +
  geom_line(size=1) +
  scale_x_continuous(breaks=ex,labels=2^ex) +
  scale_color_manual(name=NULL,values=c('#0073C2FF','#FC4E07'),
    labels=c(hov='Hypervolume',pov='95% ellipse'),aesthetics=c('color','fill')) +
  lims(y=0:1) +
  labs(title=.title, x='n',y='Overlap (Jaccard)',caption=.cap) +
  theme(legend.position = c(.8, .2)); print(p)

saveRDS(p,file.path(.figP,glue('{.simName}_gg.rds')))
ggsave(file.path(.figP,glue('{.simName}.pdf')),plot=p,height=4,width=6,device=cairo_pdf) #6,

