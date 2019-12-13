options(rgl.useNULL = TRUE)

library(glue)
library(hypervolume)
library(MASS)
library(RColorBrewer)
library(tidyverse)

source('src/funs.r')
theme_set(theme_pdf)

.figP <- 'analysis'
.resP <- 'analysis'

set.seed(1504)

#-- Set up for testing > 2 dimensions
dims <- c(2,3,4,5)
#ns <- c(10,20,30)
ex <- 4:12 #this is used as part of the plot
ns <- 2^ex
reps <- 1:2 #a vector of length equal to number of hv reps. 

dists <- expand_grid(dim=dims,n=ns,reps) %>% select(-reps)

#----
#---- create the data
#----

dat <- dists %>% 
  mutate(ndat=pmap(list(n,dim),function(n,dim){
    sig <- diag(dim)
    mu <- rep(0,dim)
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

saveRDS(hvdf,file.path(.figP,'highdim_hvdat.rds'))

#----
#---- estimate overlap
#----

t1 <- Sys.time()

ovdf <- hvdf %>%
  group_by(dim,n) %>%
  nest %>%
  mutate(
    hov=map_dbl(data,~{
      hvset <- hypervolume_set(.$hv[[1]],.$hv[[2]],check.memory = FALSE) #, num.points.max = 1e6
      hvset@HVList$Intersection@Volume / hvset@HVList$Union@Volume #Jaccard
    }))

message(glue('Complete in {diffmin(t1)} minutes'))

#saveRDS(ovdf,file.path(.figP,'overlapdat.rds'))

gdat <- ovdf %>%
  select(n,dim,hov) %>%
  ungroup %>%
  mutate(dim=as.factor(dim))


p <- ggplot(gdat,aes(x=log2(n),y=hov,color=dim)) + 
  geom_line(size=1) +
  #geom_hline(yintercept=0.95,linetype='dashed',size=0.5) +
  scale_x_continuous(breaks=ex,labels=2^ex) +
  scale_color_brewer(palette='Dark2') +
  lims(y=0:1) +
  labs(x='n',y='Overlap (Jaccard)',color='Dimensions') +
  theme(legend.position = c(.8, .2)); print(p)

ggsave(file.path(.figP,'overlap_highdim.pdf'),plot=p,height=5,width=7,device=cairo_pdf) #6,
