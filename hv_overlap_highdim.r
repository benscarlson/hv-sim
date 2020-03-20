options(rgl.useNULL = TRUE)

library(glue)
library(hypervolume)
library(MASS)
library(RColorBrewer)
library(tidyverse)

source('src/funs.r')
theme_set(themepdf)

.figP <- 'analysis'
.resP <- 'analysis'

.hvMethod <- 'svm'
.distFact <- 1
.simName <- glue('hvoverlap_highdim_distfact{.distFact}')
.title <- glue('Distance factor: {.distFact}')
.cap <- glue('Hypervolume method: {.hvMethod}')
.seed <- rnorm(1) #1504

#TODO: better way of setting up the data that should be more extensible
# first, do one row for each type of distribution (e.g. n)
# then, create reps for each of these types
# if doing pair overlap, create two of each type
# if doing many reps per type (for variability), do, say 30
# if doing pair and many reps, do 2*30 for each type

#-- Set up for testing > 2 dimensions
dims <- c(2,3,4,5) 
ex <- 4:12 #4:12 #this is used as part of the plot
ns <- 2^ex #number of samples
reps <- 1:2 #a vector of length equal to number of hv reps. 

set.seed(.seed)

#----
#---- create the data
#----

dists <- expand_grid(dim=dims,n=ns,reps) %>% select(-reps)

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
    hv=map(ndat, hypervolume, method=.hvMethod,verbose=FALSE))

message(glue('Complete in {diffmin(t1)} minutes'))

#----
#---- estimate overlap
#----

t1 <- Sys.time()

ovdf <- hvdf %>%
  group_by(dim,n) %>%
  nest %>%
  mutate(
    hov=map_dbl(data,~{
      hvset <- hypervolume_set(.$hv[[1]],.$hv[[2]],check.memory = FALSE,
        distance.factor=.distFact) #, num.points.max = 1e6
      hvset@HVList$Intersection@Volume / hvset@HVList$Union@Volume #Jaccard
    }))

message(glue('Complete in {diffmin(t1)} minutes'))

saveRDS(ovdf,file.path(.figP,glue('{.simName}_dat.rds')))

gdat <- ovdf %>%
  select(n,dim,hov) %>%
  ungroup %>%
  mutate(dim=as.factor(dim))

p <- ggplot(gdat,aes(x=log2(n),y=hov,color=dim)) + 
  geom_line(size=1) +
  geom_hline(yintercept=0.95,linetype='dashed',size=0.5,color='red') +
  scale_x_continuous(breaks=ex,labels=2^ex) +
  scale_color_brewer(palette='Dark2') +
  lims(y=0:1) +
  labs(title=.title,x='n',y='Overlap (Jaccard)',color='Dimensions',caption=.cap) +
  theme(legend.position = c(.8, .2)); print(p)

saveRDS(p,file.path(.figP,glue('{.simName}_gg.rds')))
ggsave(file.path(.figP,glue('{.simName}.pdf')),plot=p,height=5,width=7,device=cairo_pdf) #6,
