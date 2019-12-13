options(rgl.useNULL = TRUE)

library(hypervolume)
library(MASS)
library(tidyverse)

source('src/funs.r')
theme_set(theme_pdf)

#--- functions ---#
toPoly <- function(dat) {
  bind_rows(dat,dat[1,]) %>% #close polygon
    as.matrix %>%
    list %>%
    st_polygon %>%
    return
}

.figP <- 'analysis'

set.seed(1504)

#ns <- c(10,100)
ns <- 2^(4:12)
sig <- matrix(c(1,0,0,1),ncol=2)
mu <- c(0,0)

#create the dataframe that specifies the distributions
# dists <- tibble(
#   n=rep(ns,each=2),
#   sig=rep(list(sig),2*length(ns)),
#   mu=rep(list(mu),2*length(ns)))

dists <- tibble(
  n=ns,
  sig=rep(list(sig),length(ns)),
  mu=rep(list(mu),length(ns)))

#create the data
dat <- dists %>% 
  mutate(ndat=pmap(list(n,sig,mu),function(n,sig,mu){
      mvrnorm(n,mu,sig) %>% as.data.frame
    }))

#create the hvs
hvdf <- dat %>%
  mutate(
    hv=map(ndat,hypervolume_gaussian,verbose=FALSE),
    hvol=map_dbl(hv,get_volume))

#create the 95% ellipses
poldf <- hvdf %>%
  mutate(
    eldat=map(ndat,ellipsedf),
    poly=map(eldat,toPoly),
    pvol=map_dbl(poly,st_area))

saveRDS(poldf,file.path(.figP,'volumedat.rds'))

gdat <- poldf %>%
  select(n,hvol,pvol) %>%
  gather(source,vol,-n)

p <- ggplot(gdat,aes(x=log2(n),y=vol,color=source)) +
  geom_line(size=1) +
  scale_x_continuous(breaks=4:12,labels=2^(4:12)) +
  scale_color_manual(name=NULL,values=c('#0073C2FF','#FC4E07'),
    labels=c('Hypervolume','95% ellipse')) +
  labs(x='n',y='Area') +
  theme(legend.position = c(.8, .8)); print(p)

ggsave(file.path(.figP,'volume.pdf'),plot=p,height=4,width=6,device=cairo_pdf) #6,
