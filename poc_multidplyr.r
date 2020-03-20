#TODO: NOT WORKING
# Seems I need to do more to manage the clusters
# Maybe loading libraries, assigning custom functions to cluster, etc.
# Here is an example of using custom function and mutate/map workflow:
#   https://www.business-science.io/code-tools/2016/12/18/multidplyr.html
#   This link has outdated code. Get an idea but check help for the new syntax.

#devtools::install_github("hadley/multidplyr")

#https://github.com/tidyverse/multidplyr/blob/master/vignettes/multidplyr.md

options(rgl.useNULL = TRUE)

library(glue)
library(hypervolume)
library(MASS)
library(multidplyr)
library(tidyverse)
library(nycflights13)

source('src/funs/funs.r')

cl <- new_cluster(3)

cluster_library(cl,'tidyverse') #load libraries in clusters
cluster_send(cl,options(rgl.useNULL = TRUE))
cluster_library(cl,'hypervolume')

#cl <- default_cluster()

# flight_dest <- flights %>% group_by(dest) %>% partition(cluster)
# flight_dest
# 
# flight_dest %>% 
#   summarise(delay = mean(dep_delay, na.rm = TRUE), n = n()) %>% 
#   collect()

#-- parameters that define the distributions
#.mu <- list(c(-2,0),c(-1,0),c(0,0),c(1,0),c(2,0))
.mu <- list(c(-1,0),c(0,0),c(1,0))
.dim <- c(2,2,2) #dimensions
.names <- c('niche1','niche2','niche2')
.n <- 1000

dat <- tibble(dim=.dim,mu=.mu,name=.names,n=.n) %>%
  mutate(ndat=pmap(list(n,dim,mu),function(n,dim,mu){
    sig <- diag(dim)
    mvrnorm(n,mu,sig) %>% as.data.frame
  })) %>%
  mutate(group=row_number())

f1 <- function(val) { 
  Sys.sleep(2)
  return(val)
}

cluster_assign(cl, f1=f1)

system.time({
  dat %>%mutate(ret=map_dbl(group,f1))
})



system.time({
  datPar %>%
    mutate(ret=map_dbl(group,f1)) %>%
    collect()
})

system.time({
  dat %>%
    mutate(hv=pmap(list(ndat,name),function(ndat,name) {
      hypervolume(ndat,name=name,method='gaussian',verbose=FALSE)
    }))
})

datPar <- dat %>%
  partition(cl)

system.time({
  hvdf <- datPar %>%
    mutate(hv=pmap(list(ndat,name),function(ndat,name) {
      hypervolume(ndat,name=name,method='gaussian',verbose=FALSE)
    })) %>% 
    collect
})

plot(hvdf$hv[[2]])

cluster_call(cl,search())[[1]]
# .mu <- list(c(-1,0),c(1,0))
# .dim <- 2 #dimensions
# .names <- paste('niche',1:length(.mu),sep='')
# .ex <- 8
# .n <- 2^.ex #sample size
# #.n <- 10
# .rep <- 1:5 #number of times to sample this distribution/sample size
# .method <- 'gaussian' #hypervolume method
# .distFact <- 1

#Make the grid that specifies the distribution parameters
dists <- expand_grid(dim=.dim,mu=.mu) %>%
  mutate(name=.names)

#Make the groups. Each group will be expanded by the dists
groups <- expand_grid(n=.n,method=.method,dist_fact=.distFact,rep=.rep) %>%
  mutate(group_id=row_number())

#Expand dists to groups and then create the data
dat <- groups %>%
  expand_grid(dists) %>%
  mutate(ndat=pmap(list(n,dim,mu),function(n,dim,mu){
    sig <- diag(dim)
    mvrnorm(n,mu,sig) %>% as.data.frame
  }))

t1 <- Sys.time()

#Make individual hypervolumes and 95% ellipses
hvdf <- dat %>%
  mutate(hv=pmap(list(ndat,name,method),function(ndat,name,method) {
    hypervolume(ndat,name=name,method=method,verbose=FALSE)
  }))

message(glue('Complete in {diffmin(t1)} minutes'))

t1 <- Sys.time()

volDat <- hvdf %>%
  group_by(group_id,dist_fact,rep,n) %>% 
  nest %>%
  partition(cluster) %>%
  mutate(
    hvset=map(data,~{
      hypervolume_set(.$hv[[1]],.$hv[[2]],
        distance.factor=.distFact, check.memory = FALSE) #, num.points.max = 1e6
    }))
message(glue('Complete in {diffmin(t1)} minutes'))


#--- shut down cluster ----#

#to shut down cluster, need to rm all partition dataframes. then gc will clean them up
# https://github.com/tidyverse/multidplyr/issues/31