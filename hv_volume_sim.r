#Simulates hypervolumes using different algorithms and extracts volume.

t0 <- Sys.time()

options(rgl.useNULL = TRUE)

library(hypervolume)
library(MASS)
library(multidplyr)

.seed <- 1504

set.seed(.seed)

source('src/funs/funs.r')

#---- parameters ----#
.cores <- 6

.resP <- 'analysis/sim'
.simName <- 'volume1'

#-- parameters that define the distributions
.mu <- list(c(0,0))
.names <- c('niche1')
.setNames <- c('set1')

#-- Factors that result in new random samples
.ex <- 4:11
.n <- 2^.ex #sample size
.rep <- 1:15 #number of times to sample this distribution/sample size

#-- Factors that do not result in new random sample
.method <- c('gaussian','svm') #hypervolume method

#---- construct paths ----#
parPF <- file.path(.resP,'hv_volume_sim_params.csv')
resPF <- file.path(.resP,glue('{.simName}_sim.rds'))

#Save all parameters
#Note: if running again with the same name will append new values
#TODO: could save timestamp as well. Could save to yml instead of csv


ls(all.names=TRUE,pattern='^\\.') %>%
  enframe(name=NULL,value='var') %>% 
  filter(!var %in% c('.Random.seed','.simName')) %>%
  mutate(
    sim_name=get('.simName'),
    value=map_chr(var,~{toString(get(.))})) %>%
  select(sim_name,var,value) %>%
  write_csv(parPF,append=file.exists(parPF))

#----
#---- set up cluster for multidplyr ----#
#----
message('Setting up the cluster...')

cl <- new_cluster(.cores)

cluster_library(cl,'tidyverse') #load libraries in clusters
cluster_send(cl,options(rgl.useNULL = TRUE))
cluster_library(cl,'hypervolume')

#----
#---- Make the data ----#
#----

#Make the distribution specification dataframe
dists <- tibble(mu=.mu,name=.names) %>%
  expand_grid(set_name=.setNames)

#Expand by factors that should result in new random samples, then make the data
distdf <- dists %>%
  expand_grid(n=.n,rep=.rep,.) %>%
  mutate(ndat=pmap(list(n,mu),function(n,mu){
    sig <- diag(2)
    mvrnorm(n,mu,sig) %>% as.data.frame
  }))

#----
#---- Make the polygons ----#
#----

poldf <- distdf %>%
  mutate(
    eldat=map(ndat,ellipsedf),
    poly=map(eldat,toPoly),
    pvol=map_dbl(poly,st_area)
  )

#----
#---- Make the hypervolumes ----#
#----
t1 <- Sys.time()

#Expand by factors that create new hypervolumes
hvdf0 <- poldf %>% expand_grid(method=.method)

message(glue('Creating {nrow(hvdf0)} hypervolumes...'))

#Make individual hypervolumes
hvdf <- hvdf0 %>%
  partition(cl) %>%
  mutate(hv=pmap(list(ndat,name,method),function(ndat,name,method) {
      hv <- hypervolume(ndat,name=name,method=method,verbose=FALSE)
      return(hv)
    })) %>% collect

message(glue('...complete in {diffmin(t1)} minutes'))

#----
#---- Get volume info ----#
#----

stdat <- hvdf %>%
  mutate(vol=map_dbl(hv,~{.@Volume})) %>%
  arrange(method,n,rep) %>%
  ungroup

#----
#---- Save results ----#
#----
saveRDS(stdat,resPF)

message(glue('Script complete in {diffmin(t0)} minutes'))
