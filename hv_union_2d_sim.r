#This script calculates overlap and intersection seperately 
# using different distance factors, in order to accurately estimate overlap

t0 <- Sys.time()

options(rgl.useNULL = TRUE)

#library(alphahull)
#library(glue)
library(hypervolume)
library(MASS)
library(multidplyr)
#library(sf)
#library(tidyverse)

# select <- dplyr::select
# filter <- dplyr::filter

source('src/funs/funs.r')

#---- set up cluster for multidplyr ----#
cl <- new_cluster(6)

cluster_library(cl,'tidyverse') #load libraries in clusters
cluster_send(cl,options(rgl.useNULL = TRUE))
cluster_library(cl,'hypervolume')
cluster_library(cl,'sf')
cluster_assign(cl, unionHvs=unionHvs)
#---- parameters ----#

.resP <- 'analysis/sim'
.simName <- 'union_sim2'

#-- parameters that define the distributions
.mu <- list(c(-2,0),c(0,0),c(2,0))
.names <- c('niche1','niche2','niche3')
.setNames <- c('set1')

#-- Factors that result in new random samples
.ex <- 7:11
.n <- 2^.ex #sample size
.rep <- 1:15 #number of times to sample this distribution/sample size

#-- Factors that do not result in new random sample
.method <- 'gaussian' #hypervolume method
.dfUnion <- 1:6

#Save all parameters
#Note: if running again with the same name will append new values
#TODO: could save timestamp as well. Could save to yml instead of csv
parP <- file.path(.resP,'params.csv')

ls(all.names=TRUE,pattern='\\.') %>%
  enframe(name=NULL,value='var') %>% 
  filter(!var %in% c('.Random.seed','.simName')) %>%
  mutate(
    sim_name=get('.simName'),
    value=map_chr(var,~{toString(get(.))})) %>%
  select(sim_name,var,value) %>%
  write_csv(parP,append=file.exists(parP))


#----
#---- Make the data ----#
#----

#Make the distribution specification dataframe


dists <- tibble(mu=.mu,name=.names) %>%
  expand_grid(set_name=.setNames)

#Expand by factors that should result in new random samples, then make the data
dat <- dists %>%
  expand_grid(n=.n,rep=.rep,.) %>%
  mutate(ndat=pmap(list(n,mu),function(n,mu){
    sig <- diag(2)
    mvrnorm(n,mu,sig) %>% as.data.frame
  })) #%>%
  #mutate(ds_id=row_number())

#----
#---- Make the hypervolumes and ellipses ----#
#----
t1 <- Sys.time()

#Expand by factors that create new hypervolumes
#Make individual hypervolumes and 95% ellipses
message('Creating hypervolumes and ellipses...')

dat1 <- dat %>% expand_grid(method=.method)

hvdf <- dat1 %>%
  partition(cl) %>%
  mutate(hv=pmap(list(ndat,name,method),function(ndat,name,method) {
      hv <- hypervolume(ndat,name=name,method=method,verbose=FALSE)
      return(hv)
    })) %>%
  collect

poldf <- hvdf %>%
  mutate(
    eldat=map(ndat,ellipsedf),
    poly=map(eldat,toPoly),
    pvol=map_dbl(poly,st_area)
  )

message(glue('...complete in {diffmin(t1)} minutes'))

#----
#---- Perform set operations ----#
#----
t1 <- Sys.time()

poldf1 <- poldf %>%
  expand_grid(df_union=.dfUnion) %>%
  select(method,df_union,n,rep,everything()) %>%
  arrange(method,df_union,n,rep)

#Should just be able to group by group_id
#But, need access to dist_fact, n, rep, etc.
# Need to figure out a more general way
message('Performing set operations...')

setdf <- poldf1 %>%
  group_by(df_union,rep,n,set_name) %>% #This needs to be custom based on fields
  nest 

#Use seperate distance factors for union and intersection operations
setun <- setdf %>% 
  partition(cl) %>%
  mutate(
    hv_un=pmap(list(data,df_union),
      function(data,distFact) unionHvs(data$hv,distFact)),
    poly_un=map(data,~{st_union(st_sfc(.$poly))})
    )

#warning from bind_rows as part of the collect call, due to 
# the non-standard datastructure in .$data
# this can be supressed for now: https://github.com/tidyverse/dplyr/issues/2688
setun1 <- suppressWarnings(setun %>% collect)

message(glue('...complete in {diffmin(t1)} minutes'))

#----
#---- Get set stats ----#
#----

stdat <- setun1 %>%
  mutate(
    vol_union_hv=map_dbl(hv_un,~{.@Volume}),
    vol_union_poly=map_dbl(poly_un,st_area)) %>%
  arrange(df_union,n,rep,set_name) %>%
  ungroup

#----
#---- Save results ----#
#----
saveRDS(stdat,file.path(.resP,glue('{.simName}_sim.rds')))

message(glue('Script complete in {diffmin(t0)} minutes'))
