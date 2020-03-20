#This script calculates overlap and intersection seperately 
# using different distance factors, in order to accurately estimate overlap

t0 <- Sys.time()

options(rgl.useNULL = TRUE)

library(hypervolume)
library(MASS)
library(multidplyr)

source('src/funs/funs.r')

#---- parameters ----#

.simName <- 'union_df_2015'

.resP <- 'analysis/sim'
.pd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
.hvJob <- '5axes2000pts1'

.dfUnion <- 1:6

.nclust <- 6
.seed <- 1504

hvP <- file.path(.pd,'hvs',.hvJob)
set.seed(.seed)

#---- set up cluster for multidplyr ----#
message('Setting up cluster')
cl <- new_cluster(.nclust)

cluster_library(cl,'tidyverse') #load libraries in clusters
cluster_send(cl,options(rgl.useNULL = TRUE))
cluster_library(cl,'hypervolume')

cluster_assign(cl, unionHvs=unionHvs)

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
#---- Load the data ----#
#----

niches <- read_csv(file.path(.pd,'niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>%
  #filter(niche_set=='beuster-2015')
  filter(year==2015)

hvdf <- niches %>%
  select(niche_set,niche_name) %>%
  mutate(hv=map(niche_name,~{
    pfn <- file.path(hvP,'hvs',glue('{.}.rds'))
    tryCatch(readRDS(pfn),error=function(e) NULL)
  }))

#----
#---- Perform set operations ----#
#----
t1 <- Sys.time()

hvdf1 <- hvdf %>%
  group_by(niche_set) %>% #This needs to be custom based on fields
  nest %>%
  expand_grid(df_union=.dfUnion)

message('Performing union operations...')

hvun <- hvdf1 %>% 
  partition(cl) %>%
  mutate(
    hv_un=pmap(list(data,df_union),
      function(data,distFact) unionHvs(data$hv,distFact))
    )

#warning from bind_rows as part of the collect call, due to 
# the non-standard datastructure in .$data
# this can be supressed for now: https://github.com/tidyverse/dplyr/issues/2688
hvun1 <- suppressWarnings(hvun %>% collect)

message(glue('...complete in {diffmin(t1)} minutes'))

#----
#---- Get set stats ----#
#----

stdat <- hvun1 %>%
  mutate(
    vol_union_hv=map_dbl(hv_un,~{.@Volume})) %>%
  arrange(niche_set,df_union) %>%
  ungroup

#----
#---- Save results ----#
#----
saveRDS(stdat,file.path(.resP,glue('{.simName}_sim.rds')))

message(glue('Script complete in {diffmin(t0)} minutes'))
