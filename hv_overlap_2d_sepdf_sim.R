#This script calculates overlap and intersection seperately 
# using different distance factors, in order to accurately estimate overlap

t0 <- Sys.time()

options(rgl.useNULL = TRUE)

library(alphahull)
library(glue)
library(hypervolume)
library(MASS)
library(multidplyr)
library(sf)
library(tidyverse)

select <- dplyr::select
filter <- dplyr::filter

source('src/funs/funs.r')

#---- set up cluster for multidplyr ----#
cl <- new_cluster(6)

cluster_library(cl,'tidyverse') #load libraries in clusters
cluster_send(cl,options(rgl.useNULL = TRUE))
cluster_library(cl,'hypervolume')

#---- parameters ----#

.resP <- 'analysis/sim'

.simName <- 'sepdf1'

#-- parameters that define the distributions
.mu <- list(c(-1,0),c(1,0)) #overlap: ~0.3
.dim <- c(2,2) #dimensions
.names <- c('niche1','niche2')

#-- Factors that result in new random samples
.ex <- 4:11
.n <- 2^.ex #sample size
.rep <- 1:10 #number of times to sample this distribution/sample size

#-- Factors that do not result in new random sample
.method <- 'gaussian' #hypervolume method
.dfUnion <- 3
.dfIntr <- 1

#----
#---- Make the data ----#
#----

#Make the distribution specification dataframe
#Expand by factors that should result in new random samples, then make the data
dat <- tibble(dim=.dim,mu=.mu,name=.names) %>%
  expand_grid(n=.n,rep=.rep,.) %>%
  mutate(ndat=pmap(list(n,dim,mu),function(n,dim,mu){
    sig <- diag(dim)
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

dat1 <- poldf %>%
  expand_grid(df_union=.dfUnion,df_intr=.dfIntr) %>%
  select(method,df_union,df_intr,n,rep,everything()) %>%
  arrange(method,df_union,df_intr,n,rep)

#Should just be able to group by group_id
#But, need access to dist_fact, n, rep, etc.
# Need to figure out a more general way
message('Performing set operations...')

setdf <- dat1 %>%
  group_by(df_union,df_intr,rep,n) %>% #This needs to be custom based on fields
  nest 

#Use seperate distance factors for union and intersection operations
setpar <- setdf %>% 
  partition(cl) %>%
  mutate(
    hv_intr=pmap(list(df_intr,data),function(df_intr,data) {
      hvset <- hypervolume_set(data$hv[[1]],data$hv[[2]],
        distance.factor=df_intr, check.memory = FALSE) #, num.points.max = 1e6
      return(hvset@HVList$Intersection)
    }),
    hv_union=pmap(list(df_union,data),function(df_union,data) {
      hvset <- hypervolume_set(data$hv[[1]],data$hv[[2]],
                               distance.factor=df_union, check.memory = FALSE) #, num.points.max = 1e6
      return(hvset@HVList$Union)
    })) 

#warning from bind_rows as part of the collect call, due to 
# the non-standard datastructure in .$data
# this can be supressed for now: https://github.com/tidyverse/dplyr/issues/2688
setdf1 <- suppressWarnings(setpar %>% collect)

message(glue('...complete in {diffmin(t1)} minutes'))

#----
#---- Get set stats ----#
#----

stdat <- setdf1 %>%
  mutate(
    vol_intr_hv=map_dbl(hv_intr,~{.@Volume}),
    vol_union_hv=map_dbl(hv_union,~{.@Volume}),
    vol_intr_poly=map_dbl(data,~{st_area(st_intersection(.$poly[[1]],.$poly[[2]]))}),
    vol_union_poly=map_dbl(data,~{st_area(st_union(.$poly[[1]],.$poly[[2]]))}),
    jac_hv=vol_intr_hv/vol_union_hv,
    jac_poly=vol_intr_poly/vol_union_poly) %>%
  arrange(df_intr,df_union,n,rep) %>%
  #select(-data,-hvset) %>%
  ungroup
#look at the results

#Should save this with something like set_operations_2d or something
saveRDS(stdat,file.path(.resP,glue('{.simName}_sim.rds')))

message(glue('Script complete in {diffmin(t0)} minutes'))

#--- keep this code ----#
# Make union of n hypervolumes in a tidy workflow

# mutate(hv_un=pmap(list(data,dist_fact),function(data,dist_fact){
#   data$hv %>%
#     reduce(.f=function(hv1,hv2) {
#       hvSet <- hypervolume_set(hv1, hv2,
#         check.memory=FALSE, verbose=FALSE,distance.factor=dist_fact)
#       union <- hvSet@HVList[[4]]
#       union@Name <- 'union'
#       return(union)
#     })