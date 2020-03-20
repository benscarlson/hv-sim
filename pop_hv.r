library(alphahull)
library(glue)

#This should be some sort of general function that creates sets of hvs. See poc_hv_setup.r
.mu <- list(c(-2,0),c(-1,0),c(0,0),c(1,0),c(2,0))
.dim <- 2 
.n <- 2000
.rep <- 1 
.method <- 'gaussian'
.names <- paste('niche',1:length(.mu),sep='')
.simName <- glue('pop_hv_construction_{.n}')

.figP <- 'analysis'

dists <- expand_grid(dim=.dim,n=.n,mu=.mu,method=.method,rep=.rep)

dat <- dists %>% 
  mutate(ndat=pmap(list(n,dim,mu),function(n,dim,mu){
    sig <- diag(dim)
    mvrnorm(n,mu,sig) %>% as.data.frame
  })) %>%
  mutate(name=.names)

#----
#---- Make the hypervolumes ----#
#----
t1 <- Sys.time()

#Make individual hypervolumes
hvdf <- dat %>%
  mutate(hv=pmap(list(ndat,name,method),function(ndat,name,method) {
      hypervolume(ndat,name=name,method=method,verbose=FALSE)
    }))

#Make another hypervolume that is random draw from pooled niches
hvPool <- dat$ndat %>% bind_rows %>% sample_n(.n) %>%
  hypervolume(method=.method,name='pooled',verbose=FALSE)

#Make a hypervolume that is the union of all individual hypervolumes
hvUn <- hvdf$hv %>% reduce(.f=function(hv1,hv2) {
  hvSet <- hypervolume_set(hv1, hv2, check.memory=FALSE, verbose=FALSE)
  union <- hvSet@HVList[[4]]
  union@Name <- 'union'
  return(union)
})

message(glue('Complete in {diffmin(t1)} minutes'))

hvs <- c(hvdf$hv,hvPool,hvUn)

hvDat <- hvs %>% 
  map(fortify_hv,slot='RandomPoints') %>% bind_rows %>% select(-slot)

#---- look at the alpha shapes ----
asdf <- hvDat %>% group_by(name) %>% nest %>% 
  mutate(
    ashape=map(data,ashape,alpha=1),
    asdat=map(ashape,fortify))

asdat <- asdf %>% select(name,asdat) %>% unnest(cols=asdat)

p <- asdat %>%
  mutate(ntype=ifelse(name %in% c('pooled','union'),'population','individual')) %>%
ggplot(aes(x=x,y=y,color=name,linetype=ntype,size=ntype)) +
  geom_polygon(fill=NA) +
  scale_linetype_manual(values=c('dashed','solid')) +
  scale_size_manual(values=c(0.5,1)) +
  coord_equal() +
  labs(title=glue('Points per niche = {.n}')); print(p)

#saveRDS(p,file.path(.figP,glue('{.simName}_gg.rds')))
ggsave(file.path(.figP,glue('{.simName}.pdf')),plot=p,height=5,width=7,device=cairo_pdf) #6,
