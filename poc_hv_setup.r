#Come up with a general method to set up multiple hypervolumes for simulation

#TODO: better way of setting up the data that should be more extensible
# first, do one row for each type of distribution (e.g. n)
# then, create reps for each of these types
# if doing pair overlap, create two of each type
# if doing many reps per type (for variability), do, say 30
# if doing pair and many reps, do 2*30 for each type


#Try this approach. 

#See pop_hv_distfact for the most recent attempt

#general way to create distributions seems complicated.
# ideally could have a .parms list column that had all parameter info
# however, the parameters depend on the other data (e.g. dim) so this is complex
# maybe do in two steps. First, make grid for items that are dependencies for paramters
# then, make the .parms list column
# then, expand this grid by n, reps, etc.
# so, general strategy is to do multiple expand_grid calls based on need

.mu <- list(c(-1,0),c(1,0))
.dim <- 2 
.n <- 50
.rep <- 1 

dists <- expand_grid(dim=.dim,n=.n,mu=.mu,rep=.rep)

dat <- dists %>% 
  mutate(ndat=pmap(list(n,dim,mu),function(n,dim,mu){
    sig <- diag(dim)
    mvrnorm(n,mu,sig) %>% as.data.frame
  })) %>%
  mutate(name=c('n1','n2'))

dat %>%
  select(name,ndat) %>%
  unnest(cols=ndat) %>%
ggplot(aes(x=V1,y=V2,color=name)) +
  geom_point() +
  stat_ellipse(type='norm') +
  scale_color_manual(name=NULL,values=c('#0073C2FF','#FC4E07')) +
  coord_equal()
