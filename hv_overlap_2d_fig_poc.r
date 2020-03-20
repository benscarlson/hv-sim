
options(rgl.useNULL = TRUE)

library(hypervolume)
library(stringr)

source('src/funs/funs.r')
source('src/funs/themes.r')
theme_set(theme_eda)

.simName <- 'sim4'
.resP <- 'analysis/sim'
.figP <- 'analysis/fig'

stdat <- readRDS(file.path(.resP,glue('{.simName}_sim.rds')))

#figure out expected overlap. grab a set of niches and find the parameters
mu <- stdat %>% slice(1) %>% select(data) %>% unnest(cols=data) %>% pluck('mu')
expect <- ellipse_set(mu1=mu[[1]],mu2=mu[[2]])

.ex <- log(unique(stdat$n),base=2) #this is used for plotting

#----
#---- figures ----#
#----

#---- look at one of the niche sets

gdat <- stdat %>%
  filter(dist_fact==1 & n==2^11) %>%
  select(data) %>%
  unnest(data) %>%
  select(name,ndat) %>%
  unnest(ndat)

ggplot(gdat, aes(x=V1,y=V2,color=name)) +
  geom_point() +
  stat_ellipse(type='norm') +
  coord_equal()


#----
#---- figure for sims with repetitions
#----

#Calculate confidence intervals using t distribution
gdat <- stdat %>%
  select(-data,-hvset) %>%
  mutate(dist_fact=as.factor(dist_fact)) %>%
  gather(type,value,-c(n,dist_fact,rep)) %>%
  #filter(n==16,dist_fact==1,type %in% c('jac_hv','jac_poly')) %>%
  group_by(n,dist_fact,type) %>% #summarize over repetitions
  summarize(mean=mean(value),min=min(value),max=max(value),
    sd=sd(value), num=n(), se=sd/sqrt(num), df=num-1, t=qt(0.975,df=df),
    cil=mean - (t * se),ciu=mean + (t * se))


# dat <- stdat %>% select(n,dist_fact,rep,jac_hv) %>% filter(n==16 & dist_fact==1)
# 
# qt(0.025,df=4)*sd(dat$jac_hv)/sqrt(5)
#This figure shows how union changes with n and distfactor
#c('vol_intr_hv','vol_intr_poly')
#c('vol_union_hv','vol_union_poly')

types <- c('vol_intr_hv','vol_intr_poly')
yintr <- expect['intersection']
title <- 'Intersection'
figF <- glue('{.simName}_intersection.pdf')
ylab <- 'Area'

types <- c('vol_union_hv','vol_union_poly')
yintr <- expect['union']
title <- 'Union'
figF <- glue('{.simName}_union.pdf')
ylab <- 'Area'

yintr <- expect['intersection']/expect['union']

gdat %>% 
  #filter(type %in% c('jac_hv','jac_poly')) %>%
  filter(type %in% types) %>%
  #filter(dist_fact %in% 1) %>%
ggplot(aes(x=log2(n),y=mean,color=type,fill=type)) + 
  geom_ribbon(aes(ymin=cil,ymax=ciu),alpha=0.2,color=NA) +
  geom_line(size=1) +
  geom_hline(yintercept=yintr,linetype='dashed') +
  scale_x_continuous(breaks=.ex,labels=2^.ex) +
  #scale_color_brewer(palette='Paired') +
  scale_color_manual(name=NULL,values=c('#0073C2FF','#FC4E07'),
    #labels=c(jac_hv='Hypervolume',jac_poly='95% ellipse'),
    aesthetics=c('color','fill')) +
  facet_wrap(vars(dist_fact),ncol=3) +
  theme(legend.position = c(.8, .8)) + 
  #lims(y=0:1) #+
  labs(title=title,x='n',y=ylab) -> p; #,color='Dimensions',caption=.cap
  # 
#saveRDS(p,file.path(.figP,glue('{.simName}_gg.rds')))
ggsave(file.path(.figP,figF),plot=p,height=4,width=12,device=cairo_pdf) #6,

#----
#---- figure for set component x dist fact 
#----

figF <- glue('{.simName}_comp_df.pdf')

v <- c(expect,expect[1]/expect[2])
names(v) <- c('Intersection','Union','Overlap')
expdat <- expand_grid(enframe(v,name='component'),dist_fact=1:3) %>%
  mutate(component=factor(component,levels=c('Intersection','Union','Overlap')))

#Calculate confidence intervals using t distribution
gdat <- stdat %>%
  select(-data,-hvset) %>%
  gather(type,value,-c(n,dist_fact,rep)) %>%
  #filter(n==16,dist_fact==1,type %in% c('jac_hv','jac_poly')) %>%
  group_by(n,dist_fact,type) %>% #summarize over repetitions
  summarize(mean=mean(value),min=min(value),max=max(value),
            sd=sd(value), num=n(), se=sd/sqrt(num), df=num-1, t=qt(0.975,df=df),
            cil=mean - (t * se),ciu=mean + (t * se)) %>%
  ungroup %>%
  mutate(
    #dist_fact=as.factor(dist_fact),
    component = factor(case_when(
      type %in% c('vol_intr_hv','vol_intr_poly') ~ 'Intersection',
      type %in% c('vol_union_hv','vol_union_poly') ~ 'Union',
      type %in% c('jac_hv','jac_poly') ~ 'Overlap'),
      levels=c('Intersection','Union','Overlap')),
    method = ifelse(str_detect(type,'hv'),'Hypervolume','95% Ellipse'))


#yintr <- expect['intersection']/expect['union']

gdat %>% 
  ggplot(aes(x=log2(n),y=mean,color=method,fill=method)) + 
  geom_ribbon(aes(ymin=cil,ymax=ciu),alpha=0.2,color=NA) +
  geom_line(size=1) +
  geom_hline(data=expdat,aes(yintercept=value),linetype='dashed') +
  scale_x_continuous(breaks=.ex,labels=2^.ex) +
  scale_color_manual(name=NULL,values=c('#0073C2FF','#FC4E07'),
                     aesthetics=c('color','fill')) + #
  facet_grid(vars(component),vars(dist_fact),scales='free_y') +
  theme(legend.position='bottom') +
  labs(x='n',y=NULL) -> p; print(p)#,color='Dimensions',caption=.cap #title=title,
# 
#saveRDS(p,file.path(.figP,glue('{.simName}_gg.rds')))
ggsave(file.path(.figP,figF),plot=p,height=6,width=10,device=cairo_pdf) #6,
#----
#---- figure for sims with no repetitions
#----

elset <- ellipse_set(c(-1,0),c(1,0))

gdat <- stdat %>%
  select(-data,-hvset,-rep) %>%
  mutate(dist_fact=as.factor(dist_fact)) %>%
  gather(type,area,-c(n,dist_fact)) %>%
  #filter(type %in% c('jac_hv','jac_poly'))
  #filter(type %in% c('vol_intr_hv','vol_intr_poly'))
  filter(type %in% c('vol_union_hv','vol_union_poly'))

#labels=c(jac_hv='Hypervolume',jac_poly='95% ellipse')
labels=c(vol_union_hv='Hypervolume',vol_union_poly='95% ellipse')

ggplot(gdat, aes(x=log2(n),y=area,color=type)) + 
  geom_line(size=1) +
  geom_hline(yintercept = elset[2]) +
  scale_x_continuous(breaks=.ex,labels=2^.ex) +
  scale_color_manual(name=NULL,values=c('#0073C2FF','#FC4E07'),
                     labels=labels,aesthetics=c('color')) +
  facet_wrap(vars(dist_fact),ncol=2) #+
#lims(y=c(.1,.5)) #+
#theme(legend.position = c(.8, .8)) -> p; print(p) #+
#seems both hv and 95% ellipse are similar for intr and union
# so why is jaccard so different? Strange! Look at below plot but for jaccard...


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