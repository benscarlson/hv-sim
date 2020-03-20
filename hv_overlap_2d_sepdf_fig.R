
options(rgl.useNULL = TRUE)

library(hypervolume)
library(stringr)

source('src/funs/funs.r')
source('src/funs/themes.r')
theme_set(theme_eda)

.simName <- 'sepdf1'
.resP <- 'analysis/sim'
.figP <- 'analysis/fig'

stdat <- readRDS(file.path(.resP,glue('{.simName}_sim.rds')))

#figure out expected overlap. grab a set of niches and find the parameters
mu <- stdat %>% slice(1) %>% select(data) %>% unnest(cols=data) %>% pluck('mu')
expect <- ellipse_set(mu1=mu[[1]],mu2=mu[[2]])

.ex <- log(unique(stdat$n),base=2) #this is used for plotting

#----
#---- figure for set components
#----

#need to set up dataframe of expected values for components
v <- c(expect,expect[1]/expect[2])
names(v) <- c('Intersection','Union','Overlap')
expdat <- enframe(v,name='component') %>%
  mutate(component=factor(component,levels=c('Intersection','Union','Overlap')))

#Calculate confidence intervals using t distribution
gdat <- stdat %>%
  select(-data,-hv_intr,-hv_union,-df_intr,-df_union) %>%
  gather(type,value,-c(n,rep)) %>%
  group_by(n,type) %>% #summarize over repetitions
  summarize(mean=mean(value),min=min(value),max=max(value),
            sd=sd(value), num=n(), se=sd/sqrt(num), df=num-1, t=qt(0.975,df=df),
            cil=mean - (t * se),ciu=mean + (t * se)) %>%
  ungroup %>%
  mutate(
    component = factor(case_when(
      type %in% c('vol_intr_hv','vol_intr_poly') ~ 'Intersection',
      type %in% c('vol_union_hv','vol_union_poly') ~ 'Union',
      type %in% c('jac_hv','jac_poly') ~ 'Overlap'),
      levels=c('Intersection','Union','Overlap')),
    method = ifelse(str_detect(type,'hv'),'Hypervolume','95% Ellipse'))

gdat %>% 
  ggplot(aes(x=log2(n),y=mean,color=method,fill=method)) + 
  geom_ribbon(aes(ymin=cil,ymax=ciu),alpha=0.2,color=NA) +
  geom_line(size=1) +
  geom_hline(data=expdat,aes(yintercept=value),linetype='dashed') +
  scale_x_continuous(breaks=.ex,labels=2^.ex) +
  scale_color_manual(name=NULL,values=c('#0073C2FF','#FC4E07'),
                     aesthetics=c('color','fill')) + #
  facet_wrap(vars(component),scales='free_y') +
  theme(legend.position='bottom') +
  labs(x='n',y=NULL) -> p; print(p)#,color='Dimensions',caption=.cap #title=title,

#saveRDS(p,file.path(.figP,glue('{.simName}_gg.rds')))
ggsave(file.path(.figP,glue('{.simName}_comp_df.pdf')),
       plot=p,height=6,width=10,device=cairo_pdf)
