
options(rgl.useNULL = TRUE)

library(hypervolume)
library(stringr)

source('src/funs/funs.r')
source('src/funs/themes.r')
theme_set(theme_eda)

.simName <- 'volume1'
.resP <- 'analysis/sim'
.figP <- 'analysis/fig'

stdat <- readRDS(file.path(.resP,glue('{.simName}_sim.rds')))

parPF <- file.path(.resP,'hv_volume_sim_params.csv')

#TODO: pull these values from the params.csv file
pardf <- read_csv(parPF) %>% filter(sim_name==.simName)

#TODO: need to convert string represetnation of vector to vector for this to work
#pardf %>% filter(var=='.mu') %>% pluck('value')
expect <- c(0, 0) %>% ellipse_sfc %>% st_area
#pardf %>% filter(var=='.ex') %>% pluck('value')
.ex <- 4:11

#----
#---- figure for set components
#----

#TODO: include pvol at some point? Should be somehow gathered into method column
#Calculate confidence intervals using t distribution
gdat <- stdat %>%
  select(-c(name,set_name,mu,ndat,poly,eldat,pvol,hv),value=vol) %>%
  #gather(type,value,-c(method,n,rep)) %>%
  group_by(n,method) %>% #summarize over repetitions
  summarize(mean=mean(value),min=min(value),max=max(value),
            sd=sd(value), num=n(), se=sd/sqrt(num), df=num-1, t=qt(0.975,df=df),
            cil=mean - (t * se),ciu=mean + (t * se)) %>%
  ungroup

gdat %>% 
  ggplot(aes(x=log2(n),y=mean,color=method,fill=method)) + 
  geom_ribbon(aes(ymin=cil,ymax=ciu),alpha=0.2,color=NA) +
  geom_line(size=1) +
  geom_hline(yintercept=expect,linetype='dashed') +
  scale_x_continuous(breaks=.ex,labels=2^.ex) +
  scale_color_manual(name=NULL,values=c('#0073C2FF','#FC4E07'),
                     aesthetics=c('color','fill')) + #
  labs(x='n',y='Volume') -> p; print(p)#,color='Dimensions',caption=.cap #title=title,

ggsave(file.path(.figP,glue('{.simName}.pdf')),
       plot=p,height=5,width=7,device=cairo_pdf)

message('Script complete')