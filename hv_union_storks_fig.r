
options(rgl.useNULL = TRUE)

library(hypervolume)
library(stringr)

source('src/funs/funs.r')
source('src/funs/themes.r')

theme_set(theme_eda)

.simName <- 'union_df_2015'
.resP <- 'analysis/sim'
.figP <- 'analysis/fig'

stdat <- readRDS(file.path(.resP,glue('{.simName}_sim.rds')))

stdat %>% 
  ggplot(aes(x=df_union,y=vol_union_hv,color=niche_set)) + 
  geom_line(size=1) +
  scale_color_brewer(palette='Dark2') +
  scale_x_continuous(breaks=1:6) +
  labs(x='distance.factor',y='Union volume',color=NULL) -> p; print(p)

ggsave(file.path(.figP,glue('{.simName}.pdf')),
       plot=p,height=4,width=6,device=cairo_pdf)

message('Script complete')