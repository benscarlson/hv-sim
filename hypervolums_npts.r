options(rgl.useNULL = TRUE)

library(hypervolume)
library(MASS)
library(tidyverse)

source('src/funs.r')
theme_set(themepdf)

#set.seed(1504)

.figP <- 'analysis'

sig <- matrix(c(1,0,0,1),ncol=2)
n <- 2000 #recommended obsevations should be > exp(number of axes). exp(2) = 7.39
mu <- c(0,0)

n1 <- mvrnorm(n,mu,sig) %>% as.data.frame %>% mutate(niche_name='n1')
n2 <- mvrnorm(n,mu,sig) %>% as.data.frame %>% mutate(niche_name='n2')

dat <- bind_rows(n1,n2)

hvdf <- dat %>%
  group_by(niche_name) %>%
  nest %>%
  mutate(
    hv=map(data,hypervolume_svm,verbose=FALSE)
  )

hvl <- hypervolume_join(hvdf$hv)
title <- glue('n={n}')

pdf(file.path(.figP,glue('hypervolumes_{n}pts.pdf')))
ggplot(dat,aes(x=V1,y=V2,color=niche_name)) +
  geom_point() +
  stat_ellipse(type='norm') +
  scale_color_manual(name=NULL,values=c('#0073C2FF','#FC4E07')) +
  ggtitle(title)

plot(hvl,show.density=FALSE,show.data=TRUE, show.centroid = FALSE,
     show.random=FALSE,show.contour=TRUE, show.legend=FALSE,colors=c('#0073C2FF','#FC4E07')) #data and contour
title(main=title)
dev.off()