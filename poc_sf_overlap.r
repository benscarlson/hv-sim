level <- 0.95
dfn <- 2 #number of axes
dfd <- 1e6 #set as number of data points. just set to really high value

radius <- sqrt(dfn * stats::qf(level, dfn, dfd))

pts <- st_sfc(st_point(x = c(-1, 0)),st_point(x = c(1, 0)))
cir <- st_buffer(pts,radius,nQuadSegs=100)

intr <- st_area(st_intersection(cir[[1]],cir[[2]]))
un <- st_area(st_union(cir[[1]],cir[[2]]))


#make sure 95% ellipse is created correctly!

level <- 0.95
dfn <- 2 #number of axes
dfd <- 1e6 #set as number of data points. just set to really high value

radius <- sqrt(dfn * stats::qf(level, dfn, dfd))

pt <- st_sfc(st_point(x = c(0, 0)))
cir <- st_buffer(pt,radius,nQuadSegs=100)

dat <- mvrnorm(10000,c(0, 0),diag(2)) %>% as.data.frame
poly <- ellipsedf(dat) %>% toPoly

el <- st_sf(name=c('95%','circle'),st_sfc(poly,cir[[1]]))

ggplot() +
  #geom_point(data=dat,aes(x=V1,y=V2)) + 
  geom_sf(data=el,aes(color=name),fill=NA)
