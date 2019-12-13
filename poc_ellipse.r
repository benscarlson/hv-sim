library(sf)

set.seed(1504)

sig <- matrix(c(1,0,0,1),ncol=2)
n <- 5000
mu <- c(0,0)

dat <- mvrnorm(n,mu,sig) %>% as.data.frame

#this code is taken from ggplot: https://github.com/tidyverse/ggplot2/blob/master/R/stat-ellipse.R
#this is how ggplot calculates a 95% ellipse
#result is a dataframe of x y coordinates that can be plotted using geom_path
ellipsedf <- function(dat) {
  level <- 0.95
  segments <- 51
  dfn <- 2
  dfd <- nrow(dat) - 1
  
  v <- stats::cov.wt(dat)
  
  shape <- v$cov
  center <- v$center
  chol_decomp <- chol(shape)
  radius <- sqrt(dfn * stats::qf(level, dfn, dfd))
  
  angles <- (0:segments) * 2 * pi/segments
  unit.circle <- cbind(cos(angles), sin(angles))
  ellipse <- t(center + radius * t(unit.circle %*% chol_decomp))
  
  return(as.data.frame(ellipse))
}

gdat <- ellipsedf(dat)

ggplot(gdat,aes(x=V1,y=V2)) +
  geom_path() +
  geom_point(data=dat) +
  coord_fixed()

eldat <- ellipsedf(dat)
eldat <- bind_rows(eldat,eldat[1,]) #close polygon
p1 <- eldat %>%
  as.matrix %>%
  list %>%
  st_polygon 

dat2 <- mvrnorm(n,mu,sig) %>% as.data.frame
eldat2 <- ellipsedf(dat2)
eldat2 <- bind_rows(eldat2,eldat2[1,]) #close polygon
p2 <- eldat2 %>%
  as.matrix %>%
  list %>%
  st_polygon 

p2 %>% st_area

st_area(st_intersection(p1,p2)) / st_area(st_union(p1,p2))

