#Returns union of n hypervolumes given a list of hypervolumes
#TODO: also accept a HypervolumeList
unionHvs <- function(hvs,distFact) {
  hvs %>% reduce(function(hv1,hv2) {
    hvSet <- hypervolume_set(hv1, hv2, distance.factor=distFact,
                             check.memory=FALSE, verbose=FALSE)
    union <- hvSet@HVList$Union
    union@Name <- 'union'
    return(union)
  }) %>%
    return
}

#Take from here: https://stackoverflow.com/questions/33304889/filling-alphahull-with-ggplot2
fortify.ashape <- function(ashape_res) {
  xdf <- data.frame(ashape_res$edges)
  xdf <- do.call(
    rbind,
    lapply(1:nrow(xdf), function(i) {
      rbind(
        data.frame(x=xdf$x1[i], y=xdf$y1[i]),
        data.frame(x=xdf$x2[i], y=xdf$y2[i])
      )
    })
  )
  xdf <- xdf[order(-1 * atan2(
    xdf$y - mean(range(xdf$y)), 
    xdf$x - mean(range(xdf$x)))), c("x", "y")]
  xdf <- rbind.data.frame(xdf[nrow(xdf),], xdf[1:(nrow(xdf)-1),])
  xdf
}

toPoly <- function(dat) {
  require(sf)
  
  bind_rows(dat,dat[1,]) %>% #close polygon
    as.matrix %>%
    list %>%
    st_polygon %>%
    return
}

#Gets an sf representation of 95% ellipse given confidence level
#Only works for circles for now
# this also must assume a variance of 1
#mu is coordinates of the center of the distribution
# can be either a numeric vector of length two, or a list of numeric vectors
#TODO: could sfc of points
ellipse_sfc <- function(mu,level=0.95) {
  require(sf)
  
  dfn <- 2 #number of axes
  dfd <- 1e6 #set as number of data points. just set to really high value
  
  if(is.numeric(mu)) mu <- list(mu)
  
  radius <- sqrt(dfn * stats::qf(level, dfn, dfd)) #Based on code from ggplot2
  
  mu %>%
    map(st_point) %>%
    st_sfc %>%
    st_buffer(radius,nQuadSegs=100) %>%
    return
}

#Set operations on 95% ellipses (assuming circular shape)
#TODO: could update this to use ellipse_sf
ellipse_set <- function(mu1,mu2,level=0.95) {
  require(sf)
  
  dfn <- 2 #number of axes
  dfd <- 1e6 #set as number of data points. just set to really high value
  
  radius <- sqrt(dfn * stats::qf(level, dfn, dfd)) #Based on code from ggplot2
  
  pts <- st_sfc(st_point(x = mu1),st_point(x = mu2))
  cir <- st_buffer(pts,radius,nQuadSegs=100)
  
  intr <- st_area(st_intersection(cir[[1]],cir[[2]]))
  un <- st_area(st_union(cir[[1]],cir[[2]]))
  
  return(c(intersection=intr,union=un))
}

diffmin <- function(t,t2=Sys.time()) round(difftime(t2, t, unit = "min"),2)

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