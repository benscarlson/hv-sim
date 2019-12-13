theme_pdf <- theme_classic(base_family = 'Helvetica') +
  theme(
    axis.text=element_text(size=12),
    axis.title=element_text(size=16),
    legend.text = element_text(size=14),
    legend.title = element_text(size=16)
  )

toPoly <- function(dat) {
  bind_rows(dat,dat[1,]) %>% #close polygon
    as.matrix %>%
    list %>%
    st_polygon %>%
    return
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