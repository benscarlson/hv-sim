level <- 0.95
segments <- 51
dfn <- 2
dfd <- nrow(dat) - 1

#shape and center are estimated from the data. instead, put in values
# v <- stats::cov.wt(dat)
# shape <- v$cov
# center <- v$center

shape <- diag(c(1,1))
center <- c(0,0)

chol_decomp <- chol(shape)
level <- 0.95
dfn <- 2
dfd <- 1e6

radius <- sqrt(dfn * stats::qf(level, dfn, dfd))

angles <- (0:segments) * 2 * pi/segments
unit.circle <- cbind(cos(angles), sin(angles))
ellipse <- t(center + radius * t(unit.circle %*% chol_decomp))

return(as.data.frame(ellipse))