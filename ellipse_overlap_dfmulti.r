#Plot panels were created using hv_ellipse_overlap.r

library(patchwork)

.figP <- 'analysis'

p1 <- readRDS(file.path(.figP,glue('hvsvm_ellipse_overlap_df1_gg.rds')))
p2 <- readRDS(file.path(.figP,glue('hvsvm_ellipse_overlap_df2_gg.rds')))
p3 <- readRDS(file.path(.figP,glue('hvsvm_ellipse_overlap_df3_gg.rds')))

p <- p1 / p2 / p3

ggsave(file.path(.figP,'ellipse_overlap_dfmulti.pdf'),plot=p,height=12,width=6,device=cairo_pdf) #6,
