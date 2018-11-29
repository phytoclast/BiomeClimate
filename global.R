library(shiny)
Biomeclimate <- readRDS(file='data/Biomeclimate.RDS')
Biomeclimate$PP <- apply(Biomeclimate[,c('p01','p02','p03','p04','p05','p06','p07','p08','p09','p10','p11','p12')], MARGIN = 1, FUN = 'sum')
Biomeclimate$TT <- apply(Biomeclimate[,c('t01','t02','t03','t04','t05','t06','t07','t08','t09','t10','t11','t12')], MARGIN = 1, FUN = 'mean')