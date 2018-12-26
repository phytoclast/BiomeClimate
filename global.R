library(shiny)
Biomeclimate <- readRDS(file='data/Biomeclimate.RDS')
Biomeclimate$ECO_NAME<- as.character(Biomeclimate$ECO_NAME) #needed to be able to control encoding
Encoding(Biomeclimate$ECO_NAME) <- 'latin1' #needed to display correctly on server
b1990 <- Biomeclimate[Biomeclimate$Norm == 1990,]
b1990$PP <- apply(b1990[,c('p01','p02','p03','p04','p05','p06','p07','p08','p09','p10','p11','p12')], MARGIN = 1, FUN = 'sum')
b1990$TT <- apply(b1990[,c('t01','t02','t03','t04','t05','t06','t07','t08','t09','t10','t11','t12')], MARGIN = 1, FUN = 'mean')
b1990 <- b1990[,c('Latitude', 'Longitude','Elevation', 'PP', 'TT')]
colnamesb1990 <- colnames(b1990)
b1990 <- aggregate(b1990[,c('PP', 'TT')], by=list(b1990$Latitude, b1990$Longitude, b1990$Elevation), FUN='mean')
colnames(b1990) <- colnamesb1990
Biomeclimate <- merge(Biomeclimate, b1990, by=c('Latitude', 'Longitude','Elevation'))
Biomeclimate$P <- apply(Biomeclimate[,c('p01', 'p02', 'p03', 'p04', 'p05', 'p06', 'p07', 'p08', 'p09', 'p10', 'p11', 'p12')], MARGIN=c(1), na.rm=TRUE, FUN='sum')
rm(b1990, colnamesb1990)