library(sf)
library(rgdal)
library(rgeos)
library(spdep)
library(maptools)
setwd("F:/0010_Percolation/11_CLCA")

# get area and sort
pths <- readOGR("sh_Ex_Pro.shp")
areas <- gArea(pths, byid = T)
dd <- data.frame(rw = as.numeric(names(areas))+1, areas)

dd <- dd[order(dd$areas, decreasing = T), ]
dd$order<-1:nrow(dd)
dd$clu<--1
# buffer
# union
# dis
# give attri
CLU=1
DIS=10000
while(-1 %in% dd$clu){
  d<-dd[dd$clu==-1,]
  d$order<-1:nrow(d)
  rw <- d[d$order==1,1]
  wid_Buf <-  1.5*sqrt(d[d$order==1,2])
  show(paste("buffer width:",wid_Buf))
  buffer <- gBuffer(pths[rw,], width = wid_Buf, byid = TRUE)
  a<-gIntersection(buffer, pths,byid=T)
  rws <- as.numeric(do.call(rbind, strsplit(row.names(a), ' '))[,2])+1

  se_rws <-  intersect(rws,d$rw)
  for (k in se_rws){
    show(paste("ID",k))
    dis_SP <- gDistance(pths[rw,],pths[k,], byid = TRUE)
    dis_WG <- dis_SP+ log10(d[d$order==1,2])+log10(d[d$rw==k,2])   # prior knowledge
    show(paste("dis_WG",dis_WG,dis_SP,"d",d[d$order==1,2],d[d$rw==k,2]))
    if (dis_WG<DIS){
      dd[dd$rw==k,4]=CLU
      dd[dd$rw==rw,4]=CLU
    }else{
      dd[dd$rw==k,4]=-1
      dd[dd$rw==rw,4]=0
    }
  }
  CLU=CLU+1
}
df <-data.frame(dd)
write.csv(df,"./demo.csv")
