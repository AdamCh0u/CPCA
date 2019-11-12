library(rgdal)
library(rgeos)
setwd("F:/0010_Percolation/11_CLCA")

# get area and sort
pths <- readOGR("sh_Ex_Pro.shp")
#plot(pths,col = "red")
areas <- gArea(pths, byid = T)
edge <- gBoundary(pths, byid = T)
perims <- gLength(edge, byid = T)

op<-data.frame(rw = as.numeric(names(perims))+1, perims)
dd <- data.frame(rw = as.numeric(names(areas))+1, areas)

dd <- dd[order(dd$areas, decreasing = T), ]
dd <- dd[dd$areas>3000000, ]
pths_se<-pths[dd$rw,]
#plot(pths,col = "green")

CLU=1
dd$order<-1:nrow(dd)
dd$clu<--1
dd[dd$order==1,]$clu=CLU
# buffer
# union
# dis
# give attri
i=2
while (-1 %in% dd$clu){
  rw      <- dd[dd$order==i,1]
  show(rw)
  bigger  <- dd[dd$order<=i,1]
  pth_Bigger <- pths[bigger,]
  wid_Buf <- 1.5*sqrt(dd[dd$order==i,2])## 缓冲区半径
  wid_Buf<-5000
  show(wid_Buf)
  buffer  <- gBuffer(pths[rw,], width = wid_Buf, byid = TRUE)
  #plot(buffer)
  #plot(pths[rw,],add=T)
  a       <- gIntersection(buffer, pth_Bigger,byid=T)
  
  rws     <- as.numeric(do.call(rbind, strsplit(row.names(a), ' '))[,2])+1

  se_rws  <- data.frame(rw=setdiff(rws,rw))

  show(nrow(se_rws))
  if (nrow(se_rws)==0){
    CLU=CLU+1
    dd[dd$rw==rw,4] <-CLU
    show(0)
  }else{
    se_rws$dis_WG  <-  -1
    se_rws$clu  <-  -1
    for (k in se_rws$rw){
      dis_SP <- gDistance(pths[rw,],pths[k,], byid = TRUE)
      dis_WG <- dis_SP+ log10(dd[dd$rw==rw,2])+log10(dd[dd$rw==k,2])   # prior knowledge
      se_rws[se_rws$rw==k,]$dis_WG <- dis_WG
      se_rws[se_rws$rw==k,]$clu    <- dd[dd$rw==k,]$clu
    }
    min_rw <- se_rws[se_rws$dis_WG==min(se_rws$dis_WG),]$rw
    dd[dd$rw==rw,4] <- dd[dd$rw==min_rw,4]
  }
  i = i+1
}
df <-data.frame(dd)
write.csv(df,"./demo.csv")



