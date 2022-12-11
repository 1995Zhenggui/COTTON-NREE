library(raster)
library(rasterVis)
library(ggspatial)
library(ggplot2)
library(sf)
library(Rcpp)
######### cotton yield 2015
#Set import path
Raster_Path <- dir("E:/GAEZ._2015_crop_yield",full.names = TRUE)
#read data, use GAEZAct2015_Yield_Cotton_Mean.tif
cotton <- raster(Raster_Path[2])
#National boundary data from http://data.europa.eu/89h/jrc-10112-10004
boder <- read_sf("E:/gaul0_asap/gaul0_asap.shp")
#Coordinate system conversion
boder_wgs <- st_transform(boder,4326)
#data format conversion
df<- as.data.frame(as(cotton,"Raster"),xy=T)
#set color
colors <-c("#33FF00","#CCFF00","#FFCC00","#FF6600","#FF0200")
#plot
ggplot()+
  geom_sf(data=boder_wgs,aes(fill = NULL))+
  geom_tile(data=df,aes(x=x,y=y,fill = GAEZAct2015_Yield_Cotton_Mean))+
  annotation_scale(location = "bl") +
  annotation_north_arrow(location="tr",
                         style =north_arrow_nautical(
                           fill =c("grey40","white"),
                           line_col ="grey20"))+ 
  scale_fill_gradientn(colours=colors,na.value="transparent")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank())
#save to .pdf
ggsave("E:/yield_M_2015.pdf",width = 10,height = 6)

########Pesticide APR 2015-mean
#Set import path
Raster_Path <- dir("E:/pesticide data/GEOTIFF_cotton-H",full.names = TRUE)
Raster_Path <- dir("E:/pesticide data/GEOTIFF_cotton-L",full.names = TRUE)
#read multiple tiff
pesticide_all_H <- stack(Raster_Path[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)])
pesticide_all_L <- stack(Raster_Path[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)])
#sum 
pesticide_H_sum <- sum(pesticide_all_H,na.rm=TRUE)
pesticide_L_sum <- sum(pesticide_all_L,na.rm=TRUE)
#mean
pesticide_M_sum <- (pesticide_H_sum+pesticide_L_sum)/2
#National boundary data from http://data.europa.eu/89h/jrc-10112-10004
boder <- read_sf("E:/gaul0_asap/gaul0_asap.shp")
#Coordinate system conversion
boder_wgs <- st_transform(boder,4326)
#data formala conversion
df<- as.data.frame(as(pesticide_M_sum,"Raster"),xy=T)
# while data<0, data=NA
df$layer[which(df$layer < 0)] <- NA
#color
colors <-c("#33FF00","#CCFF00","#FFCC00","#FF6600","#FF0200")
ggplot()+
  geom_sf(data=boder_wgs,aes(fill = NULL))+
  geom_tile(data=df,aes(x=x,y=y,fill = layer))+
  annotation_scale(location = "bl") +
  annotation_north_arrow(location="tr",
                         style =north_arrow_nautical(
                           fill =c("grey40","white"),
                           line_col ="grey20"))+ 
  scale_fill_gradientn(colours=colors,na.value="transparent")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank())
#save to .pdf
ggsave("E:/pesticide/pesticide_M_2015.pdf",width = 10,height = 6)