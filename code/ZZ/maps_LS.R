library(raster)
# library(rasterVis)
# library(ggspatial) # annotation_scale, annotation_north_arrow
library(ggplot2)
library(sf)
# library(Rcpp)

# Paths ########################################################################

# setwd("E:/")
# Path_Pesticide_H <- dir("pesticide data/GEOTIFF_cotton-H",full.names = TRUE)
# Path_Pesticide_L <- dir("pesticide data/GEOTIFF_cotton-L",full.names = TRUE)

setwd("E:/ZZ")
Path_Pesticide_H <- dir("E:/ZZ/transfer_1864011_files_cefa7de2/GEOTIFF_cotton-H", "APR_Cotton.*2015_H", full.names = TRUE)
Path_Pesticide_L <- dir("E:/ZZ/transfer_1864011_files_cefa7de2/GEOTIFF_cotton-L", "APR_Cotton.*2015_L", full.names = TRUE)
Path_fertilizer_P<-dir("E:/ZZ/Fertilizer_cotton/A",full.names = TRUE)
Path_Yield <- dir("E:/ZZ/Production/dataverse_files/GAEZ._2015_crop_yield",full.names = TRUE)
# National boundary data from http://data.europa.eu/89h/jrc-10112-10004
Path_Countries <-dir("E:/ZZ/Production/gaul0_asap","gaul0_asap.shp",full.names = TRUE)

# outputs
Path_Map1 <- "yield_2015.pdf"
Path_Map3 <- "fertilizer_P_2000.pdf"
Path_Map4 <- "pesticide_2015.pdf"
Path_Map_Multi <- "map.pdf"

# Plot function ################################################################

my_map <- function(df, layer) {
  ggplot() +
    geom_sf(data = countries, aes(fill = NULL)) +
    geom_raster(data = df, aes_string(x = "x", y = "y", fill = layer)) +
    # annotation_north_arrow(location = "tr",
    #                        style = north_arrow_nautical(
    #                          fill = c("grey40","white"),
    #                          line_col = "grey20"))+ 
    scale_fill_viridis_c(option = "plasma", na.value = "transparent") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.title = element_blank(),
          legend.position = 'bottom',
          legend.key.width = unit(3,'line'),
          axis.title = element_blank())
}

# cotto yield 2015 ############################################################

# read data
yield <- raster(Path_Yield[grep("Mean", Path_Yield)])
countries <- read_sf(Path_Countries)
# clip
e<-extent(-178.9167,179.9167,-56.0425,84.4084)
yield<-crop(yield,e)
yield_df <- as.data.frame(as(yield, "Raster"), xy = TRUE)


# plot
p1 <- my_map(yield_df, "GAEZAct2015_Yield_Cotton_Mean")+
  labs(title=expression(bold(Cotton*" "*yield*" "*"("*10^3*kg*" "*ha^-1*")",sep="")))

# save to .pdf
ggsave(Path_Map1, p1, width = 180, height = 110, units = 'mm')

# fertilizer P 2000 ############################################################

# read data
P <- raster(Path_fertilizer_P[1])
# clip
e<-extent(-178.9167,179.9167,-56.0425,84.4084)
P<-crop(P,e)
P_df <- as.data.frame(as(P, "Raster"), xy = TRUE)
head(P_df)

# plot
p3 <- my_map(P_df, "cotton_PhosphorusApplication_Rate")+
  labs(title=expression(bold(Cotton*" "*P[2]*""*O[5]*" "*application*" "*rate*" ("*kg*" "*ha^-1*")",sep="")))

# save to .pdf
ggsave(Path_Map2, p3, width = 180, height = 110, units = 'mm')


# Pesticide APR 2015 (mean) ####################################################

# read data
pesticide_H <- stack(Path_Pesticide_H)
pesticide_L <- stack(Path_Pesticide_L)

# check negative values
# temp <- getValues(pesticide_H)
# apply(temp, 2, min, na.rm = TRUE) # -2 = ocean?
# apply(temp, 2, function(x) min(x[x != -2], na.rm = TRUE))
# apply(x, 2, function(x) min(x[!x %in% c(-2, -1.5, -1)], na.rm = TRUE))
pesticide_H[pesticide_H < 0] <- NA
pesticide_L[pesticide_L < 0] <- NA

# aggregate 
pesticide_H <- sum(pesticide_H, na.rm = TRUE)
pesticide_L <- sum(pesticide_L, na.rm = TRUE)
pesticide <- mean(stack(pesticide_H, pesticide_L))
pesticide[pesticide == 0] <- NA

pesticide_df <- as.data.frame(as(pesticide, "Raster"), xy = TRUE)

# plot
p4 <- my_map(pesticide_df, "layer")+
  labs(title=expression(bold(Cotton*" "*pesticide*" "*application*" "*rate*" ("*kg*" "*ha^-1*")",sep="",size=1)))

# save to .pdf
ggsave(Path_Map4, p4, width = 180, height = 110, units = 'mm')

# join maps ####################################################################

p_multi <- ggpubr::ggarrange(p1, p1, p3, p4, ncol = 2, nrow = 2)
ggsave(Path_Map_Multi, width = 180, height = 180, units = 'mm')