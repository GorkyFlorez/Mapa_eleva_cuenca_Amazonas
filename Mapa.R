##############################################################
#                 Mapa 57: Mapa de elevacion Amazona         #
#                     Año: 2023                              #
#                       Gorky Florez Castillo                #
#                  Parte 1: Datos raster                     #
##############################################################

#Librerias----------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(raster)
library(ggspatial)
library(cptcity)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(grid)
library(RStoolbox)


SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  , crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Intercuenca_Alto_Marañón_I = st_read("SHP/Intercuenca_Alto_Marañón_I.shp")  %>% st_as_sf()
Intercuenca_Alto_Marañón<- st_transform(Intercuenca_Alto_Marañón_I , crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Cuenca_Utcubamba = st_read("SHP/Cuenca_Utcubamba.shp")  %>% st_as_sf()
Cuenca_Utcubamb <- st_transform(Cuenca_Utcubamba , crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Cuenca_Utcubamb_xy <- cbind(Cuenca_Utcubamb, st_coordinates(st_centroid(Cuenca_Utcubamb$geometry)))
Intercuenca_Alto_Marañón_xy <- cbind(Intercuenca_Alto_Marañón, st_coordinates(st_centroid(Intercuenca_Alto_Marañón$geometry)))

Rio          = st_read("SHP/RIOS_AMAZONAS_geogpsperu_SuyoPomalia_931381206.shp")  %>% st_as_sf()
Rio_Amazonas <- st_transform(Rio , crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Rio_Utcubamb        = st_intersection(Rio_Amazonas, Cuenca_Utcubamb)
Rio_Marañón        = st_intersection(Rio_Amazonas, Intercuenca_Alto_Marañón)


Per          <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Peru          <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
Amaz          <- subset(Peru , NAME_1  == "Amazonas")


library(elevatr)
library(ggnewscale)
elev = get_elev_raster(Cuenca_Utcubamb, z=11)
Poligo_alt    <- crop(elev, Cuenca_Utcubamb)                           #
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Cuenca_Utcubamb)


slopee    = terrain(Poligo_alt  , opt = "slope")
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

colores<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")
cortes <- c(200, 500,1000,2000,3000,4000,5000, 6500)


Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")

summary(Geo_data_frame$alt)

elev1 = get_elev_raster(Intercuenca_Alto_Marañón, z=11)

Poligo_alt1    <- crop(elev1, Intercuenca_Alto_Marañón)                           #
Poligo_alt1   <- Poligo_alt1 <- mask(Poligo_alt1, Intercuenca_Alto_Marañón)


slopee1    = terrain(Poligo_alt1  , opt = "slope")
aspecte1    = terrain(Poligo_alt1, opt = "aspect")
hille1     = hillShade(slopee1, aspecte1, angle = 40, direction = 270)

hill.p1        <-  rasterToPoints(hille1)
hill.pa_1      <-  data.frame(hill.p1)

colores<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")
cortes <- c(200, 500,1000,2000,3000,4000,5000, 6500)


Geo_data1       <-  rasterToPoints(Poligo_alt1)
Geo_data_frame1 <-  data.frame(Geo_data1)
colnames(Geo_data_frame1) <- c("x","y", "alt")

summary(Geo_data_frame1$alt)


Map_Peru= ggplot()+
  geom_sf(data = Per, fill="white", color="black", size=0.8)+
  geom_sf(data = Amaz, fill="black", color="black", size=0.8)+
  theme_void()+
  annotate(geom = "text", x = -80 ,y = 0, hjust = 0, vjust = 1, 
           label = "Republic \nof Peru",size = 3, family="serif", color = 
             "black",  fontface="italic")


library(ggrepel)
AMAZONAS_MAPA= ggplot()+
  geom_sf(data = Amaz, fill=NA, color="black", size=1)+
  theme_void()+
  geom_sf_text(data =Amaz, aes(label= NAME_2), size=2 ,  fontface="italic", family="serif")+
  geom_sf(data = Intercuenca_Alto_Marañón, fill="black", color="black", size=0.8, alpha=0.5)+
  geom_sf(data = Cuenca_Utcubamb , fill="#606c38", color="black", size=0.8,  alpha=0.5)+
  annotate(geom = "text", x = -78.0, y = -2.8, hjust = 0, vjust = 1, 
           label = "Amazonas \nRegion",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  geom_label_repel(data = Cuenca_Utcubamb_xy, aes(x = X, y = Y, label = NOMBRE), 
                              family="serif", box.padding = unit(6, "lines"), size =2, face = "bold",color = 'black',
                              point.padding = unit(0.5, "lines"))+

  geom_label_repel(data = Intercuenca_Alto_Marañón_xy, aes(x = X, y = Y, label = NOMBRE), 
                   family="serif", box.padding = unit(6, "lines"), size =2, face = "bold",color = 'black',
                   point.padding = unit(0.5, "lines"))




Map1= ggplot()+
  geom_raster(data = hill.pa_1, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame1  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = colores ,
                       breaks = cortes ,
                       labels = c("[menor a - 270] ","[270 - 499]", "[500 - 999]", "[1000 - 1999]", "[2000 - 2999]",
                                  "[3000 - 3999]", "[4000 - 4999]", "[5000 -6500]"),
                       na.value = 'white',
                       name='Elevacion \n(msnm)')+
  geom_sf(data = Intercuenca_Alto_Marañón, fill=NA, color="black", size=1)+
  
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(tag = "a)", x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag.position = "top",
        plot.tag = element_text(size=14, face='bold'),
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        )+
  geom_sf(data = Rio_Marañón ,  color="#1d3557", size=0.01, alpha=0.2)+
  scale_x_continuous(breaks = c(-78.4, -78, -77.6))+
  scale_y_continuous(breaks = c(-6, -5.6, -5, -4.6))+
  
  guides(fill = guide_legend(
    title = "Elevacion \nmsnm",
    
    nrow = 9,
    keywidth = 0.5,
    keyheight = 0.7,
    
    title.position = "top",
    override.aes = list(alpha = 1)
  ))+
  guides(fill = guide_legend(nrow = 5, ncol=1))+
  annotate(geom = "text", x = -77.8, y = -5.7, hjust = 0, vjust = 1, 
           label = "Intercuenca Alto Marañón I.",size = 4, family="serif", color = 
             "black",  fontface="italic", angle=90)



Map2=ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = colores ,
                       breaks = cortes ,
                       labels = c("[menor a - 270] ","[270 - 499]", "[500 - 999]", "[1000 - 1999]", "[2000 - 2999]",
                                  "[3000 - 3999]", "[4000 - 4999]", "[5000 -6500]"),
                       na.value = 'white',
                       name='Elevacion \n(msnm)')+
  geom_sf(data = Cuenca_Utcubamb_xy, fill=NA, color="black", size=1)+

  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(tag = "b)", x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag.position = "top",
        plot.tag = element_text(size=14, face='bold'),
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
        )+

  geom_sf(data = Rio_Utcubamb ,   color="#1d3557", size=0.01, alpha=0.2)+
  scale_x_continuous(breaks = c(-78.4, -78, -77.6))+
  scale_y_continuous(breaks = c(-6.8, -6.2, -5.6))+
  guides(fill = guide_legend(
    title = "Elevacion \nmsnm",
    
    nrow = 9,
    keywidth = 0.5,
    keyheight = 0.7,
    
    title.position = "top",
    override.aes = list(alpha = 1)
  ))+
  guides(fill = guide_legend(nrow = 5, ncol=1))+
  annotate(geom = "text", x = -77.6, y = -6.8, hjust = 0, vjust = 1, 
           label = "Cuenca Utcubamba.",size = 4, family="serif", color = 
             "black",  fontface="italic", angle=90)

library(cowplot)
Expo = ggdraw() +
  coord_equal(xlim = c(0, 30), ylim = c(0, 21), expand = FALSE) +
  draw_plot(Map1 , width = 15, height = 15,x = 0, y = 0)+
  draw_plot(Map2 , width = 15, height = 15,x = 16, y = 0)+
  draw_plot(Map_Peru , width =6, height = 6,x = 7, y = 15)+
  draw_plot(AMAZONAS_MAPA , width =15, height = 15,x = 7, y = 6)+
  
  theme(panel.background = element_rect(fill = "white"))


ggsave(plot=Expo ,"Mapa de elevacion_amazonas.png",units = "cm",width = 29, #ancho
       height = 21, #alargo
       dpi=1200)













