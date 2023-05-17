library(ggplot2)
library(akima)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(sf)
library(rgeos)
library(reshape2)
library(ggnewscale)
library(jsonlite)
library(rjson)
library(raster)
library(chron)
library(ggforce)
library(ggrepel)
library(rnaturalearthhires)
library(ggalt)
library(httr)
library(metR)

###################################################################################################################
##############	BASEMAP  /  EEZ SHP  /  MAP BOUNDARIES      #######################################################
###################################################################################################################

##### Set path to shapefiles folder

shape_path <- "C:/Users/gabriel.arce/Documents/R/shp/"

##### Read shapefiles and convert to SpatialPolygons

pac <- paste(shape_path, "pacific0360.shp", sep="")
pac_poly <- st_read(pac)

useez <- paste(shape_path, "usweezh0360.shp", sep="")
useez_poly <- st_read(useez)

caneez <- paste(shape_path, "canweezh0360.shp", sep="")
caneez_poly <- st_read(caneez)

iattc <- paste(shape_path, "iattc0360.shp", sep="")
iattc_poly <- st_read(iattc)

wcpfc <- paste(shape_path, "wcpfc0360.shp", sep="")
wcpfc_poly <- st_read(wcpfc)

isc <- paste(shape_path, "isc0360.shp", sep="")
isc_poly <- st_read(isc)

hawaii <- paste(shape_path, "hawaiih0360.shp", sep="")
hawaii_poly <- st_read(hawaii)

##### Get country and state names for map labels

world <- ne_countries(scale = "medium", returnclass = "sf")
countries <- world[-c(12,71,99,184,188,189),]
world_points <- cbind(countries, st_coordinates(st_centroid(countries$geometry)))
world_points$X <- ifelse(world_points$X < 0, world_points$X+360, world_points$X)

states <- ne_states( returnclass = "sf")
states <- states[-c(440,3814),]
state_points <- cbind(states, st_coordinates(st_centroid(states$geometry)))
state_points$X <- ifelse(state_points$X < 0, state_points$X+360, state_points$X)

usacan_points <- subset(state_points, admin == "United States of America" | admin == "Canada")

##### Get world basemap as SpatialPolygon

worldmap = maps::map(wrap=c(0,360), plot=FALSE, fill=TRUE)

state_prov <- rnaturalearth::ne_states(c("united states of america", "canada"))

##### Create labels for plot scale

labelx <- c("10\u00B0E","20\u00B0E","30\u00B0E","40\u00B0E","50\u00B0E","60\u00B0E","70\u00B0E","80\u00B0E","90\u00B0E","100\u00B0E","110\u00B0E","120\u00B0E",
"130\u00B0E","140\u00B0E","150\u00B0E","160\u00B0E","170\u00B0E","180\u00B0","170\u00B0W","160\u00B0W","150\u00B0W","140\u00B0W","130\u00B0W","120\u00B0W","110\u00B0W",
"100\u00B0W","90\u00B0W","80\u00B0W","70\u00B0W","60\u00B0W","50\u00B0W","40\u00B0W","30\u00B0W","20\u00B0W","10\u00B0W","0")

labely <- c("90\u00B0S","80\u00B0S","70\u00B0S","60\u00B0S","50\u00B0S","40\u00B0S","30\u00B0S","20\u00B0S","10\u00B0S","0",
"10\u00B0N","20\u00B0N","30\u00B0N","40\u00B0N","50\u00B0N","60\u00B0N","70\u00B0N","80\u00B0N","90\u00B0N")

labelx5 <- c("5\u00B0E","10\u00B0E","15\u00B0E","20\u00B0E","25\u00B0E","30\u00B0E","35\u00B0E","40\u00B0E","45\u00B0E","50\u00B0E","55\u00B0E","60\u00B0E",
"65\u00B0E","70\u00B0E","75\u00B0E","80\u00B0E","85\u00B0E","90\u00B0E","95\u00B0E","100\u00B0E","105\u00B0E","110\u00B0E","115\u00B0E","120\u00B0E",
"125\u00B0E","130\u00B0E","135\u00B0E","140\u00B0E","145\u00B0E","150\u00B0E","155\u00B0E","160\u00B0E","165\u00B0E","170\u00B0E","175\u00B0E","180\u00B0",
"175\u00B0W","170\u00B0W","165\u00B0W","160\u00B0W","155\u00B0W","150\u00B0W","145\u00B0W","140\u00B0W","135\u00B0W","130\u00B0W","125\u00B0W","120\u00B0W",
"115\u00B0W","110\u00B0W","105\u00B0W","100\u00B0W","95\u00B0W","90\u00B0W","85\u00B0W","80\u00B0W","75\u00B0W","70\u00B0W","65\u00B0W","60\u00B0W",
"55\u00B0W","50\u00B0W","45\u00B0W","40\u00B0W","35\u00B0W","30\u00B0W","25\u00B0W","20\u00B0W","15\u00B0W","10\u00B0W","5\u00B0W","0\u00B0")

labely5 <- c("90\u00B0S","85\u00B0S","80\u00B0S","75\u00B0S","70\u00B0S","65\u00B0S","60\u00B0S","55\u00B0S","50\u00B0S","45\u00B0S","40\u00B0S",
"35\u00B0S","30\u00B0S","25\u00B0S","20\u00B0S","15\u00B0S","10\u00B0S","5\u00B0S","0\u00B0","5\u00B0N","10\u00B0N","15\u00B0N","20\u00B0N","25\u00B0N",
"30\u00B0N","35\u00B0N","40\u00B0N","45\u00B0N","50\u00B0N","55\u00B0N","60\u00B0N","65\u00B0N","70\u00B0N","75\u00B0N","80\u00B0N","85\u00B0N","90\u00B0N")

###################################################################################################################
#####  =>  Define map boundaries and create bounding box; limits defined as(Xmin, Xmax, Ymin, Ymax)
###################################################################################################################

limits <- c(170,240,-10,60)

bbox <- as(extent(limits), 'SpatialPolygons')
bboxcp <- st_bbox(c(xmin = bbox@bbox[1], xmax = bbox@bbox[3], ymax = bbox@bbox[2], ymin = bbox@bbox[4]))

###################################################################################################################
##############			BASEMAP PLOT 	          #########################################################
###################################################################################################################

##### Crop shapefiles to bounding box to avoid polygon clipping

useezmap <- as_Spatial(useez_poly) %>%  {if(nrow(st_crop(useez_poly, bboxcp)) == 0) fortify(as_Spatial(useez_poly)) else fortify(crop(as_Spatial(useez_poly), bbox))}
caneezmap <- as_Spatial(caneez_poly) %>%  {if(nrow(st_crop(caneez_poly, bboxcp)) == 0) fortify(as_Spatial(caneez_poly)) else fortify(crop(as_Spatial(caneez_poly), bbox))}
iscmap <- as_Spatial(isc_poly) %>%  {if(nrow(st_crop(isc_poly, bboxcp)) == 0) fortify(as_Spatial(isc_poly)) else fortify(crop(as_Spatial(isc_poly), bbox))}
iattcmap <- as_Spatial(iattc_poly) %>%  {if(nrow(st_crop(iattc_poly, bboxcp)) == 0) fortify(as_Spatial(iattc_poly)) else fortify(crop(as_Spatial(iattc_poly), bbox))}
wcpfcmap <- as_Spatial(wcpfc_poly) %>%  {if(nrow(st_crop(wcpfc_poly, bboxcp)) == 0) fortify(as_Spatial(wcpfc_poly)) else fortify(crop(as_Spatial(wcpfc_poly), bbox))}
hawaiimap <- as_Spatial(hawaii_poly) %>%  {if(nrow(st_crop(hawaii_poly, bboxcp)) == 0) fortify(as_Spatial(hawaii_poly)) else fortify(crop(as_Spatial(hawaii_poly), bbox))}

###################################################################################################################
#####  =>  Plot section
###################################################################################################################

ggplot() + 

##### Define the coordinate system for the map plot

coord_cartesian(clip="on",xlim = c(bbox@bbox[1], bbox@bbox[3]), ylim = c(bbox@bbox[2], bbox@bbox[4]))+

##### Shapefile section, features can be enabled/disabled by commenting indivudual lines
##### The "fill" parameter controls the fill color for each polygon, and "alpha" modifies polygon transparency
##### The "color" parameter controls the polygon outline color, and "size" modifies the line width

geom_polygon(data = useezmap, aes(x = long, y = lat), fill="steelblue4",  color="steelblue4", alpha=0.1, size=0.7) +
geom_polygon(data = caneezmap, aes(x = long, y = lat), fill="darkolivegreen", color="darkolivegreen", alpha=0.1, size=0.7)  +
geom_polygon(data = iattcmap, aes(x = long, y = lat), fill="steelblue4",  color="steelblue4", alpha=0.1, size=0.7) +
geom_polygon(data = wcpfcmap, aes(x = long, y = lat),  fill="darkolivegreen",  color="darkolivegreen", alpha=0.1, size=0.7)  +
geom_polygon(data = iscmap, aes(x = long, y = lat), fill="yellow",  color="yellow", alpha=0.1, size=0.7)  +
geom_polygon(data = hawaiimap, aes(x = long, y = lat), fill="steelblue4",  color="steelblue4", alpha=0.1, size=0.7)  +

##### Map gridlines, set for 5degree squares.

geom_hline(yintercept=seq(-90,90, by=5), color = "grey80", size=0.3, linetype = "dashed", alpha=0.6) + 
geom_vline(xintercept=seq(0,360, by= 5), color = "grey80", size=0.3,linetype = "dashed", alpha=0.6) +

##### World basemap polygon feature, can be modified using the same procedures for the shapefiles above

geom_polygon(data = worldmap, aes(x=long, y = lat, group=group), fill="antiquewhite1",color="grey70",size=0.5) + 
geom_polygon(data = state_prov, aes(x=long+360, y = lat, group=group), fill="antiquewhite1",color="grey70",size=0.5) + 
geom_text(data= world_points, aes(x=X, y=Y, label=name), color = "darkblue", fontface = "italic", check_overlap = TRUE, size=3) +
geom_text(data= usacan_points, aes(x=X, y=Y, label=name), color = "grey30", fontface = "bold.italic", check_overlap = TRUE, size=3) +

##### Plot parameters including labels and map title.

xlab("Longitude") + ylab("Latitude") +
scale_y_continuous(limits = c(-90,90), breaks=seq(-90, 90,10), labels=labely,expand=c(0,0)) + 
scale_x_continuous(limits = c(10,360), breaks=seq(10,360,10), labels=labelx,expand=c(0,0)) +
theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_rect(fill = "aliceblue"),panel.grid=element_blank(),
panel.border = element_rect(colour = "black", fill=NA,size=2), plot.background = element_rect(fill = "white"), plot.margin = margin(2, 16, 8, 2))+
ggtitle("Map Title", subtitle="Map Subtitle")

