library(rgdal)
library(ggplot2)
library(dplyr)
library(akima)
library(oce)
library(plyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(ggspatial)
library(zoo)
library(sf)
library(rgeos)
library(reshape2)
library(ggnewscale)
library(metR)
library(jsonlite)
library(rjson)
library(broom)
library(raster)


###################################################################################################################
##############	BASEMAP  /  EEZ SHP  /  MAP BOUNDARIES      #######################################################
###################################################################################################################

##### Set path to shapefiles folder

shape_path <- "C:/Users/gabriel.arce/Documents/R/shp/"

##### Read shapefiles and convert to SpatialPolygons

useez <- paste(shape_path, "usweez0360.shp", sep="")
useez_poly <- readOGR(useez, layer=ogrListLayers(useez))
useez_shp <- tidy(useez_poly)

caneez <- paste(shape_path, "canweez0360.shp", sep="")
caneez_poly <- readOGR(caneez, layer=ogrListLayers(caneez))
caneez_shp <- tidy(caneez_poly)

iattc <- paste(shape_path, "iattc0360.shp", sep="")
iattc_poly <- readOGR(iattc, layer=ogrListLayers(iattc))
iattc_shp <- tidy(iattc_poly)

wcpfc <- paste(shape_path, "wcpfc0360.shp", sep="")
wcpfc_poly <- readOGR(wcpfc, layer=ogrListLayers(wcpfc))
wcpfc_shp <- tidy(wcpfc_poly)

isc <- paste(shape_path, "isc0360.shp", sep="")
isc_poly <- readOGR(isc, layer=ogrListLayers(isc))
isc_shp <- tidy(isc_poly)

##### Get country names for map labels

world <- ne_countries(scale = "medium", returnclass = "sf")
countries <- world[-c(12,71,99,184,188,189),]
world_points <- cbind(countries, st_coordinates(st_centroid(countries$geometry)))
world_points$X <- ifelse(world_points$X < 0, world_points$X+360, world_points$X)

##### Get world basemap as SpatialPolygon

worldmap = maps::map(wrap=c(0,360), plot=FALSE, fill=TRUE)

##### Create labels for plot scale

labelx <- c("10°E","20°E","30°E","40°E","50°E","60°E","70°E","80°E","90°E","100°E","110°E","120°E",
"130°E","140°E","150°E","160°E","170°E","180°","170°W","160°W","150°W","140°W","130°W","120°W","110°W",
"100°W","90°W","80°W","70°W","60°W","50°W","40°W","30°W","20°W","10°W","0")

labely <- c("90°S","80°S","70°S","60°S","50°S","40°S","30°S","20°S","10°S","0",
"10°N","20°N","30°N","40°N","50°N","60°N","70°N","80°N","90°N")

vec <- setNames(rep("", 7), c("long", "lat", "order", "hole", "piece", "group", "id"))
bind_rows(vec)[0, ]

###################################################################################################################
#####  =>  Define map boundaries and create bounding box; limits defined as(Xmin, Xmax, Ymin, Ymax)
###################################################################################################################

limits <- c(210,300,0,70)

bbox <- as(extent(limits), 'SpatialPolygons')


###################################################################################################################
##############			BASEMAP PLOT 	          #########################################################
###################################################################################################################

##### Crop shapefiles to bounding box to avoid polygon clipping

useezmap <- tidy(crop(useez_poly, bbox)) %>%  {if(nrow(.) == 0) bind_rows(vec)[0, ] else tidy(crop(useez_poly, bbox))}
caneezmap <- tidy(crop(caneez_poly, bbox)) %>%  {if(nrow(.) == 0) bind_rows(vec)[0, ] else tidy(crop(caneez_poly, bbox))}
iscmap <- tidy(crop(isc_poly, bbox)) %>%  {if(nrow(.) == 0) bind_rows(vec)[0, ] else tidy(crop(isc_poly, bbox))}
iattcmap <- tidy(crop(iattc_poly, bbox)) %>%  {if(nrow(.) == 0) bind_rows(vec)[0, ] else tidy(crop(iattc_poly, bbox))}
wcpfcmap <- tidy(crop(wcpfc_poly, bbox)) %>%  {if(nrow(.) == 0) bind_rows(vec)[0, ] else tidy(crop(wcpfc_poly, bbox))}

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

##### Map gridlines, set for 5degree squares.

geom_hline(yintercept=seq(-90,90, by=5), color = "grey80", size=0.3, linetype = "dashed", alpha=0.6)+ 
geom_vline(xintercept=seq(0,360, by= 5), color = "grey80", size=0.3,linetype = "dashed", alpha=0.6)+

##### World basemap polygon feature, can be modified using the same procedures for the shapefiles above

geom_polygon(data = worldmap, aes(x=long, y = lat, group=group), fill="antiquewhite1",color="grey70",size=0.5) + 
geom_text(data= world_points, aes(x=X, y=Y, label=name), color = "darkblue", fontface = "italic", check_overlap = TRUE, size=3) +

##### Plot parameters including labels and map title.

xlab("Longitude") + ylab("Latitude") +
scale_y_continuous(limits = c(-90,90), breaks=seq(-90, 90,10), labels=labely,expand=c(0,0)) + 
scale_x_continuous(limits = c(10,360), breaks=seq(10,360,10), labels=labelx,expand=c(0,0)) +
theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_rect(fill = "aliceblue"),panel.grid=element_blank(),
panel.border = element_rect(colour = "black", fill=NA,size=2), plot.background = element_rect(fill = "white"), plot.margin = margin(2, 16, 8, 2))+
ggtitle("Map Title", subtitle="Map Subtitle")




###################################################################################################################
##############	READ ERDDAP SST JSON	    #####################################################################
###################################################################################################################

##### Get latitude and longitude parameters from bounding box

latbnd <- c(as.character(bbox@bbox[2]),as.character(bbox@bbox[4]))
lonbnd <- c(as.character(bbox@bbox[1]),as.character(bbox@bbox[3]))

##### Set time contraint, range and resolutions depends on the source data

timebnd <- c("2020-07-01", "2020-07-01")

##### Read .json data from ERDDAP

sstjsonstr <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/esrlIcoads2ge.json?
sst%5B(",timebnd[1],"):1:(",timebnd[2],")%5D%5B(",latbnd[2],"):1:(",latbnd[1],")%5D%5B(",lonbnd[1],"):1:(",lonbnd[2],")%5D")

sstjs <- fromJSON(paste(readLines(sstjsonstr), collapse=""))

sstjson <- sstjs$table$rows
sstna <- sapply(sstjson, function(x) ifelse(x == "NULL", NA, x))
sstls <- unlist(sstna)

sst <- data.frame(matrix(ncol = 4, nrow = (length(sstls)/4)))
colnames(sst) <- c("TIME", "LAT", "LON", "SST")

##### Organize variables in data frame

sst[,1] <- sstls[seq(1, length(sstls), 4)]
sst[,2] <- as.numeric(sstls[seq(1, length(sstls), 4)+1])
sst[,3] <- as.numeric(sstls[seq(1, length(sstls), 4)+2])
sst[,4] <- as.numeric(sstls[seq(1, length(sstls), 4)+3])


##### SST grid interpolation and raster function

sstgrid <- interp(sst[!is.na(sst$SST),]$LON, sst[!is.na(sst$SST),]$LAT,
sst[!is.na(sst$SST),]$SST, duplicate = "strip")
sstraster <- expand.grid(x=sstgrid$x, y=sstgrid$y)
sstraster$z <- as.vector(sstgrid$z)


###################################################################################################################
##############	BASEMAP W/ SST DATA	#########################################################################
###################################################################################################################

useezmap <- tidy(crop(useez_poly, bbox)) %>%  {if(nrow(.) == 0) bind_rows(vec)[0, ] else tidy(crop(useez_poly, bbox))}
caneezmap <- tidy(crop(caneez_poly, bbox)) %>%  {if(nrow(.) == 0) bind_rows(vec)[0, ] else tidy(crop(caneez_poly, bbox))}
iscmap <- tidy(crop(isc_poly, bbox)) %>%  {if(nrow(.) == 0) bind_rows(vec)[0, ] else tidy(crop(isc_poly, bbox))}
iattcmap <- tidy(crop(iattc_poly, bbox)) %>%  {if(nrow(.) == 0) bind_rows(vec)[0, ] else tidy(crop(iattc_poly, bbox))}
wcpfcmap <- tidy(crop(wcpfc_poly, bbox)) %>%  {if(nrow(.) == 0) bind_rows(vec)[0, ] else tidy(crop(wcpfc_poly, bbox))}

ggplot() + 
coord_cartesian(clip="on",xlim = c(bbox@bbox[1], bbox@bbox[3]), ylim = c(bbox@bbox[2], bbox@bbox[4]))+

##### Add SST raster feature layer

geom_raster(data=sstraster, aes(x=x,y=y,fill=z), interpolate = TRUE) +

geom_polygon(data = useezmap, aes(x = long, y = lat), fill="steelblue4",  color="steelblue4", alpha=0.1, size=0.7) +
geom_polygon(data = caneezmap, aes(x = long, y = lat), fill="darkolivegreen", color="darkolivegreen", alpha=0.1, size=0.7)  +
geom_polygon(data = iattcmap, aes(x = long, y = lat), fill="steelblue4",  color="steelblue4", alpha=0.1, size=0.7) +
geom_polygon(data = wcpfcmap, aes(x = long, y = lat),  fill="darkolivegreen",  color="darkolivegreen", alpha=0.1, size=0.7)  +
geom_polygon(data = iscmap, aes(x = long, y = lat), fill="yellow",  color="yellow", alpha=0.1, size=0.7)  +

geom_hline(yintercept=seq(-90,90, by=5), color = "grey80", size=0.3, linetype = "dashed", alpha=0.6)+ 
geom_vline(xintercept=seq(0,360, by= 5), color = "grey80", size=0.3,linetype = "dashed", alpha=0.6)+

geom_polygon(data = worldmap, aes(x=long, y = lat, group=group), fill="antiquewhite1",color="grey70",size=0.5) + 
geom_text(data= world_points, aes(x=X, y=Y, label=name), color = "darkblue", fontface = "italic", check_overlap = TRUE, size=3) +

xlab("Longitude") + ylab("Latitude") +
scale_y_continuous(limits = c(-90,90), breaks=seq(-90, 90,10), labels=labely,expand=c(0,0)) + 
scale_x_continuous(limits = c(10,360), breaks=seq(10,360,10), labels=labelx,expand=c(0,0)) +
theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_rect(fill = "aliceblue"),panel.grid=element_blank(),
panel.border = element_rect(colour = "black", fill=NA,size=2), plot.background = element_rect(fill = "white"), plot.margin = margin(2, 16, 8, 2))+
ggtitle("Map Title", subtitle="Map Subtitle") +

##### Add scale for SST raster

scale_fill_distiller(name="SST (C)", palette = "Spectral", limits=c(5,35))


