setwd("/Users/Bai/Desktop/Species_Distribution_Modeling_Tutorial")
#### Install and library packages ####
list_of_packages <- c("maptools", "rgdal", "mgcv", "classInt","RColorBrewer","maps", "mapdata",  "mgcv", "raster", "SDMTools", "dplyr", "akima", "viridis", "sp", "rgeos")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(maptools)
library(rgdal)
library(classInt) 
library(RColorBrewer)
library(maps)
library(mapdata)
library(mgcv)
library(raster)
library(SDMTools)
library(dplyr)
library(akima)
library(viridis)
library(sp)
library(rgeos)
#### Load bottom trawl survey data and map shapefile#### 
load("./data/one_stage_trawl_survey.RData")
head(lobster_catch_data)
summary(lobster_catch_data)


statistical_areas <- readShapePoly("./data/gis/Statistical_Areas_2010.shp", proj4string=CRS("+proj=longlat +datum=WGS84"))
sa511_513 <- statistical_areas[which(statistical_areas@data$Id %in% c(511:513)),]

#### Plot distribution of raw data ####
lobster_catch_data <- lobster_catch_data[-which(is.na(lobster_catch_data[,c("end_depth", "Water_Temp_DegC", "Salinity_psu", "end_lat", "end_lon", "Year", "Month")])),]

map_data <- lobster_catch_data
map_data$log_data <- ifelse(map_data$juv_num==0, NA, log(map_data$juv_num))

var="log_data"
plotvar <- map_data[,var]
nclr=6
plotclr <- rev(brewer.pal(nclr,"RdBu"))
class <- classIntervals(plotvar, nclr, style="equal")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]

jpeg(filename = paste("./plot/juv_catch_map.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
layout(matrix(c(1,1,1,3,2,2,2,3), 2, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month<7&is.na(map_data$log_data))], map_data$end_lat[which(map_data$Month<7&is.na(map_data$log_data))], pch=4, cex=0.3, col="gray")
points(map_data$end_lon[which(map_data$Month<7)], map_data$end_lat[which(map_data$Month<7)], pch=16, col=colcode[which(map_data$Month<7)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Spring", bty="n")

plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month>8&is.na(map_data$log_data))], map_data$end_lat[which(map_data$Month>8&is.na(map_data$log_data))], pch=4, cex=0.3, col="gray")
points(map_data$end_lon[which(map_data$Month>8)], map_data$end_lat[which(map_data$Month>8)], pch=16, col=colcode[which(map_data$Month>8)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Fall", bty="n")

par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table")), "No lobster"), fill=c(attr(colcode, "palette"), "gray"), cex=0.9, bty="n", title="Log(#)/Tow")
dev.off()

#### Temperature map ####
var="Water_Temp_DegC"
plotvar <- map_data[,var]
nclr=6
plotclr <- rev(brewer.pal(nclr,"RdBu"))
class <- classIntervals(plotvar, nclr, style="equal")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]

jpeg(filename = paste("./plot/temperature_map.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
layout(matrix(c(1,1,1,3,2,2,2,3), 2, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month<7)], map_data$end_lat[which(map_data$Month<7)], pch=16, col=colcode[which(map_data$Month<7)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Spring", bty="n")

plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month>8)], map_data$end_lat[which(map_data$Month>8)], pch=16, col=colcode[which(map_data$Month>8)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Fall", bty="n")

par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9, bty="n", title="Temperature")
dev.off()
#### Salinity map ####
var="Salinity_psu"
plotvar <- map_data[,var]
nclr=6
plotclr <- rev(brewer.pal(nclr,"RdBu"))
class <- classIntervals(plotvar, nclr, style="equal")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]

jpeg(filename = paste("./plot/salinity_map.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
layout(matrix(c(1,1,1,3,2,2,2,3), 2, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month<7)], map_data$end_lat[which(map_data$Month<7)], pch=16, col=colcode[which(map_data$Month<7)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Spring", bty="n")

plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month>8)], map_data$end_lat[which(map_data$Month>8)], pch=16, col=colcode[which(map_data$Month>8)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Fall", bty="n")

par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9, bty="n", title="Salinity")
dev.off()

#### Depth map####
var="end_depth"
plotvar <- map_data[,var]
nclr=6
plotclr <- rev(brewer.pal(nclr,"RdBu"))
class <- classIntervals(plotvar, nclr, style="equal")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]

jpeg(filename = paste("./plot/depth_map.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
layout(matrix(c(1,1,1,3,2,2,2,3), 2, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month<7)], map_data$end_lat[which(map_data$Month<7)], pch=16, col=colcode[which(map_data$Month<7)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Spring", bty="n")

plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month>8)], map_data$end_lat[which(map_data$Month>8)], pch=16, col=colcode[which(map_data$Month>8)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Fall", bty="n")

par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9, bty="n", title="Depth (Fathom)")
dev.off()

#### GAM ####
summary(lobster_catch_data$juv_num)
hist(lobster_catch_data$juv_num)
trawl_points <- SpatialPoints(cbind(lobster_catch_data$end_lon, lobster_catch_data$end_lat), proj4string=CRS("+proj=longlat +datum=WGS84")) 
polygon_points <- over(trawl_points , sa511_513)

gam_model_511 <- gam(juv_num~s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="511" & lobster_catch_data$Year<2017 & (lobster_catch_data$Month==6 | lobster_catch_data$Month>8)),])
summary(gam_model_511)

jpeg(filename = paste("./plot/gam511_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
par(mfrow=c(2,2), mar=c(4,4,1,1))
#plot.gam(gam_model_511, xlab=c("Depth"), ylab="Lobster density", select = 1)
plot.gam(gam_model_511, xlab=c("Temperature"), ylab="Lobster density", select = 2)
plot.gam(gam_model_511, xlab=c("Salinity"), ylab="Lobster density", select = 3)
plot.gam(gam_model_511, xlab=c("Latitude"), ylab="Lobster density", select = 3)
plot.gam(gam_model_511, xlab=c("Longitude"), ylab="Lobster density", select = 4)
dev.off()

gam_model_512 <- gam(juv_num~s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="512"& lobster_catch_data$Year<2017 & (lobster_catch_data$Month==6 |lobster_catch_data$Month>8)),])
summary(gam_model_512)

jpeg(filename = paste("./plot/gam512_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
par(mfrow=c(2,2), mar=c(4,4,1,1))
plot.gam(gam_model_512, xlab=c("Temperature"), ylab="Lobster density", select = 2)
plot.gam(gam_model_512, xlab=c("Salinity"), ylab="Lobster density", select = 3)
plot.gam(gam_model_512, xlab=c("Latitude"), ylab="Lobster density", select = 3)
plot.gam(gam_model_512, xlab=c("Longitude"), ylab="Lobster density", select = 4)
dev.off()

gam_model_513 <- gam(juv_num~s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="513"& lobster_catch_data$Year<2017 & (lobster_catch_data$Month==6 |lobster_catch_data$Month>8)),])
summary(gam_model_513)

jpeg(filename = paste("./plot/gam513_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
par(mfrow=c(2,2), mar=c(4,4,1,1))
plot.gam(gam_model_513, xlab=c("Temperature"), ylab="Lobster density", select = 2)
#plot.gam(gam_model_513, xlab=c("Salinity"), ylab="Lobster density", select = 3)
plot.gam(gam_model_513, xlab=c("Latitude"), ylab="Lobster density", select = 3)
plot.gam(gam_model_513, xlab=c("Longitude"), ylab="Lobster density", select = 4)
dev.off()

save(gam_model_511, gam_model_512, gam_model_513, file="./output/gam_fit.RData")

#### Grid time ID ####
yrs<-2012
dates<-as.data.frame(matrix(NA,length(yrs)*3,3))
colnames(dates)<-c('y','m','d')
dates$y <- rep(yrs, each=3)
dates$d<-rep(16,length(yrs))
dates$m<-rep(c(6,7,8),length(yrs))
dates$date <- do.call(paste, list(dates$m, dates$d, dates$y))
dates$date <- as.Date(dates$date, format=c("%m %d %Y"))
dates$julian_date <- as.numeric(dates$date) + 2440588
dates$modified_julian_date <- floor(dates$julian_date-2400000.5)
dates$modified_julian_date <- dates$modified_julian_date+0.17 
#### Download FVCOM time ID ####
fvcom_data <-as.data.frame(read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?time[0:1:342347]"))
fvcom_time <- as.data.frame(fvcom_data$Dataset..[5:nrow(fvcom_data)])
names(fvcom_time) <-"modified_julian_date"
fvcom_time$modified_julian_date <- as.numeric(as.character(fvcom_time$modified_julian_date))
fvcom_time$id <- 0:(nrow(fvcom_time)-1)

#### Match time ####
time_id <- c()
for(i in 1:nrow(dates)){
    temp <- fvcom_time$id[which(round(fvcom_time$modified_julian_date,2)==round(dates$modified_julian_date[i],2))]
    if(length(temp)==0) time_id[i] <- NA
    else time_id[i] <- temp
}
#### Download FVCOM location data ####
lat <- read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?lat[0:1:48450]")
lon <- read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?lon[0:1:48450]")
names(lat) <- "lat"
names(lon) <- "lon"

latitude <- lat$lat[5:nrow(lat)]
longitude <- lon$lon[5:nrow(lon)]
latitude <- as.numeric(as.character(latitude))
longitude <- as.numeric(as.character(longitude))
#### Grid location####
start_x <- floor(range(lobster_catch_data$end_lon)[1])
end_x <- ceiling(range(lobster_catch_data$end_lon)[2])
start_y <- floor(range(lobster_catch_data$end_lat)[1])
end_y <- ceiling(range(lobster_catch_data$end_lat)[2])
my_mesh=expand.grid(seq(start_x, end_x, by=0.01), seq(start_y, end_y, by=0.01))
coordinates(my_mesh) <- ~Var1 + Var2
grid_data <-as.data.frame(my_mesh@coords)
colnames(grid_data) <-c("lon", "lat")
grid_data <- grid_data[which(grid_data$lat>42.9),]

#### Download depth data ####
## Download ARC ASCII from https://www.ngdc.noaa.gov/mgg/coastal/grddas01/grddas01.htm
depth_raster <- raster("./data/gis/ne_atl_crm_v1.asc")
#grid_data$depth <- extract.data(grid_data, depth_raster)
grid_data$depth <- raster::extract(depth_raster, grid_data)
grid_data$fathom <- grid_data$depth/1.8288
write.csv(grid_data, file="./output/grid_depth_data.csv")

#### Download temperature data ####
temperature_fvcom_data<-list()
for (i in 1:length(time_id)){
    print(i)
    temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?temp[", time_id[i], ":1:", time_id[i], "][44:1:44][0:1:48450]", sep=""))
    temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
    temperature_fvcom_data[[i]] <- cbind(longitude, latitude, temp_data)
    colnames(temperature_fvcom_data[[i]]) <- c("lon", "lat", "temperature")
}
save(temperature_fvcom_data, file="./data/temperature_data.RData")

load("./data/temperature_data.RData")
temperature_raster_data <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 1:length(time_id)){
    print(i)
    temp_data <- as.data.frame(temperature_fvcom_data[[i]])
    rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
    rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
    akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
    rast <- raster(akima.smooth)
    #temperature_raster_data[[i]] <- extract.data(grid_data, rast)
    temperature_raster_data[[i]] <- raster::extract(rast, grid_data)
}
save(temperature_raster_data, file="./output/temperature_raster_data.RData")
#### Download salinity data ####
salinity_fvcom_data<-list()
for (i in 1:length(time_id)){
    print(i)
    temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?salinity[", time_id[i], ":1:", time_id[i], "][44:1:44][0:1:48450]", sep=""))
    temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
    salinity_fvcom_data[[i]] <- cbind(longitude, latitude, temp_data)
    colnames(salinity_fvcom_data[[i]]) <- c("lon", "lat", "salinity")
}
save(salinity_fvcom_data, file="./data/salinity_data.RData")

load("./data/salinity_data.RData")
salinity_raster_data <- list()
for(i in 1:length(time_id)){
    print(i)
    temp_data <- as.data.frame(salinity_fvcom_data[[i]])
    rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
    rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
    akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
    rast <- raster(akima.smooth)
    #salinity_raster_data[[i]] <- extract.data(grid_data, rast)
    salinity_raster_data[[i]] <- raster::extract(rast, grid_data)
}
save(salinity_raster_data, file="./output/salinity_raster_data.RData")
#### Plot depth grid map ####
#grid_data <- read.csv(file="./output/grid_depth_data.csv")
depth_grid_plot <- read.csv("./output/grid_depth_data.csv")
plot_data <- as.data.frame(cbind(depth_grid_plot$lon, depth_grid_plot$lat, -depth_grid_plot$fathom))
colnames(plot_data) <- c("Longitude", "Latitude", "Y")
depth_odd_id <- which(plot_data$Y<=0)
summary(plot_data)
plot_data <- na.omit(plot_data)
plot_data <- plot_data[which(plot_data$Y>0),]
summary(plot_data)

plotvar <- plot_data$Y
nclr=8
plotclr <- brewer.pal(nclr+1,"Blues")[2:length(brewer.pal(nclr+1,"Blues"))]
class <- classIntervals(plotvar, nclr, style="equal")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]
jpeg(filename = paste("./plot/grid_depth_map.jpeg", sep=""), width=100, height=50, units = "mm", res = 600)
layout(matrix(c(1,1,1,2), 1, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(plot_data$Longitude, plot_data$Latitude, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
box()
degAxis(1)
degAxis(2)
par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.75, bty="n", title="Depth (Fathom)")
dev.off()

#### Plot temperature grid map ####
load("./output/temperature_raster_data.RData")

jpeg(filename = paste("./plot/grid_temperature_map.jpeg", sep=""), width=120, height=140, units = "mm", res = 600)
par(mar=c(4,4,0,0), mfrow=c(2,2))
plotvar=unlist(temperature_raster_data)
nclr=10
plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
class <- classIntervals(plotvar, nclr, style="equal")
fix_break<-round(class$brks, digits = 2)
for(i in 1:length(temperature_raster_data)){
    print(i)
    plotvar <- temperature_raster_data[[i]]
    nclr=10
    plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
    class <- classIntervals(plotvar, nclr, style="fixed", fixedBreaks=fix_break)
    colcode <- findColours(class, plotclr)
    
    start_x <- range(lobster_catch_data$end_lon)[1]
    end_x <- range(lobster_catch_data$end_lon)[2]
    start_y <- range(lobster_catch_data$end_lat)[1]
    end_y <- range(lobster_catch_data$end_lat)[2]
    plot(depth_grid_plot$lon, depth_grid_plot$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(dates$y[i], "-", dates$m[i], sep=""), bty="n", cex=0.7)
    axis(1, cex=0.5)
    axis(2, cex=0.5)
}

par(mar=c(0.1,2,0.1,0.1))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.7, bty="n", title="Temperature (?C)")
dev.off()

#### Plot salinity grid map ####
load("./output/salinity_raster_data.RData")

jpeg(filename = paste("./plot/grid_salinity_map.jpeg", sep=""), width=120, height=140, units = "mm", res = 600)
par(mar=c(4,4,0,0), mfrow=c(2,2))

plotvar=unlist(salinity_raster_data)
nclr=10
plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
class <- classIntervals(plotvar, nclr, style="quantile")
fix_break<-round(class$brks, digits = 2)
for(i in 1:length(salinity_raster_data)){
    print(i)
    plotvar <- salinity_raster_data[[i]]
    
    nclr=10
    plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
    class <- classIntervals(plotvar, nclr, style="fixed", fixedBreaks=fix_break)
    colcode <- findColours(class, plotclr)
    
    start_x <- range(lobster_catch_data$end_lon)[1]
    end_x <- range(lobster_catch_data$end_lon)[2]
    start_y <- range(lobster_catch_data$end_lat)[1]
    end_y <- range(lobster_catch_data$end_lat)[2]
    plot(depth_grid_plot$lon, depth_grid_plot$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(dates$y[i], "-", dates$m[i], sep=""), bty="n", cex=0.7)
    
    axis(1, cex=0.5)
    axis(2, cex=0.5)
}
par(mar=c(0.1,2,0.1,0.1))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.7, bty="n", title="Salinity (psu)")
dev.off()

