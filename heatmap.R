library(tidyverse)
library(magrittr)
library(raster)
library(fields)
library(readxl)

#loads origin and destination files including name, lon/lat,
#and for origin data, travel time from either ferry terminal or Port Authority
#Travel times were acquired from internal studies or google maps api
Origins <- read_csv("Origins.csv")

Destinations <- read_csv("Destinations.csv") 

#loads walktime data. numbers represent walk time from origin(row)
#to destination(column).  data was aquired using google maps api.
walktimes <- 
  read_csv("WalktimesAll.csv", col_names = FALSE) %>%
  t() %>%
  data.frame() %>%
  set_colnames(Origins$location) %>%
  set_rownames(Destinations$location)

#creates raster, sets size and resolution
r <- raster(xmn=-74.012218, xmx=-73.963563, 
            ymn=40.736009, ymx=40.7725, 
            ncol=50, 
            nrow=60)

#creates list of rasters, destination coordinates, and walktimes from each origin to all destinations
#each list is the same length as number of origins
rlist <- rep(list(r), nrow(Origins))
destinations.list <- rep(list(Destinations[,2:3]), nrow(Origins))
walktimes.list <- purrr::map(walktimes, function(x) x)

#creates list of splines using walktimes
splines <- map2(destinations.list, walktimes.list, function(x,y) Tps(x = x, Y = y, lon.lat = TRUE))

#creates rasters from splines
rasters <- 
  list(x = destinations.list, field = walktimes.list, y = rlist) %>%
  pmap(rasterize)

#interpolates splines to fill space between measurements
#result is a list of walktime heatmaps for each origin
maps <- 
  list(model = splines, object = rasters, xyOnly = TRUE) %>%
  pmap(interpolate)

#splits origin dataframe into ferry and bus origins
Origins.ferry <- filter(Origins, type == "Ferry")
Origins.bus <- filter(Origins, type == "Bus")

Origins.list <- list(Origins.ferry, Origins.bus)
maps.list <- list(maps[Origins$type == "Ferry"], maps[Origins$type == "Bus"])

#loops through each map adding the travel time from the terminals to adjust
#walking times for travel time to origin
inters <- list(rep(list(NA), nrow(Origins.ferry)), rep(list(NA), nrow(Origins.bus)))
for(i in 1:2){
  for(j in 1:nrow(Origins.list[[i]])){
    inters[[i]][[j]] <- maps.list[[i]][[j]] + Origins.list[[i]][[j,5]]
  }
}  

#creates individual heatmaps for ferry and bus travel by choosing the minimum
#travel time to each cell in the raster from any of the interpolated maps
ferry.heatmap <- inters[[1]] %>%
  stack() %>%
  min()

bus.heatmap <- inters[[2]] %>%
  stack() %>%
  min()



