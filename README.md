# Heatmap


#Script uses R to creat interpolated thin-plate splines to map walking time throughout Midtown Manhattan.  
#Maps are then adjusted to account for travel time to origin of map.  
#For example to begin walking on the street outside of Penn Station, an idividual would have to walk to the Subway within the Port Authority, ride the subway, then walk from the train to the street.
#All of this additional time is included in the travel time.
#By stacking all the rasters associated with NJ Transit and Ferry travel together respectively, the minimum travel time to every location can be calculated.
#The resulting raster shows the minimum travel time to every location in Midtown using the desired method, starting at the Port Authority for NJ Transit and the ferry terminal for ferry commute.