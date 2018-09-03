library(dplyr)
library(treemap) # tree map visualization
library(ggmap)
library(plotly)
library(leaflet)
library(gridExtra)


schoolExplorer <- read.csv("../input/2016 School Explorer.csv")
head(schoolExplorer$Latitude)
head(schoolExplorer$Longitude)
head(schoolExplorer$School.Name)

# Setting location data of Kyoto
ny.geo <- data.frame(-74.0060,40.7128)
ny.geo <- data.frame(ny.geo)
names(ny.geo) <- c("lon", "lat")

leaflet(data = schoolExplorer) %>% 
  addProviderTiles("Stamen.TonerLite") %>% 
  setView(lng = ny.geo$lon, lat = ny.geo$lat, zoom = 13) %>% 
  addCircleMarkers(~Longitude, ~Latitude, popup = ~School.Name,
                   clusterOptions = markerClusterOptions()) %>% 
  addLegend("topright", colors= "blue", 
            labels="School Name", title="Schools in Ny")
          
schoolExplorer$Economic.Need.Index <- as.numeric(as.character(schoolExplorer$Economic.Need.Index))          
pal1 <- colorNumeric(palette = "YlGnBu",domain = schoolExplorer$Economic.Need.Index)

leaflet(data = schoolExplorer) %>% addTiles()  %>%  
addProviderTiles(providers$CartoDB.Positron, group = "City") %>%
addCircles(~Longitude, ~Latitude, radius=~Economic.Need.Index ,color = ~pal1(Economic.Need.Index),stroke = TRUE, 
             fillOpacity = 1.0,popup = paste("City:", schoolExplorer$City, "<br>","School:", schoolExplorer$School.Name, "<br>"),
              group = "Economic.Need.Index")%>% 
 addLegend("bottomright", pal = pal1, values = ~Economic.Need.Index,
    title = "Economic.Need.Index",
    opacity = 1)%>%
             setView(-73.935242, 40.730610, zoom = 10) 