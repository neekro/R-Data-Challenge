library(tidyverse)
library(ggmap)

# Read instagram data, which are not actually comma separated, but tab separated
dataInsta <- read_tsv("instagram_data.csv")

dataOsm <- read.csv("osm_data.csv", sep = ";", dec = ",")




# Data exploration

sort(table(dataOsm$TAG_VALUE))



### ggmap attempt
#plot the  hybrid Google Maps basemap

# map <- qmap("Prague", zoom = 13, maptype = 'hybrid')
# map + geom_point(data = dataOsm, aes(x = LON, y = LAT), color="red", size=3, alpha=0.5)

# # Doesnt work - temporary down?
# mapOsm <- get_openstreetmap(bbox = c(left = min(dataOsm$LON), 
#                                min(dataOsm$LAT), 
#                                max(dataOsm$LON),
#                                max(dataOsm$LAT)))


# Load a map; using zoom=15 sadly results in cutting the upper part of the map
mapGoog <- get_googlemap(center = c(lon = mean(dataOsm$LON), 
                                    lat = mean(dataOsm$LAT)), 
                         zoom = 15,
                         size = c(640, 640),
                         maptype = "roadmap")

map <- ggmap(mapGoog)

mapHeat <- map + geom_jitter(data = dataInsta, 
                         aes(x = longitude, y = latitude), 
                         color="red", size=3, alpha=0.02,
                         width = 0.0005, height = 0.0005)

# map <- map + geom_point(data = dataOsm, 
#                         aes(x = LON, y = LAT), 
#                         color="blue",
#                         size=1.5, alpha=0.8)


mapFinal <- mapHeat + geom_point(data = dataOsm %>% 
                          filter(TAG_VALUE %in% c("restaurant", "bar", 
                                                  "cafe", "hotel", "fast_food")), 
                        aes(x = LON, y = LAT, colour = TAG_VALUE), 
                        # color="blue", 
                        size=1.5, alpha=0.8)

mapFinal



minLon <- min(mapFinal$data$lon)
maxLon <- max(mapFinal$data$lon)
minLat <- min(mapFinal$data$lat)
maxLat <- max(mapFinal$data$lat)

# Change rounding
# most liked
# most discussed
# most common words
# most common hashtags

summaryInsta <- dataInsta %>% 
  # Filter out locations out of the map scope
  filter(longitude > minLon, 
         longitude < maxLon, 
         latitude > minLat, 
         latitude < maxLat) %>% 
  # group_by(longitude = round(longitude, 4), 
  #          latitude = round(latitude, 4)) %>% 
  group_by(longitude, latitude, location_name) %>% 
  summarise(nPosts = n(),
            likeSum = sum(likes_count),
            commentSum = sum(comments_count),
            likePerPost = likeSum / nPosts,
            likePerPostMedian = median(likes_count))

summaryInsta %>% 
  arrange(desc(likeSum))



dataInsta$longitude