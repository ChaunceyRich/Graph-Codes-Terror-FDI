install.packages("data.table")
install.packages("magrittr")
install.packages("leaflet")
install.packages("maps")
install.packages("maptools")
install.packages("raster")
install.packages("rgeos")
install.packages("sp")
install.packages("htmltools")
install.packages(fread)

library(data.table)
library(magrittr)
library(leaflet)
library(maps)
library(maptools)
library(raster)
library(rgeos)
library(sp)
library(htmltools)
library(fread)

GTD <- fread('Data/globalterrorismdb_0617dist.csv', showProgress = FALSE)
FDI <- fread('Data/FDI Data.csv')

dim(GTD)

# paste year, month, and day into a single date column
GTD[, date := paste(iyear, imonth, iday, sep = '-')]

# set blank locations to "Not specified"
GTD[location == "", location := "Not specified"]

# create the labels
labels <- sprintf(
    "Date: %s</br>City:%s</br>Location: %s", GTD$date, GTD$city, dat$location) %>%
    lapply(HTML)


GTD %>%
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(~ longitude,
    ~ latitude,
    radius = 4,
    label = labels,
    clusterOptions = markerClusterOptions())

    # get the world map data
world_map <- map("world", fill = TRUE, col = 1, plot = FALSE)

# get just the country names
world_map_ids <- sapply( strsplit( world_map$names, ':' ), function(x) x[1] )

# convert to a SpatialPolygon
world_sp <- map2SpatialPolygons(world_map, IDs=world_map_ids, proj4string=CRS("+proj=longlat +datum=WGS84"))

# get the names of the countries from world_sp
tmp_id_df <- data.frame(ID = names(world_sp))

# make the rownames the state name as well
rownames(tmp_id_df) <- names(world_sp)

# make the SpatialPolygonDataFrame
world_spdf <- SpatialPolygonsDataFrame(world_sp, tmp_id_df)

# get number of attacks by country
GTD[, country_txt := gsub('Czechoslovakia', 'Czech Republic', country_txt)]
attacks <- GTD[, .N, by = country_txt][, country_txt := country_txt]

# get 2016 population data
pop_2016 <- pop[year == 2016, c('country_name', 'midyear_population')]

# Correct country name mismatches between terrorism and population data
pop_2016[, country_name := gsub('Burma', 'Myanmar', country_name)]
pop_2016[, country_name := gsub('United States', 'USA', country_name)]
pop_2016[, country_name := gsub('Congo \\(Brazzaville\\)', 'Republic of Congo', country_name)]
pop_2016[, country_name := gsub('Congo \\(Kinshasa\\)', 'Democratic Republic of the Congo', country_name)]
pop_2016[, country_name := gsub("Cote d'Ivoire", 'Ivory Coast', country_name)]
pop_2016[, country_name := gsub("Czechia", 'Czech Republic', country_name)]
pop_2016[, country_name := gsub("Korea North", 'North Korea', country_name)]
pop_2016[, country_name := gsub("Korea South", 'South Korea', country_name)]
pop_2016[, country_name := gsub('United Kingdom', 'UK', country_name)]

# correct the country name mismatches between the terrorism data and the geo data
attacks[, country_txt := gsub('Bosnia-Herzegovina', 'Bosnia and Herzegovina', country_txt)]
attacks[, country_txt := gsub("United States", "USA", country_txt)]
attacks[, country_txt := gsub("People's Republic of the Congo", "Republic of Congo", country_txt)]
attacks[, country_txt := gsub("Czechoslovakia", 'Czech Republic', country_txt)]
attacks[, country_txt := gsub("United Kingdom", "UK", country_txt)]
attacks[, country_txt := gsub("Slovak Republic", "Slovakia", country_txt)]

attacks <- merge(attacks, pop_2016, by.x = 'country_txt', by.y = 'country_name')

# calculate attacks per capita
attacks[, attacks_10000 := N/midyear_population*10000]

# merge with the spatial dataframe by country name
dd <- merge(world_spdf, attacks, by.x = 'ID', by.y = 'country_txt')

# create the choropleth palette
bins <- c(0, 50, 100, 500, 1000, 2000, 5000, 10000, 30000)
pal <- colorBin("YlOrRd", domain = dd$density, bins = bins)


# create the country labels
labels <- sprintf(
    "%s</br>Number of attacks: %g", dd$ID, dd$N) %>%
    lapply(HTML)

# build the map
leaflet(dd) %>%
    addTiles() %>%
    setView(lat = 9.1383, lng = 38.7223, zoom = 1) %>%
    addPolygons(data = dd, weight = 1, fillColor = ~pal(dd$N), fillOpacity = 0.5,
                    highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                    bringToFront = TRUE),
                    label = labels) %>%
    addLegend(pal = pal, values = ~ N, opacity = 0.7, title = NULL, position = 'bottomright')