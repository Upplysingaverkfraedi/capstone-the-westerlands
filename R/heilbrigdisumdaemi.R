library(sf)
library(leaflet)
library(here)

url <- paste0(
  "https://gis.is/geoserver/embaetti_landlaeknis/wfs",
  "?request=GetFeature&service=WFS&version=1.1.0",
  "&typeName=embaetti_landlaeknis:heilbrigdisumdaemi&",
  "outputFormat=SHAPE-ZIP"
)


download.file(
  url,
  here("data-raw", "heilbrigdisumdaemi", "heilbrigdisumdaemi.zip"),
  method = "auto"
)

unzip(
  here("data-raw", "heilbrigdisumdaemi", "heilbrigdisumdaemi.zip"),
  exdir = here("data-raw", "heilbrigdisumdaemi")
)

d <- st_read(
  here("data-raw", "heilbrigdisumdaemi", "heilbrigdisumdaemi.shp"),
  options = "ENCODING=ISO-8859-10"
)

for (i in seq_len(nrow(d))) {
  n_list <- length(d$geometry[[i]])
  for (j in seq_len(n_list)) {
    X <- d$geometry[[i]][[j]][[1]][, 1]
    Y <- d$geometry[[i]][[j]][[1]][, 2]
    
    d$geometry[[i]][[j]][[1]][, 1] <- Y
    d$geometry[[i]][[j]][[1]][, 2] <- X
  }
}

d |> 
  st_transform(crs = "WGS84") |> 
  leaflet() |> 
  addProviderTiles(providers$OpenStreetMap) |> 
  addPolygons()

st_write(
  d,
  here("data", "heilbrigdisumdaemi", "heilbrigdisumdaemi.shp")
)
