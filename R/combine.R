library(here)
library(sf)
library(dplyr)
library(ggplot2)
library(leaflet)
library(tmap)

heilbr <- st_read(
  here("data", "heilbrigdisumdaemi", "heilbrigdisumdaemi.shp")
) |> 
  select(heilbrigdi, geometry) |> 
  st_transform(crs = "WGS84")

svf <- st_read(
  here("data", "sveitarfelog", "sveitarfelog.shp")
) |> 
  select(nrsveitarf, sveitarfel, geometry) |> 
  st_transform(crs = "WGS84")

svf


d <- st_join(
  svf,
  heilbr
) |> 
  mutate(
    n = n(),
    .by = sveitarfel
  ) |> 
  mutate(
    is_unique = if_else(n == 1, "Yes", "No")
  ) 


st_join(
  svf,
  heilbr,
  join = st_covered_by,
  model = "semi-open",
  sparse = FALSE
) |> 
  select(-geometry) |> 
  as_tibble() |> 
  count(sveitarfel, heilbrigdi) |> 
  count(heilbrigdi)

d |> 
  select(-geometry) |> 
  as_tibble() |> 
  count(heilbrigdi)


d |> 
  mutate(
    col = nrsveitarf %% 12
  ) |> 
  ggplot() +
  geom_sf(
    data = ~filter(
      .x, 
      is_unique == "No", 
      sveitarfel %in% c(
        "Langanesbyggð"
      )
    ),
    col = NA,
    aes(fill = sveitarfel),
    alpha = 0.7, 
    # show.legend = FALSE
  ) +
  geom_sf(
    data = heilbr,
    fill = NA,
    col = "black",
    linewidth = 2
  )  +
  # scale_fill_brewer(palette = "Paired") +
  theme_void()

tmap_mode("view")

d |> 
  filter(is_unique == "No") |> 
  tm_shape() +
  tm_polygons(
    fill = "sveitarfel",
    fill_alpha = 0.3,
    col = "heilbrigdi"
  ) +
  tm_legend(show = FALSE)

