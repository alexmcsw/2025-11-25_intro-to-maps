library(tidyverse)
library(osmdata)
library(sf)
library(sysfonts)
library(showtext)
library(grid)

bbox <- c(
  xmin = -82.41, # min longitude (western-most extent)
  ymin = 42.24, # min latitude (southern-most extent)
  xmax = -82.455, # max longitude (eastern-most extent)
  ymax = 42.28 # max latitude (northern-most extent)
)


## Step 3: Build Overpass query

query <- opq(bbox = bbox)


## Step 4: Extract OSM data

# rivers
water_a <- query |>
  add_osm_feature(
    key = "natural",
    value = "water"
  ) |>
  osmdata_sf()


# other bodies of water
water_b <- query |>
  add_osm_feature(
    key = "water"
  ) |>
  osmdata_sf()

water_c <- query |>
  add_osm_features(
    features = c(name = "Tilbury Creek")
  ) |>
  osmdata_sf()

rails <- query |>
  add_osm_feature(
    key = "railway"
  ) |>
  osmdata_sf()


# "major" roads
roads_a <- query |>
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway",
      "trunk",
      "primary",
      "motorway_link",
      "trunk_link",
      "primary_link",
      "secondary",
      "tertiary"
    )
  ) |>
  osmdata_sf()


# "minor" roads
roads_b <- query |>
  add_osm_feature(
    key = "highway",
    value = c(
      "unclassified",
      "residential",
      "secondary_link",
      "tertiary_link"
    )
  ) |>
  osmdata_sf()


# natural vegetation
natural <- query |>
  add_osm_feature(
    key = "natural",
    value = c("wood", "water", "wetland")
  ) |>
  osmdata_sf()


# recreational greenery
rec <- query |>
  add_osm_features(
    list(
      "landuse" = c(
        "grass",
        "forest",
        "recreation_ground",
        "cemetery",
        "religious"
      ),
      "leisure" = c(
        "playground",
        "park",
        "garden",
        "dog_park",
        "golf_course",
        "pitch"
      )
    )
  ) |>
  osmdata_sf()

farm <- query |>
  add_osm_features(
    list(
      "landuse" = c("farmland")
    )
  ) |>
  osmdata_sf()

res <- query |>
  add_osm_features(
    list(
      "landuse" = c("residential", "commercial")
    )
  ) |>
  osmdata_sf()


## Step 5: Crop sf objectsP

bbox_sf <- st_as_sfc(st_bbox(bbox, crs = 4326))

water_a_cropped <- st_crop(water_a$osm_polygons, bbox_sf)
water_b_cropped <- st_crop(water_b$osm_polygons, bbox_sf)
water_c_cropped <- st_crop(water_c$osm_lines, bbox_sf)
roads_a_cropped <- st_crop(roads_a$osm_lines, bbox_sf)
roads_b_cropped <- st_crop(roads_b$osm_lines, bbox_sf)
rails_cropped <- st_crop(rails$osm_lines, bbox_sf)
natural_cropped <- st_crop(natural$osm_polygons, bbox_sf)
rec_cropped <- st_crop(rec$osm_polygons, bbox_sf)
farm_cropped <- st_crop(farm$osm_polygons, bbox_sf)
res_cropped <- st_crop(res$osm_polygons, bbox_sf)

## Step 6: Visualize sf objects

# define colour palette
water_colour <- "#d3e1e4"
roads_a_colour <- "#000000"
roads_b_colour <- "#000000"
natural_colour <- "#9bac95"
rec_colour <- natural_colour
farm_colour <- "#edecd1"
res_colour <- "#E5E5E5"
bkgd_colour <- "#f0f0f0"


plt <- ggplot() +
  # natural vegetation
  geom_sf(
    data = natural_cropped,
    fill = natural_colour,
    colour = NA
  ) +
  # farmland
  geom_sf(
    data = farm_cropped,
    fill = farm_colour,
    colour = NA
  ) +
  # residential
  geom_sf(
    data = res_cropped,
    fill = res_colour,
    colour = NA
  ) +
  # recreational spaces
  geom_sf(
    data = rec_cropped,
    fill = rec_colour,
    colour = NA
  ) +
  # major roads
  geom_sf(
    data = roads_a_cropped,
    colour = roads_a_colour,
    size = 0.6,
    alpha = 0.8
  ) +
  # minor roads
  geom_sf(
    data = roads_b_cropped,
    colour = roads_b_colour,
    size = 0.5,
    alpha = 0.6
  ) +
  # rails
  geom_sf(
    data = rails_cropped,
    colour = roads_b_colour,
    size = 0.4,
    alpha = 0.3
  ) +
  # rivers
  geom_sf(
    data = water_a_cropped,
    fill = water_colour,
    colour = NA
  ) +
  # other bodies of water
  geom_sf(
    data = water_b_cropped,
    fill = water_colour,
    colour = NA
  ) +
  geom_sf(
    data = water_c_cropped,
    colour = water_colour,
    size = 1.5
  ) +
  # remove margins
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = bkgd_colour)
  )


## Step 7: Add text and export

# define colour palette
page_colour <- "#ffffff"
text_colour <- "#000000"
city <- "TILBURY"
country <- "CANADA"
coordinates <- "42.26°N | 82.43°W"


# use Grid Viewports to strategically position map and text
sysfonts::font_add_google("Montserrat", "Montserrat", regular.wt = 200)
png("tilbury-map.png", width = 5, height = 7, units = "in", res = 300)
showtext::showtext_begin()

vp1 <- viewport(x = 0.5, y = 0.5, width = 1, height = 1)
pushViewport(vp1)
grid.rect(gp = gpar(fill = page_colour))
upViewport()

vp2 <- viewport(x = 0.5, y = 0.575, width = 0.9, height = 0.75)
pushViewport(vp2)
print(plt, newpage = FALSE)
upViewport()

vp3 <- viewport(x = 0.75, y = 0.15, width = 0.4, height = 0.08, just = "left")
pushViewport(vp3)
grid.text(
  city,
  just = "right",
  gp = gpar(fontfamily = "Montserrat", fontsize = 100, col = text_colour)
)
upViewport()

vp4 <- viewport(x = 0.75, y = 0.1, width = 0.4, height = 0.03, just = "left")
pushViewport(vp4)
grid.text(
  country,
  just = "right",
  gp = gpar(fontfamily = "Montserrat", fontsize = 40, col = text_colour)
)
upViewport()

vp5 <- viewport(x = 0.75, y = 0.065, width = 0.4, height = 0.03, just = "left")
pushViewport(vp5)
grid.text(
  coordinates,
  just = "right",
  gp = gpar(fontfamily = "Montserrat", fontsize = 40, col = text_colour)
)
upViewport()

showtext::showtext_end()
dev.off()
