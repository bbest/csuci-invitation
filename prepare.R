library(tidyverse)
library(leaflet)
library(mapview)
library(mapedit)
library(ggmap)
library(sf)
library(here)
library(glue)
library(fs)
library(vembedr)

# paths
gkey_txt <- "~/private/google-maps-api-key_bdbest-csuci-invite.txt"
pts_csv <- here("data/pts.csv")


# geocode places ----

register_google(readLines(gkey_txt))

# create pts_csv
if (!file.exists(pts_csv)){
  pts_df <- tribble(
    ~place,
    "California State University Channel Islands",
    "Santa Barbara, CA") %>% 
    mutate_geocode(place)
  
  write_csv(pts_df, pts_csv)
}
pts_df <- read_csv(pts_csv)

# geocode any missing places
pts_todo <- filter(pts_df, is.na(lon), is.na(lat))
if (nrow(pts_todo) > 0){
  pts_df <- rbind(
    pts_df %>% 
      filter(!is.na(lon) & !is.na(lat)),
    pts_todo %>% 
      select(-lon, -lat) %>% 
      mutate_geocode(place))
  
  write_csv(pts_df, pts_csv)
}

# convert to points
pts <- pts_df %>% 
  st_as_sf(coords = c("lon", "lat"), remove=F, crs=4326)

# move markers ----

# only run this R chunk to move markers
pts <- editFeatures(places, label=~place)
pts_df <- pts %>% 
  mutate(
    lon = st_coordinates(geometry)[1,],
    lat = st_coordinates(geometry)[1,]) %>% 
  st_drop_geometry()
write_csv(pts_df, pts_csv)

# resize thumbs ----
library(magick)

pngs <- list.files(here("docs/img"), full.names=T)

cat(paste(list.files(here("docs/img"), ".*_sm.png"), collapse='\n'))
resize <- function(png_in){
  # png_in  <- here("docs/img/infographic.png")

  png_out <- fs::path_ext_remove(png_in) %>% glue("_sm.png")
  
  image_read(png_in) %>% 
    image_scale("100") %>% 
    image_write(path=png_out) # , format = "png")  
}

walk(pngs, resize)


image_scale(image, "200")