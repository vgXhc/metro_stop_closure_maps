library(tidyverse)
library(sf)
library(tmap)
library(tidytransit)
library(tmaptools)



# read in closed stops
closed_stops <- read_csv("data/busstop-closedlist.csv") |>
  rename(stop_id = `Stop ID`,
         direction = Direction,
         stop_name = `Stop Name`)

#geocode stops with GTFS files
gtfs <- read_gtfs("data/metro-2023-05-14-gtfs.zip")

stops <- gtfs$stops

closed_stops <- closed_stops |>
  inner_join(stops, by = c("stop_id" = "stop_code")) |>
  mutate(stop_name_file = str_replace_all(stop_name.y, "[^0-9a-zA-Z]+", "-"))

# doing an inner join because the closure list has separate IDs for each direction of a stop
# but the GTFS file does not
# closure list also has a bunch of stops that currently don't have service
closed_stops_sf <- closed_stops |>
  st_as_sf(coords = c("stop_lon", "stop_lat"))

# next we need all the stops that will still have service after the redesign
open_stops <- stops |>
  anti_join(closed_stops, by = c("stop_code" = "stop_id")) |>
  filter(!stop_code %in% c("WeTP", "EaTP", "NoTP", "SoTP")) |> #remove transfer points
  st_as_sf(coords = c("stop_lon", "stop_lat"))
st_crs(open_stops) <- 4326
st_crs(closed_stops_sf) <- 4326

# get routes that will still be in service after the change
# https://r-transit.github.io/tidytransit/articles/frequency.html

services <- gtfs$calendar |>
  filter(start_date == "2023-06-11") |> #when the new network starts
  pull(service_id)

routes <- gtfs$trips |>
  filter(service_id %in% services) |>
  pull(shape_id)

gtfs$shapes |>
  filter(shape_id %in% routes) |>
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat")) |>
  tm_shape() +
  tm_dots()

routes_sf <- get_route_geometry(gtfs_as_sf(gtfs), service_ids = services)

#sf object with the new routes
routes_new <- routes_sf |>
  left_join(gtfs$routes, by = "route_id")


# function to render a parametrized pdf for a given stop_id
render_closure_map <- function(stop_id) {
  stop_name <- closed_stops |> filter(stop_id == stop_id) |> pull(stop_name_file)
  params = list(stop_id = stop_id)
  rmarkdown::render(input = "template.Rmd",
                    params = params,
                    output_file = paste0("output/",
                                         stop_id,
                                         # "-",
                                         # stop_name,
                                         "_closure_sign.pdf"))
}


# iterate over all stops to produce the pdfs
walk(closed_stops$stop_id, render_closure_map) #index 420 and 421 caused errors



# generate list of files as markdown links and copy to clipboard.
# Then you can paste into a Jekyll page

closed_stops |>
  arrange(stop_id) |>
  mutate(
    full_path = paste0("[", stop_id, " - ", stop_name.y, "](", "assets/pdf/", stop_id, "_closure_sign.pdf)"),
         ) |>
  pull(full_path) |>
  clipr::write_clip(breaks = "\n\n")



