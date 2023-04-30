#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)
library(tmap)
library(tidytransit)

# read in closed stops
closed_stops <- read_csv("data/busstop-closedlist.csv") |>
  rename(stop_id = `Stop ID`,
         direction = Direction,
         stop_name = `Stop Name`)

#geocode stops with GTFS files
gtfs <- read_gtfs("data/metro-2023-04-17-gtfs.zip")

stops <- gtfs$stops

# doing an inner join because the closure list has separate IDs for each direction of a stop
# but the GTFS file does not
# closure list also has a bunch of stops that currently don't have service
closed_stops_sf <- closed_stops |>
  inner_join(stops, by = c("stop_id" = "stop_code")) |>
  st_as_sf(coords = c("stop_lon", "stop_lat"))

# next we need all the stops that will still have service after the redesign
open_stops <- stops |>
  anti_join(closed_stops, by = c("stop_code" = "stop_id")) |>
  st_as_sf(coords = c("stop_lon", "stop_lat"))
st_crs(open_stops) <- 4326
st_crs(closed_stops_sf) <- 4326



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Print a map for your closed bus stop"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("stopId",
                        "Choose a stop",
                        choices = closed_stops$stop_id,
                        multiple = FALSE
                        ),
            downloadButton("downloadPdf",
                           label = "Generate and download pdf")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$downloadPdf <- downloadHandler(
      filename = "report.pdf",
      content = function(file) {
        # Copy the report file and data to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        temp_dir <- tempdir()
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("template.Rmd", file.path(temp_dir, "report.Rmd"), overwrite = TRUE)
        file.copy("data/busstop-closedlist.csv", temp_dir)
        file.copy("data/metro-2023-04-17-gtfs.zip", temp_dir)

        # Set up parameters to pass to Rmd document
        params <- list(stop_id = input$stopId)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }

    )
}



# Run the application
shinyApp(ui = ui, server = server)
