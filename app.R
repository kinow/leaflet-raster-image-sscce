# SSCCE for Leaflet issue involving raster images.

library(shiny)
library(leaflet)
library(sp)
library(raster)

# --- using example from https://stackoverflow.com/questions/33742107/raster-image-seems-to-be-shifted-using-leaflet-for-r

set.seed(111)
# create dummy data -rectangular grid with random values
m = 10
n = 10
x = seq(45, 48, length.out = m)
y = seq(15, 18, length.out = n)
X = matrix(rep(x, each = n), nrow = n)
Y = matrix(rep(y, m), nrow = n)

# --- end of dummy example

# UI
ui <- fluidPage(
  titlePanel("SSCCE for Leaflet issue involving raster images"),
  
  # Show a plot of the generated distribution
  mainPanel(
    radioButtons(
      "raster",
      "Raster:",
      c("Raster 1" = "raster1",
        "Raster 2" = "raster2")
    ),
    leafletOutput("myMap", height = "350px")
  )
)

# Backend
server <- function(input, output) {
  output$myMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(16.3738, 46.2082, 6)
  })
  
  observeEvent(input$raster, {
    points = NA
    if (input$raster == "raster1") {
      points = data.frame(value = rnorm(n * m),
                          lng = c(Y),
                          lat = c(X))
    } else {
      points = data.frame(value = rnorm(n + m),
                          lng = c(Y),
                          lat = c(X))
    }
    s = SpatialPixelsDataFrame(points[, c('lng', 'lat')], data = points)
    crs(s) = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    raster = raster(s)
    # cat(file = stderr(), "YO!", raster)
    leafletProxy("myMap") %>%
      addRasterImage(raster, project = F)
  })
  
}

shinyApp(ui = ui, server = server)
