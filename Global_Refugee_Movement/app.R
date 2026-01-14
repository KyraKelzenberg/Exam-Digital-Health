library(shiny)
library(dplyr)
library(readr)
library(countrycode)
library(leaflet)
library(maps)

# ---- CSV laden ----
df <- read_csv(
  "persons_of_concern.csv",
  col_names = c("year", "from_name", "to_name",
                "from_iso", "to_iso", "trips", "id")
)

# ---- Weltkarte + Centroids ----
world_df <- map_data("world")

centroids <- world_df %>%
  group_by(region) %>%
  summarise(
    lon = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE)
  ) %>%
  mutate(
    iso3 = countrycode(region, "country.name", "iso3c"),
    country_name = countrycode(iso3, "iso3c", "country.name")
  )

# ---- UI ----
ui <- fluidPage(
  titlePanel("Global Movements – Origin → Asylum"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Jahr:",
                  choices = sort(unique(df$year)),
                  selected = max(df$year)),
      selectInput("origin", "Country of Origin:",
                  choices = c("ALL", sort(unique(df$from_name)))),
      selectInput("asylum", "Country of Asylum:",
                  choices = c("ALL", sort(unique(df$to_name))))
    ),
    mainPanel(
      leafletOutput("map", height = "800px")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Basiskarte
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 30, lat = 20, zoom = 2)
  })
  
  observe({
    
    # Filter anwenden
    d <- df %>% filter(year == input$year)
    if (input$origin != "ALL") d <- d %>% filter(from_name == input$origin)
    if (input$asylum != "ALL") d <- d %>% filter(to_name == input$asylum)
    
    # Mergen mit Centroids
    d <- d %>%
      left_join(centroids, by = c("from_name" = "country_name")) %>%
      rename(lon_from = lon, lat_from = lat) %>%
      left_join(centroids, by = c("to_name" = "country_name")) %>%
      rename(lon_to = lon, lat_to = lat)
    
    # Entfernen von Zeilen ohne Koordinaten
    d <- d %>% filter(!is.na(lon_from), !is.na(lat_from),
                      !is.na(lon_to), !is.na(lat_to))
    
    if(nrow(d) == 0) {
      leafletProxy("map") %>% clearShapes()
      return(NULL)
    }
    
    # Linien hinzufügen (konstant blau)
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolylines(
        data = d,
        lng = ~c(lon_from, lon_to),
        lat = ~c(lat_from, lat_to),
        color = "blue",
        weight = 3,
        opacity = 0.8,
        label = ~paste0(from_name, " → ", to_name, "<br>Trips: ", trips),
        labelOptions = labelOptions(direction = "auto")
      )
  })
}

shinyApp(ui, server)

