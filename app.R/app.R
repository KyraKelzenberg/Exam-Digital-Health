library(shiny)
library(dplyr)
library(plotly)
library(countrycode)
library(scales)
library(maps)

# =========================
# 1. COUNTRY CENTERS (MUSS GANZ OBEN STEHEN)
# =========================

world <- map_data("world")

country_centers <- world %>%
  group_by(region) %>%
  summarise(
    lon = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    .groups = "drop"
  )

# =========================
# 2. DATEN (TESTDATEN – FUNKTIONIEREN SICHER)
# =========================

persons_of_concern_clean <- data.frame(
  Country.of.Origin = c("Syria", "Syria", "Syria"),
  Country.of.Asylum = c("Germany", "Turkey", "Lebanon"),
  Year = c(2020, 2021, 2022),
  Refugees = c(800000, 3500000, 900000)
)

# =========================
# 3. DATEN VORBEREITEN
# =========================

df <- persons_of_concern_clean %>%
  mutate(
    Refugees = as.numeric(Refugees),
    asylum_continent = countrycode(
      Country.of.Asylum,
      "country.name",
      "continent"
    )
  ) %>%
  left_join(country_centers, by = c("Country.of.Origin" = "region")) %>%
  rename(origin_lon = lon, origin_lat = lat) %>%
  left_join(country_centers, by = c("Country.of.Asylum" = "region")) %>%
  rename(asylum_lon = lon, asylum_lat = lat)

# =========================
# 4. SHINY UI
# =========================

ui <- fluidPage(
  titlePanel("Refugees Flow 2020 - 2024"),
  plotlyOutput("map", height = "600px")
)

# =========================
# 5. SHINY SERVER
# =========================

server <- function(input, output) {
  
  output$map <- renderPlotly({
    
    plot_ly(type = "scattergeo") %>%
      add_segments(
        data = df,
        lat = ~origin_lat,
        lon = ~origin_lon,
        latend = ~asylum_lat,
        lonend = ~asylum_lon,
        line = list(
          width = ~rescale(Refugees, c(1, 8)),
          color = "steelblue"
        )
      ) %>%
      add_markers(
        data = df,
        lat = ~asylum_lat,
        lon = ~asylum_lon,
        text = ~paste(
          "Asylum:", Country.of.Asylum,
          "<br>Refugees:", Refugees
        ),
        hoverinfo = "text"
      ) %>%
      layout(
        geo = list(
          scope = "world",
          showland = TRUE,
          showcountries = TRUE
        )
      )
  })
}

# =========================
# 6. APP STARTEN
# =========================

shinyApp(ui, server)

