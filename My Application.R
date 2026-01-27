pacman::p_load( 
  rio, 
  here, 
  janitor, 
  lubridate, 
  parsedate, 
  aweek, 
  zoo, 
  matchmaker, 
  epikit, 
  tidyverse, 
  skimr 
)

# 1. Daten bereinigen

# import 
daten <- read.csv("persons_of_concern.csv")

# cleaning 
pacman::p_load(dplyr) 

persons_of_concern_clean <- daten %>% 
  dplyr::filter( 
    Refugees != 0, 
    Country.of.Origin != Country.of.Asylum 
  )

countries_to_remove <- c( 
  "British Virgin Islands", 
  "Cayman Islands", 
  "Hong Kong SAR", 
  "Curacao", 
  "Sint Maarten \\(Dutch Part\\)", 
  "Turks and Caicos Islands" 
)

country_renaming <- c( 
  "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom", 
  "United Rep of Tanzania" = "Tanzania", 
  "TÃ¼rkiye" = "Turkey", "Syrian Arab Rep\\." = "Syria", 
  "Iran \\(Islamic Rep\\. of\\)" = "Iran", 
  "Netherlands \\(Kingdom of the\\)" = "Netherlands", 
  "Rep. of Korea" = "South Korea", 
  "Russian Federation" = "Russia", 
  "Venezuela \\(Bolivarian Republic of\\)" = "Venezuela", 
  "Viet Nam" = "Vietnam", 
  "Serbia and Kosvo: S/RES/1244 \\(1999\\)" = "Serbia and Kosovo", 
  "Bolivia \\(Plurinational State of\\)" = "Bolivia" 
)

persons_of_concern_clean <- 
  persons_of_concern_clean %>%
  
  # UnerwÃ¼nschte LÃ¤nder komplett entfernen 
  filter( 
    !if_any( 
      everything(), 
      ~ str_detect(as.character(.x), 
                   str_c(countries_to_remove, collapse = "|")) 
    ) 
  ) %>%
  
  # LÃ¤ndernamen in ALLEN Spalten vereinheitlichen
  mutate( 
    across( 
      everything(), 
      ~ { 
        x <- as.character(.x) 
        for (old in names(country_renaming)) { 
          x <- str_replace_all(x, old, 
                               country_renaming[[old]]) 
        } 
        x 
      } 
    ) 
  )

library(shiny)
library(dplyr)
library(plotly)
library(countrycode)
library(scales)
library(stringr)

europe_countries <- c(
  "Albania", "Andorra", "Austria", "Belarus", "Belgium",
  "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Czechia",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
  "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", "Latvia",
  "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova",
  "Monaco", "Montenegro", "Netherlands", "North Macedonia",
  "Norway", "Poland", "Portugal", "Romania", "Russia",
  "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain",
  "Sweden", "Switzerland", "Ukraine", "United Kingdom",
  "Vatican City"
)

asia_countries <- c(
  "Armenia", "Azerbaijan", "Cyprus", "Georgia", "Kazakhstan", "Turkey",
  "Afghanistan", "Bahrain", "Bangladesh", "Bhutan", "Brunei",
  "Cambodia", "China", "India", "Indonesia", "Iran", "Iraq",
  "Israel", "Japan", "Jordan", "Kuwait", "Kyrgyzstan", "Laos",
  "Lebanon", "Malaysia", "Maldives", "Mongolia", "Myanmar",
  "Nepal", "North Korea", "Oman", "Pakistan", "Philippines",
  "Qatar", "Saudi Arabia", "Singapore", "South Korea",
  "Sri Lanka", "Syria", "Taiwan", "Tajikistan", "Thailand",
  "Timor-Leste", "Turkmenistan", "United Arab Emirates",
  "Uzbekistan", "Vietnam", "Yemen"
)

africa_countries <- c(
  "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso",
  "Burundi", "Cabo Verde", "Cameroon", "Central African Republic",
  "Chad", "Comoros", "Cote d'Ivoire",
  "Democratic Republic of the Congo", "Djibouti", "Egypt",
  "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia",
  "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau",
  "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar",
  "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco",
  "Mozambique", "Namibia", "Niger", "Nigeria",
  "Republic of the Congo", "Rwanda",
  "Sao Tome and Principe", "Senegal", "Seychelles",
  "Sierra Leone", "Somalia", "South Africa", "South Sudan",
  "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda",
  "Zambia", "Zimbabwe"
)

south_america_countries <- c(
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
  "Ecuador", "Guyana", "Paraguay", "Peru",
  "Suriname", "Uruguay", "Venezuela"
)

north_central_america_countries <- c(
  "Antigua and Barbuda", "Bahamas", "Barbados", "Belize",
  "Canada", "Costa Rica", "Cuba", "Dominica",
  "Dominican Republic", "El Salvador", "Grenada",
  "Guatemala", "Haiti", "Honduras", "Jamaica",
  "Mexico", "Nicaragua", "Panama",
  "Saint Kitts and Nevis", "Saint Lucia",
  "Saint Vincent and the Grenadines",
  "Trinidad and Tobago",
  "United States of America"
)

australia_oceania_countries <- c(
  "Australia", "Federated States of Micronesia", "Fiji",
  "Kiribati", "Marshall Islands", "Nauru",
  "New Zealand", "Palau", "Papua New Guinea",
  "Samoa", "Solomon Islands", "Tonga",
  "Tuvalu", "Vanuatu"
)


# -------------------------
# Daten vorbereiten
# -------------------------

df <- persons_of_concern_clean %>%
  mutate(
    Refugees = as.numeric(as.character(Refugees)),
    
    asylum_continent = countrycode(
      Country.of.Asylum,
      "country.name",
      "continent"
    )
  ) %>%
  filter(!is.na(Refugees))


df <- persons_of_concern_clean %>%
  mutate(
    Year = as.numeric(as.character(Year)),
    Refugees = as.numeric(as.character(Refugees))
  ) %>%
  filter(
    !is.na(Year),
    !is.na(Refugees)
  )


# -------------------------
# UI
# -------------------------

ui <- fluidPage(
  
  # ------------------------
  # CSS für Hintergrund und Titel
  # ------------------------
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #e6f2ff; /* Helles Blau für die Website */
      }
      .title-panel {
        background-color: #3399ff; /* Mittleres Blau für die obere Zeile */
        color: white;
        padding: 15px;
        text-align: center;
        font-size: 24px;
        font-weight: bold;
      }
      .well-panel {
        background-color: #f8f9fa !important; /* Optional: beibehalten hellgrau für Panels */
      }
    "))
  ),
  
  # ------------------------
  # Titelzeile
  # ------------------------
  div(class = "title-panel", "Post-Pandemic Global Refugee Flow"),
  
  
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3, 
      style = "background-color:#f8f9fa; padding:20px;",
      
      sliderInput(
        "year",
        "Year",
        min = min(df$Year),
        max = max(df$Year),
        value = c(min(df$Year), max(df$Year)),
        step = 1,
        sep = ""
      ),
      
      selectInput(
        "origin",
        "Country of Origin",
        choices = sort(unique(df$Country.of.Origin))
      ),
      
      selectInput(
        "asylum",
        "Country / Region of Asylum",
        choices = c(
          "Worldwide",
          "Europe",
          "Asia",
          "Africa",
          "South America",
          "North & Central America",
          "Australia & Oceania",
          sort(unique(df$Country.of.Asylum))
        ),
        selected = "Worldwide",
        selectize = TRUE
      )
    ),
    
    mainPanel(
      plotlyOutput("map"),
      
      wellPanel( 
        style = "background-color: #f8f9fa;", 
        h4("How to read the map"), 
        p("This map visualizes global refugee movements from a selected country of origin to countries of asylum."), 
        p("Each line represents a refugee flow. The thickness of the line corresponds to the total number of refugees for the selected years."), 
        p("When multiple years are selected, refugee numbers are aggregated."), 
        p("Hover over a destination country to see detailed information, including the country of origin, country of asylum, total refugees, and selected years.")
      ),
      
      
    )
  )
)

# -------------------------
# Server
# -------------------------

server <- function(input, output) {
  
  filtered_data <- reactive({
    
    data <- df %>%
      filter(
        Country.of.Origin == input$origin,
        Year >= input$year[1],
        Year <= input$year[2]
      ) %>%
      

      group_by(Country.of.Origin, Country.of.Asylum) %>%
      summarise(
        Refugees = sum(Refugees, na.rm = TRUE),
        .groups = "drop"
      )
    
    
    if (input$asylum == "Worldwide") {
      
      data
      
    } else if (input$asylum == "Europe") {
      
      data %>% filter(Country.of.Asylum %in% europe_countries)
      
    } else if (input$asylum == "Asia") {
      
      data %>% filter(Country.of.Asylum %in% asia_countries)
      
    } else if (input$asylum == "Africa") {
      
      data %>% filter(Country.of.Asylum %in% africa_countries)
      
    } else if (input$asylum == "South America") {
      
      data %>% filter(Country.of.Asylum %in% south_america_countries)
      
    } else if (input$asylum == "North & Central America") {
      
      data %>% filter(Country.of.Asylum %in% north_central_america_countries)
      
    } else if (input$asylum == "Australia & Oceania") {
      
      data %>% filter(Country.of.Asylum %in% australia_oceania_countries)
      
    } else {
      
      data %>% filter(Country.of.Asylum == input$asylum)
    }
  }) 
  
  
  
  output$map <- renderPlotly({
    
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0, "No data available for selected filters")
    )
    
    line_data <- data %>%
      mutate(group_id = row_number()) %>%
      tidyr::pivot_longer(
        cols = c(Country.of.Origin, Country.of.Asylum),
        names_to = "type",
        values_to = "country"
      )
    
    
    plot_ly() %>%
      
      # Linien zwischen Origin und Asylum
      add_trace(
        type = "scattergeo",
        data = line_data,
        locations = ~country,
        locationmode = "country names",
        split = ~group_id,
        mode = "lines",
        showlegend = FALSE,
        line = list(
          width = ~rescale(Refugees, c(1, 8)),
          color = "steelblue"
        ),
        hoverinfo = "none"
      ) %>%
      
      # Zielpunkte mit Hover-Infos
      add_trace(
        type = "scattergeo",
        data = data,
        locations = ~Country.of.Asylum,
        locationmode = "country names",
        mode = "markers",
        showlegend = FALSE,
        text = ~paste0(
          "<b>Origin:</b> ", Country.of.Origin, "<br>",
          "<b>Asylum:</b> ", Country.of.Asylum, "<br>",
          "<b>Total refugees:</b> ", format(Refugees, big.mark = ","), "<br>",
          "<b>Years:</b> ", input$year[1], " - ", input$year[2]
        ),
        hoverinfo = "text",
        marker = list(size = 6, color = "red")
      ) %>%
      
      layout(
        margin = list(l=0,r=0,t=0,b=0),
        showlegend = FALSE,
        geo = list(
          scope = "world",
          showland = TRUE,
          landcolor = "#f0f0f0",
          showcountries = TRUE,
          countrycolor = "#bdbdbd",
          projection = list(type = "equirectangular")
          
        )
      ) %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        scrollZoom = TRUE
      )
  })
} 

# -------------------------
# App starten
# -------------------------

shinyApp(ui, server)