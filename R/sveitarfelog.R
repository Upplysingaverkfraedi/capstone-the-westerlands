# Hlöðum inn nauðsynlegum bókasöfnum
library(shiny)
library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)
library(yaml)
library(leaflet)
library(sf)
library(here)
library(scales)
library(ggplot2)
library(tidyr)
library(ggrepel)


# Stillum kerfisstafasett á UTF-8
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Hlöðum stillingaskrá
config <- yaml::read_yaml("config.yml")

# Tengjumst PostgreSQL gagnagrunni með upplýsingum úr config.yml
conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = config$database$dbname,
  host = config$database$host,
  port = config$database$port,
  user = config$database$user,
  password = config$database$password,
  options = "-c client_encoding=UTF8"
)

# Hlaða inn shapefile fyrir sveitarfélög
d <- st_read(here("R", "data-raw", "sveitarfelog", "Sveitarfelog_timalina.shp"), options = "ENCODING=ISO-8859-1") %>%
  filter(endir_tima == max(endir_tima)) %>%
  st_transform(crs = "WGS84")

# Úthlutun lita fyrir kjördæmin
district_colors <- c(
  "Norðvesturkjördæmi" = "#FFD700",
  "Norðausturkjördæmi" = "#9370DB",
  "Suðurkjördæmi" = "#FFC0CB",
  "Suðvesturkjördæmi" = "#32CD32",
  "Reykjavíkurkjördæmi norður og suður" = "#FFA500"
)

district_mapping <- list(
  "Norðvesturkjördæmi" = c("Akraneskaupstaður", "Hvalfjarðarsveit", "Skorradalshreppur", "Borgarbyggð", 
                           "Eyja- og Miklaholtshreppur", "Snæfellsbær", "Grundarfjarðarbær", 
                           "Sveitarfélagið Stykkishólmur", "Dalabyggð", "Reykhólahreppur", "Vesturbyggð", 
                           "Bolungarvíkurkaupstaður", "Ísafjarðarbær", "Súðavíkurhreppur", "Skagabyggð",
                           "Árneshreppur", "Kaldrananeshreppur", "Strandabyggð", "Húnaþing vestra", 
                           "Húnabyggð", "Sveitarfélagið Skagaströnd", "Skagafjörður", "Tálknafjarðarhreppur"),
  "Norðausturkjördæmi" = c("Fjallabyggð", "Dalvíkurbyggð", "Hörgársveit", "Akureyrarbær", 
                           "Eyjafjarðarsveit", "Svalbarðsstrandarhreppur", "Grýtubakkahreppur", 
                           "Þingeyjarsveit", "Skútustaðahreppur", "Norðurþing", "Tjörneshreppur", 
                           "Svalbarðshreppur", "Langanesbyggð", "Vopnafjarðarhreppur", 
                           "Múlaþing", "Fljótsdalshreppur", "Fjarðabyggð"),
  "Suðurkjördæmi" = c("Sveitarfélagið Hornafjörður", "Skaftárhreppur", "Mýrdalshreppur", 
                      "Rangárþing eystra", "Rangárþing ytra", "Ásahreppur", "Vestmannaeyjabær", 
                      "Flóahreppur", "Sveitarfélagið Árborg", "Skeiða- og Gnúpverjahreppur", 
                      "Hrunamannahreppur", "Bláskógabyggð", "Grímsnes- og Grafningshreppur", 
                      "Hveragerðisbær", "Sveitarfélagið Ölfus", "Grindavíkurbær", 
                      "Suðurnesjabær", "Reykjanesbær", "Sveitarfélagið Vogar"),
  "Suðvesturkjördæmi" = c("Hafnarfjarðarkaupstaður", "Garðabær", "Kópavogsbær", "Seltjarnarnesbær", 
                          "Mosfellsbær", "Kjósarhreppur"),
  "Reykjavíkurkjördæmi norður og suður" = c("Reykjavíkurborg")
)

# Úthlutum litum til sveitarfélaga
get_district_color <- function(sveitarfel) {
  for (district in names(district_mapping)) {
    if (sveitarfel %in% district_mapping[[district]]) {
      return(district_colors[district])
    }
  }
  return("#CCCCCC")
}

d$color <- sapply(d$sveitarfel, get_district_color)

# Hlaða sveitarfélagagögn úr hjálpartöflu
sveitarfelog_data <- dbGetQuery(conn, "
  SELECT 
    sveitarfelag, 
    ar AS year, 
    tegund AS property_type, 
    medal_fermetraverd AS avg_price_per_sqm, 
    mannfjoldi AS population 
  FROM 
    sveitarfelog_upplysingar
")
# Hlaða póstnúmeragögn úr hjálpartöflu
postnr_data <- dbGetQuery(conn, "
  SELECT 
    sveitarfelag, 
    postnr, 
    ar AS year, 
    tegund AS property_type, 
    medal_fermetraverd AS avg_price_per_sqm
  FROM 
    samantekt_postnumer
")

# Hlaða mannfjöldagögn úr hjálpartöflu
mannfjoldi_data <- dbGetQuery(conn, "
  SELECT 
    sveitarfelag, 
    ar AS year, 
    mannfjoldi AS population
  FROM 
    sveitarfelog_upplysingar
")

# Hlaða herbergjagögn úr hjálpartöflu
herbergi_data <- dbGetQuery(conn, "
  SELECT 
    sveitarfelag, 
    ar AS year, 
    tegund AS property_type, 
    fjherb AS rooms, 
    medal_fermetraverd AS avg_price_per_sqm
  FROM 
    samantekt_herbergi
")

# Hlaða gögn fyrir kökurit úr hjálpartöflu
property_type_data <- dbGetQuery(conn, "
  SELECT 
    sveitarfelag, 
    ar AS year, 
    property_type AS property_type, 
    total_properties AS total_properties
  FROM 
    samantekt_tegund_eigna
")

# Loka tengingu við gagnagrunn
dbDisconnect(conn)

# Viðmót fyrir Shiny
ui <- fluidPage(
  titlePanel("Sveitarfélög Íslands - Fermetraverð, Mannfjöldi og Herbergjafjöldi"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Veldu ár", 
                  min = min(sveitarfelog_data$year), 
                  max = max(sveitarfelog_data$year), 
                  value = max(sveitarfelog_data$year), 
                  step = 1, sep = ""),
      selectInput("property_type", "Veldu fasteignategund",
                  choices = unique(sveitarfelog_data$property_type),
                  selected = "Fjölbýli")
    ),
    mainPanel(
      leafletOutput("map"),
      plotOutput("postnr_plot"),
      plotOutput("rooms_plot"),
      plotOutput("population_plot"),
      plotOutput("property_type_pie")
    )
  )
)

# Lógík fyrir Shiny
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    d_with_data <- d %>%
      left_join(
        sveitarfelog_data %>%
          filter(year == input$year, property_type == input$property_type),
        by = c("sveitarfel" = "sveitarfelag")
      )
    
    leaflet(d_with_data) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(
        fillColor = ~color, 
        weight = 2,
        opacity = 1,
        color = "black",
        fillOpacity = 0.6,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "white",
          bringToFront = TRUE
        ),
        label = ~paste(
          sveitarfel, "- Fermetraverð: ",
          ifelse(is.na(avg_price_per_sqm), "Gögn ekki tiltæk",
                 paste0(scales::comma(round(avg_price_per_sqm, 2)), " þús kr/m²"))
        ),
        labelOptions = labelOptions(
          direction = "auto",
          textsize = "15px",
          style = list(
            "font-family" = "Arial, sans-serif",
            "font-size" = "13px",
            "font-weight" = "normal",
            "padding" = "5px",
            "border-radius" = "5px",
            "background-color" = "rgba(255, 255, 255, 0.9)",
            "box-shadow" = "2px 2px 4px rgba(0,0,0,0.2)"
          ),
          htmlEscape = FALSE
        ),
        layerId = ~sveitarfel
      ) %>%
      addLegend(
        position = "bottomright",
        colors = unname(district_colors),
        labels = names(district_colors),
        title = "Kjördæmi",
        opacity = 0.7
      )
  })
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$id)) {
      selected_municipality <- click$id
      
      # Gögn fyrir póstnúmer stöplarit
      filtered_postnr <- postnr_data %>%
        filter(sveitarfelag == selected_municipality, year == input$year, property_type == input$property_type)
      
      output$postnr_plot <- renderPlot({
        ggplot(filtered_postnr, aes(x = as.factor(postnr), y = avg_price_per_sqm)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          labs(
            title = paste("Fermetraverð miðað við póstnúmer í", selected_municipality),
            x = "Póstnúmer",
            y = "Fermetraverð (þús kr/m²)"
          ) +
          theme_minimal()
      })
      
      # Gögn fyrir herbergjafjölda stöplarit
      filtered_rooms <- herbergi_data %>%
        filter(sveitarfelag == selected_municipality, year == input$year, property_type == input$property_type)
      
      output$rooms_plot <- renderPlot({
        ggplot(filtered_rooms, aes(x = as.factor(rooms), y = avg_price_per_sqm)) +
          geom_bar(stat = "identity", fill = "orange") +
          labs(
            title = paste("Áhrif herbergjafjölda á fermetraverð í", selected_municipality),
            x = "Fjöldi herbergja",
            y = "Fermetraverð (þús kr/m²)"
          ) +
          theme_minimal()
      })
      
      # Gögn fyrir mannfjölda línurit
      filtered_population <- mannfjoldi_data %>%
        filter(sveitarfelag == selected_municipality)
      
      output$population_plot <- renderPlot({
        ggplot(filtered_population, aes(x = year, y = population)) +
          geom_line(color = "blue", size = 1) +
          geom_point(color = "blue", size = 2) +
          labs(
            title = paste("Mannfjöldaþróun í", selected_municipality),
            x = "Ár",
            y = "Mannfjöldi"
          ) +
          theme_minimal() +
          scale_y_continuous(labels = scales::comma)
      })
      
      # Sía gögn fyrir kökurit af hlutfalli tegunda eigna
      filtered_pie_data <- property_type_data %>%
        filter(sveitarfelag == selected_municipality, year == input$year) %>%
        group_by(property_type) %>%
        summarize(
          total_properties = sum(total_properties, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        tidyr::complete(property_type = unique(property_type_data$property_type), fill = list(total_properties = 0)) %>%
        mutate(
          percentage = total_properties / sum(total_properties) * 100
        )
      
      # Kóði fyrir kökurit
      output$property_type_pie <- renderPlot({
        ggplot(filtered_pie_data, aes(x = "", y = total_properties, fill = property_type)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar(theta = "y") +
          labs(
            title = paste("Hlutföll seldra eigna eftir tegund í", selected_municipality, "árið", input$year),
            fill = "Tegund eigna"
          ) +
          theme_minimal() +
          theme(
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5)
          ) +
          geom_label_repel(
            aes(label = paste0(property_type, ": ", total_properties, " (", round(percentage, 1), "%)")),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            size = 4,
            max.overlaps = 10
          )
      })
    }
  }) 
} 

# Keyra Shiny App
shinyApp(ui = ui, server = server)
