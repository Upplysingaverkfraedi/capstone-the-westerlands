library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)
library(DBI)
library(RPostgres)
library(sf)
library(here)
library(scales)
library(yaml)
library(ggrepel)

library(readxl)
library(dplyr)
library(tidyr)

# File Path
path <- "Capstone-toflur.xlsx"

# Read data
dat <- read_excel(path, 3)
visitalabygg <- read_excel(path, 5)
visitala <- read_excel(path, 4)
tekjur <- read_excel(path, 6, skip = 2)
fasteignaverd <- read_excel(path, 1, skip = 1)

# REGEX to replace Icelandic characters with English for column names
new_names <- names(dat)
new_names <- gsub("ð", "d", new_names)
new_names <- gsub("Ð", "D", new_names)
new_names <- gsub("þ", "th", new_names)
new_names <- gsub("Þ", "Th", new_names)
new_names <- gsub("æ", "ae", new_names)
new_names <- gsub("Æ", "Ae", new_names)
new_names <- gsub("ö", "o", new_names)
new_names <- gsub("Ö", "O", new_names)
new_names <- gsub("á", "a", new_names)
new_names <- gsub("Á", "A", new_names)
new_names <- gsub("é", "e", new_names)
new_names <- gsub("É", "E", new_names)
new_names <- gsub("í", "i", new_names)
new_names <- gsub("Í", "I", new_names)
new_names <- gsub("ó", "o", new_names)
new_names <- gsub("Ó", "O", new_names)
new_names <- gsub("ú", "u", new_names)
new_names <- gsub("Ú", "U", new_names)
new_names <- gsub("ý", "y", new_names)
new_names <- gsub("Ý", "Y", new_names)
names(dat) <- new_names

# REGEX fyrir að taka út NA gildi
filtered_dat <- dat[
  grepl("^[0-9]+(\\.[0-9]+)?$", dat$`Fermetraverd eftir sveitafelogum arid 2022`) &
    grepl("^[0-9]+(\\.[0-9]+)?$", dat$`Midgildi tekna 2023`), 
]
# REGEX til að breyta í töluleg gildi 
filtered_dat$`Fermetraverd eftir sveitafelogum arid 2022` <- as.numeric(filtered_dat$`Fermetraverd eftir sveitafelogum arid 2022`)
filtered_dat$`Midgildi tekna 2023` <- as.numeric(filtered_dat$`Midgildi tekna 2023`)
# REGEX til að setja gildin í hækkandi röð
filtered_dat <- filtered_dat[order(filtered_dat$`Fermetraverd eftir sveitafelogum arid 2022`), ]
filtered_dat <- filtered_dat %>%
  arrange(`Fermetraverd eftir sveitafelogum arid 2022`)


##endurstilla víssitöluna þannig að hún byrji í 100 árið 1994
upphafsgildi <- visitala$Vísitala[1]
# Breyttu vísitölunni þannig að hún byrji í 100
visitala$Vísitala<- round((visitala$Vísitala / upphafsgildi) * 100 ,2)
#nota REGEX til að flokka út bara þann tíma sem endar á 01
visitala <- visitala[grepl("01$", visitala$Tími), ]
visitala$Tími <- gsub("M[0-9]{2}", "", visitala$Tími)  # Fjarlægja MXX
visitala$Tími <- as.numeric(visitala$Tími)  # Breyta í tölulegt gildi
visitalabygg <- visitalabygg %>%
  filter(Mánuður == 1) # notum bara fyrsta mánuðinn í visitalabygg líka
visitala<- visitala %>%
  slice(-n())
sameinuð_tafla <- visitalabygg %>%
  left_join(visitala, by = c("Ár" = "Tími")) #búum til nýja töflu til þess að nota

fasteignaverd <- fasteignaverd %>%
  mutate(across(-SVEITARFELAG, ~ as.numeric(gsub(",", ".", .))))
tekjur <- tekjur %>%
  mutate(across(-Sveitarfelag, ~ . / 12)) 

# Gögn fyrir fyrstu útfærsla (scatter plot)
scatter_data <- data.frame(
  Sveitarfelag = rep(filtered_dat$`samsetningartafla`, 2),
  Type = c(rep("Fermetraverð", nrow(filtered_dat)), rep("Laun", nrow(filtered_dat))),
  Value = c(filtered_dat$`Fermetraverd eftir sveitafelogum arid 2022`, 
            filtered_dat$`Midgildi tekna 2023`)
)
sorted_municipalities <- sort(unique(scatter_data$Sveitarfelag))

# Load shapefile for municipalities
d <- st_read(here("data-raw", "sveitarfelog", "Sveitarfelog_timalina.shp"), options = "ENCODING=ISO-8859-1") %>%
  filter(endir_tima == max(endir_tima)) %>%
  st_transform(crs = "WGS84")

# Assign colors to districts
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

get_district_color <- function(sveitarfel) {
  for (district in names(district_mapping)) {
    if (sveitarfel %in% district_mapping[[district]]) {
      return(district_colors[district])
    }
  }
  return("#CCCCCC")
}
d$color <- sapply(d$sveitarfel, get_district_color)

# PostgreSQL data loading
config <- yaml::read_yaml("config.yml")
conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = config$database$dbname,
  host = config$database$host,
  port = config$database$port,
  user = config$database$user,
  password = Sys.getenv("PG_PASSWORD")
)

# Hlaða sveitarfélagagögn úr hjálpartöflu
sveitarfelog_data <- dbGetQuery(conn, "
  SELECT 
    sveitarfelag, 
    CAST(ar AS INTEGER) AS year,
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
    CAST(ar AS INTEGER) AS year, 
    tegund AS property_type, 
    medal_fermetraverd AS avg_price_per_sqm
  FROM 
    samantekt_postnumer
")

# Hlaða mannfjöldagögn úr hjálpartöflu
mannfjoldi_data <- dbGetQuery(conn, "
  SELECT 
    sveitarfelag, 
    CAST(ar AS INTEGER) AS year, 
    mannfjoldi AS population
  FROM 
    sveitarfelog_upplysingar
")

# Hlaða herbergjagögn úr hjálpartöflu
herbergi_data <- dbGetQuery(conn, "
  SELECT 
    sveitarfelag, 
    CAST(ar AS INTEGER) AS year, 
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
    CAST(ar AS INTEGER) AS year, 
    property_type AS property_type, 
    total_properties AS total_properties
  FROM 
    samantekt_tegund_eigna
")

#loka tengingu við gagnagrunn
dbDisconnect(conn)


# Shiny UI
ui <- fluidPage(
  titlePanel("Sameinað Shiny App"),
  tabsetPanel(
    # Tab for App 1: Fasteignamarkaðurinn
    tabPanel("App 1: Fasteignamarkaðurinn",
             tabsetPanel(
               
               # Fyrsti flipa fyrir línurit
               tabPanel("Vísitala íbúðarverðs", 
                        sidebarLayout(
                          sidebarPanel(
                            helpText("Línurit sem sýnir breytingu á vísitölum.")
                          ),
                          mainPanel(
                            plotOutput("linePlot")
                          )
                        )
               ),
               
               # Annar flipa fyrir sameinað tekju- og fasteignaverðsgraf
               tabPanel("Þróun á tekjum og fasteignaverði",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("sveitarfelag", "Veldu sveitarfélag:", choices = unique(tekjur$Sveitarfelag))
                          ),
                          mainPanel(
                            plotOutput("combinedPlot")
                          )
                        )
               ),
               
               # Þriðji flipa fyrir scatter plot
               tabPanel("Fermetraverð og laun sveitarfélaga", 
                        sidebarLayout(
                          sidebarPanel(
                            selectizeInput("sveitarfelog_plot", "Veldu sveitarfélög:",  
                                           choices = sorted_municipalities, 
                                           selected = sorted_municipalities[1:5], 
                                           multiple = TRUE),
                            radioButtons("plot_type", "Veldu hvaða gögn þú vilt sjá:",  
                                         choices = list("Fermetraverð og laun" = "fermetraverð",  
                                                        "Útborgun og laun" = "útborgun")),
                            actionButton("clear_plot", "Afvelja öll sveitarfélög"),
                            
                            br(), hr(),
                            
                            h4("Reiknivél fyrir húsnæðissparnað"),
                            selectizeInput("sveitarfelag_calc", "Veldu sveitarfélag:",  
                                           choices = sorted_municipalities, 
                                           selected = sorted_municipalities[1], 
                                           multiple = FALSE),
                            numericInput("savings_rate", "Hlutfall launa í húsnæðissparnað (%):", value = 33, min = 1, max = 100),
                            radioButtons("buyer_type", "Hver er að kaupa?:", choices = list("Einstaklingur" = "single", "Par" = "couple"), selected = "single"),
                            htmlOutput("savings_result")
                          ),
                          mainPanel(
                            plotOutput("scatterPlot")
                          )
                        )
               )
             )
             ),
    # Tab for App 2: Sveitarfélög Íslands
    tabPanel("App 2: Sveitarfélög Íslands",
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
  )
)

# Shiny Server
server <- function(input, output, session) {
  ### App 1 Code ###
  
  # Fyrsta útfærsla: Scatter Plot
  observeEvent(input$clear_plot, {
    updateSelectizeInput(session, "sveitarfelog_plot", selected = character(0))
  })
  
  output$scatterPlot <- renderPlot({
    filtered_data <- subset(scatter_data, Sveitarfelag %in% input$sveitarfelog_plot)
    
    if (input$plot_type == "fermetraverð") {
      filtered_data$Y_Value <- ifelse(filtered_data$Type == "Fermetraverð", 
                                      filtered_data$Value, filtered_data$Value / 10)
      y_axis_label <- "Fermetraverð (þús. kr.)"
      y_limits <- c(0, 800)
      sec_axis_transformation <- ~ . * 10
      legend_labels <- c("Fermetraverð", "Laun")
    } else {
      filtered_data$Y_Value <- ifelse(filtered_data$Type == "Fermetraverð", 
                                      filtered_data$Value * 12, filtered_data$Value)
      y_axis_label <- "Útborgun (þús. kr.)"
      y_limits <- c(0, 8000)
      sec_axis_transformation <- ~ .
      legend_labels <- c("Útborgun", "Laun")
    }
    
    ggplot(filtered_data, aes(x = Sveitarfelag, y = Y_Value, color = Type)) +
      geom_point(size = 4) +
      scale_y_continuous(
        name = y_axis_label,
        limits = y_limits,
        sec.axis = sec_axis(sec_axis_transformation, name = "Miðgildi launa (þús. kr.)")
      ) +
      labs(x = "Sveitarfélag", title = "Scatter plot: Verð og Laun eftir sveitarfélögum") +
      scale_color_manual(values = c("Fermetraverð" = "#53c68c", "Laun" = "darkblue"),
                         labels = legend_labels) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 12)
      )
  })
  
  output$savings_result <- renderUI({
    selected_data <- subset(scatter_data, Sveitarfelag == input$sveitarfelag_calc & Type == "Laun")
    income <- selected_data$Value
    
    selected_data <- subset(scatter_data, Sveitarfelag == input$sveitarfelag_calc & Type == "Fermetraverð")
    down_payment <- selected_data$Value * 12
    
    monthly_savings <- (income * (input$savings_rate / 100)) / 12
    months_needed <- ceiling(down_payment / monthly_savings)
    
    if (input$buyer_type == "couple") {
      months_needed <- ceiling(months_needed / 2)
    }
    
    if (months_needed >= 12) {
      years <- floor(months_needed / 12)
      months <- months_needed %% 12
      result_text <- paste("Með", input$savings_rate, "% sparnað nærðu útborgun fyrir 80 fm íbúð á", years, "ári og", months, "mánuðum.")
    } else {
      result_text <- paste("Með", input$savings_rate, "% sparnað nærðu útborgun á", months_needed, "mánuðum.")
    }
    
    tags$div(style = "background-color: #ff9999; padding: 15px; border-radius: 8px; font-weight: bold; font-size: 16px;",
             result_text)
  })
  
  # Annar flipa: Línurit
  output$linePlot <- renderPlot({
    ggplot(data = sameinuð_tafla, aes(x = Ár)) +
      geom_line(aes(y = Vísitala, color = "Vísitala neysluverðs án húsnæðis"), size = 1) +
      geom_line(aes(y = Vísitala_fjölb, color = "Vísitala fjölbílishús"), size = 1) +
      geom_line(aes(y = Visitala_serb, color = "Vísitala sérbíli"), size = 1) +
      geom_line(aes(y = Visitala_heild, color = "Vísitala íbúðarverðs"), size = 1) +
      labs(title = "Vísitölur yfir Tíma", x = "Ár", y = "Vísitala") +
      scale_color_manual(name = "Vísitala", values = c(
        "Vísitala neysluverðs án húsnæðis" = "green",
        "Vísitala fjölbílishús" = "orange",
        "Vísitala sérbíli" = "blue",
        "Vísitala íbúðarverðs" = "red"
      )) +
      theme_minimal()
  })
  
  # Þriðji flipa: Samanburður á tekjum og fasteignaverði
  output$combinedPlot <- renderPlot({
    tekjur_filtered <- tekjur %>%
      filter(Sveitarfelag == input$sveitarfelag) %>%
      pivot_longer(cols = -Sveitarfelag, names_to = "Ar", values_to = "Tekjur") %>%
      mutate(Ar = as.numeric(Ar)) %>%
      filter(Ar >= 2010)
    
    fasteignaverd_filtered <- fasteignaverd %>%
      filter(SVEITARFELAG == input$sveitarfelag) %>%
      pivot_longer(cols = -SVEITARFELAG, names_to = "Ar", values_to = "Fasteignaverð") %>%
      mutate(Ar = as.numeric(Ar)) %>%
      filter(Ar >= 2010)
    
    combined_data <- merge(tekjur_filtered, fasteignaverd_filtered, by = "Ar")
    
    ggplot(combined_data, aes(x = Ar)) +
      geom_line(aes(y = Tekjur, color = "Mánaðaralegar ráðstöfunartekjur"), size = 1) +
      geom_line(aes(y = Fasteignaverð, color = "Meðal fermetraverð"), size = 1) +
      labs(title = "Þróun á Tekjum og Fasteignaverði", x = "Ár", y = "Þús. krónur") +
      scale_color_manual(name = "Breytur", values = c("Mánaðaralegar ráðstöfunartekjur" = "darkblue", "Meðal fermetraverð" = "#53c68c")) +
      theme_minimal()
  })
  
  
  
  ### App 2 Code ###
  
  # Map (Sveitarfélög Íslands)
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
      
      # Postcode Bar Plot
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
      
      # Room Count Bar Plot
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
      
      # Population Line Plot
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
          theme_minimal()
      })
      
      # Sía gögn fyrir hlutföll eigna
      filtered_pie_data <- property_type_data %>%
        filter(sveitarfelag == selected_municipality, year == input$year) %>%
        group_by(property_type) %>%
        summarize(
          total_properties = sum(total_properties, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          percentage = total_properties / sum(total_properties) * 100
        )
      
      # Bar chart for property types
      output$property_type_pie <- renderPlot({
        ggplot(filtered_pie_data, aes(x = property_type, y = percentage, fill = property_type)) +
          geom_bar(stat = "identity") +
          labs(
            title = paste("Hlutföll seldra eigna eftir tegund í", selected_municipality, "árið", input$year),
            x = "Tegund eigna",
            y = "Hlutfall (%)",
            fill = "Tegund"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5)
          ) +
          scale_y_continuous(labels = scales::percent_format(scale = 1))
      })
    }
  })
}

# Run Shiny app
shinyApp(ui = ui, server = server)