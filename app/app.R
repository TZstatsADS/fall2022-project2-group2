#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#######################################Install Related Packages#######################################
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)}

if (!require("broom")) {
  install.packages("broom")
  library(broom)}

if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)}

if (!require("geojsonio")) {
  install.packages("geojsonio")
  library(geojsonio)}

if (!require("ggnewscale")) {
  install.packages("ggnewscale")
  library(ggnewscale)}

if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)}

if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)}

if (!require("osmdata")) {
  install.packages("osmdata")
  library(osmdata)}

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)}

if (!require("RSocrata")) {
  install.packages("RSocrata")
  library(RSocrata)}

if (!require("rstudioapi")) {
  install.packages("rstudioapi")
  library(rstuioapi)}

if (!require("sf")) {
  install.packages("sf")
  library(sf)}

if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)}

if (!require("wesanderson")) {
  install.packages("wesanderson")
  library(wesanderson)}
if (!require("bslib")) {
  install.packages("bslib")
  library(bslib)}

if (!require("leaflet.extras")) {
  install.packages("leaflet.extras")
  library(leaflet.extras)}

if (!require("htmltools")) {
  install.packages("htmltools")
  library(htmltools)}


# Set working directory
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
getwd()

#data preparation
borough_list <- readRDS("../output/borough_list.Rda")
cuisine_list <- readRDS("../output/cuisine_list.Rda")

#==============================================Shiny UI=================================================
# Define UI for application that display food inspections result visualization
ui <- navbarPage(
  theme = bs_theme(bootswatch = "litera"), 
  "Restaurant Inspection",
  # Tab 1: Introduction
  tabPanel("Introduction",
           tags$img(
             src = "https://cdn.vox-cdn.com/thumbor/ZfB51FGxZnbt6YrmvVJqgXnSaRI=/0x0:4200x2800/1820x1213/filters:focal(1764x1064:2436x1736):format(webp)/cdn.vox-cdn.com/uploads/chorus_image/image/67220269/shutterstock_406742713.0.jpg",
             width = "100%",
             style = "opacity: 0.90"
           ),
           fluidRow(
             absolutePanel(
               top = "40%",
               left = "auto",
               right="auto",
               height = 150,
               width = 650,
               fixed = T,
               tags$p(
                 style = "padding: 5%; background-color: white; font-family: alegreya; font-size: 100%",
                 "This app uses the New York City Restaurant Inspection dataset, provided by the Department of Health and Mental Hygiene, to examine the most recent inspection conducted for restaurants and college cafeterias in NYC from 2019 to 2022. We provide visualizations of violation trend throughout the years, interactive maps of number of violations as well as inspection score, and comparison of number of violations between years to show the effect of COVID-19 on restaurant inspection activities in NYC."
               )
  ))),
  
  # Tab 2: Violation Trend
  tabPanel("Violation Trend",
           fluidRow(
             column(3,
                    selectInput("borough", "Borough", borough_list[!borough_list %in% c("0")], selected = "Overall"),
                    selectInput("cuisine", "Cuisine", cuisine_list, selected = "Overall")
             ),
             column(9, 
                    plotOutput("plot_action"),
                    plotOutput("plot_critical_level"),
                    plotOutput("plot_top_5_violation")
             )
           )
  ),
  
  # Tab 3: Violation Map
  navbarMenu("Violation Map",
             # Subtab 3.1: Number of Violations
             tabPanel("Number of Violations",
                      fluidRow(
                        column(3,
                               selectInput("type", "Type of Violations",c("Number of Total Violations", "Number of Crital Violations"))
                        ),
                        column(9,
                               leafletOutput("map", height = 600)
                        )
                      )
             ),
             
             # Subtab 3.2:Inspection Score
             tabPanel("Inspection Score",
                      fluidRow(
                        column(3,
                               selectInput("score_year","Year:", c("2019", "2020", "2021", "2022"))),
                        column(9,
                               leafletOutput("score_map", height=600))
                      )
             )
  ),
  
  # Tab 4: Comparison by Years
 navbarMenu("Comparison by Years",
           tabPanel("Number of Violations",
                    fluidRow(
             column(4,
                    selectInput("type_comp", "Type of Violations", c("Number of Total Violations", "Number of Crital Violations"))
             ),
             column(4,
                    selectInput("time1", "Year", c("2019", "2020", "2021", "2022"))
             ),
             column(4,
                    selectInput("time2", "Year", c("2019", "2020", "2021", "2022"), selected = "2020")
             )
           ),
           
           fluidRow(
             column(6,
                    leafletOutput("map_comp1", height = 600)
             ),
             column(6,
                    leafletOutput("map_comp2", height = 600)
             )
           )),
           tabPanel("Mean Scores",
                    fluidRow(
                      
                      column(6,
                             selectInput("t1", "Year", c("2019", "2020", "2021", "2022"))
                      ),
                      column(6,
                             selectInput("t2", "Year", c("2019", "2020", "2021", "2022"), selected = "2020")
                      )
                    ),
                    
                    fluidRow(
                      column(6,
                             leafletOutput("score_comp1", height = 600)
                      ),
                      column(6,
                             leafletOutput("score_comp2", height = 600)
                      )
                    ))
  ),
  
  # Tab 5: References
  tabPanel("Reference")
)

#========================================Shiny Server=================================================
# Define server logic required to draw a histogram
server <- function(input, output) {
  #import pre-processed data
  violations <- readRDS("../output/violations.Rda")
  score_map <- readRDS("../output/score_map.Rda")
  df <- readRDS("../output/df.Rda")
  American <- readRDS("../output/American.Rda")
  Chinese <- readRDS("../output/Chinese.Rda")
  Coffee <- readRDS("../output/Coffee.Rda")
  Italian <- readRDS("../output/Italian.Rda")
  Mexican <- readRDS("../output/Mexican.Rda")
  Pizza <- readRDS("../output/Pizza.Rda")
  critical_2022 <- readRDS("../output/critical_2022.Rda")
  critical_2021 <- readRDS("../output/critical_2021.Rda")
  critical_2020 <- readRDS("../output/critical_2020.Rda")
  critical_2019 <- readRDS("../output/critical_2019.Rda")
  total_2022 <- readRDS("../output/total_2022.Rda")
  total_2021 <- readRDS("../output/total_2021.Rda")
  total_2020 <- readRDS("../output/total_2020.Rda")
  total_2019 <- readRDS("../output/total_2019.Rda")
  comparison <- readRDS("../output/comparison.Rda")

  # Filtered plots
  output$plot_action <- renderPlot({
    if (input$borough == 'Overall' & input$cuisine == 'Overall') {
      df_action <- df
    } else if (input$borough != 'Overall' & input$cuisine == 'Overall') {
      df_action <- df %>%
        filter(boro == input$borough)
    } else if (input$borough == 'Overall' & input$cuisine != 'Overall') {
      df_action <- df %>%
        filter(cuisine_description == input$cuisine)
    } else {
      df_action <- df %>%
        filter(boro == input$borough,
               cuisine_description == input$cuisine)
    }
    
    df_action %>%
      group_by(year, action) %>%
      summarize(num_violations = n()) %>%
      ggplot(aes(x = year, y = num_violations, fill = action)) +
      geom_bar(stat = "identity") +
      labs(title = "Number of Actions over Time", x = "Year", y = "Count", fill = "Action") +
      scale_fill_manual(values = wes_palettes$Moonrise3[c(3, 4, 5, 1, 2)]) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(nrow = 5, byrow = TRUE))
  })
  
  output$plot_critical_level <- renderPlot({
    if (input$borough == 'Overall' & input$cuisine == 'Overall') {
      df_critical <- df
    } else if (input$borough != 'Overall' & input$cuisine == 'Overall') {
      df_critical <- df %>%
        filter(boro == input$borough)
    } else if (input$borough == 'Overall' & input$cuisine != 'Overall') {
      df_critical <- df %>%
        filter(cuisine_description == input$cuisine)
    } else {
      df_critical <- df %>%
        filter(boro == input$borough,
               cuisine_description == input$cuisine)
    }
    
    df_critical %>%
      filter(critical_flag != "Not Applicable") %>%
      group_by(year, critical_flag) %>%
      summarize(num_violations = n()) %>%
      ggplot(aes(x = year, y = num_violations, fill = critical_flag)) +
      geom_bar(stat = "identity") +
      labs(title = "Number of Violations by Critical Level over Time", x = "Year", y = "Count", fill = "Critical Level") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$plot_top_5_violation <- renderPlot({
    if (input$borough == 'Overall' & input$cuisine == 'Overall') {
      df_top_5 <- df
    } else if (input$borough != 'Overall' & input$cuisine == 'Overall') {
      df_top_5 <- df %>%
        filter(boro == input$borough)
    } else if (input$borough == 'Overall' & input$cuisine != 'Overall') {
      df_top_5 <- df %>%
        filter(cuisine_description == input$cuisine)
    } else {
      df_top_5 <- df %>%
        filter(boro == input$borough,
               cuisine_description == input$cuisine)
    }
    
    df_top_5 %>%
      group_by(year, violation_code) %>%
      summarize(num_violations = n()) %>%
      ungroup() %>%
      group_by(year) %>%
      slice_max(num_violations, n = 5) %>%
      ggplot(aes(x = year, y = num_violations, fill = violation_code)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 5 Violations over Time", x = "Year", y = "Count", fill = "Violation Code") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  
  
  ########### Interactive map ###############
  #violation map
  nc_pal= colorNumeric(palette="YlOrBr", domain= total_2022@data$Total, na.color = 'transparent')
  output$map <- renderLeaflet({
    ##### colors
    selectedData <- reactive({
      if(input$type == "Number of Total Violations"){
        list(spdf_file_2022 = total_2022,
             spdf_file_2021 = total_2021,
             spdf_file_2020 = total_2020,
             spdf_file_2019 = total_2019)
      } else if(input$type == "Number of Crital Violations") {
        list(spdf_file_2022 = critical_2022,
             spdf_file_2021 = critical_2021,
             spdf_file_2020 = critical_2020,
             spdf_file_2019 = critical_2019)
      }
    })
    leaflet()%>%
      addProviderTiles("CartoDB")%>%
      setView(lng= -73.95223 , lat =40.78410	 , zoom = 10)%>%
      addSearchOSM()%>%
      #### First Layer of PolyGons
      addPolygons(
        data = selectedData()$spdf_file_2022 ,
        weight = 0.5,
        color = "black",
        stroke=TRUE ,
        opacity = 1 ,
        fillColor = ~nc_pal(Total),
        label = ~paste0 ('Number of Violations: ' , Total),
        group = '2022',
        fillOpacity = 0.7,   
        highlight = highlightOptions(weight  = 3, color = "red", bringToFront =  T)
      ) %>%
      
      #### Second Layer of PolyGons
      addPolygons(
        data = selectedData()$spdf_file_2021 ,
        weight = 0.5,
        color = "black",
        stroke=TRUE ,
        opacity = 1 ,
        fillColor = ~nc_pal(Total),
        label =~paste0 ('Number of Violations: ' , Total),
        group = '2021',
        fillOpacity = 0.7,
        highlight = highlightOptions(weight  = 3, color = "red", bringToFront =  T)
      ) %>%
      addLayersControl(overlayGroups = c("2022", "2021"))%>%
      
      #####Third layer
      addPolygons(
        data = selectedData()$spdf_file_2020 ,
        weight = 0.5,
        color = "black",
        stroke=TRUE ,
        opacity = 1 ,
        fillColor = ~nc_pal(Total),
        fillOpacity = 0.7,
        label =~paste0 ('Number of Violations: ' , Total),
        group = '2020',
        highlight = highlightOptions(weight  = 3, color = "red", bringToFront =  T)
      ) %>%
      
      ####Fourth Layer
      addPolygons(
        data = selectedData()$spdf_file_2019 ,
        weight = 0.5,
        color = "black",
        stroke=TRUE ,
        opacity = 1 ,
        fillColor = ~nc_pal(Total),
        label =~paste0 ('Number of Violations: ' , Total),
        group = '2019',
        fillOpacity = 0.7,
        highlight = highlightOptions(weight  = 3, color = "red", bringToFront =  T)
      ) %>%
      
      
      ##### Fifth layer of grades of restaurants
      
      
      
      addMarkers(data = American,lng = ~longitude, lat = ~latitude, 
                 
                 label = ~htmlEscape(dba),
                 group = 'American',
                 popup  = paste0("<b>",American$dba,"</b>", 
                                 "<br/>", 'Cuisine Type: ', American$cuisine_description, 
                                 "<br/>", 'Phone Number: ', American$phone,
                                 "<br/>", 'Grade: ', American$grade,
                                 "<br/>",'Latest Violation: ', American$violation_description),
                 clusterOptions = markerClusterOptions()
      ) %>% 
      addMarkers(data = Chinese,lng = ~longitude, lat = ~latitude, 
                 label = ~htmlEscape(dba),
                 group = 'Chinese',
                 popup  = paste0("<b>",Chinese$dba,"</b>", 
                                 "<br/>", 'Cuisine Type: ', Chinese$cuisine_description, 
                                 "<br/>", 'Phone Number: ', Chinese$phone,
                                 "<br/>", 'Grade: ', Chinese$grade,
                                 "<br/>",'Latest Violation: ', Chinese$violation_description ),
                 clusterOptions = markerClusterOptions()
      ) %>% 
      
      addMarkers(data = Pizza,lng = ~longitude, lat = ~latitude, 
                 
                 label = ~htmlEscape(dba),
                 group = 'Pizza',
                 popup  = paste0("<b>",Pizza$dba,"</b>", 
                                 "<br/>", 'Cuisine Type: ', Pizza$cuisine_description, 
                                 "<br/>", 'Phone Number: ', Pizza$phone,
                                 "<br/>", 'Grade: ', Pizza$grade,
                                 "<br/>",'Latest Violation: ', Pizza$violation_description),
                 clusterOptions = markerClusterOptions()
      ) %>% 
      
      
      addMarkers(data = Mexican,lng = ~longitude, lat = ~latitude, 
                 label = ~htmlEscape(dba),
                 group = 'Mexican',
                 popup  = paste0("<b>",Mexican$dba,"</b>", 
                                 "<br/>", 'Cuisine Type: ', Mexican$cuisine_description, 
                                 "<br/>", 'Phone Number: ', Mexican$phone,
                                 "<br/>", 'Grade: ', Mexican$grade,
                                 "<br/>",'Latest Violation: ', Mexican$violation_description ),
                 clusterOptions = markerClusterOptions()
      ) %>% 
      
      
      addMarkers(data = Italian,lng = ~longitude, lat = ~latitude, 
                 label = ~htmlEscape(dba),
                 group = 'Italian',
                 popup  = paste0("<b>",Italian$dba,"</b>", 
                                 "<br/>", 'Cuisine Type: ', Italian$cuisine_description, 
                                 "<br/>", 'Phone Number: ', Italian$phone,
                                 "<br/>", 'Grade: ', Italian$grade,
                                 "<br/>",'Latest Violation: ', Italian$violation_description ),
                 clusterOptions = markerClusterOptions()
      ) %>% 
      addMarkers(data = Coffee,lng = ~longitude, lat = ~latitude, 
                 label = ~htmlEscape(dba),
                 group = 'Coffee',
                 popup  = paste0("<b>",Coffee$dba,"</b>", 
                                 "<br/>", 'Cuisine Type: ', Coffee$cuisine_description, 
                                 "<br/>", 'Phone Number: ', Coffee$phone,
                                 "<br/>", 'Grade: ', Coffee$grade,
                                 "<br/>",'Latest Violation: ', Coffee$violation_description ),
                 clusterOptions = markerClusterOptions()
      ) %>% 
      addMarkers(data = Others,lng = ~longitude, lat = ~latitude, 
                 label = ~htmlEscape(dba),
                 group = 'Others',
                 popup  = paste0("<b>",Others$dba,"</b>", 
                                 "<br/>", 'Cuisine Type: ', Others$cuisine_description, 
                                 "<br/>", 'Phone Number: ', Others$phone,
                                 "<br/>", 'Grade: ', Others$grade,
                                 "<br/>",'Latest Violation: ', Others$violation_description ),
                 clusterOptions = markerClusterOptions()
      ) %>% 
      
      
      addLayersControl( baseGroups = c("2022", "2021","2020","2019"),overlayGroups = c("American", "Chinese","Coffee","Pizza", "Italian","Mexican", "Others"))%>%
      addLegend( pal=nc_pal, values= selectedData()$spdf_file_2022$Total, opacity=0.9, title = "Total Violation Counts", position = "bottomleft" )
    
  })
  
  # Interactive map compared by year  
  output$map_comp1 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB") %>%
      addPolygons(
        data = violations[[input$type_comp]][[input$time1]],
        weight = 0.5,
        color = "black",
        stroke = TRUE,
        opacity = 1,
        fillOpacity = 1,
        fillColor = ~nc_pal(Total),
        label = ~paste0 ('Total Violations : ', Total),
        group = '2022',
        highlight = highlightOptions(weight = 3, color = "red", bringToFront = TRUE)
      ) %>%
      addLegend(pal = nc_pal, values= violations[[input$type]][[input$time1]]$Total, opacity=0.9, title = "Count of Total Violation", position = "bottomleft" )
  })
  
  output$map_comp2 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB") %>%
      addPolygons(
        data = violations[[input$type_comp]][[input$time2]],
        weight = 0.5,
        color = "black",
        stroke = TRUE,
        opacity = 1,
        fillOpacity = 1,
        fillColor = ~nc_pal(Total),
        label = ~paste0 ('Total Violations : ', Total),
        group = '2022',
        highlight = highlightOptions(weight = 3, color = "red", bringToFront = TRUE)
      ) %>%
      addLegend(pal = nc_pal, values = violations[[input$type]][[input$time2]]$Total, opacity = 0.9, title = "Count of Total Violation", position = "bottomleft" )
  })
  
  # Interactive score map
  output$score_map <- renderLeaflet({
    nc_pal <- colorNumeric(palette ="Greens", domain = score_map[[3]]@data$mean_score, na.color = 'transparent')
    leaflet() %>%
      addProviderTiles("CartoDB") %>%
      addPolygons(
        data = score_map[[input$score_year]],
        weight = 0.5,
        color = "black",
        stroke = TRUE,
        fillOpacity = 1,
        fillColor = ~nc_pal(mean_score),
        label = ~paste0 ('Mean Inspection Score: ', mean_score),
        group = '2022',
        highlight = highlightOptions(weight = 3, color = "red", bringToFront = TRUE)
      ) %>%
      addLegend(pal = nc_pal, values = score_map[[input$score_year]]$mean_score, opacity = 0.9, title = "Mean Score", position = "bottomleft" )
  })
  # Interactive mean score map compared by year  
  output$score_comp1 <- renderLeaflet({
    nc_pal <- colorNumeric(palette ="Greens", domain = score_map[[3]]@data$mean_score, na.color = 'transparent')
    leaflet() %>%
      addProviderTiles("CartoDB") %>%
      addPolygons(
        data = score_map[[input$t1]],
        weight = 0.5,
        color = "black",
        stroke = TRUE,
        fillOpacity = 1,
        fillColor = ~nc_pal(mean_score),
        label = ~paste0 ('Mean Inspection Score: ', mean_score),
        group = '2022',
        highlight = highlightOptions(weight = 3, color = "red", bringToFront = TRUE)
      ) %>%
      addLegend(pal = nc_pal, values = score_map[[input$t1]]$mean_score, opacity = 0.9, title = "Mean Score", position = "bottomleft" )
  })
  
  output$score_comp2 <- renderLeaflet({
    nc_pal <- colorNumeric(palette ="Greens", domain = score_map[[3]]@data$mean_score, na.color = 'transparent')
    leaflet() %>%
      addProviderTiles("CartoDB") %>%
      addPolygons(
        data = score_map[[input$t2]],
        weight = 0.5,
        color = "black",
        stroke = TRUE,
        fillOpacity = 1,
        fillColor = ~nc_pal(mean_score),
        label = ~paste0 ('Mean Inspection Score: ', mean_score),
        group = '2022',
        highlight = highlightOptions(weight = 3, color = "red", bringToFront = TRUE)
      ) %>%
      addLegend(pal = nc_pal, values = score_map[[input$t2]]$mean_score, opacity = 0.9, title = "Mean Score", position = "bottomleft" )
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)