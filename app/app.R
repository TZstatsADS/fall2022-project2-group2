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
borough_list <- readRDS("../data/borough_list.Rda")
cuisine_list <- readRDS("../data/cuisine_list.Rda")

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
           )
  ),
  
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
  tabPanel("Comparison by Years",
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
           )
  ),
  
  # Tab 5: References
  tabPanel("Reference")
)

#========================================Shiny Server=================================================
# Define server logic required to draw a histogram
server <- function(input, output) {
  #import pre-processed data
  violations <- readRDS("../data/violations.Rda")
  score_map <- readRDS("../data/score_map.Rda")
  df <- readRDS("../data/df.Rda")
  critical_cluster_map <- readRDS("../lib/critical_cluster.Rda")
  total_cluster_map <- readRDS("../lib/total_cluster.Rda")
  cluster_map <- list(critical_cluster_map,total_cluster_map)
  names(cluster_map) <- c("Number of Total Violations", "Number of Crital Violations")
  
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
  nc_pal <- colorNumeric(palette ="YlOrBr", domain = violations[[1]][[4]]@data$Total, na.color = 'transparent')
  output$map <- renderLeaflet({
    cluster_map[[input$type]]
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)