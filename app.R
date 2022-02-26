library(tidyverse)
library(ggplot2)
library(ggmap)
library(stringr)
library(jpeg)
library(grid)
library(shiny)
library(rsconnect)
library(data.table)

rawdat <- read.csv("C:/R Studio_and_Git_and_Github/R shiny app for Philadelphia Crime Data Analysis/Philadelphia-Crime-Data-Analysis-using-R-Shiny-App/crime2.csv", 
                   stringsAsFactors = FALSE, 
                   header = TRUE, 
                   na.strings = c("", " ", "NA"))

newdat <- rawdat


img <- readJPEG("C:/R Studio_and_Git_and_Github/R shiny app for Philadelphia Crime Data Analysis/mapimage.jpg")


newdat$Crime <- as.factor(newdat$Crime)

orig_locale <- Sys.getlocale("LC_TIME") 
Sys.setlocale("LC_TIME", "C")

crimes <- as.data.frame(table(newdat$Crime))

crimes <-
  crimes %>% 
  arrange(-Freq)

colnames(crimes) <- c("Crime", "Freq")

ui <- fluidPage(
  
  titlePanel("Philadelphia Crime Reports"),
  
  sidebarPanel(
    
    selectInput("crime", "Select Crime:", levels(newdat$Crime)),
    sliderInput("year", "Year:", min = 2006, max = 2016, value = c(2006, 2016), sep = "", ticks = FALSE),
    selectInput("season", "Season (Months):", c("All" = "all",
                                                "Spring (Mar, Apr, May)" = "Spring",
                                                "Summer (Jun, Jul, Aug)" = "Summer", 
                                                "Autumn (Sep, Oct, Nov)" = "Autumn", 
                                                "Winter (Dec, Jan, Feb)" = "Winter")),
    selectInput("dayofweek", "Day of Week:", c("All" = "all",
                                               "Weekdays" = "Weekday", 
                                               "Weekends" = "Weekend")),
    sliderInput("timestart", "From:", min = 0, max = 24, value = 0),
    sliderInput("timeend", "To:", min = 0, max = 24, value = 24),
    tags$hr(),
    tableOutput("table")
    
  ),
  
  mainPanel(
    
    plotOutput("graph", width = "700px", height = "700px")
    
  )
)

server <- function(input, output) {
  
  output$table <- renderTable(crimes)
  
  temp <- reactive({
    
    crime <- input$crime
    minyear <- input$year[1]
    maxyear <- input$year[2]
    season <- input$season
    dayofweek <- input$dayofweek
    timestart <- input$timestart
    timeend <- input$timeend
    
    #Apply filters
    
    test <- 
      newdat %>% 
      filter(Crime == crime,
             Year >= minyear,
             Year <= maxyear)
    
    test <- 
      
      if(timeend >= timestart){
        
        test %>% 
          filter(Hour >= input$timestart & Hour <= input$timeend)
        
      } else {
        
        test %>% 
          filter(Hour >= input$timestart | Hour <= input$timeend)
        
      }
    
    test <-
      
      if(season != "all") {
        
        test %>%
          filter(Season == season)
        
      } else {
        test
      }
    
    test <-
      
      if (dayofweek != "all") {
        
        test %>%
          filter(Weekendornot == dayofweek)
        
      } else {
        test
      }
    
    test <- as.data.frame(test)
    
    test
    
  })
  
  output$graph <- renderPlot({
    
    temp2 <- temp()
    
    temp2 %>% 
      ggplot() +
      aes(x = Lon, y = Lat) +
      xlim(-75.3, -74.95) +
      ylim(39.85, 40.15) +
      annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                        -Inf, Inf, -Inf, Inf) +
      geom_point(size = 3, alpha = 0.6, color = "firebrick1" )
    
  })
}

shinyApp(ui = ui, server = server)
