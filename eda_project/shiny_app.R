library(shiny); library(dplyr); library(ggplot2); library("rgdal"); library("sp") ; library(tmap); library(leaflet)      

df <- read.csv("dc_residential_data_clean.csv")
df$SALEDATE <- as.Date(df$SALEDATE)

ui <- fluidPage(
  
#sliderInput(inputId = "num", label = "Choose a number", value = 25, min = 1 , max = 100),
dateRangeInput(inputId = "dates", label ="Choose a range of sale dates",
            start = "2018-01-01", end = "2018-07-01",   min = "2015-01-01", max = "2019-01-01" ),

sliderInput("price", label = h3("Sale Price Range"), min = 0, 
            max = 50000000, value = c(1000000, 5000000)),
leafletOutput("map"), p()

)
server <- function(input, output) {


#data <- reactive({df[df$SALEDATE > input$dates[1] & df$SALEDATE < input$dates[2],]})

  data <-reactive({ df[df$SALEDATE > input$dates[1] & df$SALEDATE < input$dates[2] &
                      df$PRICE > input$price[1] & df$PRICE < df$PRICE[2]
                       
                       
                       ,]})
  
output$summary <- renderPrint({

  sum <- data()
  summary(sum[,c("BATHRM","ROOMS")])})
   
   
output$map <- renderLeaflet({
map <- data() 


  leaflet() %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap) %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng=map$LONGITUDE, lat=map$LATITUDE, popup=map$content,
               options = popupOptions(closeButton = FALSE))
 




})  
  
}
shinyApp(ui = ui, server = server )


