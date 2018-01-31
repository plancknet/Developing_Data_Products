#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

URL1      <- "https://raw.github.com/plancknet/Developing_Data_Products/master/DistRoyal.csv"
destfile_DistRoyal <- "DistRoyal.csv"

URL2      <- "https://raw.github.com/plancknet/Developing_Data_Products/master/Municipios_Brasileiros.csv"
destfile_Municipios <- "Municipios_Brasileiros.csv"

if(!file.exists(destfile_DistRoyal)){
  download.file(URL1, destfile_DistRoyal)
}


if(!file.exists(destfile_Municipios)){
  download.file(URL2, destfile_Municipios)
}

distRoyal <- read.csv("DistRoyal.csv")
LatLong   <- read.csv("Municipios_Brasileiros.csv", sep = ";", fileEncoding = "UTF-8")

#Loading data
distRoyal <-  read.csv("DistRoyal.csv")
LatLong   <- read.csv("Municipios_Brasileiros.csv", sep = ";", fileEncoding = "UTF-8")


#Cleaning data
distRoyal$NOM_MUNICIPIO      <- iconv(distRoyal$NOM_MUNICIPIO, to = "ASCII//TRANSLIT")
LatLong$NOM_MUNICIPIO <- iconv(LatLong$Nome.do.Municipio , to = "ASCII//TRANSLIT")

distRoyal$SIG_ESTADO         <- trimws(distRoyal$SIG_ESTADO)
distRoyal$NOM_MUNICIPIO      <- trimws(distRoyal$NOM_MUNICIPIO)
distRoyal$LOCALIDADE         <- paste(distRoyal$SIG_ESTADO, distRoyal$NOM_MUNICIPIO)
distRoyal$VAL_DISTRIBUIDO    <- gsub(",", "", distRoyal$VAL_DISTRIBUIDO)
distRoyal$VAL_DISTRIBUIDO    <- as.double(as.character(distRoyal$VAL_DISTRIBUIDO))

LatLong$NOM_MUNICIPIO <- trimws(LatLong$NOM_MUNICIPIO)
LatLong$UF            <- trimws(LatLong$UF)
LatLong$LOCALIDADE    <- paste(LatLong$UF, LatLong$NOM_MUNICIPIO)


# Merging Royaltie distribuition with Coordinate dataset
total <- merge(distRoyal, LatLong, by = c("LOCALIDADE"))

UFs   <- levels(as.factor(total$SIG_ESTADO))
ANOs  <- levels(as.factor(as.integer(sub(",", "", total$NUM_ANO))))

#Defining Palette Color
heatColors  <- heat.colors(7)
heatColors  <- heatColors[order(rev(heatColors))]
pal         <- colorQuantile(heatColors, domain = as.numeric(total$VAL_DISTRIBUIDO), n = 7)

#Formatting value into currency format
printCurrency <- function(value, currency.sym="R$", digits=2, sep=".", decimal=",") {
  paste(
    currency.sym,
    formatC(value, format = "f", big.mark = sep, digits=digits, decimal.mark=decimal),
    sep=""
  )
}



# Defining UI for application that draws a map
ui <- fluidPage(
  # Application title
  titlePanel("Oil Royalties Distribution - Brazilian Cities"),
  
  # Sidebar with a checkboxs input  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("uf", "Select States:", 
                         choiceNames = UFs, 
                         choiceValues = UFs, 
                         selected = NULL, inline = TRUE), 
      actionLink("selectall","Select All"), 
      h3(), h5("Documentation"), 
      h6("This application exibs the oil royalties distribution among brazilian cities between 2010 and 2017.
"),
      h6("Selecting the states, the map will focus in the region and will display circles that represents the location of the cities. If you desire see all data set, click in select all.
"),
      h6("The circle size represents the amount of money that city received in this year. Clicking on the circle you will see the details.
"), 
      h6("The map is interactive and you can apply zoom in and zoom out
"), 
      h6("Clicking on the play button will be showed the evolution between years.
"),
      h6("Selecting the Table tab the data will be showed in table format.")
      
      
    ), 
    
    # Show map and data table 
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map")), 
        tabPanel("Table", tableOutput('table'))
      ),
      sliderInput("ano", "Year", 
                  min = as.integer(min(ANOs)), 
                  max = as.integer(max(ANOs)), 
                  step = 1, 
                  value = 2010, 
                  animate = animationOptions(interval = 700, loop = TRUE,
                                             playButton = icon('play', "fa-3x"),
                                             pauseButton = icon('pause', "fa-3x")))
    )
  )
  
)

# Defining server logic required to draw a map and data table
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    df <- data.frame(lat    = total$Latitude, 
                     lng    = total$Longitude,
                     ano    = total$NUM_ANO,
                     cidade = total$NOM_MUNICIPIO.x, 
                     uf     = total$UF, 
                     valor  = total$VAL_DISTRIBUIDO)
    
    df2 <- subset(df, df$uf %in% input$uf)
    
    df2 <- subset(df2, as.integer(sub(",", "", df2$ano)) == as.integer(input$ano))
    
    df2 %>% leaflet() %>%
      addTiles() %>%
      addCircles(radius = (df2$valor)/10000,  
                 color = ~pal(as.integer(df2$valor)), 
                 popup = paste(df2$uf, " - ", df2$cidade, "<br>", printCurrency(df2$valor)))
    
    
  })
  
  output$table <- renderTable({
    df <- data.frame(lat = total$Latitude, 
                     lng = total$Longitude, 
                     ano = total$NUM_ANO,
                     cidade = total$NOM_MUNICIPIO.x, 
                     uf = total$UF, 
                     valor = printCurrency(total$VAL_DISTRIBUIDO))
    
    df2 <- subset(df[,3:6], df$uf %in% input$uf)
    
    df2 <- subset(df2, as.integer(sub(",", "", df2$ano)) == as.integer(input$ano))
    
  })
  
  observe({
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"uf", "Select States:", 
                               choiceNames = UFs, 
                               choiceValues = UFs, inline = TRUE)
    }
    else
    {
      updateCheckboxGroupInput(session,"uf", "Select States:", 
                               choiceNames = UFs, 
                               choiceValues = UFs, 
                               selected = UFs, inline = TRUE)
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
