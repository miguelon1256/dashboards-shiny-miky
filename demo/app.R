##This file implements stratified PPS sampling from a list of locations provided to us by GIZ
# install.packages("tidyr")
# install.packages("shiny")
# install.packages("datasets")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("rsconnect")
# install.packages("shinydashboard")
# install.packages("sjlabelled")
# install.packages("httr")
# install.packages("haven")
# install.packages("rworldmap")
# install.packages("leaflet")
# install.packages("pps")

library(shiny)
library(datasets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rsconnect)
library(shinydashboard)
library(sjlabelled)
library(httr)
library(haven) 
# 

library(rworldmap)
##library(ggmap)
library(leaflet)
library(pps)

##This is to implement a sampling dashboard

##setwd("C:/Users/pjasper/Dropbox (Personal)/Groots/09 GIZ NGA Outputs/01 Sampling Locations/dashboard")


##14 Nov - with Sidebar

##sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Map", tabName="map", icon=icon("map"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName="map", column(leafletOutput("mymap",height="90vh"), width=12))
  )
)



ui <- dashboardPage(
  dashboardHeader(title = "NESP - sampling locations",titleWidth = 400),
  sidebar,
  body
)


##server
##

server <- function(input, output) { 
  ##load dta into R
  data <- read.csv("Jigawa_State_Grid_Connected_Settlements.csv")
  
  ##set seed for replicability
  set.seed(34171956)
  
  ##Step 2: Split into four strata by area size. 
  ## - I create four quartiles as strata.
  
  data <- mutate(data, qtile=ntile(area_km2,4))
  
  ##Step 2: draw sample
  ##I am using sampling proportional to size - in terms of population size
  data<-data[order(data$qtile),] 
  sample <- data[ppssstrat(data$pop_hrsl,data$qtile,c(5,5,5,5)),]
  
  ##Step 3: check that sizes are OK
  ##Note: data$qtile == xx <-- the xx needs to be changed to the stratum that you want to check 
  str4<-data[data$qtile==4,]
  sizesok(str4$pop_hrsl,5) # is it ok to select five units?
  
  ##Step 4: merge sample indicator back in
  sample$sample = 1
  data2 <-  left_join(data,sample,by="FID")
  data2 <- mutate(data2,sample = replace_na(sample,0))
  
  
  ##Step4: map
  dd <- reactive(data2)
  pal <- colorFactor(c("navy", "red"), domain = c(0,1))
  output$mymap <- renderLeaflet({
    leaflet(data=dd()) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addCircles(~xcoord.x, ~ycoord.x, 
                 radius = ~ifelse(sample==0, 10, 1000),
                 color=~pal(sample), label=paste0("LGA:",data2$admin2.x," ID:",data2$FID))
  })
  
}

shinyApp(ui, server)
##rsconnect::deployApp('C:/Users/pjasper/Dropbox (Personal)/Groots/09 GIZ NGA Outputs/01 Sampling Locations/dashboard')













