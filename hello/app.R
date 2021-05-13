
library(shiny)
library(httr)
library(jsonlite)
library(readr)

library(datasets)
library(dplyr)
library(ggplot2)
library(rsconnect)
library(shinydashboard)
library(sjlabelled)
library(httr)
library(haven)
library(leaflet)


##This is a first test dashboard for Shiny for our app in PA 2030




###-----------
##Building the dashboard

##sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName="dashboard", icon=icon("dashboard")),
        menuItem("Map", tabName="map", icon=icon("map"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(box(title="Principal fuente de agua para consumo humano en la escuela ",plotOutput("TestPlot"),width=8), 
                         infoBoxOutput("countbox")
                         ), 
                fluidRow(box(title="Principal fuente de agua de acuerdo a categor�as JMP ", plotOutput("Plot_mej"),width=6))
                ),
        tabItem(tabName="map", column(leafletOutput("mymap",height="90vh"), width=12))
        )
    )


ui <- dashboardPage(
    dashboardHeader(title = "PA2030 Pilot: Centros Educativos",titleWidth = 400),
    sidebar,
    body
)


##server
##

server <- function(input, output) { 
    ##load data
    
    
    ##This code downloads data from kf.nexion-dev.tk
    ##Note: there is the api for the kobo server - where you have the forms and such: https://kf.nexion-dev.tk. 
    ##And there is the old_API: https://kc.nexion-dev.tk
    
    ##Original source: https://github.com/nzfarhad/koboAPI/blob/master/R/koboAPI.R
    
    ##You can get info on forms from API but data only from old API, the IDs across those of forms are different. 
    ##so the process is: get forms first from new API - retreive new ID and old ID
    ##Match with old ID and then load the data. 
    
    
    ##Define url where our data is 
    api <- "https://kf.nexion-dev.tk"
    ##the old API is needed for the actual data. 
    old_api <- "https://kc.nexion-dev.tk"
    
    ##get form ID form website - this is the new form_id
    form_id <- "aG43m9VaJENFVng7zgeqJR"
    formid <- "aG43m9VaJENFVng7zgeqJR"
    
    url_form <- paste0(api, "/assets/", form_id, "/")
    kobo_token <- Sys.getenv('DASHBOARDS_KOBO_TOKEN')
    
    ##Full URL for the new API
    url_form <- paste0(api, "/assets/", form_id, "/")
    
    ##-------------------------------------------------------------------------------------------------------------------
    ##Get the forms - as in the questionnaires
    
    ##The below gets the list of forms from the old api
    url_old_form <- paste0(old_api, "/api/v1/data")
    download_forms_all_old <- GET(url_old_form, add_headers(Authorization = paste("Token ", kobo_token)), progress())
    download_forms_all_old <- content(download_forms_all_old, "text", encoding = "UTF-8")
    download_forms_all_old <- fromJSON(download_forms_all_old)
    download_forms_all_old <- as.data.frame(download_forms_all_old)%>%
        mutate(old_id = id)%>%
        select(old_id, id_string)
    
    ###
    ##this gets the old ID from old API
    old_id <- as.character(download_forms_all_old%>%filter(id_string == form_id)%>%select(old_id))
    
    ##using the old ID get data from old API
    url_data <- paste0(old_api, "/api/v1/data/",old_id, ".csv")
    
    ##download data
    raw_data <- GET(url_data, add_headers(Authorization = paste("Token ", kobo_token)), progress())
    
    print(raw_data)
    
    ##bit of data formatting
    raw_data <- content(raw_data, "raw", encoding = "UTF-8")
    raw_data <- read_csv(raw_data, na = c("", "NA", "n/a"))

    
    raw_data <- as.data.frame(raw_data)
    
    ##fuente
    raw_data$fuente <- raw_data$`Agua_saneamiento_y_lavado_de_/_Cu_l_es_la_principal_fuente_d`
    
    ##latitude
    raw_data$latitude <- raw_data$`Informaci_n_general/_Le_pido_por_favor_indicar_el_l_latitude`
    
    ##longitude
    raw_data$longitude <- raw_data$`Informaci_n_general/_Le_pido_por_favor_indicar_el_l_longitude`
    
    ##Some data cleaning. 
    raw_data <- raw_data %>% mutate(fuente=recode(fuente,
                                                  "caneria_dentro" = "Ca�er�a de red dentro del inmueble", 
                                                  "caneria_fuera" = "Ca�er�a de red fuera del inmueble", 
                                                  "pozo_perforado" = "Pozo perforado o entubado, con bomba", 
                                                  "pozo_cubierto" = "Pozo excavado cubierto", 
                                                  "pozo_no_cubierto" = "Pozo excavado no cubierto", 
                                                  "manantial_protegido" = "Manantial o vertiente protegida", 
                                                  "manantial_no_protegido" = "Manantial o vertiente no protegida", 
                                                  "lluvia" = "Agua de lluvia", 
                                                  "carro"="Carro repartior (aguatero)", 
                                                  "botella"="Agua embotellada", 
                                                  "superficie"="Agua de superficie", 
                                                  "sin_fuente"="Sin fuente de agua"))
    
    ##fuente mejorada
    raw_data <- raw_data %>% mutate(f_mejorada = recode (fuente,"Ca�er�a de red dentro del inmueble"="Mejorada",
                                                         "Ca�er�a de red fuera del inmueble"="Mejorada", 
                                                         "Pozo perforado o entubado, con bomba"="Mejorada", 
                                                         "Pozo excavado cubierto"="Mejorada", 
                                                         "Manantial o vertiente protegida"="Mejorada", 
                                                         "Carro repartior (aguatero)"="Mejorada", 
                                                         "Agua embotellada"="Mejorada", 
                                                         "Agua de lluvia"="Mejorada",
                                                         "Pozo excavado no cubierto" = "No Mejorada",
                                                         "Manantial o vertiente no protegida" = "No Mejorada",
                                                         "Agua de superficie" = "No Mejorada", 
                                                         "Sin fuente de agua" = "Sin fuente"))
    
    
    ##----------------------
    ##count total entries
    total <- raw_data %>%
        summarise(n=n())
    
 ###graphs and analysis
    
    
    ##box countbox
    data3 <- reactive(total[1,1])
    output$countbox <- renderInfoBox({
        infoBox("N�mero total de entrevistas aprovadas",data3(),icon=icon("list"),color="purple")
    })
    
    
    ##first static graph
    data_r <- reactive(raw_data)
    output$TestPlot <- renderPlot({
        ggplot(data_r()) + 
            geom_bar(aes(x=fuente,fill=fuente)) + 
            labs(x = "Fuente principal de agua", y="Numero de respuestas") +
            theme(axis.text.x = element_blank())  + 
            theme(legend.title = element_blank())
    })
    
    ##second graph
    output$Plot_mej <- renderPlot({
        ggplot(data_r()) + 
            geom_bar(aes(x=f_mejorada,fill=f_mejorada)) + 
            labs(x = "Fuente principal de agua", y="Numero de respuestas") +
            theme(axis.text.x = element_blank()) + 
            theme(legend.title = element_blank())
    })
    
    
    #map
    output$mymap <- renderLeaflet({
        leaflet(data=data_r()) %>%
            addTiles %>%  # Add default OpenStreetMap map tiles
            addMarkers(~longitude, ~latitude, clusterOptions = markerClusterOptions())
    })
    
    
}

shinyApp(ui, server)









