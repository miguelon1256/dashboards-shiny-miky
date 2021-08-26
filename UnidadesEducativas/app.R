library(shiny)
library("httr")
library("jsonlite")
library("ggplot2")
library("readr")
library(plotly)
library(plyr)
library("dplyr")

library(rsconnect)
library(shinydashboard)
library(sjlabelled)
library(httr)
library(haven)
library(leaflet)
library(datasets)
library(ggplot2)




##
##
## 1. Dashboard actualizado para **unidades educativas**  -- test
##
##  Fecha: 24.08.2021
##
##  - v2 - correción lavado de manos


###------------------------------------------------------------------------------------------------------------------------------------
##Building the dashboard

##Sidebar
##Sidebar para los diferentes niveles de analisis - Agua, Saneamiento, Higiene
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Agua", tabName="agua", icon=icon("tint")),
    menuItem("Saneamiento", tabName="san",icon=icon("user")),
    menuItem("Higiene", tabName="hig",icon=icon("hand-paper-o")),
    menuItem("Mapa", tabName="map", icon=icon("map"))
  )
)

body <- dashboardBody(
  tabItems(
    ##Tab agua
    tabItem(tabName = "agua",
            fluidRow(
                    box(title="Servicios de agua en la unidad educativa",plotOutput("plot_agua_1"),width=8),
                    infoBoxOutput("countbox"), 
                    box(title="Definiciones de escaleras de servicio del JMP", 
                        "Servicio básico: en el momento de la encuesta, la escuela dispone de agua para consumo procedente de una fuente mejorada.",br(), 
                        "Servicio limitado: la escuela dispone de una fuente de agua mejorada, pero en el momento de la encuesta no hay agua.", br(), 
                        "Sin servicio: la escuela no dispone de una fuente de agua o dispone de agua para consumo de una fuente no mejorada.", width=4)
                        ),
            fluidRow(
                    box(title="Servicios en Villa Serrano", plotOutput("plot_agua_VS"),width=6),
                    box(title="Servicios en Padilla", plotOutput("plot_agua_P"),width=6) 
                    )
            ),
    tabItem(tabName = "san",
             fluidRow(box(title="Servicios de en la unidad educativa",plotOutput("plot_san_1"),width=8),
                      box(title="Definiciones de escaleras de servicio del JMP", 
                          "Servicio básico: en el momento de la encuesta, la escuela dispone de instalaciones de saneamiento mejoradas, separadas por sexo y utilizables (disponibles, en funcionamiento y privadas.",br(), 
                          "Servicio limitado: en el momento de la encuesta, la escuela dispone de instalaciones de saneamientos mejoradas, pero no están separadas por sexo o no son utilizables.", br(), 
                          "Sin servicio: la escuela no dispone de instalaciones de saneamiento o dispone de instalaciones de saneamiento no mejoradas.", width=4)
                        ),
             fluidRow(
                      box(title="Servicios en Villa Serrano", plotOutput("plot_san_VS"),width=6),
                      box(title="Servicios en Padilla", plotOutput("plot_san_P"),width=6)
                      )
            ),
    tabItem(tabName = "hig",
            fluidRow(box(title="Servicios de lavado de manos en la unidad educativa",plotOutput("plot_hig_1"),width=8),
                     box(title="Definiciones de escaleras de servicio del JMP", 
                         "Servicio básico: en el momento de la encuesta, la escuela dispone de instalaciones para el lavado de manos con agua y jabón.",br(), 
                         "Servicio limitado: la escuela cuenta con instalaciones para el lavado de manos que, en el momento de la encuesta, disponen de agua, pero no de jabón.", br(), 
                         "Sin servicio: a escuela no dispone de agua o de instalaciones para el lavado de manos.", width=4)
                         ),
            fluidRow(box(title="Servicios en Villa Serrano", plotOutput("plot_hig_VS"),width=6),
                     box(title="Servicios en Padilla", plotOutput("plot_hig_P"),width=6)
                     )
            ),
    tabItem(tabName="map", column(leafletOutput("mymap",height="90vh"), width=12))
  )
)


ui <- dashboardPage(
  dashboardHeader(title = "PA2030: Unidades Educativas",titleWidth = 400),
  sidebar,
  body
)

##--------------------------------------------------------------------------------------------------------------------------------------




  
  ##Steps: 
  ## 1. Download data
  ## 2. Data management 
  ## 3. Clean and indicator creation
  ## 4. Analysis
  
  
##1. Download the data-----------------------------------------------------------------------------------------------
  
  ##Define url where our data is
  api <- "https://kf.nexion-dev.tk"
  ##the old API is needed for the actual data.
  old_api <- "https://kc.nexion-dev.tk"
  
  ##get form ID form website - this is the new form_id
  ##data from unidades educativas
  form_id <- "aPWG6ZEXC2sosk2Ke6qECD"
  formid  <- "aPWG6ZEXC2sosk2Ke6qECD"
  
  url_form <- paste0(api, "/assets/", form_id, "/")
  kobo_token <- Sys.getenv('DASHBOARDS_KOBO_TOKEN')
  
  ##Full URL for the new API
  url_form <- paste0(api, "/assets/", form_id, "/")
  
  ##-------------------------------------------------------------------------------------------------------------------
  ##Get the forms - as in the questionnaires
  raw_form <- GET(url_form, add_headers(Authorization = paste("Token ", kobo_token)), progress())
  
  ##check code
  print(paste0("Status Code: ",raw_form$status_code))
  
  ##Format forms
  raw_form_text <- content(raw_form, "text", encoding = "UTF-8")
  raw_form_text_json <- fromJSON(raw_form_text)
  languages <- as.vector(raw_form_text_json$content$translations)
  languages_labels <- paste0("label::", languages)
  
  ##The below gets the list of forms from the old api
  url_old_form <- paste0(old_api, "/api/v1/data")
  download_forms_all_old <- GET(url_old_form, add_headers(Authorization = paste("Token ", kobo_token)), progress())
  download_forms_all_old <- content(download_forms_all_old, "text", encoding = "UTF-8")
  download_forms_all_old <- fromJSON(download_forms_all_old)
  download_forms_all_old <- as.data.frame(download_forms_all_old)%>%
    mutate(old_id = id)%>%
    select(old_id, id_string)
  
  ##Looked at download_forms_all_old manually and check the right form
  ##using the old ID get data from old API
  url_data <- paste0(old_api, "/api/v1/data/",26, ".csv")
  
  raw_data <- GET(url_data, add_headers(Authorization = paste("Token ", kobo_token)), progress())
  
  ##bit of data formatting
  raw_data <- content(raw_data, "raw", encoding = "UTF-8")
  raw_data <- read_csv(raw_data, na = c("", "NA", "n/a"))
  
##2. Data management ---------------------------------------------------------------------------------------------------------------
  
  
  ##1. Keep only approved data
  
  raw_data <- tidyr::separate(raw_data,`_validation_status`,c("whom","timestamp","uid","color","label"),",")
  raw_data <- tidyr::separate(raw_data,label,c("label2","status2"), ":")
  raw_data <- dplyr::filter(raw_data,status2==" u'Approved'}")
  
  ##2. Rename variables
  raw_data <- rename_with(raw_data,~gsub("Introducci_n/","",.x,fixed=TRUE))
  raw_data <- rename_with(raw_data,~gsub("Informaci_n_general/","",.x,fixed=TRUE))
  raw_data <- rename_with(raw_data,~gsub("Agua_saneamiento_y_lavado_de_/","",.x,fixed=TRUE))
  
  ##latitude
  raw_data$latitude <- raw_data$`_gps_latitude`
  ##longitude
  raw_data$longitude <- raw_data$`_gps_longitude`

   
##3. Cleaning and indicator creation -----------------------------------------------------------------------------------------------


  ##3.1 Agua

  ## Fuente mejorada
  
  ##Cambiar nombre

  raw_data <- raw_data %>% mutate(fuente=recode(fuente, "caneria_dentro" = "Cañería de red dentro del inmueble",
                                                "caneria_fuera" = "Cañería de red fuera del inmueble",
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
  raw_data <- raw_data %>% mutate(f_mejorada = recode(fuente,"Cañería de red dentro del inmueble"="Mejorada",
                                                       "Cañería de red fuera del inmueble"="Mejorada",
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


  ##Indicador principal:
  ##Servicio basico: en el momento de la encuesta, la escuela dispone de agua para consumo procedente de una fuente mejorada
  ##Servicio limitado: la escuela dispone de una fuente de agua mejorada, pero en el momento de la encuesta no hay agua
  ##Sin servicio:  la escuela no dispone de una fuente de agua o dispone de agua para consumo de una fuente no mejorada

  raw_data <- raw_data %>%
              mutate(ind_basico=case_when(f_mejorada == "Mejorada" & dispo_fuente == "si"  ~ "si",))
  raw_data$ind_basico[is.na(raw_data$ind_basico)] <- "no"

  raw_data <- raw_data %>%
    mutate(servicio_agua=case_when(f_mejorada == "Mejorada" & dispo_fuente == "si"  ~ "Servicio básico",
                          f_mejorada == "Mejorada" ~ "Servicio limitado"))
  raw_data$servicio_agua[is.na(raw_data$servicio_agua)] <- "Sin servicio"

  
  #3.2 Saneamiento

  ##Instalación mejorada vs no mejorada
  
  ##Mejorada:
  ## - inodoro con desagua al alcantrillado o camara septica
  ## - letrina con piso, compostaje
  
  raw_data <- raw_data %>%
            mutate(i_mejorado=case_when((letrina == "inodoro" & (desague=="alcantarillado" | desague=="septica")) |
                                          letrina == "letrina_piso" | letrina == "compostaje" ~ "Mejorada"))
  raw_data$i_mejorado[is.na(raw_data$i_mejorado)] <- "No Mejorada"
  
  ##Indicador principal
  ## Servicio basico: en el momento de la encuesta, la escuela dispone de instalaciones de saneamiento mejoradas, separadas por sexo y utilizables (disponibles, en funcionamiento y privadas)
  ## Servicio limitado:  en el momento de la encuesta, la escuela dispone de instalaciones de saneamientos mejoradas, pero no están separadas por sexo o no son utilizables 
  ## Sin servicio:  la escuela no dispone de instalaciones de saneamiento o dispone de instalaciones de saneamiento no mejoradas

  raw_data <- raw_data %>%
    mutate(servicio_saneamiento = case_when(i_mejorado == "Mejorada" & letrina_sexo == "si" & letrina_utilizable >= 1  ~ "Servicio básico",
                                            i_mejorado == "Mejorada" ~ "Servicio limitado"))
  raw_data$servicio_saneamiento[is.na(raw_data$servicio_saneamiento)] <- "Sin servicio"

  #3.3 Lavado de manos

  ##Indicador principal
  ## Servicio Basico: en el momento de la encuesta, la escuela dispone de instalaciones para el lavado de manos con agua y jabón
  ## Servicio Limitado: la escuela cuenta con instalaciones para el lavado de manos que, en el momento de la encuesta, disponen de agua, pero no de jabón
  ## Sin Servicio: a escuela no dispone de agua o de instalaciones para el lavado de manos 

  raw_data <- raw_data %>%
    mutate(servicio_lavado = case_when(instalacion_lavado == "si" & instalacion_jabon == "si" & instalacion_agua == "si" ~ "Servicio básico",
                                       (instalacion_lavado == "si" & instalacion_agua == "si" ~ "Servicio limitado")))
  raw_data$servicio_lavado[is.na(raw_data$servicio_lavado)] <- "Sin servicio"
  

##----------------------
  ##count total entries
  total <- raw_data %>%
    summarise(n=n())

  
  

##4. Graphs and analysis -----------------------------------------------------------------------------------------------


# General graph parameters
  
  pct_format = scales::percent_format(accuracy = .1)
  
  gtext <- geom_text(aes(label = sprintf(
    '%d (%s)',
    ..count..,
    pct_format(..count.. / sum(..count..))
  )
  ),
  stat = 'count',
  nudge_y = 1,
  colour = 'black',
  size = 5
  )

server <- function(input, output) {
    
  ### Graphs and boxes
  
  ##box countbox
  data1 <- reactive(total[1,1])
  output$countbox <- renderInfoBox({
    infoBox("Número total de respuestas aprobadas",data1(),icon=icon("list"),color="purple")
  })
  
  ##Infoboxes
  ##Agua
  def_agua_txt <- ""
  output$def_agua <- renderInfoBox({
    infoBox("Definiciones de la escalera del JMP",def_agua_txt,icon=icon("tint"),color = "blue")
  })
  
  ##reactive data
  data2 <- reactive(raw_data)


  ##Agua
  output$plot_agua_1 <- renderPlot({
    ggplot(data2(), aes(x=servicio_agua, fill=servicio_agua)) +
        geom_bar() +
        labs(title = "Servicio de agua en unidades educativas", x = "Escalera de servicio", y="Número de unidades",fill="Servicio de agua") +
        scale_fill_manual(values = c("Servicio básico" = "#6dc4ef", "Servicio limitado" = "#fff272", "Sin servicio" = "#fab301")) +
        gtext
    })
  

  output$plot_agua_VS <- renderPlot({
       ggplot(subset(data2(), municipio %in% c("serrano")), aes(x=servicio_agua, fill=servicio_agua)) +
        geom_bar() +
        labs(title = "Villa Serrano", x = "Escalera de servicio", y="Número de unidades",fill="Servicio de agua") +
        scale_fill_manual(values = c("Servicio básico" = "#6dc4ef", "Servicio limitado" = "#fff272", "Sin servicio" = "#fab301")) +
        gtext
   })
  
  output$plot_agua_P <- renderPlot({
    ggplot(subset(data2(), municipio %in% c("padilla")), aes(x=servicio_agua, fill=servicio_agua)) +
        geom_bar() +
        labs(title = "Padilla", x = "Escalera de servicio", y="Número de unidades",fill="Servicio de agua") +
        scale_fill_manual(values = c("Servicio básico" = "#6dc4ef", "Servicio limitado" = "#fff272", "Sin servicio" = "#fab301")) + 
        gtext
  })
  
  ##Saneamiento
  
  output$plot_san_1 <- renderPlot({
    ggplot(data2(), aes(x=servicio_saneamiento, fill=servicio_saneamiento)) +
      geom_bar() +
      labs(title = "Servicio de saneamiento en unidades educativas", x = "Escalera de servicio", y="Número de unidades",fill="Servicio de saneamiento") +
      scale_fill_manual(values = c("Servicio básico" = "#8bc378", "Servicio limitado" = "#fff272", "Sin servicio" = "#fab301")) +
      gtext
  })
  
  output$plot_san_VS <- renderPlot({
    ggplot(subset(data2(), municipio %in% c("serrano")), aes(x=servicio_saneamiento, fill=servicio_saneamiento)) +
      geom_bar() +
      labs(title = "Villa Serrano", x = "Escalera de servicio", y="Número de unidades",fill="Servicio de saneamiento") +
      scale_fill_manual(values = c("Servicio básico" = "#8bc378", "Servicio limitado" = "#fff272", "Sin servicio" = "#fab301")) +
      gtext
  })
  
  output$plot_san_P <- renderPlot({
    ggplot(subset(data2(), municipio %in% c("padilla")), aes(x=servicio_saneamiento, fill=servicio_saneamiento)) +
      geom_bar() +
      labs(title = "Padilla", x = "Escalera de servicio", y="Número de unidades",fill="Servicio de saneamiento") +
      scale_fill_manual(values = c("Servicio básico" = "#8bc378", "Servicio limitado" = "#fff272", "Sin servicio" = "#fab301")) + 
      gtext
  })
  
  
  ##Lavado de manos
  

  output$plot_hig_1 <- renderPlot({
    ggplot(data2(), aes(x=servicio_lavado, fill=servicio_lavado)) +
      geom_bar() +
      labs(title = "Servicio de lavado de manos en unidades educativas", x = "Escalera de servicio", y="Número de unidades",fill="Servicio de lavado de manos") + 
      scale_fill_manual(values = c("Servicio básico" = "#802997", "Servicio limitado" = "#fff272", "Sin servicio" = "#fab301")) + 
      gtext
  })
  
  output$plot_hig_VS <- renderPlot({
    ggplot(subset(data2(), municipio %in% c("serrano")), aes(x=servicio_lavado, fill=servicio_lavado)) +
      geom_bar() +
      labs(title = "Villa Serrano", x = "Escalera de servicio", y="Número de unidades",fill="Servicio de lavado de manos") + 
      scale_fill_manual(values = c("Servicio básico" = "#802997", "Servicio limitado" = "#fff272", "Sin servicio" = "#fab301")) +
      gtext
  })
  
  output$plot_hig_P <- renderPlot({
    ggplot(subset(data2(), municipio %in% c("padilla")), aes(x=servicio_lavado, fill=servicio_lavado)) +
      geom_bar() +
      labs(title = "Padilla", x = "Escalera de servicio", y="Número de unidades",fill="Servicio de lavado de manos") + 
      scale_fill_manual(values = c("Servicio básico" = "#802997", "Servicio limitado" = "#fff272", "Sin servicio" = "#fab301")) +
      gtext
  })
  
  #map
  output$mymap <- renderLeaflet({
    leaflet(data=data2()) %>%
      addTiles %>%  # Add default OpenStreetMap map tiles
      addMarkers(~longitude, ~latitude, clusterOptions = markerClusterOptions())
  })
  
}

shinyApp(ui, server)


