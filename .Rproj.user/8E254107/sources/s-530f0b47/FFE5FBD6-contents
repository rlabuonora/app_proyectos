library(shiny)
library(shinydashboard)
library(ggplot2)  
library(tidyr)
library(readxl)
library(stringr)
library(pdfetch)
library(Quandl)
library(dplyr)
library(kableExtra)

Datos <- read_excel("./estado_de_proyectos.xlsm",sheet = "Actualización por Proyecto",skip=2)

Datos <- Datos %>% filter(!is.na(`Estado General`))

colnames(Datos) <- gsub(" ", "_", colnames(Datos))

colnames(Datos) <- gsub("/", "_", colnames(Datos))

Totales_Eje <- Datos %>%
  group_by(Eje) %>%
  summarise(Asignado=sum(as.numeric(`Monto_Efectivo_Asignado_(actualizado)`)),Ejecutado=sum(as.numeric(Ejecutado))) %>%
  pivot_longer(cols=Asignado:Ejecutado,names_to = "Gasto",
               values_to = "valor")

Totales_UE <- Datos %>%
  group_by(Ministerio_Responsable) %>%
  summarise(Asignado=sum(as.numeric(`Monto_Efectivo_Asignado_(actualizado)`)),Ejecutado=sum(as.numeric(Ejecutado))) %>%
  pivot_longer(cols=Asignado:Ejecutado,names_to = "Gasto",
               values_to = "valor")

Tabla <- Datos %>%
  select(Eje,ID,Rubros_Actividades,Ministerio_Responsable,Estado_General,Estado) 


ui <- dashboardPage(
  #Create header 
  dashboardHeader(title = "INICIATIVA RIO NEGRO", titleWidth = 450
                  
  ),
  #Create sidebar 
  dashboardSidebar(  selectInput(inputId = "Eje",
                                 label = "Tipo_Eje",
                                 choices = unique(Datos$Eje),
                                 selected = unique(Datos$Eje)[1],
                                 multiple = TRUE), #allowing multiple country selection
                     selectInput(inputId = "Min",
                                 label = "Ministerio",
                                 choices = unique(Datos$Ministerio_Responsable),
                                 selected = unique(Datos$Ministerio_Responsable)[1],
                                 multiple = TRUE),
                     selectInput(inputId = "EG",
                                 label = "Estado_General",
                                 choices = unique(Datos$Estado_General),
                                 selected = unique(Datos$Estado_General)[1],
                                 multiple = TRUE),
                     downloadButton("downloadData", "DOWNLOAD RESUMEN")
                     
  ),
  #Create dashboard main panel
  dashboardBody(fluidRow(
    valueBoxOutput("value1",width = 3)
    ,valueBoxOutput("value2",width = 3)
    ,valueBoxOutput("value3",width = 3)
    ,valueBoxOutput("value4",width = 3)
  ),fluidRow( 
    tabsetPanel(type = "tabs",
                tabPanel("TOTALES", splitLayout(cellWidths = c("50%", "50%"), plotOutput("Eje"),plotOutput("ue"))),
                tabPanel("GRAFICAS POR VARIABLES", splitLayout(cellWidths = c("50%", "50%"), plotOutput("MA"), plotOutput("ME"))),
                tabPanel("TABLA RESUMEN", tableOutput("mytable"))
    ))
  
  )
)

#The following tells the server what to do
server <- function(input, output){ 
  #some data manipulation to derive the values of KPI boxes
  
  Estancado <- Datos %>% group_by(Estado_General) %>% dplyr::summarise(nro = n(),ejec=sum(as.numeric(Ejecutado),na.rm = TRUE)) %>% filter(Estado_General=="Estancado")
  
  Ejecutado <- Datos %>% group_by(Estado_General) %>% dplyr::summarise(nro = n(),ejec=sum(as.numeric(Ejecutado),na.rm = TRUE)) %>% filter(Estado_General=="Ejecutado")
  
  Avance <- Datos %>%group_by(Estado_General) %>% dplyr::summarise(nro = n(),ejec=sum(as.numeric(Ejecutado),na.rm = TRUE)) %>% filter(Estado_General=="En Avance" | Estado_General=="En Avance Sin Problemas")%>% summarise(nro=sum(nro),ejec=sum(ejec))
  
  ME <- Datos %>% dplyr::summarise(ejec=sum(as.numeric(Ejecutado),na.rm = TRUE))
  
  MA <- Datos %>% dplyr::summarise(asig=sum(as.numeric(`Monto_Efectivo_Asignado_(actualizado)`),na.rm = TRUE))
  
  porcentaje <- round(ME/MA*100)
  
  Tot_eje <- Datos %>% group_by(Eje) %>% dplyr::summarise(nro = n(),ejec=sum(as.numeric(Ejecutado),na.rm = TRUE))
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      paste((formatC(Estancado$nro, digits=2, format="f", big.mark=',')),"ESTANCADO")
      ,paste(" Monto Ejecutado=",Estancado$ejec)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "red")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      paste((formatC(Ejecutado$nro, digits=0, format="f", big.mark=',')),"EJECUTADO")
      ,paste(" Monto Ejecutado=",formatC(Ejecutado$ejec,digits=2, format="f", big.mark=','))
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "green")  
  })
  output$value3 <- renderValueBox({
    valueBox(
      paste((formatC(Avance$nro, digits=0,  format="f", big.mark=',')),"EN AVANCE")
      ,paste(" Monto Ejecutado=",formatC(Avance$ejec,digits=2, format="f", big.mark=','))
      ,icon = icon("stats",lib='glyphicon')
      ,color = "yellow")   
  })
  output$value4 <- renderValueBox({
    valueBox(
      formatC(ME$ejec, digits=2,  format="f", big.mark=',')
      ,paste("Monto Ejecutado",porcentaje,"%")
      ,icon = icon("usd",lib='glyphicon')
      ,color = "navy")   
  })
  dataset <- reactive({return(Datos %>% 
                                filter(Eje %in% input$Eje & Ministerio_Responsable %in% input$Min & Estado_General %in% input$EG) %>%
                                group_by(Eje,UE_Responsable) %>%
                                summarise(asignado=sum(as.numeric(`Monto_Efectivo_Asignado_(actualizado)`)),ejecutado=sum(as.numeric(Ejecutado))))})
  
  output$Eje <- renderPlot(ggplot(Totales_Eje, aes(x = Eje, y=as.numeric(valor),fill=Gasto,group=Gasto)) + 
                             geom_col(position="dodge")+
                             ggtitle("Gasto Asignado y Ejecutado por Eje")+
                             ylab("USD")+
                             scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                                            scientific = FALSE))+
                             theme(axis.text.x  = element_text(vjust=0.5, size=10)))
  
  output$ue <- renderPlot(ggplot(Totales_UE, aes(x = Ministerio_Responsable, y=as.numeric(valor),fill=Gasto,group=Gasto)) + 
                            geom_col(position="dodge")+
                            ggtitle("Gasto Asignado y Ejecutado por Unidad Responsable")+
                            ylab("USD")+
                            scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                                           scientific = FALSE))+
                            theme(axis.text.x  = element_text(vjust=0.5, size=10)))
  
  output$ME <- renderPlot(ggplot(dataset(), aes(x = UE_Responsable, y=as.numeric(ejecutado),fill=Eje,group=Eje)) + 
                            geom_col() +
                            ggtitle("Monto Ejecutado")+
                            ylab("USD")+
                            scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                                           scientific = FALSE))+
                            theme(axis.text.x  = element_text(vjust=0.5, size=10)))
  
  output$MA <- renderPlot(ggplot(dataset(), aes(x = UE_Responsable, y=as.numeric(asignado),fill=Eje,group=Eje)) + 
                            geom_col() +
                            ggtitle("Monto Asignado")+
                            ylab("USD")+
                            scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                                           scientific = FALSE))+
                            theme(axis.text.x  = element_text(vjust=0.5, size=10)))
  
  output$mytable <- function() {
        tbl <- Tabla %>%
          dplyr::filter(Eje %in% input$Eje & 
                 Ministerio_Responsable %in% input$Min & 
                 Estado_General %in% input$EG) 
        
        print(count(tbl, Estado_General))
        
        tbl %>% 
          kableExtra::kbl()
  }

  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Resumen-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      file.copy("~/nube opp/compartiendo/Transforma Uruguay/Coordinación General/Para Dirección/FIS/app/Tarifas Correo.pdf", file)
    }
  )
}


#create the Shiny App
shinyApp(ui = ui, server = server)


