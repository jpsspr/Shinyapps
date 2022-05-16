library(readxl)
library(ggplot2)
library(shiny)
library(lubridate)
library(dplyr)
library(stringr)
library(DT)
library(formattable)

# Cargamos el fichero de datos
myfile <- file.path("data", "pend.xls") 
Pend <- read_xls(myfile, header=T)

# Establecemos tipo de datos fecha en la 1ª columna y fusionamos CodConc y Concepto
Pend[,"Fcopia"]=as.Date(Pend$Fcopia,"%m/%d/%Y")
Pend=mutate(Pend, Concep=str_c(Pend$CodConc,Pend$Concepto, sep="-"))

# Formateamos las columnas monetarias
Pend$TOTAL=currency(Pend$TOTAL,"???",format="f",big.mark = ".",sep=" ")
Pend$IVA=currency(Pend$IVA,"???",format="f",big.mark = ".",sep=" ")
Pend$Ejecutiva=currency(Pend$Ejecutiva,"???",format="f",big.mark = ".",sep=" ")
Pend$Voluntaria=currency(Pend$Voluntaria,"???",format="f",big.mark = ".",sep=" ")


ui <- fluidPage(
  fluidRow(column(width = 5),
           selectInput(inputId="Fecha",label="Selecciona fecha:", choices=c("Todas",unique(Pend[,1])))
  ),
  fluidRow(
    plotOutput("plot"),
    
    plotOutput("plot2")
  ),
  dataTableOutput("Summary")
)

#### SERVER
server <- function(input, output) {
  
  Filtro<-reactive({filter(Pend,Fcopia==input$Fecha)})
  Fechas=unique(Pend$Fcopia)
  
  output$plot <- renderPlot(
    
    if(input$Fecha=="Todas"){
      
      ggplot(Pend, aes(x=Acontable,y=TOTAL/1000000,fill=CodConc))+geom_col()+facet_grid(.~Fcopia)
      
    }
    else{
      
      ggplot(Filtro(), aes(x=Acontable,y=TOTAL/1000000,fill=CodConc))+geom_col()+facet_grid(.~Fcopia)
      
    })
  
  output$plot2 <- renderPlot(
    
    if(input$Fecha=="Todas"){
      
      
      ggplot(Pend, aes(x=reorder(Concep,+CodConc),y=TOTAL/1000000,fill=Acontable))+geom_col()+facet_grid(Fcopia~.)+theme(axis.text.x = element_text(
        angle = 90, hjust = 1))
    }
    else{
      
      
      ggplot(Filtro(), aes(x=reorder(Concep,+CodConc),y=TOTAL/1000000,fill=Acontable))+geom_col()+facet_grid(Fcopia~.)+theme(axis.text.x = element_text(
        angle = 90, hjust = 1))
    })
  
  output$Summary<- renderDataTable( 
    
    if(input$Fecha=="Todas"){
      
      datatable({ Pend })
      
    }
    else{
      
      datatable({ Filtro() })
      
    })
}
shinyApp(ui = ui, server = server)
