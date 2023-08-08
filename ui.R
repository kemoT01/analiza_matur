# definicja interfejsu 
library(shiny)
library(ggplot2)
library(plotly)
library(data.table)

shinyUI(fluidPage(

  titlePanel("Analiza matur"),

    sidebarLayout(
      
        sidebarPanel(

          # Ładowanie pliku
          fileInput("fileInPath", 
                    label= h4("Import danych")
          ),
          
          # Wybór klas
          selectInput("wyborKlasa",
                      label = "Wybór klas",
                      choices = NULL,
                      multiple = TRUE),
          
          # Zapisywanie prezentacji
          downloadButton("downloadPresentation", "Pobierz prezentację")          
        ),

        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Tabela", tableOutput("tabela")),
                      tabPanel("Średnie", plotlyOutput("srednie")),
                      tabPanel("Zdawalność", tableOutput("zdawalnosc")),
                      tabPanel("Zadania", plotlyOutput("zadania")),
                      tabPanel("Łatwość", plotlyOutput("latwosc")),
                      tabPanel("Surowe dane", tableOutput("tabelaSurowe"))
          )
        )

    )
))
