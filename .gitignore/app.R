
# Please, install packages, if you don't have them
#install.packages(“ggplot2”)
#install.packages(“forecast”)
#install.packages('prophet')

library(shiny)
library(ggplot2)
library(forecast)
library(prophet)

ui <- fluidPage(
   
   # Titre de l'application
   titlePanel("Application de prévision avec R-Shiny"),

   
   navbarPage("Prévision",
              tabPanel("Télécharger des données",
                       sidebarLayout(
                         
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           
                           # Input: Select a file ----
                           fileInput("file1", "Choisissez un fichier CSV",
                                     multiple = F,
                                     accept = c("text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv")),
                           
                           # Horizontal line ----
                           tags$hr(),
                           
                           # Input: Checkbox if file has header ----
                           checkboxInput("header", "Des données d'en-tête", TRUE),
                           
                           # Horizontal line ----
                           tags$hr(),
                           
                           # Input: Select separator ----
                           radioButtons("sep", "Séparateur",
                                        choices = c(Virgule = ",",
                                                    "Point-virgule" = ";",
                                                    Tab = "\t"),
                                        selected = ","),
                           
                           
                           # Horizontal line ----
                           tags$hr(),
                           
                           # Input: Select number of rows to display ----
                           radioButtons("disp", "Affichage de l'information",
                                        choices = c("Tête de la table" = "head",
                                                    "Toutes les entrées de la table" = "all"),
                                        selected = "head")
                           
                         ),
                         
                         # Main panel for displaying outputs ----
                         mainPanel(
                           
                           # Output: Data file ----
                           tableOutput("table")
                           
                         )
                         
                       )
              ),
              tabPanel("Prévision",
                       sidebarLayout(
                         
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                       
                       selectInput("select", "Choisissez une méthode de prévision", 
                                   choices = list("ARIMA model" = 1, "Prophet package" = 2, "TBATS model" = 3), 
                                   selected = 1),
                       
                       # Horizontal line ----
                       tags$hr(),
                       
                       sliderInput("n", "Période de prévision (jours)", min = 1, max = 30, value = 10,
                                   step = 1),
                       
                       # Button
                       downloadButton("Data.csv", "Télécharger les resultats")
                       ),
                         # Show a plot of the generated distribution
                         mainPanel(
                           plotOutput("plot")
                         )
                       )
              )
                       
              )
   )
   

server <- function(input, output, session) {
  output$table <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  output$method <- renderPrint({ input$select })

  
    output$plot <- renderPlot({
      req(input$file1)
      
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep)
      colnames(df)<-c("ds","y")
      if(input$select=='1'){
        #arima
        fit <- arima(df$y, order=c(5, 0, 0))
        fcst<-forecast(fit,h=input$n*24)
        
        #plot arima
        autoplot(fcst)+
          labs(title="Des séries chronologiques",
               x="Temps", y = "Consommation d'électricité")+
          theme_minimal()
      } else if(input$select=='2'){
        
        m <- prophet(df, weekly.seasonality='auto', daily.seasonality = 'auto')
        future <- make_future_dataframe(m, periods = input$n*24, freq = 60*60, include_history = FALSE)
        fcst <- predict(m, future)
        
        #plot
        plot(m, fcst)+
          labs(title="Des séries chronologiques",
               x="Temps", y = "Consommation d'électricité")+
          theme_minimal()+
          geom_line()
      }else if(input$select=='3'){
        
        taylor <- msts(df$y, seasonal.periods=c(24, 7, 52))
        taylor.fit <- tbats(taylor)
        fcst<-forecast(taylor.fit,h=input$n*24)
        
        autoplot(fcst)+
          labs(title="Des séries chronologiques",
               x="Temps", y = "Consommation d'électricité")+
          theme_minimal()
      }
      
    })
    
    
    # Downloadable csv of selected dataset ----
    output$Data.csv <- downloadHandler(
      filename = function() {
        paste("data", "csv", sep = ".")
      },
      content = function(file) {
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep)
        colnames(df)<-c("ds","y")
        if(input$select=='1'){
          
          fit <- arima(df$y, order=c(5, 0, 0))
          fcst<-forecast(fit,h=input$n*24)
          
          write.csv(fcst$mean, file, row.names = FALSE)
          
        } else if(input$select=='2'){
          
          m <- prophet(df, weekly.seasonality='auto', daily.seasonality = 'auto')
          future <- make_future_dataframe(m, periods = input$n*24, freq = 60*60, include_history = FALSE)
          fcst <- predict(m, future)
          
          write.csv(fcst$yhat, file, row.names = FALSE)
          
        }else if(input$select=='3'){
          
          taylor <- msts(df$y, seasonal.periods=c(24, 7, 52))
          taylor.fit <- tbats(taylor)
          fcst<-forecast(taylor.fit,h=input$n*24)
          
          write.csv(fcst$mean, file, row.names = FALSE)
        }
      }
    )
    
}


# Run the application 
shinyApp(ui = ui, server = server)

