library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(bslib)

thematic::thematic_shiny(font = "auto")

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty"
  ),

    # titre de l'application
    titlePanel("Diamonds App"),
    h2("Exploration des diamants"),

  sidebarLayout(
    sidebarPanel(
      actionButton(inputId = "boutton",
                   label = "bouton"),
      actionButton(inputId = "boutton2",
                   label = "Notifications"),
      
      sliderInput(inputId = "Prix",
                  label = "Prix maximun",
                  min = 300,
                  max = 20,000,
                  value = 5,000),
      
      selectInput(inputId = "color_input",
                  label = "Choisir une couleur à filtrer",
                  choices = c("D","E","F","G","H","I","J")),
      radioButtons(inputId = "",
                   label = "",
                   choices = "",
                   selected = NULL,
                   inline = FALSE,
                   width = NULL,
                   choiceNames = NULL,
                   choiceValues = NULL
      
      ),
    ),
    
    mainPanel(
      textOutput(outputId = "textstarwars"),
      plotOutput(outputId = "StarWarsPlot"),
      DTOutput(outputId = "tblo")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  rv<-reactiveValues(df=NULL)
  
  observeEvent(input$boutton, {
    rv$df<-starwars |> 
      filter(height > input$taille & gender == input$gender_input)
    
    rv$plot<-rv$df  |>
      ggplot(aes(x = height)) +
      geom_histogram(
        binwidth = 10, 
        fill = "darkgray", 
        color = "white"
      )+
      labs(
        title = paste ("Gender choice:",input$gender_input)
      )
    message("vous avez cliqué sur le bouton")
    
  })
  
  observeEvent(input$boutton2, {
    showNotification(
      "La valeur du slider a changé !",
      type = "error"
    )
    
  })
  
  output$StarWarsPlot <- renderPlot({
    rv$plot
    
  })
  output$textstarwars<- renderText({
    rv$df |>
      nrow()
    
    paste("Nb ligne:",nombre_ligne)
  })
  output$tblo<-renderDT({
    rv$df
  })
}

# Run the application 
shinyApp(ui = ui, server = server)