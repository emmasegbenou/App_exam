library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(bslib)
library(plotly)
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
      
      radioButtons(
        inputId = "pink_bouton",
        label = "Colorier les points en rose ?",
        choices = c("Oui", "Non")
      ),
      
      selectInput(inputId = "color_input",
                  label = "Choisir une couleur à filtrer",
                  choices = c("D","E","F","G","H","I","J")),
      
      sliderInput(inputId = "prix_input",
                  label = "Prix maximun",
                  min = 300,
                  max = 20000,
                  value = 5000),
      
      actionButton(
        inputId = "go",
        label = "Visualiser le graph")
      ),
      
    
    mainPanel(
      plotOutput(outputId = "diamondPlot"),
      DTOutput(outputId = "table")
    )
    )
  )
  

  
# Define server logic required to draw a histogram
server <- function(input, output) {
  rv<-reactiveValues()
  observeEvent(input$go,{
    rv$graphe<-diamonds|> 
        filter(price <= input$prix_input & color == input$color_input) |>
        ggplot(aes(x = carat, y=price))+
        geom_point(
          color= if (input$pink_bouton == "Oui") "pink" else "black") +
        theme_minimal() +
        labs(
          title = paste("prix:", input$prix, "& color:", input$color_input)
        )
    rv$table<-diamonds|>
      filter(price <= input$prix_input & color == input$color_input)|>
        select(carat, cut, clarity, depth, table, price)
    })
    
  output$diamondPlot <- renderPlot({
    rv$graphe})
 
  output$table<-renderDT({
   rv$table
  })
  
  showNotification(
    type
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)