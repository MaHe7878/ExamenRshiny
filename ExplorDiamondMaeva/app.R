library(shiny)
library(dplyr)
library(ggplot2)
library(glue)
library(DT)
library(bslib)
library(thematic)

thematic_shiny(font = "Minty")

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty" 
  ),
  h2("Exploration des Diamants"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "choix_rose", 
        label = "Colorier les points en rose ?", 
        choices = c("Oui" = "opt1", "Non" = "opt2")
      ),
      selectInput(
        inputId = "choix_couleur",
        choices = c("D", "E","F", "G", "H", "I", "J"),
        label = "Choisir une couleur à filtrer :",
        selected = NULL,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      sliderInput(
        inputId = "prix",
        label = "Prix maximum :",
        min = 300,
        max = 20000,
        value = 5000
      ),
      textOutput(outputId = "Bouton"),
      actionButton(
        inputId = "bouton",
        label = "Afficher une notification"
      )
    ),
    mainPanel(
      plotOutput("DiamondPlot"),
      DTOutput(outputId = "DiamondTable")
    )
  ),

)

server <- function(input, output) {
  output$DiamondPlot <- renderPlot({
    diamonds |>
      filter(price <= input$prix) |>
      ggplot(aes(x = carat, y = price)) +
      geom_point(
        color = ifelse(input$choix_rose == "opt1", "#ffc4cf", "#5f5f5f"))+
      labs(
        title = glue("prix: {input$prix} & color: {input$choix_couleur}")
      )
  })
  
  output$DiamondTable <- renderDT({
    diamonds |>
      select(carat, cut, color, clarity, depth, table, price) |>
      # filter(price > input$prix) |>  Le tableau final se modifie sans le prix
      filter(color %in% input$choix_couleur)
  })
  
  observeEvent(input$bouton, {
    showNotification(
      glue("prix: {input$prix} & color: {input$choix_couleur}"),
      type = "message"
    )
  })
}


shinyApp(ui = ui, server = server)