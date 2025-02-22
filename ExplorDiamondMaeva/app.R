library(shiny)
library(dplyr)
library(ggplot2)
library(glue)
library(DT)
library(bslib)
library(thematic)

thematic_shiny(font = "auto")
data(diamonds)

ui <- fluidPage(
  theme = bs_theme(
    #version = 5,
    bootswatch = "bootstrap"
  ),
  h1("Exploration des Diamants"),
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
      plotOutput("DiamondPlot")
    )
  ),
  DTOutput(outputId = "DiamondTable")
)

server <- function(input, output) {
  output$DiamondPlot <- renderPlot({
    diamonds |>
      filter(price > input$prix) |>
      filter(color %in% input$choix_couleur) |>
      ggplot(aes(x = height)) +
      geom_histogram(
        binwidth = 10,
        fill = "white",
        color = "black"
      ) +
      labs(
        title = glue("Vous avez selectionné le genre : {input$choix_couleur}")
      )
  })
  
  output$Nombre_personnes <- renderText({
    glue("Nombre de personnages selectionnés : {
    nrow(
      starwars |>
        filter(price > input$prix) |>
        filter(color %in% input$choix_couleur))}")
  })
  
  output$DiamondTable <- renderDT({
    diamonds |>
      # filter(price > input$prix) |>  Le tableau final nest pas pas prix eheh
      filter(color %in% input$choix_couleur)
  })
  
    output$resultat <- renderText({ paste("Vous avez choisi :", input$choix_rose) })
  
  observeEvent(c(input$bouton, input$prix), {
    message("T'as cliqué fdp.")
    showNotification(
      "La valeur du slider a changé...",
      type = "message"
    )
  })
}


shinyApp(ui = ui, server = server)