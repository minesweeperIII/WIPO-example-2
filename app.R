options(shiny.useragg = TRUE)

library(shiny)
library(ggplot2)

plots <- readRDS("data/app_plots.rds")

plot_choices <- setNames(
  names(plots),
  vapply(plots, function(p) p$labels$title, character(1))
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #0A192F; color: white; font-family: sans-serif; }
      .control-label { color: white; font-weight: bold; }
      .well { background-color: #112240; border: 1px solid #233554; }
    "))
  ),
  titlePanel("Vietnam Creative Industry Employment (2016–2018)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_plot",
        label = "Select Graph:",
        choices = plot_choices,
        selected = names(plots)[1]
      )
    ),
    mainPanel(
      plotOutput("main_plot", height = "580px"),
      tags$p("Data source: Vietnam Labor Force Survey 2016 - 2018", class = "data-source-note")
    )
  )
)

server <- function(input, output, session) {
  output$main_plot <- renderPlot({
    req(input$selected_plot)
    plots[[input$selected_plot]]
  }, bg = "#0A192F", res = 96)
}

shinyApp(ui = ui, server = server)