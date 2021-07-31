library(shiny)
ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      img(src = "pch_01.png", height = 16, width = 16)
    )
  )
)
server <- function(input, output) {}
shinyApp(ui=ui, server=server)
