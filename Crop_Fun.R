library(shiny)

ui <- fluidPage(
  h2("Fun Crop Facts"),
  selectInput("crop", "Pick a crop:", c("Maize", "Rice", "Wheat", "Coffee")),
  actionButton("go", "🌟 Show Fact"),
  textOutput("crop_fact")
)

server <- function(input, output, session) {
  facts <- reactive({
    switch(input$crop,
           Maize  = "🌽 Staple cereal; used for food, feed, and biofuel. Likes moderate rain and warm temperatures",
           Rice   = "🍚 Staple food in Asia; grows in flooded paddies; needs lots of water",
           Wheat  = "🌾 Wheat is used for bread, pasta, and delicious pastries.",
           Coffee = "☕ Coffee beans are roasted seeds that power mornings worldwide."
    )
  })
  output$crop_fact <- renderText(eventReactive(input$go, facts())())
}

shinyApp(ui, server)

