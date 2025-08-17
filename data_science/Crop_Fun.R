library(shiny)

ui <- fluidPage(
  h2("Fun Crop Facts"),
  selectInput("crop", "Pick a crop:", c("Peas", "Potato", "Wheat", "Coffee")),
  actionButton("go", "🌟 Show Fact"),
  textOutput("crop_fact")
)

server <- function(input, output, session) {
  facts <- reactive({
    switch(input$crop,
           Peas  = "🫛 Staple cereal; Cool-season legume; good protein source; fixes nitrogen in soil.",
           Potato   = "🥔 Tuber crop; grows in cooler climates; important source of carbohydrates.",
           Wheat  = "🌾 Wheat is used for bread, pasta, and delicious pastries.",
           Coffee = "☕ Coffee beans are roasted seeds that power mornings worldwide."
    )
  })
  output$crop_fact <- renderText(eventReactive(input$go, facts())())
}

shinyApp(ui, server)

