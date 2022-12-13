#######
## Making seawater calcs

library(shiny)
library(ggplot2)
library(stringr)

# Define UI ----
ui <- fluidPage(
  titlePanel("Making seawater calcs"),
  sidebarLayout(
    sidebarPanel( width = 2,
                  numericInput("init_sal", "Initial Salinity (psu)", "32"),
                  numericInput("init_vol", "Initial volume (L)", "20"),
                  numericInput("target_sal", "Targret Salinity (psu)", "31"),
                  numericInput("target_vol", "Target Volume (L)", "60"),
                  textOutput("sm_add_salt", "Salt Mixing: Add Salt (g)"),
                  textOutput("sm_add_di", "Salt Mixing: Add DI (L)"),
                  textOutput("wm_add_sw", "Water Mixing: Add Initial Seawater (L)"),
                  textOutput("wm_add_di", "Water Mixing: Add Initial Seawater (L)")
    ),
    mainPanel(
      width = 10,
      # this suppresses error messages
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      #plotOutput("plot", width = "100%", height = "740px")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  # output$wm_add_sw <- renderText({ input$target_sal / input$init_sal * input$target_vol })
  # output$wm_add_di <- renderText({ input$target_vol -input$target_sal /
  # input$init_sal * input$target_vol})


  # output$plot <- renderPlot({
  # })

}

# Run the app ----
shinyApp(ui = ui, server = server)
