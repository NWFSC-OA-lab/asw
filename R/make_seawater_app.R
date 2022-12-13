#######
## Making seawater calcs

library(shiny)
library(ggplot2)
library(stringr)

# Define UI ----
ui <- fluidPage(
  titlePanel("Making seawater calcs (ballpark estimates)"),
  sidebarLayout(
    sidebarPanel( width = 4,
                  numericInput("hst_init_sal", "HST Initial Salinity (psu)", "35.3"),
                  numericInput("hst_init_vol", "HST Initial volume (L)", "20"),
                  numericInput("hst_target_sal", "HST Target Salinity (psu)", "34.9"),
                  numericInput("hst_target_vol", "HST Target Volume (L)", "60"),
                  textOutput("hst_add_salt"),
                  textOutput("hst_add_di"),

                  numericInput("sst_init_sal", "SST Initial Salinity (psu)", "30.1"),
                  numericInput("sst_init_vol", "SST Initial Volume (L)", "15"),
                  numericInput("sst_target_sal", "SST Target Salinity (psu)", "30"),
                  numericInput("sst_target_vol", "SST Target Volume (L)", "50"),
                  textOutput("sst_add_hst"),
                  textOutput("sst_add_di"),
                  textOutput("sst_just_di_dilute")
    ),
    mainPanel(
      width = 8,
      # this suppresses error messages
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  output$hst_add_salt <- renderText({ paste("HST Add Salt (g):",
                                           round(input$hst_target_sal *
                                                   input$hst_target_vol -
                                                   input$hst_init_sal *
                                                   input$hst_init_vol, 2)) })
  output$hst_add_di <- renderText({ paste("HST Add DI (L):",
                                         input$hst_target_vol - input$hst_init_vol) })



  output$sst_add_hst <- renderText({paste("Add HST to SST (L):",
                                          round(((input$sst_target_sal *  input$sst_target_vol) -
                                            (input$sst_init_sal * input$sst_init_vol)) /
                                            input$hst_init_sal, 2))})
  output$sst_add_di <- renderText({paste("Add DI Water (with HST) to SST (L):",
                                         round(input$sst_target_vol -
                                                 input$sst_init_vol -
                                                 ((input$sst_target_sal *  input$sst_target_vol) -
                                                  (input$sst_init_sal * input$sst_init_vol)) /
                                                 input$hst_init_sal, 2))})
  output$sst_just_di_dilute <- renderText({paste("Add DI Water (just dilute) to SST (L):",
                                                 round(input$sst_init_sal *
                                                         input$sst_init_vol /
                                                         input$sst_target_sal -
                                                         input$sst_init_vol, 2))})



}

# Run the app ----
shinyApp(ui = ui, server = server)
