# library(shiny)

# # Vector of countries
# list_of_countries <- c("USA", "CHN", "ENG", "CAN", "JPN")

# ui <- fluidPage(
#   # Application title
#   titlePanel("Olympics Teams"),

#   # Sidebar with dropdown menus
#   sidebarLayout(
#     sidebarPanel(
#       # Dropdown for choosing Male or Female team
#       selectInput("team", "Select Team", choices = c("Male", "Female")),

#       # Dropdown for selecting a country
#       selectInput("country", "Select Country", choices = list_of_countries),

#       # Checkboxes for selecting multiple countries to compare
#       checkboxGroupInput("compare_countries", "Compare with Countries", choices = list_of_countries)
#     ),

#     # Main panel
#     mainPanel(
#       # Output for displaying the selected team and country
#       textOutput("selection_text")
#     )
#   )
# )

# server <- function(input, output) {
#   # Create a reactive expression for displaying the selected team and country
#   output$selection_text <- renderText({
#     paste("Selected Team:", input$team, "Selected Country:", input$country)
#   })
# }

# shinyApp(ui, server)

# source("main.r")

top_countries_m <- c("CHN", "IRI", "ARM", "ALB", "PHI")
top_countries_w <- c("ITA", "CHN", "ENG", "GER", "MEX")
library(shiny)

ui <- fluidPage(
  # Application title
  titlePanel("Olympics Teams"),

  # Sidebar with dropdown menus
  sidebarLayout(
    sidebarPanel(
      # Dropdown for choosing Male or Female team
      selectInput("team", "Select Team", choices = c("Male", "Female")),

      # Dropdown for choosing Score Prediction or Winning Probability
      selectInput("metric", "Select Metric", choices = c("Score Prediction", "Winning Probability")),

      # UI Output for the reactive checkbox group
      uiOutput("countryCheckboxes")
    ),

    # Main panel
    mainPanel(
      # Table output for displaying the results
      tableOutput("resultsTable"),
      # Output for displaying the selected team and metric
      textOutput("selection_text")
    )
  )
)

server <- function(input, output, session) {
  # Reactive UI for the checkboxes based on the team selection
  output$countryCheckboxes <- renderUI({
    if (input$team == "Male") {
      checkboxGroupInput("compare_countries", "Compare with Countries", choices = top_countries_m)
    } else if (input$team == "Female") {
      checkboxGroupInput("compare_countries", "Compare with Countries", choices = top_countries_w)
    } else {
      return(NULL) # Return nothing if no team is selected
    }
  })

  # Render the result tables based on the selections
  output$resultsTable <- renderTable({
    if (input$team == "Female" && input$metric == "Score Prediction") {
      return(result_pred_w)
    } else if (input$team == "Male" && input$metric == "Score Prediction") {
      return(result_pred_m)
    } else if (input$team == "Female" && input$metric == "Winning Probability"){
      return(result_prob_w)
    } else {
      return(result_prob_m)
    }
  })

  # Create a reactive expression for displaying the selected team and metric
  output$selection_text <- renderText({
    paste("Selected Team:", input$team, "| Selected Metric:", input$metric, "| Compared Countries:", paste(input$compare_countries, collapse = ", "))
  })
}

shinyApp(ui, server)
