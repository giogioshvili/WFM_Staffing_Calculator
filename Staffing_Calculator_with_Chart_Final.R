# app.R
library(shiny)
library(shinythemes)
library(plotly)
library(bslib)


# Define UI
ui <- fluidPage(
  
  # Add a theme from shinythemes
  theme = shinytheme("cerulean"),
  
  # Title with color styling
  titlePanel(
    div(style = "color: #2C3E50; font-weight: bold; text-align: center;", 
        "Staff Calculator")
  ),
  
  # Sidebar layout with custom background color
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #ECF0F1; border-radius: 10px;",
      
      # Inputs for Chat Channel
      h4("Chat Channel"),
      numericInput("contacts_chat", "Number of Contacts (Chat):", value = 1000, min = 0),
      numericInput("aht_chat", "Average Handle Time (Minutes, Chat):", value = 2, min = 0),
      
      # Inputs for Email Channel
      h4("Email Channel"),
      numericInput("contacts_email", "Number of Contacts (Email):", value = 1000, min = 0),
      numericInput("aht_email", "Average Handle Time (Minutes, Email):", value = 4, min = 0),
      
      
      # Shrinkage Breakdown Inputs
      numericInput("shrink_in", "In-Office Shrinkage (%):", value = 30, min = 0, max = 100),
      numericInput("shrink_unplanned", "Unplanned Out-of-Office Shrinkage (%):", value = 5, min = 0, max = 100),
      numericInput("shrink_planned", "Planned Out-of-Office Shrinkage (%):", value = 10, min = 0, max = 100),
      
      
      #Input for Total Agent Weekly Working Hours
      numericInput("total_weekly_hours", "Total Weekly Hours per Agent:", value = 42, min = 0),
      
      # Input for Current Staffing
      numericInput("current_staffing", "Current Number of Agents:", value = NULL, min = 0),
      
      
      actionButton("calc", "Calculate Staffing", 
                   style = "color: black; background-color: #2980B5; border-radius: 5px;")
    ),
    
    # Main panel to display the result with custom font color
    mainPanel(
      div(style = "color: #2980B9; font-size: 20px; margin-top: 20px;",
          textOutput("staff_required"),
          textOutput("total_shrinkage"),
          plotlyOutput("staffing_chart")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive function to calculate staffing
  calculate_staff <- eventReactive(input$calc, {
    # Ensure all inputs are provided
    req(input$contacts_chat, input$aht_chat, input$contacts_email, input$aht_email,
        input$total_weekly_hours, input$shrink_in, input$shrink_planned, input$shrink_unplanned,
        input$current_staffing)
    
    # Inputs for Chat Channel
    contacts_chat <- input$contacts_chat
    aht_chat <- input$aht_chat
    
    # Inputs for Email Channel
    contacts_email <- input$contacts_email
    aht_email <- input$aht_email
    
    # Shared Inputs
    total_weekly_hours <- input$total_weekly_hours
    shrink_in <- input$shrink_in / 100
    shrink_planned <- input$shrink_planned / 100
    shrink_unplanned <- input$shrink_unplanned / 100
    current_staffing <- input$current_staffing
    
    # Total Shrinkage
    total_shrinkage <- shrink_in + shrink_planned + shrink_unplanned
    
    # Weighted AHT (in hours)
    total_contacts <- contacts_chat + contacts_email
    weighted_aht <- ((contacts_chat * aht_chat) + (contacts_email * aht_email)) / total_contacts / 60
    
    # Total Workload (in hours)
    total_workload <- total_contacts * weighted_aht
    
    # Effective Weekly Hours per Agent
    effective_weekly_hours <- total_weekly_hours * (1 - total_shrinkage)
    
    # Staff Requirement
    staff_required <- total_workload / effective_weekly_hours
    
    return(list(
      staff_required = staff_required,
      total_contacts = total_contacts,
      total_workload = total_workload,
      effective_weekly_hours = effective_weekly_hours,
      total_shrinkage = total_shrinkage * 100,
      current_staffing = current_staffing
    ))
  })
  
  # Display the staffing result
  output$staff_required <- renderText({
    req(calculate_staff())
    paste("Total Staff Required: ", round(calculate_staff()$staff_required, 2))
  })
  
  # Display the total shrinkage
  output$total_shrinkage <- renderText({
    req(calculate_staff())
    paste("Total Shrinkage: ", round(calculate_staff()$total_shrinkage, 2), "%")
  })
  
  # Generate the staffing chart
  output$staffing_chart <- renderPlotly({
    req(calculate_staff())
    data <- data.frame(
      Category = c("Staff Required", "Current Staffing"),
      Value = c(calculate_staff()$staff_required, calculate_staff()$current_staffing)
    )
    plot_ly(data, x = ~Category, y = ~Value, type = 'bar', mode = 'lines',
            name = 'Staffing Metrics',
            line = list(color = 'rgb(55, 50, 50)', width = 2)) %>%
      add_trace(x = ~Category, y = ~Value, type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor = 'rgba(55, 50, 50, 0.3)', showlegend = FALSE) %>%
      layout(xaxis = list(title = ''),
             yaxis = list(title = 'Number of HC'),
             showlegend = FALSE)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
