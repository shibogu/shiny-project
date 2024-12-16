library(shiny)

ui <- fluidPage(
  titlePanel("Shiny App with dplyr Commands"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload a CSV File", accept = ".csv"),
      
      selectInput("filter_field", "Select Field to Filter", choices = NULL),
      textInput("filter_value", "Enter Filter Value"),
      
      selectInput("select_fields", "Select Fields to Display", choices = NULL, multiple = TRUE),
      
      selectInput("group_field", "Select Field to Group By", choices = NULL),
      
      radioButtons("summary_stat", "Select Summary Statistic", 
                   choices = c("Min" = "min", "Mean" = "mean", "Max" = "max", "SD" = "sd"))
    ),
    
    mainPanel(
      tableOutput("filtered_data"),
      tableOutput("summary_table")
    )
  )
)

server <- function(input, output, session) {
  # Reactive expression to read the uploaded file
  uploaded_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Update field selection inputs dynamically based on uploaded data
  observe({
    req(uploaded_data())
    fields <- names(uploaded_data())
    updateSelectInput(session, "filter_field", choices = fields)
    updateSelectInput(session, "select_fields", choices = fields)
    updateSelectInput(session, "group_field", choices = fields)
  })
  
  # Filtered data based on user input
  filtered_data <- reactive({
    req(input$filter_field, input$filter_value)
    uploaded_data() %>%
      dplyr::filter(get(input$filter_field) == input$filter_value)
  })
  
  # Display filtered data
  output$filtered_data <- renderTable({
    req(filtered_data(), input$select_fields)
    filtered_data() %>%
      dplyr::select(all_of(input$select_fields))
  })
  
  # Summary table based on grouping and statistic
  summary_table <- reactive({
    req(input$group_field, input$summary_stat)
    stat_function <- match.fun(input$summary_stat)
    filtered_data() %>%
      dplyr::group_by(get(input$group_field)) %>%
      dplyr::summarise(across(everything(), stat_function, .names = "{.col}_{.fn}"))
  })
  
  # Display summary table
  output$summary_table <- renderTable({
    req(summary_table())
    summary_table()
  })
}

shinyApp(ui, server)
