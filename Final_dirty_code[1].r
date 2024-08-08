library(tidyverse)
library(lubridate)
library(shiny)


#UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("sales_file", "Upload Sales Data CSV", accept = ".csv"),
      fileInput("inventory_file", "Upload Inventory Data CSV", accept = ".csv"),
      actionButton("process_data", "Process Data")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Significant Discrepancy", DTOutput("discrepancy_table")),
        tabPanel("Reconciliation Histogram", plotOutput("histogram")),
        tabPanel("Regression Model Summary", verbatimTextOutput("model_summary")),
        tabPanel("Average Reconciliation Difference", verbatimTextOutput("avg_reconciliation"))
    )
  )
)
)
server <- function(input, output, session) {
  processed_data <- eventReactive(input$process_data, {
    req(input$sales_file, input$inventory_file)
    sales_data <- read.csv(input$sales_file$datapath)
    inventory_data <- read.csv(input$inventory_file$datapath)
    merged_data <- merge(sales_data, inventory_data, by = "product_id", all = TRUE)
    clean_data <- merged_data %>% 
              filter(!is.na(sales_amount) & !is.na(inventory_count))

  clean_data$total_sales <- clean_data$sales_amount * clean_data$unit_price
  clean_data$total_inventory_value <- clean_data$inventory_count * clean_data$unit_price

  clean_data$reconciliation_difference <- clean_data$total_sales - clean_data$total_inventory_value

  significant_discrepancy <- clean_data %>% 
                            filter(reconciliation_difference > 100)
  clean_data
})
 output$summary <- renderPrint({
  req(processed_data())(summary(processed_data))})

output$discrepancy_table <- renderDT({
    req(processed_data())
    significant_discrepancy <- processed_data() %>% 
      filter(reconciliation_difference > 100)
    significant_discrepancy
  })

output$histogram <- renderPlot({
    req(processed_data())
    plot_reconciliation_histogram(processed_data())
  })
  

  output$model_summary <- renderPrint({
    req(processed_data())
    model <- fit_regression_model(processed_data())
    summary(model)
  })

  output$avg_reconciliation <- renderPrint({
    req(processed_data())
    calculate_avg_reconciliation_difference(processed_data())
  })
}
calculate_avg_reconciliation_difference <- function(data) {
  mean(data$reconciliation_difference, na.rm = TRUE)
}

plot_reconciliation_histogram <- function(data) {
  hist(data$reconciliation_difference, 
       main = "Histogram of Reconciliation Difference", 
       xlab = "Reconciliation Difference", 
       col = "blue", 
       breaks = 20)
}

fit_regression_model <- function(data) {
  model <- lm(reconciliation_difference ~ total_sales + total_inventory_value, data = data)
  return(model)
}

shinyApp(ui = ui, server = server)