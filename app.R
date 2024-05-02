#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#install.packages("DBI")
#install.packages("RSQLite")

library(shiny)
library(DBI)
library(RSQLite)
library(lubridate)
library(plotly)
library(dplyr)
library(rsconnect)

con <- dbConnect(RSQLite::SQLite(), dbname ='final.db')
categories <- dbGetQuery(con, "SELECT DISTINCT cate FROM vendors WHERE cate NOT IN ('Home', 'Gifts & Donations', 'Personal', 'Automotive', 'Professional Services', 'Bills & Utilities')")
transactions <- dbGetQuery(con, "SELECT t.*, v.cate
FROM transactions AS t
JOIN vendors AS v ON t.vendor = v.vendor;")

#UI Section
ui <- fluidPage(

  numericInput("annual_income", "What's Your Annual Income:", value = 0),
  
  dateRangeInput("dates", "Select Timeframe",
                 start = "2023-01-01", end = "2023-12-31",
                 min = "2023-01-01", max = "2023-12-31"),
  checkboxGroupInput("cate", "Select Category", 
                     choices = c("All Categories", categories$cate),
                     selected = "All Categories"),
  textOutput("text"),
  
  textOutput("net"),
  
  textOutput("value"),
  
  plotlyOutput("plot")
)

server <- function(input, output, session) {
  #total spending hopefully
  tot_spending <- reactive({
    transactions %>%
      filter(tra_date >= input$dates[1] & tra_date <= input$dates[2]) %>%
      summarise(amount = sum(amount))
  })
  
  av_spending <- reactive({
    days <- as.numeric(difftime(as.Date(input$dates[2]), as.Date(input$dates[1]), units = "days")) + 1
    tot_spending() / days
  })
  
  net_inc <- reactive({
    days <- as.numeric(difftime(as.Date(input$dates[2]), as.Date(input$dates[1]), units = "days")) + 1
    (input$annual_income * days / 365) - tot_spending()
  })
  
  # Create a reactive function for plot data
  plot_data <- reactive({
    if ("All Categories" %in% input$cate) {
      transactions %>%
        filter(tra_date >= input$dates[1] & tra_date <= input$dates[2]) %>%
        group_by(tra_date) %>%
        summarise(amount = sum(amount))
    } else {
      transactions %>%
        filter(cate %in% input$cate) %>%
        filter(tra_date >= input$dates[1] & tra_date <= input$dates[2]) %>%
        group_by(tra_date) %>%
        summarise(amount = sum(amount))
    }
  })
  
  observeEvent(input$cate, {
    if ("All Categories" %in% input$cate) {
      # Uncheck other categories when "All Categories" is selected
      updateCheckboxGroupInput(session, "cate", selected = "All Categories")
    }
  })
  

  output$text <- renderText({
    paste("You are losing $", round(av_spending(), 2), "per day!")
  })
  output$net <- renderText({
    paste("Wow you saved: $", round(net_inc(), 2))
  })
  output$value <- renderText({
    gain <- net_inc() * (1.07) ** 100
    # Round gain to two decimal points
    rounded_gain <- round(gain, 2)
    
    # Format rounded_gain with commas every three numbers
    formatted_gain <- format(rounded_gain, big.mark = ",")
    
    paste("This becomes $", formatted_gain, "in 100 years!")
  })
  
  output$plot <- renderPlotly({
    # Create a scatter plot with hover text showing the amount
    p <- plot_ly(plot_data(), x = ~tra_date, y = ~amount,
                 type = "scatter", mode = "lines+markers",
                 text = ~paste("Expense: $", amount), 
                 hoverinfo = "text",
                 hovertemplate = "Date: %{x|%m-%d} <br> Amount: $%{y:.2f}",
                 name = "Expenses") %>%
      layout(title = "Total Expenses Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Amount ($)"))
    
    p
  })
  
  on.exit(dbDisconnect(con), add = TRUE)
}

shinyApp(ui, server)

#rsconnect::setAccountInfo(name='guanhchen', token='D5A4CBE6F0573259FB31D8527ACF7D50', secret='RsZa0hKMeEUZhfj28yVFQCia6mcJeVNwRNLiyt+4')