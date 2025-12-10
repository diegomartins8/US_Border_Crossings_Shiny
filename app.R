#Libraries.
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(DT)
library(scales)
library(lubridate)
library(readr)
library(janitor)

#Loading dataset.
border_data <- read_csv("Border_Crossing_Entry_Data.csv")

#Clean field names, data formatting (mdy), numeric year field.
border_data <- border_data %>%
  clean_names() %>%
  mutate(date = mdy(paste0("01-", date)), year = year(date))


#Dashboard UI.
ui <- dashboardPage(
  #Header.
  dashboardHeader(title = "U.S. Border Crossings Analytics"),
  #Sidebar.
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-column")),
      menuItem("State Trends", tabName = "state", icon = icon("chart-line")),
      menuItem("Port Insights", tabName = "ports", icon = icon("map")))),
  
  #Body with three tabs.
  dashboardBody(
    tabItems(
      
      #Overview Tab/Page.
      tabItem(tabName = "overview",
              fluidRow(
                #Cards/Boxes for dataset stats.
                valueBox(formatC(nrow(border_data), format="d", big.mark=","), "Total Records", color = "light-blue", icon = icon("database")),
                valueBox(formatC(length(unique(border_data$state)), format="d", big.mark=","), "Border States", color = "green", icon = icon("flag")),
                valueBox(formatC(length(unique(border_data$port_name)), format="d", big.mark=","), "Total Border Ports", color = "purple", icon = icon("road"))),
              
              #Filter for measures and year ranges.
              box(width = 12, title = "Filter Selection", status = "primary", solidHeader = TRUE,
                  #Dropdown.
                  selectInput("measure", "Select Measure:", choices = unique(border_data$measure)),
                  #Slider.
                  sliderInput("yearRange", "Select Year Range:",
                              min = min(border_data$year, na.rm = TRUE), max = max(border_data$year, na.rm = TRUE),
                              value = c(min(border_data$year, na.rm = TRUE), max(border_data$year, na.rm = TRUE)), sep = "")),
              #Plot and table. 
              fluidRow(
                box(width = 6, plotlyOutput("overview_plot")),
                box(width = 6, DTOutput("overview_table")))),
      
      
      #State Yrends Tab.
      tabItem(tabName = "state",
              fluidRow(
                #Filter for state and measure. 
                box(width = 4, title = "Filter by State & Measure", status = "primary", solidHeader = TRUE,
                    selectInput("stateInput", "Select State:", choices = unique(border_data$state)),
                    selectInput("measureInput", "Select Measure:", choices = unique(border_data$measure))),
                #Plot based on chosen inputs.
                box(width = 8, plotlyOutput("state_trend_plot")))),
      
      #Port Insights Tab.
      tabItem(tabName = "ports",
              fluidRow(
                #Filter by state for port hierarchy.
                box(width = 4, title = "Select State", status = "primary", solidHeader = TRUE,
                    selectInput("portState", "Select State:", choices = unique(border_data$state))),
                #Plot beside filter box.
                box(width = 8, plotlyOutput("port_plot"))),
              #Table for port stats.
              fluidRow(
                box(width = 12, DTOutput("port_table"))))
    )
  )
)


#############################################################
#Server
#############################################################

#Server.
server <- function(input, output, session) {
  
  #Overview tab.
  #Reactive filters chosen measure and year ranges.
  #Reactives update when input received. 
  filtered_data <- reactive({
    border_data %>%
      filter(measure == input$measure, year >= input$yearRange[1], year <= input$yearRange[2])})
  
  #Renders plot.
  output$overview_plot <- renderPlotly({
    req(filtered_data())
    #Group bar chart.
    plot <- ggplot(filtered_data(), aes(x = year, y = value, fill = border)) +
      #bars aligned for bordrs and commas added.
      geom_col(position = "dodge") + scale_y_continuous(labels = comma) +
      labs(title = paste("Total Border Crossings for", input$measure), x = "Year", y = "Total Crossings") +
      theme_minimal()
    #Interactive Plotly version of ggplot.
    ggplotly(plot)})
  
  #Table for overview tab.
  output$overview_table <- renderDT({
    filtered_data() %>%
      #Summarizes year and border.
      group_by(year, border) %>%
      summarise(Total = sum(value, na.rm = TRUE), .groups = "drop") %>%
      #Desc total.
      arrange(desc(Total)) %>%
      mutate(Total = formatC(Total, format="d", big.mark=",")) %>%
      #10 rows per page.
      datatable(options = list(pageLength = 10))})
  
  
  #State Trends tab.
  #Reactive dataset.
  state_data <- reactive({
    border_data %>%
      #Input state and measure.
      filter(state == input$stateInput, measure == input$measureInput)})
  
  #Renders time series trent plot for chosen input.
  output$state_trend_plot <- renderPlotly({
    req(state_data())
    plot <- ggplot(state_data(), aes(x = date, y = value, color = border)) +
      #Thicker trend lines.
      geom_line(linewidth = 1.1) +
      #Commas on y-axis.
      scale_y_continuous(labels = comma) +
      labs(title = paste("Trend of", input$measureInput, "in", input$stateInput), x = "Date", y = "Crossings") +
      theme_minimal()
    ggplotly(plot)})
  
  
  #Port Insights tab.
  #Reactive dataset for ports by state.
  port_data <- reactive({
    border_data %>%
      #By chosen state.
      filter(state == input$portState) %>%
      #Aggregates by port name.
      group_by(port_name) %>%
      summarise(total_crossings = sum(value, na.rm = TRUE), .groups = "drop") %>%
      #Lists by volume.
      arrange(desc(total_crossings))})
  
  #Renders bar chart for top ports from input.
  output$port_plot <- renderPlotly({
    req(port_data())
    plot <- ggplot(port_data(), aes(x = reorder(port_name, total_crossings), y = total_crossings)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      #Commas.
      scale_y_continuous(labels = comma) +
      labs(title = paste("Top Ports in", input$portState), x = "Port Name", y = "Total Crossings") +
      #Lighter theme.
      theme_light()
    ggplotly(plot)})
  
  #Renders interactive table for port crossings.
  output$port_table <- renderDT({
    port_data() %>%
      mutate(total_crossings = formatC(total_crossings, format="d", big.mark=",")) %>%
      datatable(options = list(pageLength = 10))})
}

#Runs the Shiny app.
shinyApp(ui, server)