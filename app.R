library(tidyverse)
library(lubridate)
library(plotly)
library(shiny)
load("system.Rdata")

zscore_outlier <- function(x){
  y = which( ((x-mean(x))/sd(x)) < -3 | ((x-mean(x))/sd(x)) >3 )
  x[y] <- NA
  return(x)
  
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Animal systems data"),
  
  sidebarLayout(
    sidebarPanel(
      p("Data is gathered from the system monitors, compiled, and then uploaded to this app as often as possible."),
      br(),
      
      #select box - choose system
      #update: user can choose multiple systems
      selectInput(inputId = "system",
                  label = "choose system",
                  choices = names(systems),
                  multiple = TRUE,
                  selected = names(systems[1])
      ),
      
      #select box for parameters. choices by user selected system
      selectInput(inputId = "param", 
                  label = "Choose parameter", 
                  choices = NULL
      ),
      
      #date range input
      dateRangeInput('dateRange',
                     label = 'Choose date range',
                     start = Sys.Date() - 7, 
                     end = Sys.Date() + 1
      ),
      
      helpText("If nothing is displayed just pick an ealier date until you see something."),
      br(),
      
      #checkbox for average
      checkboxInput("checkbox_avg", label = "Show average over date range?", value = FALSE),
      br(),
      
      #remove outlier
      checkboxInput("checkbox_outlier", label = "Remove outliers?", value = FALSE),
      br(),
      
      #button to download 
      downloadButton("downloadData", "Download")
    ),
    
    #create area for plot and table
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotlyOutput("plot", width = "100%", height = "400px")),
                  tabPanel("Table", DT::dataTableOutput("table"))
      )
    )
  )
)

server <- function(input, output, session) {
  #reactive - select system
  #update: select system(s)
  which_sys <- reactive({
    if(is.null(input$system)){
      systems[1]
    }
    else
      systems %>%
      keep(names(.) %in% input$system)
    
    
    #pluck(input$system)
  })
  
  #update select input for paramters
  #update: retrieve names of parameters 
  observeEvent(which_sys(),{
    #choices <- names(which_sys())
    choices <- names(which_sys()[[1]])
    updateSelectInput(session, "param", choices = choices)
  })
  
  #reactive - filter user-selected date range from selected system. 
  #update: filter list of system(s), then combine, add column for system name.
  which_date <- reactive({
    map(which_sys(), filter, date >= input$dateRange[1] & date <= input$dateRange[2])%>%
      bind_rows(.id = "system")
    
  })
  
  outlier <- reactive({
    if(input$checkbox_outlier == FALSE) return(which_date())
    else {
      
      group_by(which_date(), system)%>%
        mutate(across(where(is.numeric), ~zscore_outlier(.x)))
      
    }
    
    
  })
  
  
  #create plot
  #update: line colors for different system
  output$plot <- renderPlotly({
    #this seems to get rid of warning message
    validate(need(input$param, "")
    )
    p = ggplot(outlier(), aes(.data$date, .data[[input$param]], group = 1, text = paste("Date:", format(date, "%a %b-%d %H:%M"),
                                                                                        "<br>", input$param, ":", .data[[input$param]]
    ),
    color = system
    )
    ) +
      geom_line() +
      labs(y = input$param, x = "Date")
    
    if(input$checkbox_avg)
      p = p + geom_smooth(aes(group = system))
    ggplotly(p, tooltip = "text")
    
  })
  
  #create table
  output$table <- DT::renderDT({
    t <- outlier()%>%
      arrange(desc(.data$date))
    t$date <- format(t$date, '%m-%d-%Y %H:%M')
    t
    
  })
  
  #download button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$system, input$dateRange[1], "-", input$dateRange[2], ".csv", sep = "")
    },
    content = function(file) {
      write.csv(which_date(), file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
