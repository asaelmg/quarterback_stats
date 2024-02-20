library(shiny)
library(shinydashboard)
library(magrittr)
library(dplyr)
library(ggplot2)
library(rsconnect)
library(xfun)
library(plotly)
library(DT)
library(stringr)                          
library(purrr)                            
library(rlang)                           
library(r2d3)                            
library(nycflights13)
library(knitr)

qb_stats <- read.csv("Quarterback_Stats.csv", stringsAsFactors = F,header = TRUE)
qb_stats$Current_Team <- as.factor((qb_stats$Current_Team))
qb_stats$Players <- as.factor((qb_stats$Player))
qb_stats <- arrange(qb_stats, qb_stats$Year, qb_stats$Players)


ui <- dashboardPage(title = 'Title Goes Here', 
                    
                    header <- dashboardHeader(title = "Quarterback Stats Dashboard"), 
                    
                    sidebar <- dashboardSidebar(
                      
                      width = 250,
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("home"))
                                 ),
                      
                      selectInput(
                        inputId = "Players",
                        label = "Players",
                        choices = unique(c(qb_stats$Players)),
                        selected = "Russell Wilson",
                        selectize = FALSE
                                 ),

                      selectInput(
                        inputId = "Years",
                        label = "Years",
                        choices = unique(c(qb_stats$Year)),
                        selected = "2023",
                        selectize = FALSE
                      ),
                      
                      
                      downloadButton(outputId = "download_data", label = "Download", width = 4)
                      
                                               ), 
                    

                    
                    # The body of the dashboard
                    body <- dashboardBody(
                      tabsetPanel(id = "tabs",
                                  tabPanel(title = "Main Dashboard",
                                           value = "page1",
                                           fluidRow(
                                             valueBoxOutput("value1"),
                                             valueBoxOutput("value2"),
                                             valueBoxOutput("value3")
                                           ),
                                           fluidRow(
                                             column(width = 6, plotlyOutput("TouchdownsbyQtr")),
                                             column(width = 6, plotlyOutput("YardsbyQtr"))
                                           ),
                                           fluidRow(
                                             DT::dataTableOutput("data_table")  # Add the data table output here
                                           )
                                  )
                      )
                    )
                    , skin='blue'
                    
                  )




server <- function(input, output) {
  
  base_stats <- reactive({
    res <- qb_stats %>% 
      filter(Players == input$Players)
    if (input$Players != "") res <- filter(res, Players == input$Players)
    res
  })
  
  base_data <- reactive({
    res <- qb_stats %>% 
      filter(Players == input$Players, Year == input$Years)
    if (input$Players != "") res <- filter(res, Players == input$Players)
    res
  })
  
  output$value1 <- renderValueBox({
    base_data() %>%
      group_by(Players, Year) %>%
      summarise(value = sum(TD)) %>%
      pull() %>%
      as.integer() %>%
      valueBox(icon = icon("table"), color = "light-blue", subtitle = "Touchdowns") 
  })
  
  output$value2 <- renderValueBox({
    base_data() %>%
      group_by(Players, Year) %>%
      summarise(value = sum(Yds)) %>%
      pull() %>%
      as.integer() %>%
      valueBox(icon = icon("table"), color = "purple", subtitle = "Yards") 
  })
  
  output$value3 <- renderValueBox({
    base_data() %>%
      group_by(Players, Year) %>%
      summarise(value = sum(Sk)) %>%
      pull() %>%
      as.integer() %>%
      valueBox(icon = icon("table"), color = "teal", subtitle = "Sacks") 
  })
  
  output$TouchdownsbyQtr <- renderPlotly({
    ggplot(data = base_stats(), aes(x = Year)) +
      geom_line(aes(y = `Cmp.`, color = "Completion Percentage"), size = 1.5) +
      geom_point(aes(y = `Cmp.`, color = "Completion Percentage")) +
      geom_line(aes(y = QBR, color = "QBR"), size = 1.5) +
      geom_point(aes(y = QBR, color = "QBR")) +
      ylab("Percentage / Rating") +
      xlab("Year") +
      theme(legend.position = "bottom", 
            plot.title = element_text(size = 15, face = "bold")) +
      ggtitle("Completion Percentage and QBR by Year") +
      scale_color_manual(values = c("Completion Percentage" = "blue", "QBR" = "magenta")) +
      labs(color = "Metric")
  })
  
  output$YardsbyQtr <- renderPlotly({
    ggplot(data = base_stats(), aes(x = Year)) +
      geom_bar(aes(y = Yds, fill = factor(Tm)), position = "dodge", stat = "identity") +
      scale_y_continuous(name = "Yards") +  # Only primary Y axis for Yds
      ylab("Yards") +
      xlab("Year") +
      theme(legend.position = "bottom", 
            plot.title = element_text(size = 15, face = "bold")) +
      ggtitle("Yards by Quarterback") +
      labs(fill = "Tm")
  })
  
  output$download_data <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      data <- base_stats()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$data_table <- DT::renderDataTable({
    base_data()
  })
}

shinyApp(ui, server)
