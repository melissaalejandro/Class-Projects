library(shiny)
library(tidyverse)
library(plotly)
library(shinythemes)
load(url("https://github.com/bandcar/Unemployment-Rate-Pre-and-Post-Covid/blob/main/ue_wider.RData?raw=true"))

# pivot data to long format
q_long <- q %>%
  pivot_longer(cols = -Year, names_to = "State", values_to = "unemployment")
q_long <- q_long %>%   mutate(State = str_replace(State, '\\.', ' '))

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage( "Unemployment Rate Comparison Tool",
              tabPanel("Interactive Graph",
                       titlePanel("US Unemployment Rates Before and After COVID-19"),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(
                             inputId = "y",
                             label = "Select State(s) to Graph",
                             choices = unique(q_long$State),
                             selected = "United States",
                             multiple = TRUE
                           ), # select input end
                           radioButtons(
                             inputId = "x",
                             label = "Displaying Unemployment Rates for 2013-2022",
                             choices = c("Year"),
                             selected = "Year",
                           ), # Radio buttons end
                           actionButton("run_plot", "Run Plot"),
                           actionButton("reset", "Clear Output"),
                         ), # side bar panel end
                         mainPanel(
                           span(strong("Compare State Unemployment Rates Pre and Post COVID.", style = "color:black"),style = "font-si16pt"),
                           div("Select the state(s) you wish to view from the drop down menu. Once you have made your selections, click \"Run Plot\".", style = "color:black"),
                           br(),
                           plotlyOutput(outputId = "graph"),
                           em("Source: bls.gov"),
                           br(), br(),
                           div("The vertical black line over the year 2020 reflects the year of the pandemic. Notice the spike in unemployment compared to the prior year. In 2019, unemployment in the U.S. was at 3.5% and reached a staggering 7.4% in 2020.", style = "color:black"),
                           p("Interestingly, the unemployment rate dropped by 35% the following year. As of March 2022, the U.S. unemployment rate is almost at pre-COVID levels; 3.8%. This is a remarkable recovery!",style = "color:black"),
                           p(span(strong("Note:")), "The United States unemployment rate shown here does not include Puerto Rico or the District of Columbia and will therefore differ from the unemployment rate reported by the BLS.",style = "color:black"),
                           br(),
                         ) # Main panel end
                       ) # select input end
              ), #navbar interactive graph
              tabPanel("Data", DT::dataTableOutput(outputId="datasheet")),# tabPanel 1 end
              tabPanel("Code", HTML("<p>If you would like to view the code for this project, please visit my <a href='https://github.com/bandcar/Unemployment-Rate-Pre-and-Post-Covid/blob/main/app.r'>github repository</a>!</p>"))
  ) #Navbar end
) # fluid page end 

server <- function(input, output) {
  
  Plot <- reactiveVal()
  
  q_filtered <- eventReactive(input$run_plot, {
    filter(q_long, State %in% input$y)
  })
  
  observe({
    gg <- ggplot(q_filtered(), aes(x = .data[[input$x]], y = unemployment, color = State)) + geom_point() + geom_line() + geom_vline(aes(xintercept = 2020)) + scale_x_continuous(breaks = q$year)
    Plot(gg)
  })
  
  observeEvent(input$reset, {
    Plot(plotly_empty())
  })
  
  output$graph <- renderPlotly({
    Plot()  
  }) 
  
  output$datasheet <- DT::renderDataTable({
    DT::datatable(data=q,
                  rownames=FALSE)}
  )
} # server end

shinyApp(ui = ui, server = server)
