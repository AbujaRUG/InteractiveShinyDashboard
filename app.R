# install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
#library(shinyjs)
library(DT)
library(tidyverse)
library(plotly)
library(leaflet)  # interative mapping
library(tibbletime)

# Data Import

daily_updates <- read_csv("csv/ncdc-covid19-dailyupdates.csv")
molecular_labs <- read_csv("csv/ncdc-covid19-molecular-labs.csv")
states_daily_cases <- read_csv("csv/ncdc-covid19-states-daily-cases.csv")
states_daily_death <- read_csv("csv/ncdc-covid19-states-daily-deaths.csv")
states_daily_recovered <- read_csv("csv/ncdc-covid19-states-daily-recovered.csv")

covid19_by_states <- read_csv("csv/ncdc-covid19-states.csv")

# Data manipulation
states_daily_cases_modified <- states_daily_cases %>% 
    pivot_longer(cols = -Date, names_to = "States", values_to = "Counts") %>% 
    mutate(Date = as.Date(Date, "%Y-%m-%d"))

total_daily_cases <- sum(states_daily_cases_modified$Counts)

cases_str = paste("Total Cases by", max(states_daily_cases$Date),sep = " ")    

states_daily_death_modified <- states_daily_death %>% 
    pivot_longer(cols = -Date, names_to = "States", values_to = "Counts") %>%  
    mutate(Date = lubridate::parse_date_time(Date, "mdy"))


total_daily_death <- sum(states_daily_death_modified$Counts)

death_str = paste("Total Death by", max(states_daily_death_modified$Date),sep = " ")    


states_daily_recovered_modified <- states_daily_recovered %>% 
    pivot_longer(cols = -Date, names_to = "States", values_to = "Counts") %>% 
     
    mutate(Date = lubridate::parse_date_time(Date, "mdy"))


total_daily_recovered <- sum(states_daily_recovered_modified$Counts)

recovered_str = paste("Total Patients Recovered by", max(states_daily_recovered_modified$Date),sep = " ")    


daily_updates_by_states <- states_daily_cases_modified %>% 
    left_join(states_daily_death_modified, 
              by = c("Date" = "Date", 
                     "States" = "States")) %>% 
    left_join(states_daily_recovered_modified, 
              by = c("Date" = "Date", 
                     "States" = "States")) %>% 
    set_names("Date", "States", "Cases", "Deaths", "Recovered")

max_date <- max(daily_updates_by_states$Date)

Cases_df <- daily_updates_by_states %>% 
    group_by(States) %>% 
    summarise(across(.cols = Cases:Recovered,.fns = sum)) %>% 
    ungroup() %>% 
    mutate(States = States %>% fct_reorder(Cases))


body <- dashboardBody(
    h1("Nigeria COVID 19 Dashboard"),
    fluidPage(
        fluidRow(style = "background-color: #ffffff; height :20vh; padding: 1em 3em 1em 3em;",
                 column(3,uiOutput("boxCases")),
                 column(3,uiOutput("boxDeath")),
                 column(3,uiOutput("boxRecovered"))
        ),
        fluidRow(
            column(4,
                   # for selecting ONLY 1 date
                   # dateInput(
                   #     inputId = "select_date", 
                   #     label   = h4("Date Range"),
                   #     min     = min(daily_updates_by_states$Date), 
                   #     max     = Sys.Date(), 
                   #     startview = "month"),
                   
                   # Selecting range of dates
                   dateRangeInput(inputId = "date_range",
                                  label = h4("Select Date Range"),
                                  start = min(daily_updates_by_states$Date),
                                  end = max(daily_updates_by_states$Date),
                                  min = min(daily_updates_by_states$Date),
                                  max = max(daily_updates_by_states$Date),
                                  startview = "month"),
                   br(),
                   selectInput(inputId = "select_states", 
                               label = "Select State", 
                               choices = daily_updates_by_states$States,
                               selected = NULL,
                               multiple = TRUE),
                   br(),
                   actionButton(inputId = "apply",label = "Update")
            ),
            column(8,
                   fluidRow(
                       style = "background-color: #ffffff; height :55vh; padding: 1em 2em 1em 2em;",
                       column(12,
                                   leafletOutput("map"))
                   ),
                   fluidRow(
                       style = "background-color: #ffffff; height :55vh; padding: 1em 2em 1em 2em;",
                            column(12,
                                   DT::dataTableOutput("daily_updates_by_states_DT"))),
                   fluidRow(column(12, plotlyOutput("barplot")))
            )),
        
    )
)



ui <- dashboardPage(title = "Nigeria COVID 19 Dashboard",
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    body
)

server <- function(input, output, session) {
    
    data <- reactiveValues(
        daily_data = daily_updates_by_states
    )
    
    observeEvent(input$apply, {
        # Filter based on multiple States and range of dates
        data$daily_data <- as_tbl_time( daily_updates_by_states, index = Date) %>% 
            filter_time(time_formula = input$date_range[1] ~ input$date_range[2]) %>%
            filter(States %in% input$select_states) 
        
        # Filter based on multiple States and ONLY 1 selected date
        # data$daily_data <- daily_updates_by_states %>%     
        #     filter(States %in% input$select_states & Date == input$select_date)
            
        data   
    })
    
    output$boxCases <-  renderUI({
        valueBox(value = total_daily_cases,
                 subtitle = cases_str,
                 width = 12)
    })
    output$boxDeath <-  renderUI({
        valueBox(value = total_daily_death,
                 subtitle = death_str,
                 width = 12, color = "red")
    })
    output$boxRecovered <-  renderUI({
        valueBox(value = total_daily_recovered,
                 subtitle = recovered_str,
                 width = 15,color = "green")
    })
    
    output$map <- renderLeaflet({
        # Map -------------------------------------------------
        
        covid19_by_states %>% 
            leaflet::leaflet() %>%
            addTiles() %>%
            leaflet::addProviderTiles(providers$OpenStreetMap) %>% 
            leaflet::addAwesomeMarkers(
                popup = ~paste0(
                    "<h1>", covid19_by_states$ADMIN_NAME, "</h1>",
                    
                    "<table style='width:100%'>",
                    
                    "<tr>",
                    "<th>CASES</th>",
                    "<th>", covid19_by_states$CASES, "</th>",
                    "</tr>",
                    
                    "<tr>",
                    "<tr>",
                    "<th>DEATHS</th>",
                    "<th>", covid19_by_states$DEATHS, "</th>",
                    "</tr>",
                    
                    "<tr>",
                    "<tr>",
                    "<th>RECOVERED</th>",
                    "<th>", covid19_by_states$RECOVERED, "</th>",
                    "</tr>",
                    
                    "<tr>",
                    "<tr>",
                    "<th>ACTIVE</th>",
                    "<th>", covid19_by_states$ACTIVE, "</th>",
                    "</tr>",
                    
                    "<tr>",
                    "<tr>",
                    "<th>SCREENED</th>",
                    "<th>", covid19_by_states$SCREENED, "</th>",
                    "</tr>"
                )%>%
                    lapply(htmltools::HTML)  # end popup()
                
            ) %>%   # end addAwesomeMarkers()
            leaflet::addMeasure()
    })
    # 
    output$daily_updates_by_states_DT <- DT::renderDataTable({
        DT::datatable(data$daily_data, 
                      options = list(orderClasses = TRUE, pageLength = 5))
    })
    
    output$barplot <- renderPlotly({
       g <-  Cases_df %>% 
            ggplot(aes(States, Cases, fill= States))+
            geom_bar(stat = "identity")+
            coord_flip()
       
       ggplotly(g)
        
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
