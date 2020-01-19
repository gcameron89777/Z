# libraries and external files
library(tidyverse)
library(dbplyr)
library(odbc)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(lubridate)
library(DT)
library(scales)
library(plogr)
source("functions.R")



ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Ecommerce Dashboard"),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Channel Analysis", tabName = "channel", icon = icon("dashboard")),
                            menuItem("Funnel Analysis", icon = icon("th"), tabName = "funnel")
                        )
                    ),
                    dashboardBody(
                        tags$head(
                            tags$link(rel = "stylesheet", type = "text/css", href = "ecomstyles.css"),
                            
                            fluidRow(
                                
                            ),
                        ),
                        tabItems(
                            tabItem(tabName = "channel",
                                    
                                    # filters
                                    fluidRow(
                                        box(width = 3,
                                            title = NULL,
                                            uiOutput("channel_filter"),
                                            uiOutput("device_filter")
                                        ),
                                        box(width = 3,
                                            title = NULL,
                                            uiOutput("user_filter"),
                                            dateRangeInput("input_date", "Date Range", start = (Sys.Date()-31), end = (Sys.Date()-1), min = as.Date("2019-11-12"),
                                                           max = (Sys.Date()-1), format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                                           language = "en", separator = " to ", width = NULL)
                                        )
                                    ),
                                    
                                    h2("Channel Analysis"),
                                    
                                    # info boxes
                                    fluidRow(
                                        # A icons available here: http://fontawesome.io/icons/
                                        infoBoxOutput("SessionsBox", width = 3),
                                        infoBoxOutput("TransactionsBox", width = 3),
                                        infoBoxOutput("RevenueBox", width = 3),
                                        infoBoxOutput("ConversionRateBox", width = 3)
                                    ),
                                    
                                    # timeline metric selector
                                    fluidRow(
                                        box(width = 12,
                                            box(width = 2,
                                                
                                                # metric selections
                                                radioButtons(
                                                    inputId = "kpi_overlay",
                                                    "Overlay:",
                                                    c("Sessions",
                                                      "Transactions",
                                                      "Revenue")),
                                                
                                                # breakdown by selection
                                                selectInput(inputId = "breakdown", 
                                                            label =  "Breakdown", 
                                                            choices = c("Channel", "UserType", "Device"),
                                                            selected = c("Channel"), 
                                                            multiple = F) # There can be only one
                                            ),
                                            
                                            # primary visual timeline
                                            box(width = 10,
                                                title = "Timeline", 
                                                plotOutput("timeline")
                                            )
                                        )
                                    ),
                                    
                                    
                                    # bottom plots
                                    fluidRow(
                                        box(width = 4,
                                            #height = 375,
                                            title = "Revenue",
                                            plotOutput("revenue_channel")
                                        ),
                                        box(width = 4,
                                            #height = 375,
                                            title = "Transactions",
                                            plotOutput("transactions_channel")
                                        ),
                                        box(width = 4,
                                            #height = 375,
                                            title = "Conversion Rate",
                                            plotOutput("cr_channel")
                                        )
                                    ),
                                    fluidRow(
                                        box(width = 4,
                                            #height = 375,
                                            title = "Channel Spend",
                                            plotOutput("spend_channel")
                                        ),
                                        box(width = 4,
                                            #height = 375,
                                            title = "Cost Per Transaction (CPA)",
                                            plotOutput("cpa_channel")
                                        ),
                                        box(width = 4,
                                            #height = 375,
                                            title = "Cost Per Dollar (ROI)",
                                            plotOutput("roi_channel")
                                        )
                                    ),
                                    fluidRow(
                                        tabBox(width = 12,
                                               title = "Data Tables",
                                               id = "summary_tables",
                                               tabPanel("Sessions",
                                                        downloadButton('downloadSessions', 'Download Sessions'),
                                                        DT::dataTableOutput("sessions_table")
                                               ),
                                               tabPanel("Transactions",
                                                        downloadButton('downloadTransactions', 'Download Transactions'),
                                                        DT::dataTableOutput("transactions_table")
                                               ),
                                               tabPanel("Revenue",
                                                        downloadButton('downloadRevenue', 'Download Revenue'),
                                                        DT::dataTableOutput("revenue_table")
                                               )
                                        )
                                    )
                            ),
                            
                            tabItem(tabName = "funnel",
                                    h2("Funnel Analysis"),
                                    fluidRow(
                                        
                                        # Filters
                                        box(width = 3,
                                            title = "Filters",
                                            
                                            # min date for funnel is 12/28/19 since only added custom metric for zero priced products on the 27th
                                            dateRangeInput("fun_input_date", "Date Range", start = (Sys.Date()-31), end = (Sys.Date()-1), min = as.Date("2019-11-12"),
                                                           max = (Sys.Date()-1), format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                                           language = "en", separator = " to ", width = NULL),
                                            uiOutput("fun_channel_filter"),
                                            uiOutput("fun_device_filter"),
                                            uiOutput("fun_user_filter"),
                                            uiOutput("fun_zero_filter"), 
                                            uiOutput("contains_download_filter"),
                                            uiOutput("fun_elabel_filter"),
                                            
                                            # metrics
                                            # metric selections
                                            radioButtons(
                                                inputId = "funnel_metrics",
                                                "Metric:",
                                                c("Sessions",
                                                  "DailyUsers"))
                                        ),
                                        box(width = 9,
                                            title = "Funnel",
                                            plotOutput("funnel_plot")
                                            )
                                        )
                                    )
                            )
                        )
                    )

### SERVER ###

server <- function(input, output) {
    
    # add css
    addClass(selector = "body", class = "sidebar-collapse")
    
    # connection, data and data prep
    ## session data
    con <- dbConnect(odbc(), "PostgreSQL ANSI")
    
    ecom_channel_raw <- con %>% 
        tbl(in_schema("flagship_reporting", "ecom_dashboard")) %>% 
        collect() %>% 
        select(date, channel_grouping, device_category, user_type, daily_users, sessions, transactions, revenue) %>%
        group_by(date, channel_grouping, device_category, user_type) %>%
        summarise(DailyUsers = sum(daily_users), Sessions = sum(sessions), Transactions = sum(transactions), Revenue = sum(revenue)) %>%
        rename(Channel = channel_grouping,
               Device = device_category,
               UserType = user_type,
               Date = date) %>%
        mutate_at(vars(DailyUsers, Sessions, Transactions), as.numeric) %>%
        ungroup()
    
    
    ecom_channel <- reactive({
        ecom_channel_raw %>%
            filter(Date >= input$input_date[1] & Date <= input$input_date[2]) %>% 
            filter(Channel %in% input$channel_filter) %>%
            filter(Device %in% input$device_filter) %>%
            filter(UserType %in% input$user_filter) %>%
            group_by_("Date", input$breakdown) %>% 
            summarise_if(is.numeric, sum) %>% 
            ungroup()
        })

    # untrend channel data for full time frame plots
    untrended_data <- reactive({
        ecom_channel() %>%
        select(-Date) %>%
        group_by_(input$breakdown) %>%
        summarise_at(vars(DailyUsers, Sessions, Transactions, Revenue), sum)
    })
    
    
    ## funnel data
    funnel_data_raw <- con %>% 
        tbl(in_schema("flagship_reporting", "funnel_dashboard")) %>% 
        collect() %>% 
        filter(event_action != "removefromcart") %>% # not useful in viewing the funnel
        mutate_at(vars(sessions, daily_users), as.numeric) %>% 
        mutate(zero_val_product = as.character(zero_val_product)) %>%
        mutate(zero_val_product = if_else(zero_val_product == 1, T, F)) %>%
        mutate(download = if_else(download == 1, T, F)) %>%
        rename(Date = date,
               Channel = channel_grouping,
               Device = device_category,
               UserType = user_type,
               EventAction = event_action,
               EventLabel = event_label,
               ZeroValProduct = zero_val_product,
               ContainsDownload = download,
               Sessions = sessions,
               DailyUsers = daily_users)
        
    # funnel data reactive
    funnel_data <- reactive({
        funnel_data_raw %>% 
            filter(Date >= input$fun_input_date[1] & Date <= input$fun_input_date[2]) %>% 
            filter(Channel %in% input$fun_channel_filter) %>% 
            filter(Device %in% input$fun_device_filter) %>% 
            filter(UserType %in% input$fun_user_filter) %>% 
            filter(ZeroValProduct %in% input$fun_zero_filter) %>% 
            filter(ContainsDownload %in% input$contains_download_filter) %>% 
            filter(EventLabel %in% input$fun_elabel_filter) %>% 
            group_by(EventAction) %>% 
            summarise_at(vars(Sessions, DailyUsers), sum) %>% 
            ungroup()
    })
    
    # channel analysis tab
    source('channel_analysis_tab.R', local = T)
    
    # funnel analysis tab
    source('funnel_analysis_tab.R', local = T)
    
}

shinyApp(ui, server)