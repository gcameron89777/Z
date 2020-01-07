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
        tbl(in_schema("flagship_reporting", "ecom_channel")) %>% 
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
        tbl(in_schema("flagship_reporting", "ecom_funnel")) %>% 
        collect() %>% 
        mutate_at(vars(sessions, daily_users), as.numeric) %>% 
        mutate(zero_val_product = as.character(zero_val_product)) %>%
        mutate(zero_val_product = if_else(zero_val_product == 1, T, F)) %>%
        rename(Date = date,
               Channel = channel_grouping,
               Device = device_category,
               UserType = user_type,
               EventAction = event_action,
               EventLabel = event_label,
               ZeroValProduct = zero_val_product,
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
            filter(EventLabel %in% input$fun_elabel_filter) %>% 
            group_by(EventAction) %>% 
            summarise_at(vars(Sessions, DailyUsers), sum) %>% 
            ungroup()
    })
    
    # channel analysis tab
    source('channel_analysis_tab.R', local = T)
    
    # funnel analysis tab
    source('funnel_analysis_tab.R', local = T)
    
    
    ## Ecommerce Funnel Tab
    
    # make funnel df
    # lastmonth_funnel <- filter(dataset, Month == last_month) %>% select(Channel:Transactions) %>%
    #     mutate(Checkout = ceiling(Transactions * (1 + abs(randwalk(0.2, n = 6, mean = 0, sd = 0))))) %>%
    #     mutate(ShippingDetails = ceiling(Checkout * (1 + abs(randwalk(0.6, n = 6, mean = 0, sd = 0))))) %>%
    #     mutate(Registrations = ceiling(ShippingDetails * (2 + abs(randwalk(0.6, n = 6, mean = 0, sd = 0))))) %>%
    #     mutate(AddToCart = ceiling(Registrations * (2 + abs(randwalk(0.6, n = 6, mean = 0, sd = 0))))) %>%
    #     select(Channel:Sessions, AddToCart:Checkout, Transactions)
    
    # make a reactive version of last months data
    # create reactive set of data with filters applied by user
    # filtered_funnel <-  reactive({ lastmonth_funnel %>%
    #         filter(input$channel == "All" | Channel == input$channel) %>%
    #         filter(input$promo == "All Transactions" | Promo == input$promo) %>%
    #         {if(input$promo != "All Transactions") select(., c(Channel, Promo, ShippingDetails, Checkout:Transactions)) else .} %>%
    #         gather(Funnel, Sessions, -Channel, -Promo) %>%
    #         group_by(Channel, Promo, Funnel) %>%
    #         summarise(Sessions = sum(Sessions)) %>%
    #         gather(key, value, -Channel, -Promo, - Funnel) %>%
    #         group_by(Funnel) %>%
    #         summarise(Sum = sum(value)) %>%
    #         arrange(-Sum) %>%
    #         mutate(End = lag(Sum),
    #                xpos = 1:n() - 0.5,
    #                Diff = End - Sum,
    #                Percent = paste("-", round(Diff / End * 100, 1), "%"))
    # })
    
    
    # funnel bar blot
    # output$funnel_plot <- renderPlot({
    #     
    #     ggplot(filtered_funnel(), aes(x = reorder(Funnel, -Sum), y = Sum)) +
    #         geom_col(alpha = 0.6, fill = "#008080") +
    #         stat_summary(aes(label = scales::comma(..y..)), fun.y = 'sum',
    #                      geom = 'text', col = 'white', vjust = 1.5) +
    #         geom_segment(aes(x = xpos, y = End, xend = xpos, yend = Sum)) +
    #         geom_text(aes(x = xpos, y =  End - Diff / 2, label = Percent), hjust = -0.2, vjust = -1) +
    #         theme(axis.title.x = element_blank(),
    #               axis.title.y = element_blank()) +
    #         scale_y_continuous(labels = function(l) {l = l / 1000; paste0(l, "K")}) +
    #         
    #         # adds dummy bars for negative y axis values to show more green on the labels so not squished
    #         geom_bar(data = data.frame(x = filtered_funnel()$Funnel, y = -(sum(filtered_funnel()$Sum) * 0.02)), 
    #                  aes(x, y),
    #                  stat = "identity", position = "dodge",
    #                  alpha = 0.6, fill = "#008080") +
    #         stat_summary(aes(label = scales::comma(..y..)), fun.y = 'sum', 
    #                      geom = 'text', col = 'white', vjust = 1.5) +
    #         theme(axis.title.x = element_blank(),
    #               axis.title.y = element_blank())
    
}

shinyApp(ui, server)