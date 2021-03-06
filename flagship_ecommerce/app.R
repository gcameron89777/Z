
# libraries and external files ----
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
library(forcats)
source("functions.R")
filter <- dplyr::filter


# Start UI ----
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Flagship Dashboard"),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Ecom Channel Analysis", tabName = "channel", icon = icon("dashboard")),
                            menuItem("Ecom Funnel Analysis", icon = icon("th"), tabName = "funnel"),
                            menuItem("Marketing Site Analysis", tabName = "marketing", icon = icon("dashboard"))
                        )
                    ),
                    dashboardBody(
                        tags$head(
                            tags$link(rel = "stylesheet", type = "text/css", href = "ecomstyles.css"),
                            
                            fluidRow(
                                
                            ),
                        ),
                        
                        
                        tabItems(
                            
                            # Channel analysis tab ----
                            tabItem(tabName = "channel",
                                    
                                    # filters
                                    fluidRow(
                                        box(width = 3,
                                            title = NULL,
                                            uiOutput("channel_filter"),
                                            uiOutput("gallery_filter"),
                                            uiOutput("device_filter")
                                        ),
                                        box(width = 3,
                                            title = NULL,
                                            uiOutput("user_filter"),
                                            uiOutput("date_range_filter")
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
                                                            choices = c("Channel", "GalleryInvite", "UserType", "Device"),
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
                            
                            # Funnel analysis tab ----
                            tabItem(tabName = "funnel",
                                    h2("Funnel Analysis"),
                                    fluidRow(
                                        
                                        # Filters
                                        box(width = 3,
                                            title = "Filters",
                                                
                                            # min date for funnel is 12/28/19 since only added custom metric for zero priced products on the 27th
                                            uiOutput("fun_date_range_filter"),
                                            uiOutput("fun_channel_filter"),
                                            uiOutput("fun_device_filter"),
                                            uiOutput("fun_user_filter"),
                                            uiOutput("gallery_invite_filter"), 
                                            uiOutput("contains_download_filter"),
                                            uiOutput("fun_elabel_filter"),
                                            
                                            # metrics
                                            # metric selections
                                            radioButtons(
                                                inputId = "funnel_metrics",
                                                "Metric:",
                                                c("Sessions",
                                                  "DailyUsers"),
                                                selected = "DailyUsers")
                                            ),
                                        box(width = 9,
                                            title = "Funnel",
                                            plotOutput("funnel_plot"))
                                        
                                        )
                                ),
                            
                        
                            # Marketing site tab
                            tabItem(tabName = "marketing",
                                    h2("Marketing Site Analysis"),
                                    fluidRow(
                                        
                                        # breakdown
                                        box(width = 3,
                                            title = NULL,
                                            checkboxGroupButtons(inputId = "mk_dims", label = "Select Breakdown:",
                                                                 choices = c("Source", "Medium", "Campaign", "Channel"),
                                                                 selected = "Channel",
                                                                 justified = T, status = "primary",
                                                                 direction = "vertical")
                                        ),

                                        # Filters
                                        box(width = 3,
                                            title = NULL,
                                            uiOutput("mk_date_range_filter"),
                                            uiOutput("mk_channel_filter")),
                                        box(width = 3,
                                            title = NULL,
                                            uiOutput("mk_device_filter"),
                                            uiOutput("mk_user_filter"))
                                        ),
                                    
                                    # Marketing site metrics trend tabs
                                    fluidRow(
                                        tabBox(width = 12,
                                               title = "Marketing Trends",
                                               id = "marketing_tables",
                                               tabPanel("Sessions",
                                                        downloadButton("AllSessionsDL","Download All Sessions"),
                                                        DT::dataTableOutput("marketing_data_sessions_trend")),
                                               tabPanel("NewSubscriptions",
                                                        downloadButton("AllSubscriptionsDL","Download All Subscriptions"),
                                                        DT::dataTableOutput("marketing_data_subscriptions_trend")),
                                               tabPanel("SubscriptionRevenue",
                                                        downloadButton("AllRevenueDL","Download All Revenue"),
                                                        DT::dataTableOutput("marketing_data_revenue_trend"))
                                               )
                                       ) # end fluid row for trend tables                                
                                    
                                    ) # end marketing site tab
                        ) # end tab items
                        ) # end dashboard body 
                    ) # end ui

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
        filter(device_category != "na") %>% # tiny amount, remove
        select(date, channel_grouping, gallery_invite, device_category, user_type, 
               daily_users, sessions, transactions, revenue) %>%
        group_by(date, channel_grouping, gallery_invite, device_category, user_type) %>%
        summarise(DailyUsers = sum(daily_users), Sessions = sum(sessions), Transactions = sum(transactions), Revenue = sum(revenue)) %>%
        rename(Channel = channel_grouping,
               GalleryInvite = gallery_invite,
               Device = device_category,
               UserType = user_type,
               Date = date) %>%
        mutate_at(vars(DailyUsers, Sessions, Transactions), as.numeric) %>%
        ungroup()
    
    
    ecom_channel <- reactive({
        req(input$input_date)
        ecom_channel_raw %>%
            filter(Date >= input$input_date[1] & Date <= input$input_date[2]) %>% 
            filter(Channel %in% input$channel_filter) %>%
            filter(GalleryInvite %in% input$gallery_filter) %>%
            filter(Device %in% input$device_filter) %>%
            filter(UserType %in% input$user_filter) %>%
            group_by_("Date", input$breakdown) %>% 
            summarise_if(is.numeric, sum) %>% 
            ungroup()
        })

    # untrend channel data for full time frame plots
    untrended_data <- reactive({
        req(input$breakdown)
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
        mutate(download = if_else(download == 1, T, F)) %>%
        mutate(event_action = factor(event_action, 
                                     levels = c("productdetails", "addtocart", "checkout - view cart", "checkout - view registration", "checkout - shipping address",
                                                "checkout - shipping method", "checkout - payment", "transaction")
                                     )) %>%
        rename(Date = date,
               Channel = channel_grouping,
               Device = device_category,
               UserType = user_type,
               EventAction = event_action,
               EventLabel = event_label,
               GalleryInvite = gallery_invite,
               ContainsDownload = download,
               Sessions = sessions,
               DailyUsers = daily_users)
        
    # funnel data reactive
    funnel_data <- reactive({
        
        req(input$fun_input_date)
        
        funnel_data_raw %>% 
            filter(Date >= input$fun_input_date[1] & Date <= input$fun_input_date[2]) %>% 
            filter(Channel %in% input$fun_channel_filter) %>% 
            filter(Device %in% input$fun_device_filter) %>% 
            filter(UserType %in% input$fun_user_filter) %>% 
            filter(GalleryInvite %in% input$gallery_invite_filter) %>% 
            filter(ContainsDownload %in% input$contains_download_filter) %>% 
            filter(EventLabel %in% input$fun_elabel_filter) %>% 
            group_by(EventAction) %>% 
            summarise_at(vars(Sessions, DailyUsers), sum) %>% 
            ungroup()
    })
    
    
    # Marketing site data ----
    marketing_data_raw <- con %>% 
        tbl(in_schema("flagship_reporting", "subscription_marketing_dashboard")) %>% 
        filter(device_category != 'na') %>% # very tiny single digit number don't know why not fighting this one
        rename(Date = date,
               Channel = channel_grouping,
               Medium = medium,
               Source = source,
               Campaign = campaign,
               Device = device_category,
               UserType = user_type,
               Sessions = sessions,
               Bounces = bounces,
               NewSubscriptions = new_subscriptions,
               SubscriptionRevenue = transaction_revenue) %>% 
            collect()
    

    
    marketing_data <- reactive({

        req(input$mk_date_range_filter)

        marketing_data_raw %>%
            filter(Date >= input$mk_date_range_filter[1] & Date <= input$mk_date_range_filter[2]) %>%
            filter(Channel %in% input$mk_channel_filter) %>%
            # filter(Source %in% input$mk_source_filter) %>%
            # filter(Medium %in% input$mk_medium_filter) %>%
            # filter(Campaign %in% input$mk_campaign_filter) %>%
            filter(Device %in% input$mk_device_filter) %>%
            filter(UserType %in% input$mk_user_filter) %>%
        group_by_at(vars(Date, input$mk_dims)) %>%
        summarise(Sessions = sum(Sessions),
                  Bounces = sum(Bounces),
                  NewSubscriptions = sum(NewSubscriptions),
                  SubscriptionRevenue = sum(SubscriptionRevenue)) %>%
        ungroup()
    })
        
    
    # channel analysis tab
    source('channel_analysis_tab.R', local = T)
    
    # funnel analysis tab
    source('funnel_analysis_tab.R', local = T)
    
    # marketing site analysis tab
    source('marketing_site_analysis_tab.R', local = T)
    
}

shinyApp(ui, server)