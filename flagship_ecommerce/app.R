# libraries and external files
library(tidyverse)
library(dbplyr)
library(odbc)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(lubridate)
library(DT)
library(scales)
library(plogr)
source("queries.R")
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
                            tags$link(rel = "stylesheet", type = "text/css", href = "ecomstyles.css")
                        ),
                        tabItems(
                            tabItem(tabName = "channel",
                                    h2("Channel Analysis"),
                                    fluidRow(
                                        # A icons available here: http://fontawesome.io/icons/
                                        infoBoxOutput("SessionsBox", width = 3),
                                        infoBoxOutput("TransactionsBox", width = 3),
                                        infoBoxOutput("RevenueBox", width = 3),
                                        infoBoxOutput("ConversionRateBox", width = 3)
                                    ),
                                    fluidRow(
                                        box(width = 12,
                                            box(width = 2,
                                                radioButtons(
                                                    inputId = "kpi_overlay",
                                                    "Overlay:",
                                                    c("Sessions",
                                                      "Transactions",
                                                      "Revenue"))
                                            ),
                                            # primary visual timeline
                                            box(width = 10,
                                                title = "Timeline", 
                                                plotOutput("timeline")
                                            )
                                        )
                                    ),
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
                                    h2("Funnel Analysis (Last Month)"),
                                    fluidRow(
                                        box(width = 3,
                                            title = "Filters",
                                            selectInput("channel",
                                                        "Channel", 
                                                        c("All", "Facebook", "Youtube", "SEM", "Organic", "Direct", "Email"), selected = "All"),
                                            
                                            selectInput("promo",
                                                        "Promo Offer (Only valid after registration step)", 
                                                        c( "All Transactions", "None", "Partner Offer", "Print Code", "Affiliate Promo"), selected = "All Transactions")
                                            
                                        ),
                                        box(width = 9,
                                            title = "Funnel",
                                            plotOutput("funnel_plot"))
                                    )
                            )
                        )
                    )
)

### SERVER ###

server <- function(input, output) {
    
    # add css
    addClass(selector = "body", class = "sidebar-collapse")
    
    # connection and data
    con <- dbConnect(odbc(), "PostgreSQL ANSI")
    #session_data <- dbGetQuery(con, sessions_trend_query)
    session_data <- readRDS("session_data.rds")
    
    # ecom_funnel <- con %>% 
    #     tbl(in_schema("flagship_reporting", "ecom_funnel")) %>% 
    #     select(-users) %>% 
    #     collect() %>% 
    #     mutate_at(vars(sessions, daily_users, bounces, transactions, revenue), as.numeric) %>% 
    #     mutate(revenue = round(revenue, 2))
    
    dat.trended <- session_data %>%
        filter(date >= (Sys.Date()-31) & date <= (Sys.Date()-1)) %>% # default 30 day trend
        select(date, channel_grouping, daily_users, sessions, transactions, revenue) %>%
        group_by(date, channel_grouping) %>%
        summarise(DailyUsers = sum(daily_users), Sessions = sum(sessions), Transactions = sum(transactions), Revenue = sum(revenue)) %>% 
        rename(Channel = channel_grouping) %>% 
        mutate_at(vars(DailyUsers, Sessions, Transactions), as.numeric) %>% 
        ungroup()

    
    # last 30 days data as a whle, not split by date
    last_30days_data <- dat.trended %>% 
        filter(date >= (Sys.Date()-31) & date <= (Sys.Date()-1)) %>% # default 30 day trend
        select(-date) %>% 
        group_by(Channel) %>% 
        summarise_at(vars(DailyUsers, Sessions, Transactions, Revenue), sum)
    
    ## Info boxes
    output$SessionsBox <- renderInfoBox({
        infoBox(
            "Sessions", format(sum(dat.trended$Sessions), big.mark = ","), icon = icon("bar-chart"), color = "teal"
        )
    })
    
    output$TransactionsBox <- renderInfoBox({
        infoBox(
            "Transactions", format(sum(dat.trended$Transactions), big.mark = ","), icon = icon("bar-chart"), color = "teal"
        )
    })
    
    output$RevenueBox <- renderInfoBox({
        infoBox(
            "Revenue", paste0("$",format(sum(dat.trended$Revenue), big.mark = ",")), icon = icon("dollar"), color = "teal"
        )
    })
    
    output$ConversionRateBox <- renderInfoBox({
        infoBox(
            "Conversion Rate", paste0(round(sum(dat.trended$Transactions) / sum(last_30days_data$Sessions), 2) * 100, "%"), icon = icon("line-chart"), color = "teal"
        )
    })
    
    ##  Timeline Plot
    output$timeline <- renderPlot({
        ggplot(dat.trended, aes_string(x = "date", y = input$kpi_overlay, fill = "Channel", group = "Channel")) +
            geom_area(alpha = 0.3) +
            stat_summary(aes(group = 2), fun.y = sum, geom = 'line', size = 2, alpha = 0.5) +
            theme(axis.text.x = element_text(angle=90, hjust=1),
                  axis.title.x = element_blank()) +
            
            scale_y_continuous(labels = function(l) {
                if(input$kpi_overlay == "Revenue") {
                    paste0("$", format(l, big.mark = ",", format = "f", digits = 2, scientific = F))
                } else {
                    scales::comma(l)}
            })
    })
    
    ## Plots
    # revenue plot
    output$revenue_channel <- renderPlot({
        ggplot(last_30days_data, aes(x = reorder(Channel, Revenue), y = Revenue), label = Revenue) +
            geom_bar(stat="identity", fill = "#008080", alpha = 0.6) +
            coord_flip() +
            geom_text(aes(label = paste0("$", format(Revenue, big.mark = ","))), color = "white", hjust= 1.2) +
            scale_y_continuous(labels = function(l) {paste0("$", format(l, big.mark = ","))}) +
            xlab("")
    }, height = 300)
    
    # transactions plot
    output$transactions_channel <- renderPlot({
        ggplot(last_30days_data, aes(x = reorder(Channel, Transactions), y = Transactions), label = Transactions) +
            geom_bar(stat="identity", fill = "#008080", alpha = 0.6) +
            coord_flip() +
            geom_text(aes(label = format(Transactions, big.mark = ",")), color = "white", hjust= 1.2) +
            xlab("")
    }, height = 300)
    
    # conversion rate plot
    output$cr_channel <- renderPlot({
        ggplot(last_30days_data, aes(x = reorder(Channel, Transactions / Sessions), y = Transactions / Sessions), label = Transactions / Sessions) +
            geom_bar(stat="identity", fill = "#008080", alpha = 0.6) +
            coord_flip() +
            geom_text(aes(label = paste0(formatC(Transactions / Sessions * 100, format = "f", digits = 1), "%")), color = "white", hjust= 1.2) +
            scale_y_continuous(labels = function(l) {paste0(formatC(l * 100, format = "f", digits = 1), "%")}) +
            xlab("")
    }, height = 300)
    
    
    ## Summary KPI tables
    # sessions table
    sessions_table <- metricTables(dat.trended, Channel, Sessions)
    output$sessions_table <- DT::renderDataTable(DT::datatable({
        sessions_table
    }, options = list(dom = 't', bPaginate = FALSE, sScrollX = "100%"))
    )
    output$downloadSessions <- downloadTable("sessions", sessions_table)

    # transactions table
    transactions_table <- metricTables(dat.trended, Channel, Transactions)
    output$transactions_table <- DT::renderDataTable(DT::datatable({
        transactions_table
    }, options = list(dom = 't', bPaginate=FALSE, sScrollX="100%"))
    )
    output$downloadTransactions <- downloadTable("transactions", transactions_table)

    # revenue table
    revenue_table <- metricTables(dat.trended, Channel, Revenue) %>% mutate_at(vars(-Channel), scales::dollar)
    output$revenue_table <- DT::renderDataTable(DT::datatable({
        revenue_table},
        options = list(dom = 't', bPaginate=FALSE, sScrollX="100%"))
    )
    output$downloadRevenue <- downloadTable("revenue", revenue_table)

    
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
    # })
    
}

shinyApp(ui, server)