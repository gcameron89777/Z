# Marketing Site Tab ----
# Filters, render here in server so can refer to data in server in the output ui's

## Date
output$mk_date_range_filter <- renderUI({
  mindate <- max(marketing_data_raw$Date) - 30
  maxdate <- max(marketing_data_raw$Date)
  dateRangeInput("mk_date_range_filter", "Date Range", start = mindate, end = maxdate, min = as.Date("2019-11-12"),
                 max = maxdate, format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                 language = "en", separator = " to ", width = NULL)
})


## Channel
output$mk_channel_filter <- renderUI({
  channel_options <- unique(marketing_data_raw$Channel) %>% sort()
  pickerInput(inputId = "mk_channel_filter",
              label = "Channel",
              choices = channel_options,
              selected = channel_options,
              multiple = T,
              options = list(`actions-box` = T)
  )
})


## Device
output$mk_device_filter <- renderUI({
  device_options <- unique(marketing_data_raw$Device) %>% sort()
  pickerInput(inputId = "mk_device_filter",
              label = "Device",
              choices = device_options,
              selected = device_options,
              multiple = T,
              options = list(`actions-box` = T)
  )
})


## UserType
output$mk_user_filter <- renderUI({
  user_options <- unique(marketing_data_raw$UserType) %>% sort()
  pickerInput(inputId = "mk_user_filter",
              label = "UserType",
              choices = user_options,
              selected = user_options,
              multiple = T,
              options = list(`actions-box` = T)
  )
})



# Marketing analysis data wrangling ----
marketing_data_base <- reactive({
  marketing_data() %>%
  mutate(Date = ordered(format(Date, "%d-%b"), levels = format(sort(unique(Date)), "%d-%b"))) %>%
  group_by_at(vars(Date, input$mk_dims)) %>%
  summarise(Sessions = sum(Sessions),
            Bounces = sum(Bounces),
            NewSubscriptions = sum(NewSubscriptions),
            SubscriptionRevenue = sum(SubscriptionRevenue)) %>%
  pivot_longer(cols = -c(Date, input$mk_dims), names_to = 'Key', values_to = 'value')
})


# Metric trend tables ----
## Apply t all DTs

# apply the same options to all 3 DTs
marketing_dt_options <- reactive({
  list(pageLength=100, scrollX=T, autoWidth = T, 
       dom='tlipr',
       lengthMenu = c(10, 100, 1000, 10000),
       deferRender = T, 
       scrollY = 200, 
       scrollX=T, 
       autoWidth = T, 
       scroller = T,
       fixedColumns = list(leftColumns = length(input$mk_dims) + 1),
       columnDefs = list(list(width = '150px', targets = 1:length(input$mk_dims)))
       )
  })


## sessions
marketing_data_sessions_trend <- reactive({
  
  extable <- marketing_data_base() %>%
    dplyr::filter(Key == "Sessions") %>%
    pivot_wider(names_from = Date, values_from = value) %>%
    select(-Key) %>%
    replace(is.na(.), 0) %>%
    ungroup() %>%
    add_column(Total = rowSums(select(., -input$mk_dims), na.rm = T)) %>%
    select_at(vars(input$mk_dims, Total, everything())) %>%
    dplyr::filter(Total > 0) %>%
    arrange(desc(Total))
  
  if(length(input$mk_dims) > 0) {
    csums <- colSums(select(extable, -input$mk_dims), na.rm = T)
    table_top <- map_dfc(csums, ~.) %>% mutate(!! sym(head(input$mk_dims, 1)) := "Total")
    union_all(table_top, extable) %>% select(names(extable)) %>% 
      mutate_at(vars(-input$mk_dims), scales::comma)
    } else {
      extable 
      }
})

output$marketing_data_sessions_trend <- DT::renderDataTable(DT::datatable(
  {marketing_data_sessions_trend()}, 
  filter = 'top', 
  extensions = c('Scroller'),
  options = marketing_dt_options))
output$AllSessionsDL <- downloadTable("download_sessions", marketing_data_sessions_trend())


## NewSubscriptions
marketing_data_subscriptions_trend <- reactive({
  extable <- marketing_data_base() %>%
    dplyr::filter(Key == "NewSubscriptions") %>%
    pivot_wider(names_from = Date, values_from = value) %>%
    select(-Key) %>%
    replace(is.na(.), 0) %>%
    ungroup() %>%
    add_column(Total = rowSums(select(., -input$mk_dims), na.rm = TRUE)) %>%
    select_at(vars(input$mk_dims, Total, everything())) %>%
    dplyr::filter(Total > 0) %>%
    arrange(desc(Total))
  
  if(length(input$mk_dims) > 0) {
    csums <- colSums(select(extable, -input$mk_dims), na.rm = T)
    table_top <- map_dfc(csums, ~.) %>% mutate(!! sym(head(input$mk_dims, 1)) := "Total")
    union_all(table_top, extable) %>% select(names(extable)) %>% 
      mutate_at(vars(-input$mk_dims), scales::comma)
  } else {
    extable 
  }
  
  
})

output$marketing_data_subscriptions_trend <- DT::renderDataTable(DT::datatable(
  {marketing_data_subscriptions_trend()}, 
  filter = 'top', 
  extensions = c('Scroller'), 
  options = marketing_dt_options))
output$AllSubscriptionsDL <- downloadTable("download_subscriptions", marketing_data_subscriptions_trend())


## SubscriptionRevenue
marketing_data_revenue_trend <- reactive({
  extable <- marketing_data_base() %>%
    dplyr::filter(Key == "SubscriptionRevenue") %>%
    pivot_wider(names_from = Date, values_from = value) %>%
    select(-Key) %>%
    replace(is.na(.), 0) %>%
    ungroup() %>%
    add_column(Total = rowSums(select(., -input$mk_dims), na.rm = TRUE)) %>%
    select_at(vars(input$mk_dims, Total, everything())) %>%
    dplyr::filter(Total > 0) %>%
    arrange(desc(Total))

    if(length(input$mk_dims) > 0) {
    csums <- colSums(select(extable, -input$mk_dims), na.rm = T)
    table_top <- map_dfc(csums, ~.) %>% mutate(!! sym(head(input$mk_dims, 1)) := "Total")
    union_all(table_top, extable) %>% select(names(extable)) %>% 
      mutate_at(vars(-input$mk_dims), scales::dollar)
  } else {
    extable 
  }
  
})

output$marketing_data_revenue_trend <- DT::renderDataTable(DT::datatable(
  {marketing_data_revenue_trend()}, 
  filter = 'top', 
  extensions = c('Scroller'), 
  options = marketing_dt_options))
output$AllRevenueDL <- downloadTable("download_revenue", marketing_data_revenue_trend())


