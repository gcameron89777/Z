# filters, render here in server so can refer to data in server in the output ui's
output$channel_filter <- renderUI({
  channel_options <- unique(ecom_channel_raw$Channel) %>% sort()
  pickerInput(inputId = "channel_filter",
              label = "Channel",
              choices = channel_options,
              selected = channel_options,
              multiple = T,
              options = list(`actions-box` = T)
              )
})


output$device_filter <- renderUI({
  device_options <- unique(ecom_channel_raw$Device) %>% sort()
  pickerInput(inputId = "device_filter",
              label = "Device",
              choices = device_options,
              selected = device_options,
              multiple = T,
              options = list(`actions-box` = T)
  )
})

output$user_filter <- renderUI({
  user_options <- unique(ecom_channel_raw$UserType) %>% sort()
  pickerInput(inputId = "user_filter",
              label = "UserType",
              choices = user_options,
              selected = user_options,
              multiple = T,
              options = list(`actions-box` = T)
  )
})


## Info boxes
output$SessionsBox <- renderInfoBox({
  infoBox(
    "Sessions", format(sum(ecom_channel()$Sessions), big.mark = ","), icon = icon("bar-chart"), color = "teal"
  )
})

output$TransactionsBox <- renderInfoBox({
  infoBox(
    "Transactions", format(sum(ecom_channel()$Transactions), big.mark = ","), icon = icon("bar-chart"), color = "teal"
  )
})

output$RevenueBox <- renderInfoBox({
  infoBox(
    "Revenue", paste0("$",format(sum(ecom_channel()$Revenue), big.mark = ",")), icon = icon("dollar"), color = "teal"
  )
})

output$ConversionRateBox <- renderInfoBox({
  infoBox(
    "Conversion Rate", scales::percent(sum(ecom_channel()$Transactions) / sum(ecom_channel()$Sessions), accuracy = 0.01), icon = icon("line-chart"), color = "teal"
  )
})

##  Timeline Plot
output$timeline <- renderPlot({
  ggplot(ecom_channel(), aes_string(x = "Date", y = input$kpi_overlay)) +
    geom_area(aes_string(fill = input$breakdown), alpha = 0.3) +
    stat_summary(fun.y = sum, geom = 'line', size = 1, alpha = 0.5) +
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
# output$revenue_channel <- renderPlot({
#   ggplot(untrended_channel_data, aes(x = reorder(Channel, Revenue), y = Revenue), label = Revenue) +
#     geom_bar(stat="identity", fill = "#008080", alpha = 0.6) +
#     coord_flip() +
#     geom_text(aes(label = paste0("$", format(Revenue, big.mark = ","))), color = "white", hjust= 1.2) +
#     scale_y_continuous(labels = function(l) {paste0("$", format(l, big.mark = ","))}) +
#     xlab("")
# }, height = 300)

# transactions plot
# output$transactions_channel <- renderPlot({
#   ggplot(untrended_channel_data, aes(x = reorder(Channel, Transactions), y = Transactions), label = Transactions) +
#     geom_bar(stat="identity", fill = "#008080", alpha = 0.6) +
#     coord_flip() +
#     geom_text(aes(label = format(Transactions, big.mark = ",")), color = "white", hjust= 1.2) +
#     xlab("")
# }, height = 300)

# conversion rate plot
# output$cr_channel <- renderPlot({
#   ggplot(untrended_channel_data, aes(x = reorder(Channel, Transactions / Sessions), y = Transactions / Sessions), label = Transactions / Sessions) +
#     geom_bar(stat="identity", fill = "#008080", alpha = 0.6) +
#     coord_flip() +
#     geom_text(aes(label = paste0(formatC(Transactions / Sessions * 100, format = "f", digits = 1), "%")), color = "white", hjust= 1.2) +
#     scale_y_continuous(labels = function(l) {paste0(formatC(l * 100, format = "f", digits = 1), "%")}) +
#     xlab("")
# }, height = 300)


## Summary KPI tables
# sessions table
# sessions_table <- metricTables(ecom_channel(), Channel, Sessions)
# output$sessions_table <- DT::renderDataTable(DT::datatable({
#   sessions_table
# }, options = list(dom = 't', bPaginate = FALSE, sScrollX = "100%"))
# )
# output$downloadSessions <- downloadTable("sessions", sessions_table)

# transactions table
# transactions_table <- metricTables(ecom_channel, Channel, Transactions)
# output$transactions_table <- DT::renderDataTable(DT::datatable({
#   transactions_table
# }, options = list(dom = 't', bPaginate=FALSE, sScrollX="100%"))
# )
# output$downloadTransactions <- downloadTable("transactions", transactions_table)

# revenue table
# revenue_table <- metricTables(ecom_channel(), Channel, Revenue) %>% mutate_at(vars(-Channel), scales::dollar)
# output$revenue_table <- DT::renderDataTable(DT::datatable({
#   revenue_table},
#   options = list(dom = 't', bPaginate=FALSE, sScrollX="100%"))
# )
# output$downloadRevenue <- downloadTable("revenue", revenue_table)