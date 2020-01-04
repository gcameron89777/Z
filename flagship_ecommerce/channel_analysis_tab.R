

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