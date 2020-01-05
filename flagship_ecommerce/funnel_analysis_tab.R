# Ecommerce Funnel Tab

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