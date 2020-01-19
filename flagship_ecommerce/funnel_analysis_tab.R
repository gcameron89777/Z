# Ecommerce Funnel Tab

# Filters
# filters, render here in server so can refer to data in server in the output ui's
output$fun_channel_filter <- renderUI({
        channel_options <- unique(funnel_data_raw$Channel) %>% sort()
        pickerInput(inputId = "fun_channel_filter",
                    label = "Channel",
                    choices = channel_options,
                    selected = channel_options,
                    multiple = T,
                    options = list(`actions-box` = T)
        )
})


output$fun_device_filter <- renderUI({
        device_options <- unique(funnel_data_raw$Device) %>% sort()
        pickerInput(inputId = "fun_device_filter",
                    label = "Device",
                    choices = device_options,
                    selected = device_options,
                    multiple = T,
                    options = list(`actions-box` = T)
        )
})

output$fun_user_filter <- renderUI({
        user_options <- unique(funnel_data_raw$UserType) %>% sort()
        pickerInput(inputId = "fun_user_filter",
                    label = "UserType",
                    choices = user_options,
                    selected = user_options,
                    multiple = T,
                    options = list(`actions-box` = T)
        )
})

output$fun_zero_filter <- renderUI({
        zero_options <- unique(funnel_data_raw$ZeroValProduct) %>% sort()
        pickerInput(inputId = "fun_zero_filter",
                    label = "ZeroValueProduct",
                    choices = zero_options,
                    selected = zero_options,
                    multiple = T,
                    options = list(`actions-box` = T)
        )
})

output$contains_download_filter <- renderUI({
        dl_options <- unique(funnel_data_raw$ContainsDownload) %>% sort()
        pickerInput(inputId = "contains_download_filter",
                    label = "ContainsDownload",
                    choices = dl_options,
                    selected = dl_options,
                    multiple = T,
                    options = list(`actions-box` = T)
        )
})

output$fun_elabel_filter <- renderUI({
        elabel_options <- unique(funnel_data_raw$EventLabel) %>% sort()
        pickerInput(inputId = "fun_elabel_filter",
                    label = "EventLabel",
                    choices = elabel_options,
                    selected = elabel_options,
                    multiple = T,
                    options = list(`actions-box` = T)
        )
})


# funnel bar blot
output$funnel_plot <- renderPlot({
        
        metric <- rlang::sym(input$funnel_metrics)
        
        plot_data <- funnel_data() %>% 
                arrange(- !!metric) %>% 
                mutate(End = lag(!!metric),
                       #xpos = 1:n() - 0.5,
                       xpos = row_number() - 0.5,
                       Diff = End - !!metric,
                       Percent = paste("-", round(Diff / End * 100, 1), "%"))
        
        # build funnel plot
        plot_data %>% ggplot(aes(x = reorder(EventAction, -!!metric), y = !!metric)) +
                geom_col(alpha = 0.6, fill = "#008080") + # base column chart
                stat_summary(aes(label = scales::comma(..y..)), fun.y = 'sum', geom = 'text', col = 'white', vjust = 1.5) + # labels on the ends of the bars
                geom_segment(aes(x = xpos, y = End, xend = xpos, yend = !!metric), na.rm = T) + # creates the solid line showing drop to the right of each bar
                geom_text(aes(x = xpos, y =  End - Diff / 2, label = Percent), hjust = -0.2, vjust = -1, na.rm = T) + # adds the text showing the % drop at ecah step
                theme(axis.title.x = element_blank(), # remove default x and y titles
                      axis.title.y = element_blank()) +
                scale_y_continuous(label = scales::label_comma(scale = 0.001, suffix = "K")) + # y axis in thousands K
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) # make x axis right angled
})