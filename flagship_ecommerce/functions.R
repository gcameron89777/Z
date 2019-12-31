
# downloadable tables on channel analysis tab
metricTables <- function(df, dim, metric) {
  
  dim <- enquo(dim)
  metric <- enquo(metric)
  
  
  df %>% mutate(date = ordered(
    format(date, "%d-%b"),
    levels = format(sort(unique(date)), "%d-%b")
  )) %>% 
    group_by(date, !! dim) %>% 
    summarise(!! rlang::as_name(metric) := sum(!! metric))  %>%
    pivot_longer(cols = -c(date, !!dim), names_to = 'Key', values_to = 'value') %>%
    pivot_wider(names_from = date, values_from = value) %>%
    select(-Key) %>% 
    replace(is.na(.), 0) 
}


# for download buttons
downloadTable <- function(metric, table) {
  file = function(m) paste(m, Sys.Date(), '.csv', sep = '')
  downloadHandler(
    filename = file(metric),
    content = function(file) write.csv(table, file)
  )
}
