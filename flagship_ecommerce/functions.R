
# downloadable tables on channel analysis tab
metricTables <- function(df, dim, metric) {
  
  dim <- rlang::sym(dim)
  metric <- enquo(metric)
  
  df %>% mutate(date = ordered(
    format(Date, "%d-%b"),
    levels = format(sort(unique(Date)), "%d-%b"))) %>% 
    group_by(Date, !! dim) %>%
    summarise(!! rlang::as_name(metric) := sum(!! metric))  %>%
    pivot_longer(cols = -c(Date, !! dim), names_to = 'Key', values_to = 'value') %>%
    pivot_wider(names_from = Date, values_from = value) %>%
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
