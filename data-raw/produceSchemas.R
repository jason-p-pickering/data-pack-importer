
ProduceJSON <-
  function(row = 6,
           start_col = 3,
           end_col = 1000,
           sheet_name,
           sheet_path) {
    foo <-
      list(
        sheet = sheet_name,
        row = row,
        start_col = start_col,
        end_col = end_col,
        fields = as.list(names(as.list(
          read_excel(
            path = sheet_path,
            sheet = sheet_name,
            range = cell_limits(c(row, start_col),
                                c(row, end_col))
          )
        )))
      )
    
    foo$fields <- foo$fields[!grepl("X_", foo$fields)]
    foo$end_col = start_col + length(foo$fields)
    
    return(foo)
    
  }

sheets <- excel_sheets(sheet_path)
sheets <- sheets[grepl("Targets", sheets)]

foo <- list()
for (i in 1:length(sheets)) {
  bar <- ProduceJSON(sheet_path = sheet_path, sheet_name = sheets[i])
  foo <- list.append(foo, bar)
}
