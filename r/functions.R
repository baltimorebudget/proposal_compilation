make_proposal_pdf <- function(x, rmd, file_name_end, dir) {
  
  tryCatch({
    
    summary_agency <- summary %>%
      filter(`Agency Name - Cleaned` == x) %>%
      select(-starts_with("Agency"))
    
    rmarkdown::render(rmd,
                      output_file = paste0(x, file_name_end),
                      output_dir = dir)
  },
  
  error = function(cond) {
    
    warning("File could not be generated for ", x, ". ", cond)
    
  })
}

format_pm_table <- function(df) {
  df <- df %>%
    kable(booktabs = TRUE, longtable = TRUE,
          align = c("l", "l", rep("r", 8))) %>%
    column_spec(2, width = "2.2in") %>%
    column_spec(3:9, width = ".5in") %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(position = "center")
}