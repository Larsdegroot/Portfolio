#Export dataframe as csv and rds
export <- function(x, path){
  write.csv(x, file = paste0(path, ".csv"))
  saveRDS(x, file = paste0(path, ".rds"))
}

#Show basic info about a dataframe
general_inspection <- function(df, x, y){
  defined <- ls()
  passed <- names(as.list(match.call())[-1])

  dim <- dim(df)
  paste("This data frame has", as.character(dim[2]), "columns:", str_c(names(df), collapse = ", ")) %>% print()
  paste("And", as.character(dim[1]), "rows") %>% print()

  df %>% summary() %>% print()

  if (any(!defined %in% passed)) {
    warning(paste("missing values for", paste(setdiff(defined, passed), collapse = ", ")))
  }
  else {
    ggplot(df, aes(x = x ,y = y)) +
      geom_point()
  }

}


