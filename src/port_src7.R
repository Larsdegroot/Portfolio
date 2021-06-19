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
  name <-deparse(substitute(df))

  paste("The data frame", name, "has", as.character(dim[2]), "columns:", str_c(names(df), collapse = ", ")) %>% print()
  paste("And", as.character(dim[1]), "rows") %>% print()

  df %>% summary() %>% print()

  if (any(!defined %in% passed)) {
    warning(paste("missing values for", paste(setdiff(defined, passed), collapse = ", ")))
  }
  else {
    ggplot(df, aes(x = x, y = y)) +
      geom_point()
  }

}

amount_factor <- function(x){
  name <-deparse(substitute(x))
  length <- x %>% as.factor() %>% levels() %>% length()

  paste(name, "has", length, "levels:", sep = " ") %>% print()
  x %>% as.factor() %>% levels() %>% print()

}

#looks which element of a vector 2 vectors have in common
common <- function(x, pattern1, pattern2){
  #takes first element of x
  itterations <- seq(from = 1, to =  length(x))
  commons <- c()

  for (i in itterations){
    y <- str_detect(x[i], pattern1)
    if (TRUE == (TRUE %in% y)){
      commons <- append(commons, x[i])
    }
  }

  commons %>% print()
}

#testing common function
# test1 <- c("A1", "A2", "A3", "A4", "A5")
# test2 <- c("A1", "A2", "A3", "B4", "B5")
#
# common(test1, test2)
