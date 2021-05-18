#Export dataframe as csv and rds
export <- function(x, path){
  write.csv(x, file = paste0(path, ".csv"))
  saveRDS(x, file = paste0(path, ".rds"))
}


filecsv <- here("7_data", x)), "_tidy.csv")
filerds <- paste0(paste0(here("7_data", x)), "_tidy.rds")
for (x in c("flu", "dengue", "gapminder")) {

}
