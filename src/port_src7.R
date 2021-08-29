#Export dataframe as csv and rds
export <- function(x, path){
  write.csv(x, file = paste0(path, ".csv"))
  saveRDS(x, file = paste0(path, ".rds"))
}

#Show basic info about a dataframe
# general_inspection <- function(df, x, y){
#
#   defined <- ls()
#   passed <- names(as.list(match.call())[-1])
#
#   dim <- dim(df)
#   name <-deparse(substitute(df))
#
#   paste("The data frame", name, "has", as.character(dim[2]), "columns:", str_c(names(df), collapse = ", ")) %>% print()
#   paste("And", as.character(dim[1]), "rows") %>% print()
#
#   df %>% summary() %>% print()
#
#   if (any(!defined %in% passed)) {
#     warning(paste("missing values for", paste(setdiff(defined, passed), collapse = ", ")))
#   }
#   else {
#     ggplot(df, aes(x = x, y = y)) +
#       geom_point()
#   }
#
# }

amount_factor <- function(x){
  name <-deparse(substitute(x))
  length <- x %>% as.factor() %>% levels() %>% length()

  paste(name, "has", length, "levels.", sep = " ") %>% print()
  #x %>% as.factor() %>% levels() %>% print()

}

#looks which element of a vector 2 vectors have in common
common <- function(x, pattern){
  #takes first element of x
  itterations <- seq(from = 1, to =  length(x))
  commons <- c()

  for (i in itterations){
    y <- str_detect(x[i], pattern)
    if (TRUE == (TRUE %in% y)){
      commons <- append(commons, x[i])
    }
  }

  return(commons)
}

#testing common function
# test1 <- c("A1", "A2", "A3", "A4", "A5")
# test2 <- c("A1", "A2", "A3", "B4", "B5")
#
# common(test1, test2)


# create_py_env <- function(dir){
#
# }
# #create python enviroment
#
# #check if virtualenv is installed
#
# #create virtualenv in directory
# system('powershell -command "virtualenv python"')
#
# #activate python enviroment
#
# #check
# Sys.which()

#check if you're currently in a r project
# check_proj <- function(){
#   files <- list.files()
#   files <- grepl("Rproj$", files)
#   return(any(files))
# }


format_version <- function(packages){

  ver_num = c()

  for (p in packages){
    ver_num <- append(ver_num, as.character(packageVersion(p)))
  }

  pck <- packages
  index <- seq(length(packages))

  for (i in index){
    cat(paste0("library(", pck[i], ")", " #", ver_num[i], "\n"))
  }

}

capture_package_name <- function(string){
  str <- strsplit(string, "library")

  #formatting output of strsplit
  str = str[[1]][2:length(str[[1]])]

  #finding locations of ")"
  grep_res = gregexpr(pattern = ")", str)
  str_index = c()


  for (i in seq(length(grep_res))){
    str_index <- append(str_index, grep_res[[i]][1])
  }

  #extracting package name
  package_names = c()

  for (i in seq(length(str_index))){
    package_names <- append(package_names, substr(str[i], 2, str_index[i]-1))
  }

  return(package_names)
}


