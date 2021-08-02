#create a new project in current directory
library(usethis) #2.0.1
currect_dir <- getwd()
go <- readline(prompt = paste("Do you want to creat a new project folder at", currect_dir, "? y/n: ", sep = " "))

if (go == "y"){

  #make project folder
  project.name <- readline(prompt = "Enter project name: ")
  dir.create(project.name)
  for (i in c("data", "doc", "external", "output", "src")){
    dir.create(paste0(project.name, "/", i))
  }

  #make a project
  usethis::create_project(project.name)

  # #connect to git
  # usethis::use_git()
  # usethis::use_github()


  } else {
    stop()}

