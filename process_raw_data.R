clean <- function(str, from = "_", to = "-", FUN = tolower) {
  str <- gsub(from, to, str)
  return(FUN(str))
}

generate_labels <- function(inputFile, overwrite = FALSE){
  data <- read.csv(inputFile)
  for (i in 1:nrow(data)) {
    new_Rmd_name <- paste0("content/label/", clean(data[i, 1]), "-", clean(data[i, 2]), ".Rmd")
    if (!file.exists(new_Rmd_name) || overwrite) {
      new_Rmd <- file(new_Rmd_name)
      writeLines(
        c("---",
          paste0("title: ", data[i,1], " ", data[i, 2]),
          "author: Roberto Villegas-Diaz",
          paste0("slug: '", clean(data[i, 1]), "/", clean(data[i, 2]),"'"),
          "categories:",
          paste0("  - ", clean(data[i, 1], to = "", FUN = toupper)),
          "tags:",
          paste0("  - ", clean(data[i, 2], "_\\d*", "", toupper)),
          "aliases:",
          paste0("  - label/", clean(data[i, 1]), "-", clean(data[i, 2])),
          paste0("  - label/", clean(data[i, 1], to = ""), "-", clean(data[i, 2], to = "")),
          "---",
          "",
          paste0("# **House**: ", data[i, 1]),
          paste0("# **Plant ID**: ", data[i, 2])
        ), new_Rmd)
      close(new_Rmd)
      print(paste0("FILE: ", new_Rmd_name, " created."))
    }
    else
      print(paste0("FILE: ", new_Rmd_name, " already exists."))
  }
}