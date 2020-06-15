clean <- function(str, from = "_", to = "-", FUN = tolower) {
  str <- gsub(from, to, str)
  return(FUN(str))
}

generate_labels <- function(inputFile, overwrite = FALSE){
  data <- read.csv(inputFile)
  IDs <- NULL
  COLS <- 6
  LABEL_PATH <- ""
  for (i in 1:nrow(data)) {
    if (data[i, 1] == "" || data[i, 2] == "") # Verify that entry is not empty
      next
    #IDs <- paste0(IDs, "[", data[i, 2], "](", clean(data[i, 2]), ") ")
    IDs <- rbind(IDs, paste0("[", data[i, 2], "](", clean(data[i, 2]), ")"))
    #IDs <- rbind(IDs, "")
    new_Rmd_name <- paste0("content/label/", clean(data[i, 1]), "-", clean(data[i, 2]), ".Rmd")
    if (file.exists(new_Rmd_name) && overwrite ) { # Store old files
      if(!dir.exists("content/.old/label/"))
        dir.create("content/.old/label/", recursive = TRUE)
      file.rename(new_Rmd_name, paste0("content/.old/label/", Sys.Date(), "_", clean(data[i, 1]), "-", clean(data[i, 2]), ".Rmd"))
    }
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
          paste0("  - ", LABEL_PATH, tolower(data[i, 1]), "-", tolower(data[i, 2])),
          paste0("  - ", LABEL_PATH, clean(data[i, 1]), "-", clean(data[i, 2])),
          paste0("  - ", LABEL_PATH, clean(data[i, 1], to = ""), "-", clean(data[i, 2], to = "")),
          "---",
          "",
          paste0("# **House**: ", data[i, 1]),
          paste0("# **Plant ID**: ", data[i, 2]),
          paste0("# **Updates**: "),
          paste0(Sys.Date(),": Nothing new.")
        ), new_Rmd)
      close(new_Rmd)
      print(paste0("FILE: ", new_Rmd_name, " created."))
    }
    else
      print(paste0("FILE: ", new_Rmd_name, " already exists."))
  }
  
  for(i in 1:(COLS - nrow(IDs) %% COLS))
    IDs <- c(IDs, "")
  
  links <- knitr::kable(matrix(IDs, ncol = COLS, byrow = TRUE), "html")
  
  new_house_Rmd_name <- paste0("content/label/", clean(data[1, 1]), ".Rmd")
  new_house_Rmd <- file(new_house_Rmd_name)
  writeLines(
    c("---",
      "author: Roberto Villegas-Diaz",
      "tags:",
      "  - index",
      "title: GH118",
      "aliases:",
      paste0("  - ", LABEL_PATH, tolower(data[1, 1])),
      paste0("  - ", LABEL_PATH, clean(data[1, 1])),
      paste0("  - ", LABEL_PATH, clean(data[1, 1], to = "")),
      "---",
      "```{r, echo = FALSE}",
      "knitr::kable(matrix(c(", paste0("'",IDs,"'", collapse = ", "), 
                           "), ncol = ", COLS, ", byrow = TRUE), 'html', booktabs = TRUE, table.attr='class=\"id_links\"')",
      "```"
    ),
    new_house_Rmd_name
  )
  close(new_house_Rmd)
}
