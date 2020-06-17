#' Clean strings by replacing characters and changing case
#'
#' @param str original string
#' @param from old character (to be replaced) [default: "_"]
#' @param to new character (replacement) [default: "-"]
#' @param FUN casing function [default: tolower]
#'
#' @return formated string
#' @export
#'
#' @examples
#' clean("GH_118")
#' clean("GH_118", to = "")
#' clean("GH_118", FUN = toupper)
clean <- function(str, from = "_", to = "-", FUN = tolower) {
  str <- gsub(from, to, str)
  return(FUN(str))
}

#' Generate labels metadata from raw data
#'
#' @param inputFile filename with or without full path to raw data
#' @param overwrite whether or not to overwrite existing files [default: FALSE]
#' @param cols number of labels per row to display (columns) [default: 6]
#' @param location location on disk to store the index [default: "content/label/"]
#' @param format file format [default: "Rmd"]
#' @param author index's author [default: "Roberto Villegas-Diaz"]
#' @param label_path path to reference aliases [default: ""]
#'
#' @return
#' @export
#'
#' @examples
generate_labels <- function(inputFile, 
                            overwrite = FALSE, 
                            cols = 6, 
                            location = "content/label/", 
                            format = "Rmd", 
                            author = "Roberto Villegas-Diaz", 
                            label_path = ""){
  data <- read.csv(inputFile) # Read raw data in CSV format, [House][PlantID]
  data <- data[data[, 1] != "", ] # Drop any records with [House] missing
  
  for (i in 1:nrow(data)) {
    # Create markdown link
    data$Label[i] <- paste0("[", data[i, 2], "](", label_path, clean(data[i, 2]), ")")
    
    # Create new label filename
    new_Rmd_name <- paste0("content/label/", clean(data[i, 1]), "-", clean(data[i, 2]), ".", format)
    
    # Verify if file already exists, if so, then rename it by appending timestamp
    if (file.exists(new_Rmd_name) && overwrite ) { # Store old files
      backup_dir <- paste0(location, ".old/", Sys.Date())
      if(!dir.exists(backup_dir))
        dir.create(backup_dir, recursive = TRUE)
      file.rename(new_Rmd_name, paste0(backup_dir , "/", clean(data[i, 1]), "-", clean(data[i, 2]), ".", format))
    }
    
    # Verify if the label file exisis or if overwrite is TRUE
    if (!file.exists(new_Rmd_name) || overwrite) {
      new_Rmd <- file(new_Rmd_name) # Create reference to file
      writeLines(
        c("---",
          paste0("title: ", data[i,1], " ", data[i, 2]),
          paste0("author: ", author),
          paste0("slug: '", clean(data[i, 1]), "/", clean(data[i, 2]),"'"),
          "categories:",
          paste0("  - ", clean(data[i, 1], to = "", FUN = toupper)),
          "tags:",
          paste0("  - ", clean(data[i, 2], "_\\d*", "", toupper)),
          "aliases:",
          paste0("  - ", label_path, tolower(data[i, 1]), "-", tolower(data[i, 2])),
          paste0("  - ", label_path, clean(data[i, 1]), "-", clean(data[i, 2])),
          paste0("  - ", label_path, clean(data[i, 1], to = ""), "-", clean(data[i, 2], to = "")),
          "---",
          "",
          paste0("# **House**: ", data[i, 1]),
          paste0("# **Plant ID**: ", data[i, 2]),
          paste0("# **Updates**: "),
          paste0(" - ", Sys.Date(),": Nothing new.")
        ), new_Rmd)
      close(new_Rmd)
      print(paste0("FILE: ", new_Rmd_name, " created."))
    }
    else
      print(paste0("FILE: ", new_Rmd_name, " already exists."))
  }
  
  # Generate Greenhouse indices
  for (i in unique(data$House)) {
    idx <- data$House == i # Extract indices for current House ID
    if (length(idx) < 1) # Verify there are records linked to the House ID
      next
    labels <- data$Label[idx]
    create_house_index(i, labels, cols,location, format, author, label_path)
    print(paste0("Index for ", i, " created."))
  }
}


#' Create Greenhouse labels index
#'
#' @param house_id greenhouse identifier, GH_XXX
#' @param labels labels in markdown URL format, [LABEL](label)
#' @param cols number of labels per row to display (columns) [default: 6]
#' @param location location on disk to store the index [default: "content/label/"]
#' @param format file format [default: "Rmd"]
#' @param author index's author [default: "Roberto Villegas-Diaz"]
#' @param label_path path to reference aliases [default: ""]
#'
#' @export 
#'
#' @examples
#' create_house_index("GH_118", c("[F2_002](f2-002)", "[F2_003](f2-003)"), format = "txt")
create_house_index <- function(house_id, 
                               labels, 
                               cols = 6, 
                               location = "content/label/", 
                               format = "Rmd", 
                               author = "Roberto Villegas-Diaz",
                               label_path = "") {
  # Create new house index name
  new_house_Rmd_name <- paste0(location, clean(house_id), ".", format)
  
  # Create empty old labels
  old_labels <- ""
  
  # Verify if the file exists, if so, then rename it by appending timestamp
  if (file.exists(new_house_Rmd_name)) {
    # Read contents of old file
    old_house_index <- file(new_house_Rmd_name)
    old_house_index_contents <- readLines(old_house_index)
    close(old_house_index)
    
    # Parse contents, lookup for specific line displaying the knitr::kable command
    old_labels_idx <- grep("knitr::kable", old_house_index_contents) + 1
    old_labels <- trimws(unlist(strsplit(old_house_index_contents[old_labels_idx], ",")))
    old_labels <- gsub("\'", "", old_labels)
    old_labels <- old_labels[old_labels != ""]
    
    backup_dir <- paste0(location, ".old/", Sys.Date())
    if(!dir.exists(backup_dir))
      dir.create(backup_dir, recursive = TRUE)
    
    # Rename old file
    file.rename(new_house_Rmd_name, paste0(backup_dir , "/", clean(house_id), ".", format))
  }
  
  # Combine new labels with old ones and keep unique only
  labels <- unique(sort(c(labels, old_labels)))
  
  # Verify that the number of labels is multiple of the number of columns
  for(i in 1:(cols - length(labels) %% cols))
    labels <- c(labels, "")
  
  # Create reference to file
  new_house_Rmd <- file(new_house_Rmd_name)
  
  # Create file
  writeLines(
    c("---",
      paste0("author: ", author),
      "tags:",
      "  - index",
      paste0("title: ", clean(house_id, to = "", FUN = toupper)),
      "aliases:",
      paste0("  - ", label_path, tolower(house_id)),
      paste0("  - ", label_path, clean(house_id)),
      paste0("  - ", label_path, clean(house_id, to = "")),
      "---",
      "",
      "```{css, echo = FALSE}",
      ".id_links td {",
      "  padding-right: 20px !important;",
      "}",
      "",
      ".split-content {",
      "  min-width: 600px !important;",
      "}",
      "```",
      "",
      "```{r, echo = FALSE}",
      "knitr::kable(matrix(c(", 
      paste0("'", labels, "'", collapse = ", "),
      paste0("), ncol = ", cols, ", byrow = TRUE), 'html', booktabs = TRUE, table.attr='class=\"id_links\"')"),
      "```"
    ),
    new_house_Rmd_name
  )
  
  # Close file
  close(new_house_Rmd)
}


csv2dbf <- function(filename) {
  data <- read.csv(filename)
  colnames(data) <- c("HOUSE", "PLANTID")
  foreign::write.dbf(data, gsub(".csv", ".dbf", filename))
}
