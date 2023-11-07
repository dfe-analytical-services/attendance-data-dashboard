#!/usr/bin/env Rscript
cat("Running commit hooks...",fill=TRUE)
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(dplyr))
shhh(library(xfun))


error_flag <- FALSE

datalog <- "datafiles_log.csv"
log_files <- read.csv(datalog, stringsAsFactors = FALSE)
ign_files <- read.csv(".gitignore", header = FALSE, stringsAsFactors = FALSE)
colnames(ign_files)[1] <- "filename"

cat("Contents of the .gitignore file:")
print(ign_files)

# Run a pass through the .gitignore files and look for any issues
if(ncol(ign_files)>1){
  cat("ERROR: It looks like you've got commas in the .gitignore. Please correct the .gitignore file and try again.")
  error_flag <- TRUE
} else {
  for(i in 1:nrow(ign_files)){
    if(grepl(' ',ign_files$filename[i])){
      cat("ERROR: It looks like you've got spaces in filenames in the .gitignore. Please rename your files if they contain spaces and update the .gitignore file accordingly.")
      error_flag <- TRUE
    }
  }
}



suffixes <- "xlsx$|dat$|csv$|tex$|pdf$"

current_files <- data.frame(files = list.files("./", recursive = TRUE)) %>%
  filter(grepl(suffixes, files), !grepl("renv|datafiles_log.csv", files))

for (file in current_files$files) {
  if (!file %in% log_files$filename) {
    cat("Error:", file, "is not recorded in datafiles_log.csv.\n\n")
    cat("Please add an entry to datafiles_log.csv for this file and mark it as unpublished, published or reference.\n\n")
    error_flag <- TRUE
  } else {
    file_status <- (log_files %>% filter(filename == file))$status
    if (!file_status %in% c("published", "Published", "reference", "Reference", "dummy", "Dummy")) {
      if (!file %in% ign_files$filename & !grepl("unpublished",file)) {
        cat("Error:", file, "is not logged as published or reference data in datafiles_log.csv and is not found in .gitignore.\n\n")
        cat("If the file contains published or reference data then update its entry in datafiles_log.csv.\n\n")
        cat("If the file contains unpublished data then add it to the .gitignore file.\n\n")
        error_flag <- TRUE
      }
      else {
        cat(file,"is recorded in the logfile as unpublished data and in .gitignore and so will not be included as part of the commit.\n\n")
      }
    }
  }
}

if(grepl('G-Z967JJVQQX', htmltools::includeHTML(("google-analytics.html"))) & 
   !(toupper(Sys.getenv("USERNAME")) %in% c("CFOSTER4", "CRACE", "LSELBY","RBIELBY", "JMACHIN"))){
  cat("Cleaning out the template's Google Analytics tag.",fill=TRUE)
  gsub_file("google-analytics.html", pattern = "G-Z967JJVQQX", replacement = "G-XXXXXXXXXX")
  gsub_file("ui.R", pattern = "Z967JJVQQX", replacement = "XXXXXXXXXX")
  system2(command = "git", args=c("add","google-analytics.html"))
}

if (error_flag) {
  cat("Warning, aborting commit. Unrecognised data files found, please update .gitignore or datafiles_log.csv.\n")
  quit(save = "no", status = 1, runLast = FALSE)
}

# End of hooks
