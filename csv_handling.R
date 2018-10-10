
list_folder_csv <- function(directory, pattern="*.csv", ...){
	files <- list.files(directory, pattern, full.names=TRUE)
	names <- make.names(gsub(".*\\/(.*)\\..*", "\\1", files))
	cat("\nimporting ", names, "to list\n")
	lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE, ...)))
}

read.csv.fun <- function(x, ...){
  read.csv(x, stringsAsFactors=FALSE, ...)
}

assign_folder_csv <- function(directory, pattern="*.csv", ...){
	files <- list.files(directory, pattern, full.names=TRUE)
	cat("\nloading csvs\n")
	cat("\n", gsub(".*\\/(.*)\\..*", "\\1", files), "\n")
	list2env(
		lapply(setNames(files, make.names(gsub(".*\\/(.*)\\..*", "\\1", files))), 
           read.csv.fun), envir = .GlobalEnv)	
}