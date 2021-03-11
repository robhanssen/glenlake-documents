
library(jsonlite)
library(dplyr)
json_file <- "./bibliography/biblio.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=" "), flatten=TRUE)


x <- toJSON(json_data, pretty=TRUE)
write(x, "./bibliography/test.json")
