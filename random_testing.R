

files <- list.files('.', recursive = TRUE)
for (fi in files) {
  if (!(grepl('fcs$', fi)) || !(grepl('png$', fi))) {
  fp <-paste0(getwd(), '/', fi)
  #print(fp)
  grep_tmp <- grep('openCyto', readLines(fp), value = TRUE)
  if (length(grep_tmp) > 0) {
    print(paste0(fp, '>>>>', grep_tmp))
  }
  }
}



Sys.setenv("R_PAPERSIZE"=10)
Sys.setenv("R_PRINTCMD"="testing")
Sys.setenv("HOST"= "localhost")
l <- Sys.getenv(c("R_HOME", "R_PAPERSIZE", "R_PRINTCMD", "HOST"))
sapply(l, function(x) names(x))

mydata = read.table(text="
INC_A SAC_A INC_B ASD_A
2 1 5 12
3 4 2 13
", header=TRUE)

mydata

library(dplyr)

select(mydata, -INC_A)


library(httr)
library(jsonlite)
url <- "localhost:5000/meta1/post"
url <- "137.135.18.174:5000/meta1/post"
df <- read.csv("fcs_data/20191218-10299178-01-ST/20191218-10299178-01-ST/20191218-10299178-01.csv", stringsAsFactors = FALSE)
na_cols <- sapply(df, function(x) all(is.na(x)))
check_na_cols <- names(na_cols[na_cols == TRUE])
df[, check_na_cols] <- "NA"
df$Batch.Number <- as.character(df$Batch.Number)

dfjs <- toJSON(df)
status <- POST(url, body = list(data = dfjs, script_name = 'main_driver'), encode = "form", verbose())
