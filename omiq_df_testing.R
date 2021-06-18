library(httr)
library(jsonlite)
url <- "localhost:5000/meta1/post"
df <- read.csv("fcs_data/20191218-10299178-01-ST/20191218-10299178-01-ST/20191218-10299178-01.csv", stringsAsFactors = FALSE)
na_cols <- sapply(df, function(x) all(is.na(x)))
check_na_cols <- names(na_cols[na_cols == TRUE])
df[, check_na_cols] <- "NA"
df$Batch.Number <- as.character(df$Batch.Number)

dfjs <- toJSON(df)
status <- POST(url, body = list(data = dfjs, script_name = 'main_driver', barcode = '20210701SDFY21W3P7'), encode = "form", verbose())


POST(url, body = list(data = dfjs, script_name = 'test'), encode = "form", verbose())



df <- read.csv("C:/users/10322096/Downloads/20212001STFY21W3P12-01-metadata (1).csv", stringsAsFactors = FALSE)

head(df)


selected_colnames <- c(
                       "Well.ID",
                       "Plate.ID",
                       "Stain.Date",
                       "Plate.Row",
                       "Plate.Column",
                       "Control.Row",
                       "Target.Species",
                       "Specificity..CD.",
                       "Isotype.Host.Species",
                       "Clone",
                       "Fluorochrome",
                       "Parameter",
                       "Batch.Number",
                       "ug.test",
                       "units",
                       "Sample.Types",
                       "Sample.Species",
                       "Sample.Strain",
                       "Donor.ID",
                       "spec1_range",
                       "spec2_range",
                       "spec3_range",
                       "gating_method",
                       "gating_argument")

colnames(df)


sapply(selected_colnames, function(x) x %in% colnames(df))


df[, selected_colnames]


df_filenames <- df[grepl("filename", colnames(df), ignore.case=TRUE)]

df_remainder <- df[, selected_colnames]


dff <- cbind(df_filenames, df_remainder)

dff$Batch.Number[sample.int(75,22)] <- "NA"

sum(sapply(df$Batch.Number, function(x) is.numeric(x))) > 1

typeof(dff$Batch.Number[1])

bn<- sample.int(10000,1)
dff$Batch.Number[is.na(dff$Batch.Number)] <- bn
#dff$Bath.Number <- as.character(dff$Batch.Number)
str(dff)

head(dff)
dff$Batch.Number










settings_path = "C:/Users/10322096/Documents/gitrepos/romiq-meta-docker-setup/common_drive/reports/20210701SDFY21W3P7/metafile.csv"
data_path = "C:/Users/10322096/Documents/gitrepos/romiq-meta-docker-setup/common_drive/reports/20210701SDFY21W3P7/"
df = read.csv(settings_path, stringsAsFactors = F)
df = df[!is.na(df$Parameter),]

# original one Rachel sent
data_path = "C:/Users/10322096/Documents/fcs_data/20191218-10299178-01-ST/20191218-10299178-01-ST/"
settings_path = "C:/Users/10322096/Documents/fcs_data/20191218-10299178-01-ST/20191218-10299178-01-ST/20191218-10299178-01.csv"

# Jenn's dataset
data_path_jenn = "C:/Users/10322096/Downloads/20191218-10299178-01/20191218-10299178-01/"
settings_path_jenn = "C:/Users/10322096/Downloads/20191218-10299178-01_Jenn.csv"


df = read.csv(settings_path)
dfj = read.csv(settings_path_jenn)


df
dfj

setdiff(df, dfj)
