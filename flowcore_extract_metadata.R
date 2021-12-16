library(dplyr)
filepath = "C:/Users/10322096/Documents/fcs_data/8Chl_vs_96CR_redo/28Jul2021 8 Channel vs 96 COREHead/8 Channel Plate 3"
samp <-  flowCore::read.FCS(filename = file.path(filepath, "Specimen_001_A8_A08_008.fcs"), transformation="linearize")
meta_data = flowCore::keyword(samp)


meta_data

find_param <- function(fdata) {
  df <- data.frame(flowCore::exprs(fdata)) %>%
    select(!contains(c("SC", "Time")))
  cnames <- colnames(df)
  df <- df %>% summarise(across(cnames, median))
  return(cnames[which(df == max(df) )])
}


fcs_files <- list.files(filepath)
check_max_vals_wellid <- fcs_files[which(grepl("[A-H]08.*fcs$",fcs_files))]


grab_meta_data <- function(check_max_vals_wellid) {
  
  mdat_ls <- list()
  
  for (wi in check_max_vals_wellid) {
    w = substr(wi, 14,15)
    fdat <-  flowCore::read.FCS(filename = file.path(filepath, wi), transformation="linearize")
    mdat_ls[[paste0("well_",w)]] <- find_param(fdat)
  }
  
  meta_data = flowCore::keyword(fdat)
  
  mdat_ls$'CYT' <- meta_data$`$CYT`
  mdat_ls$'CYTNUM' <- meta_data$CYTNUM
  mdat_ls$'CSTLOT' <- meta_data$`CST BEADS LOT ID`
  mdat_ls$'PLATENAME' <- meta_data$`PLATE NAME`
  mdat_ls$'DATE' <- meta_data$`$DATE`
  mdat_ls$'EXPTNAME' <- meta_data$`EXPERIMENT NAME`
  mdat_ls$'USERNAME' <- meta_data$`EXPORT USER NAME`  
  
  return(mdat_ls)
}

mdata <- grab_meta_data(check_max_vals_wellid)


mdata


ldata <- lapply(names(mdata), grep, pattern = "well", value= F)
pindx <- names(mdata)[which(sapply(ldata, FUN=function(x) 1 %in% x))]

param <- mdata[pindx]
param <- unlist(param, use.names = F)
#p <- as_tibble(gsub("_", "-", param))
rep(gsub("[\\.]", "-", param), each = 12)
p



df <- read.csv("C:/Users/10322096/Documents/gitrepos/romiq-meta-docker-setup/common_drive/reports/20210701SDFY21W3P8/metafile.csv")


df$ug.test
