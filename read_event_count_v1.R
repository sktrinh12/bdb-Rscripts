library(flowCore)
library(gridExtra)
library(ggplot2)

parentpath = "C:/Users/10322096/Documents/fcs_data/CHOexper_96headcheck/2nd_trial_20211703/"
subdirName = "CHO Count "

# ORDER the fcs files manually

getAlpha <- function(filename) {
  split_string = stringr::str_split(filename, pattern="_")
  alphaNum = split_string[[1]][length(split_string[[1]])]
  alphaNum = substr(alphaNum,1,3)
  return(alphaNum)
}


zeroPadpaste0 <- function(letter, number) {
  if (strtoi(number) < 10) {
    paste0(letter, "0", number)
  } else {
    paste0(letter, number)
  }
}

letterMatrix <- sapply(LETTERS[1:8], function(x) rep(x,12), USE.NAMES = F)
orderedWellID <- mapply(zeroPadpaste0, as.vector(letterMatrix), rep(seq(1,12),8), USE.NAMES = F)


extractMetadata <- function(filepath) {
  samp <-  read.FCS(filename = file.path(filepath), transformation="linearize")
  meta_data = keyword(samp)
  data_list <- list(
    "cyt_num" = meta_data$CYTNUM,
    "cyt_cfg" = meta_data$`CYTOMETER CONFIG NAME`,
    "cyt" = meta_data$`$CYT`,
    "plate_name" = meta_data$`PLATE NAME`,
    "beads_lot" = meta_data$`CST BEADS LOT ID`
  )
  return(data_list)
} 


generatePlots <- function(dirname, count) {
  fcs_files <- list.files(paste0(parentpath, dirname), pattern = ".fcs")
  orderedIndex <- sapply(fcs_files, function(x) {
    match(getAlpha(x), orderedWellID)
  }, USE.NAMES = F)
  
  fcs_files_ordered <- fcs_files[order(orderedIndex, fcs_files)]
  fs <- read.flowSet(fcs_files_ordered, path = (paste0(parentpath, dirname)))
  rectGate <- rectangleGate(filterId="FSC-A Region",
                             "FSC-A"=c(5000, Inf))
  
  fsOutput = sapply(seq_along(fs), function(x) {
    filter_res = flowCore::filter(fs[[x]],rectGate)
    summary(filter_res)$true
  })
 
  intNumColms = length(fcs_files) / 8
  x <- seq(1:intNumColms)
  y <- LETTERS[1:8]
  data <- expand.grid(X=x, Y=y)
  data$Z <- fsOutput
  
  validWells <- data[data$X<9,]
  summaryWells <- summary(validWells$Z)
  std <- sd(validWells$Z)
  stdErr <- std/sqrt(length(validWells))
  
  dataTable <- data.frame(
    std=std, 
    stdErr=stdErr,
    min=as.vector(round(summaryWells[1],2)),
    median=as.vector(round(summaryWells[3],2)),
    mean=as.vector(round(summaryWells[4],2)),
    max=as.vector(round(summaryWells[6],2))
  )
  
  plt <- ggplot(data, aes(X,ordered(Y, levels=rev(y)), fill=Z)) + 
    geom_tile(color = "white") +
    scale_x_continuous(breaks=x, position = "top") +
    geom_text(aes(label = Z, colour = ifelse(Z <1000, "white", "black"))) +
    scale_colour_manual(values=c("white"="white", "black"="black"), guide = FALSE) +

    scale_fill_gradientn(
      name = "Cell Count", # changes legend title
      colours = c("gray","#8EACC8","#2077C5"), values = c(0.,0.4,1)
    ) +
    theme_minimal() +
    ggtitle(dirname) + # for the main title
    xlab("Columns") + # for the x axis label
    ylab("Rows")
  
    adjustTheme <- ttheme_default(
      core = list(fg_params=list(cex = 0.7)),
      colhead = list(fg_params=list(cex = 0.7)),
      rowhead = list(fg_params=list(cex = 0.7)))
    
    tableObject = tableGrob(dataTable,theme= adjustTheme)
    main_plt <- grid.arrange(arrangeGrob(plt, tableObject, ncol = 1,widths = 1/8))
    
  
    ggsave(
      filename = paste0(parentpath, Sys.Date(), "_plate", count, ".png"),
      plot = main_plt,
      scale = 1,
      width = 250,
      height = 175,
      units = "mm",
      dpi = 300,
      limitsize = TRUE
    )
}

generatePlots(paste0(subdirName, 1), 1)


for (i in seq(1,3)) {
  dirname = paste0(subdirName, i)
  generatePlots(dirname, i)
}
