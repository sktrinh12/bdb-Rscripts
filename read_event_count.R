library(flowCore)
library(gridExtra)
library(ggplot2)
library(ggcyto)
library(magrittr)
library(officer)
library(rvg)
library(ggridges)


intDefaultNumColms = 12

## ORDER the fcs files manually

getAlpha <- function(filename, exclude_wash) {
  split_string = stringr::str_split(filename, pattern="_")
  alphaNum = split_string[[1]][length(split_string[[1]])]
  alphaNum = substr(alphaNum,1,3)
  if (exclude_wash) {
    nbr = as.numeric(substr(alphaNum,2,3))
    #print(nbr)
    if (nbr < 9) {
      return(alphaNum)
    } else {
      return(NULL)
    }
  }
  return(alphaNum)
}

getCounter <- function(filename) {
  split_string = stringr::str_split(filename, pattern="\\.")
  lengthSubstring = nchar(split_string[[1]][1])
  counter = split_string[[1]][1]
  counter = substr(counter,lengthSubstring-2,lengthSubstring)
  return(as.numeric(counter))
}

zeroPadpaste0 <- function(letter, number) {
  if (strtoi(number) < 10) {
    paste0(letter, "0", number)
  } else {
    paste0(letter, number)
  }
}

getMinMax <- function(fcsobj, cname){
  min <- summary(fcsobj)["Min.", cname]
  max <- summary(fcsobj)["Max.", cname]
  min <- min - min*0.15
  max <- max*0.75
  return (c(min, max))
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

createBlankPlot <- function(df, title, topShift, leftShift) {
  p <- ggplot(df) + geom_point() +
    ggtitle(title) +
    xlab("") +
    ylab("") +
    theme(
      #strip.text = element_blank(),
      plot.margin = unit(c(topShift,1,1,leftShift),"mm"),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      plot.title = element_text(size = 6, hjust = 0.5, vjust = -3)
    )
  return(p)
}

#extractMetadata(paste0(parentpath, subdirName, "1", "/", fcs_files_ordered[1]))

cmpr_max <- function(value, curr_max) {
  if (value > curr_max) {
   return( c(T, value))
  }
  return(c(F, curr_max))
}

RSD_FN <- function(df) {
  diff = 0.3413
  lowerUpper = quantile(df, c(0.5-diff, 0.5+diff))
  res = (lowerUpper[[2]] - lowerUpper[[1]]) /2
  #message(paste0("lowerUpper 1: ", lowerUpper[[1]],"lowerUpper 2: ", lowerUpper[[2]], " res: ", res))
  res
}

gcy_plot <- function(p, max_x, max_y, topShift, leftShift, wellIDTitle, xlabel) {
  suppressMessages(
  as.ggplot(
      p + ggcyto_par_set(limits = list(x = c(0, max_x), y =c(0, max_y))) +
      ylab("") +
      xlab(xlabel) +
      ggtitle(wellIDTitle) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 2)) +
      theme(
        strip.text = element_blank(),
        plot.title = element_text(size = 6, hjust = 0.5, vjust = -3),
        axis.text.y = element_text(size = 4.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(topShift,1,1,leftShift),"mm"), # (top, right, bottom, and left)
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=5, vjust = 5)
      )
  )
  )
}

popRidgePlot <- function(row_letter, param, da, fcs_files_ordered) {
  int_nbr <- utf8ToInt(row_letter) - 64 
  end <- int_nbr*12
  start <- end-11
  nbr_range <- end+1-start
  message(paste0("row: ", row_letter, 
                 " param: ", param," 
                 start: ", start, 
                 " end: ", end, 
                 " range: ", nbr_range))
  df <- data.frame()
  
  for (i in seq(1,8)) {
    d <- da[[i]]
    #head(d)
    message(paste0("cnt: ", row_letter, i))
    well_id <- fcs_files_ordered[i+start-1]
    well_id <- regmatches(well_id, regexpr("[A-H]\\d{2}", well_id))
    d <- cbind(d, data.frame("well_id" = well_id))
    df <- rbind(df, d)
  }
  names(df)[1] <- param
  #print(head(df))
  
  xvar = sym(param)
  p <- ggplot(df, aes(x = !!xvar, y = well_id, fill = ..x..)) +
    geom_density_ridges_gradient(show.legend = F) +
    theme_ridges() +
    ylab("Well ID") +
    ggtitle(paste("Density plot for row:", row_letter, "parameter:", param)) +
    theme(
      panel.spacing = unit(0.1, "lines"),
    )
  return(p)
  }




generatePlots <- function(dirname, count, fileNameType, autogate, autogate_index, replaceString, pltsave, trigger, filter_param) {
  fcs_files <- list.files(paste0(parentpath, dirname, count), pattern = ".fcs")
  print(fcs_files)
  
  if (fileNameType == "counter") {
      orderedIndex <- sapply(fcs_files, getCounter, USE.NAMES = F)
  } else {
      orderedIndex <- sapply(fcs_files, function(x) {
        match(getAlpha(x,F), orderedWellID)
      }, USE.NAMES = F)
  }
  fcs_files_ordered <- fcs_files[order(orderedIndex, fcs_files)]
  fs <- read.flowSet(fcs_files_ordered, path = (paste0(parentpath, dirname, count)))
  #print(fs)
  #rectGate <- rectangleGate(list("FSC-A" = filter_param$gate_fsc, "SSC-A" = filter_param$gate_ssc))
  dat = fs[[autogate_index]]
  #dat = dat[rowMeans(exprs(dat[,c("FSC-A","SSC-A")])>0) == 1,]
  set_gate <- openCyto::gate_flowclust_2d(dat, 
                                          xChannel = "FSC-A", 
                                          yChannel = "SSC-A", 
                                          K = filter_param$gate_tgt[4], 
                                          target = c(filter_param$gate_tgt[1], filter_param$gate_tgt[2]), 
                                          quantile=filter_param$gate_tgt[3])

  fsOutput = sapply(seq_along(fs), function(x) {
    filter_res = flowCore::filter(fs[[x]], set_gate)
    summary(filter_res)$true
  })

  #intNumColms = length(fcs_files) / 8
  #x <- seq(1:intNumColms)
  plateSeq <- seq(1,96)
  #blanks <- setdiff(plateSeq,orderedIndex)
  #print(setdiff(plateSeq,orderedIndex))
  blanks <- c() #- bc 07June2021 96 No Stain -1 has re-dos that were counted
  # set this only if counter digits are not 1-96 and/or there are missing fcs files with proper naming convention
  x <- seq(1:12)
  y <- LETTERS[1:8]

    if (trigger[[1]] == T) {
    data <- expand.grid(X=x, Y=y)
    tmpVector <- c()
    cnt = 1
    for (i in plateSeq) {
      if (i %in% blanks) {
         tmpVector <- c(tmpVector,NA)
      } else {
         tmpVector <- c(tmpVector, fsOutput[cnt])
         cnt = cnt + 1
      }
    }

    data$Z <- tmpVector

    subdata = subset(data, is.na(Z))
    emptyCells = nrow(subdata)>1
    validWells <- data[!is.na(data$Z),]
    validWells <- validWells[validWells$Z > 50, ]
    validWells <- validWells[2:nrow(validWells), ]
    summaryWells <- summary(validWells$Z)
    std <- sd(validWells$Z)
    stdErr <- std/sqrt(nrow(validWells))
    min_val <- as.vector(summaryWells[1])
    
    write.csv(data, paste0(parentpath, Sys.Date(), "_CELLCNT_plate", count, ".csv"), row.names = FALSE)
    message('created cell count df')

    dataTable <- data.frame(
      std=std,
      stdErr=stdErr,
      min=min_val,
      median=as.vector(summaryWells[3]),
      mean=as.vector(summaryWells[4]),
      max=as.vector(summaryWells[6])
    ) %>% flextable::flextable() %>%
      flextable::colformat_num()

    message('create flextable')

    plt <- ggplot(data, aes(X,ordered(Y, levels=rev(y)), fill=Z)) +
      geom_tile(color = "white") +
      scale_x_continuous(breaks=x, position = "top") +
      geom_text(data = subset(data, !is.na(Z)),
                aes(label = Z, colour = ifelse(Z < min_val + min_val*0.15, "white", "black"))) +
      {if(emptyCells)geom_text(data = subdata, aes(y = 1, label = ""))} +
      scale_colour_manual(values=c("white"="white", "black"="black"), guide = FALSE) +
      scale_fill_gradientn(
        name = "Cell Count", # changes legend title
        colours = c("gray","#8EACC8","#2077C5"), values = c(0.,0.4,1)
      ) +
      theme_minimal() +
      ggtitle(paste(dirname,count)) + # for the main title
      xlab("Columns") + # for the x axis label
      ylab("Rows")

      #adjustTheme <- ttheme_default(
      #  core = list(fg_params=list(cex = 0.7)),
      #  colhead = list(fg_params=list(cex = 0.7)),
      #  rowhead = list(fg_params=list(cex = 0.7)))

      #tableObject = tableGrob(dataTable,theme= adjustTheme)
      #main_plt <- grid.arrange(arrangeGrob(plt, tableObject, ncol = 1,widths = 1/8))

      message('created heatmap')


      if (pltsave) {
      ggsave(
        filename = paste0(parentpath, Sys.Date(), "_HM_plate", count, ".png"),
        plot = plt,
        scale = 1,
        width = 250,
        height = 175,
        units = "mm",
        dpi = 300,
        limitsize = TRUE
      ) }

      doc <- read_pptx() %>%
        add_slide(layout = "Title and Content") %>%
        ph_with(value=paste(dirname,count, "- Heatmap"), location = ph_location_type(type = "title")) %>%
#        ph_with(value=plt, location = ph_location(left = 1, top= 2, width = 8)) %>%
        ph_with(value=plt, location = ph_location_type(type = "body")) %>%
        ph_with(value=dataTable, location = ph_location(left = 2, top= 6, width = 8))
    } else {
      doc <- read_pptx() %>%
        add_slide() %>%
        ph_with(value=paste(dirname,count, "- Scatter"), location = ph_location_type(type = "title"))
      
    }

    blankDF <- data.frame()
    topShift = 1
    leftShift = 1

    if (trigger[[2]]) {

      # scatter
        #if (fileNameType %in% c("ggcyto", "counter")) {
        plist = list()
        cnt = 1
        if (!autogate) {
          rectGate <- rectangleGate(list("FSC-A" = filter_param$gate_fsc, "SSC-A" = filter_param$gate_ssc))
          set_gate <- sapply(sampleNames(fs), function(sn) rectGate)  
        }

        for (i in seq(1,96)) {
          if (i == 13) {
            topShift = -2
        }

        if (i %in% c(1,13,25,37,49,61,73,85) ) {
          leftShift = -1.75
        }

        if (i %in% blanks) {
          plist[[i]] = createBlankPlot(blankDF, orderedWellID[i], topShift, leftShift)
        }  else {


          tryCatch( {

          #wellIDTitle <- gsub(replaceString,"",basename(keyword(fs[[cnt]])$FILENAME))
          wellIDTitle <- basename(keyword(fs[[cnt]])$FILENAME)
          #wellIDTitle <- fcs_files_ordered[cnt]
          wellIDTitle <- regmatches(wellIDTitle, regexpr("[A-H]\\d{2}", wellIDTitle))
          colID <- as.numeric(substr(wellIDTitle, 2, nchar(wellIDTitle)))
          #message(paste0("colID: ", colID, " wellID: ", wellIDTitle))
          da = fs[[cnt]]
          #da = da[rowMeans(exprs(da[,c("FSC-A","SSC-A")])>0) == 1,]
          
          # if (autogate && i == 1) {
          #      if (!trigger[[1]]) {
          #        if (autogate_index != 1) {
          #           dat = fs[[autogate_index]]
          #           #dat = dat[rowMeans(exprs(dat[,c("FSC-A","SSC-A")])>0) == 1,]
          #        } else {
          #           dat = da
          #        }
                 # set_gate <- openCyto::gate_flowclust_2d(dat, 
                 #            xChannel = "FSC-A", 
                 #            yChannel = "SSC-A", 
                 #            K = filter_param$gate_tgt[4], 
                 #            target = c(filter_param$gate_tgt[1], filter_param$gate_tgt[2]), 
                 #            quantile=filter_param$gate_tgt[3])
               # }
          # }
           
          #p <- ggcyto(da, aes(x=`FSC-A`, y=`SSC-A`)) +
          p <- ggcyto(da, aes(x=!!filter_param$sct_x, y=!!filter_param$sct_y)) +
                      geom_hex(bins=128) +
                      ylab("") +
                      xlab("") +
                      ggtitle(wellIDTitle) +
                      theme(
                        strip.text = element_blank(),
                        plot.title = element_text(size = 6, hjust = 0.5, vjust = -3),
                        axis.text.y = element_blank(),
                        axis.text.x = element_blank(),
                        axis.ticks = element_blank(),
                        plot.margin = unit(c(topShift,1,1,leftShift),"mm"), # (top, right, bottom, and left)
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank()
                )
          
          if (filter_param$sct_x == "FSC-A") {
            if (colID < 9) {
                  p <- suppressMessages(
                          p + 
                          geom_gate(set_gate) +
                          geom_stats(size = 1) +
                          #ggcyto_par_set(limits = list(x = c(10,2.5e5), y = c(10, 2.5e5)))
                          ggcyto_par_set(limits = "instrument")
                        )
            }
          }
          
          plist[[i]] <- as.ggplot(p)
          
          cnt = cnt + 1
        },
        error= function(cond) {
          message(paste("Blank data in flowset:", i, "-", cond))
        } )
        } # else stmt
      } # for loop

      #do.call(grid.arrange,c(plist,ncol=intNumColms))
      #p <- arrangeGrob(grobs=plist, top = paste(dirname, count), bottom = "FSC-A", left = "SSC-A", ncol=intNumColms)

    # } else {
    # 
    #   p <- autoplot(fs,
    #               x = "FSC-A",
    #               y = "SSC-A"
    #             )
    # 
    #   p <- p + ggtitle(dirname) +
    #     ylab("FSC-A") +
    #     xlab("SSC-A") +
    #     theme(
    #       strip.text = element_text(size = 5),
    #       axis.text = element_text(size=8),
    #       plot.title = element_text(size=24, colour="#2077C5", hjust=0.5)
    #     )
    #   }

    message('created scatter plot')

    if (pltsave) {
      p <- arrangeGrob(grobs=plist, top = paste(dirname, count), bottom = filter_param$sct_x, left = filter_param$sct_y, ncol=intDefaultNumColms) #ncol=intNumColms)
      ggsave(
        filename = paste0(parentpath, Sys.Date(), "_SCT_plate", count, ".png"),
        plot = p,
        scale = 1,
        width = 300,
        height = 225,
        units = "mm",
        dpi = 300,
        limitsize = TRUE
      ) }
    } # if-else for trigger[[2]]

    if (trigger[[3]] == T && length(filter_param) > 4) {
      lenDiffparams = length(filter_param)
      param_name_list = c()
      rect.g.hist.list <- c()
      gate_vtr = c()
      plist2 = list()
      hst_seq_vtr <- list()
      cnt = 1
      var_pholder = 1
      var_pholder2 = 1
      nbr_bins = 100
      max_xvals = c(10.0)
      max_yvals = c(100.0)
      left_idxs = c()
      top_idxs = c()
      wellIDs = c()
      reset_lims = c()
      cnt_reset_lims = c()
      hst_fill_colour = c()
      da_list <- list()
      nbr_wells <- 96
      first_colm_hist_cnt <- 55
      first_column <- c(1,13,25,37,49,61,73,85)
      blanks <- union(blanks, c(seq(9,12), seq(21,24), seq(33,36), seq(45,48), seq(57,60),seq(69,72), seq(81,84), seq(93,96)))
      max_fs <- length(fs)
      df_stats <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("INDEX", "SAMPLE_NAME", "MFI+", "MFI-", "rSD+", "rSD-"))
      
      
      message('starting histogram generation')

      non_mapvar = sapply(names(filter_param), function(x) {grep("gate|colour|sct",x)}, USE.NAMES = F)
      exclude_vals = sum(unlist(non_mapvar))
      message(paste0('exclude vals: ', exclude_vals))
      
      nbr_rows <- lenDiffparams-exclude_vals
      
      for (i in seq(1,nbr_rows)) {
          idxRange = names(filter_param)[i]
          pname = filter_param[[i]]
          strSplitRange = strsplit(idxRange, "-")
          endNbr = as.numeric(strSplitRange[[1]][2])
          startNbr = as.numeric(strSplitRange[[1]][1])
          nbr_range = endNbr+1-startNbr
          param_name_list = c(param_name_list, rep(pname, (nbr_range)))
          gate_vtr = c(gate_vtr, rep(i, nbr_range))
          param_val_list = list()
          param_val_list[[pname]] = filter_param[[ paste0("gate_hst_", i) ]]
          rect.g.hist.list <- c(rect.g.hist.list, rectangleGate(param_val_list) )
          hst_fill_colour <- c(hst_fill_colour, 
                               rep(filter_param[[ paste0("colour_", i)]], nbr_range))
      }
      
      print(paste0(rep('-', 25), collapse = ''))
      cat('INDICES FOR DA')
      da_idx_list <- rep(seq(1,12), nbr_rows)
      print(da_idx_list)
      print(paste0(rep('-', 25), collapse = ''))
      cat('BLANKS')
      print(blanks)
      print(paste0(rep('-', 25), collapse = ''))
      cat('GATE VECTOR')
      print(gate_vtr)
      print(paste0(rep('-', 25), collapse = ''))
      cat('PARAM NAME LIST')
      print(param_name_list)
      print(paste0(rep('-', 25), collapse = ''))
      cat('RECT G HSIT LIST')
      print(rect.g.hist.list)
      print(paste0(rep('-', 25), collapse = ''))
      cat('HST FILL COLOUR')
      print(hst_fill_colour)

      biexpTrans <- flowjo_biexp_trans(channelRange=4096, maxValue=262144
                                       , pos=4.4,neg=0, widthBasis=-100)

      #fs_sub <- Subset(fs, rectGate)
      col8_wids <- c(1, grep("[A-H]08", orderedWellID))
      
      # create new flow set with only 64 experiments
      for (idx in seq_along(col8_wids)) {
        #print(idx)
        if (idx == length(col8_wids)) {
          break
        }
        if (idx == 1) {
          tmp_fset <- fs[idx:8]
          hst_seq_vtr[[idx]] <- c(idx, 8)
        } else {
          start = col8_wids[idx]+5
          end <- col8_wids[idx+1]
          hst_seq_vtr[[idx]] <- c(start, end)
          tmp_fset <- rbind2(tmp_fset, fs[start:end])
          #print(paste("idx:", idx, "start:", start, "end:", end))
        }
      }
      
      # overwrite fs
      fs <- tmp_fset
      #print(paste0(rep('-', 25), collapse = ''))
      #cat("FLOWSET")
      #print(fs)
      print(paste0(rep('-', 25), collapse = ''))
      cat("HST SEQ VTR")
      print(hst_seq_vtr)
      
      for (i in seq(1,nbr_wells)) {
        if (i == 13) {
          topShift = -1
        } else {
          topShift = 0.25
        }

        if (i %in% first_column ) {
          leftShift = -1.75
        } else {
          leftShift = 0.25
        }

        if (i %in% blanks) {
          message(paste0("Inserting blank plot: ", i))
          plist2[[i]] = createBlankPlot(blankDF, orderedWellID[i], topShift, leftShift)
          
          
          # reset limits for current row
          if (i %% 12 == 0) {
              j = 1  
              max_x <- max(max_xvals)
              max_y <- max(max_yvals)
              
              start = hst_seq_vtr[[var_pholder]][1]
              end = hst_seq_vtr[[var_pholder]][2]
              print(paste0("start: ", start, " end: ", end))
              
              for (k in seq(start, end)) {
                
                message(paste0('generating hst plot #: ', k))
                # have to retrieve the data again since not within same loop counter
                gs <- Subset(fs[var_pholder2], set_gate)
                gs <- GatingSet(gs)
                tf <- transformerList(param_name_list[k], biexpTrans)
                tfd <- transform(gs, tf)
                da <- gs_cyto_data(tfd)
                g_pop2 <- openCyto:::.mindensity(da[[1]], channels = param_name_list[k])
                p <- ggcyto(da, aes(x = !!param_name_list[k]))
                if (!k %in% first_column) {
                  p <- p + 
                       geom_histogram(stat = "bin", bins = nbr_bins, fill = hst_fill_colour[k]) +
                       geom_gate(g_pop2) + 
                       geom_stats(size = 1.5,
                                  adjust = c(0.5, filter_param$gate_yadj),
                                  label.padding = unit(0.05, "lines"))
                  #theme(panel.border = element_rect(colour = "green", fill=NA, size=5))
                } else {
                  p <- p + 
                    geom_histogram(stat = "bin", bins = first_colm_hist_cnt, fill = hst_fill_colour[k])
                }
                
                
                plist2[[k]] <- gcy_plot(p, max_x, max_y, top_idxs[j], left_idxs[j], wellIDs[j], param_name_list[k])
                j <- j + 1
                var_pholder2 <- var_pholder2 + 1
                
              }
            
            #print(length(plist2))
            
            # init for next row
            max_xvals = c(10.0)
            max_yvals = c(100.0)
            left_idxs = c()
            top_idxs = c()
            reset_lims = c()
            cnt_reset_lims = c()
            wellIDs = c()
            
            ## RIDGEPLOTS
            if (trigger[[4]] && pltsave) {
              row_nbr <- round(i / 12)
              letter <- LETTERS[1:8][row_nbr] 
              #message(paste0("row nbr: ", row_nbr, " letter: ", letter))
              
              p <- popRidgePlot(letter, param_name_list[i], da_list, fcs_files_ordered)
              ggsave(
                filename = paste0(parentpath, Sys.Date(), "_RGD_row_", letter, "_plate", count, ".png"),
                plot = p,
                scale = 1,
                width = 300,
                height = 225,
                units = "mm",
                dpi = 300,
                limitsize = TRUE
              )
              message(paste0('created ridge plot row: ', letter))
            }
            da_list <- list() # init for next row    
            var_pholder = var_pholder + 1
          } # end of i %% 12 == 0
          
        } else {

        tryCatch( {
          if (cnt > max_fs) {
            plist2[[i]] = createBlankPlot(blankDF, orderedWellID[i], topShift, leftShift)
            next
          }
          wellIDTitle <- basename(keyword(fs[[cnt]])$FILENAME)
          wellIDTitle <- regmatches(wellIDTitle, regexpr("[A-H]\\d{2}", wellIDTitle))
          wellIDs <- c(wellIDs, wellIDTitle)
          gs <- Subset(fs[cnt], set_gate)
          gs <- GatingSet(gs)
          tf <- transformerList(param_name_list[i], biexpTrans)
          tfd <- transform(gs, tf)
          da <- gs_cyto_data(tfd)
          g_pop2 <- openCyto:::.mindensity(da[[1]], channels = param_name_list[i])
          dtmp <- data.frame(exprs(da[[1]])[, param_name_list[i]])
          names(dtmp) <- param_name_list[i]
          # for ridge plots
          da_list[[da_idx_list[i]]] <- dtmp
          
          # stats
          d_neg <- gs@data[[1]]@exprs[, param_name_list[i]][gs@data[[1]]@exprs[, param_name_list[i]] < g_pop2@min]
          d_pos <-gs@data[[1]]@exprs[, param_name_list[i]][gs@data[[1]]@exprs[, param_name_list[i]] > g_pop2@min]
          rsd_neg <- RSD_FN(d_neg)
          rsd_pos <- RSD_FN(d_pos)
          mfi_neg <- median(d_neg)
          mfi_pos <- median(d_pos)
          
          df_stats[cnt, "INDEX"] <- cnt
          df_stats[cnt, "SAMPLE_NAME"] <- basename(keyword(fs[[cnt]])$FILENAME)
          df_stats[cnt, "MFI+"] <- mfi_pos
          df_stats[cnt, "MFI-"] <- mfi_neg
          df_stats[cnt, "rSD+"] <- rsd_pos
          df_stats[cnt, "rSD-"] <- rsd_neg
          
          
          xvar <- sym(param_name_list[i])
          p <- ggplot(da, aes(x= !!xvar)) +
            geom_histogram(stat="bin", bins = nbr_bins)
          
          df <- data.frame(ggplot_build(p)$data[[1]])
          
          # get max x value
          
          max_x <- max(max_xvals)
          curr_xmax <- (df$xmax[nbr_bins])
          if (is.na(curr_xmax)) {curr_xmax = 1e-2}
          #message(paste0('max_x: ', max_x,' curr_xmax: ', curr_xmax))
          max_xvals <- c(max_xvals, curr_xmax)
          curr_xmax <- cmpr_max(curr_xmax, max_x)
          # get max y value and increase it by a little
          max_y <- max(max_yvals)
          curr_ymax <- max(df$count)
          if (is.na(curr_ymax)) {curr_ymax = 1e-10}
          #message(paste0('max_y: ', max_y,' curr_ymax: ', curr_ymax))
          max_yvals <- c(max_yvals, curr_ymax)
          curr_ymax <- cmpr_max(curr_ymax, max_y)
          
          # p <- ggcyto(da,
          #       aes(x = !!param_name_list[i]))
          #       # geom_density(fill='black', alpha = 0.6) +
          #       # geom_gate(rect.g.hist.list[[gate_vtr[i]]]) +
          #       
          # if (!i %in% first_column) {
          #       p <- p + 
          #            geom_histogram(stat="bin", bins = nbr_bins, fill = hst_fill_colour[i]) +
          #            geom_gate(g_pop2) + 
          #            geom_stats(size = 1.5,
          #                  adjust = c(0.5, filter_param$gate_yadj),
          #                  label.padding = unit(0.05, "lines"))
          # } else {
          #   p <- p + geom_histogram(stat="bin", bins = first_colm_hist_cnt, fill = hst_fill_colour[i])
          # }
          
          if (curr_xmax[2] > max_x | curr_ymax[2] > max_y) {
            message(paste0('requires resetting lims: ', i))
            reset_lims <- c(reset_lims, i)
            cnt_reset_lims <- c(cnt_reset_lims, cnt)
            max_y <- max(max_yvals)
          }
          
          top_idxs <- c(top_idxs, topShift)
          left_idxs <- c(left_idxs, leftShift)
            
          # plist2[[i]] <- gcy_plot(p,
          #                         max_x,
          #                         max_y,
          #                         topShift,
          #                         leftShift,
          #                         wellIDTitle,
          #                         param_name_list[i])
          
          cnt = cnt + 1
          
          },
            error= function(cond) {
            message(paste("Error in producing histogram: ", i, " -", cond, "/", cnt))
            plist2[[i]] = createBlankPlot(blankDF, orderedWellID[i], topShift, leftShift)
          } )
        }
      }
      
      
      #test <<- plist2
      
      if (pltsave) {
        p <- arrangeGrob(grobs=plist2, top = paste(dirname, count), bottom = "Param", left = "Count", ncol=intDefaultNumColms)
        
        ggsave(
          filename = paste0(parentpath, Sys.Date(), "_HST_plate", count, ".png"),
          plot = p,
          scale = 1,
          width = 300,
          height = 225,
          units = "mm",
          dpi = 300,
          limitsize = TRUE
        )
        message('created histogram plot')
        
        # calculate SI and SN
        df_stats <- df_stats %>% 
          dplyr::mutate(SI= (`MFI+` - `MFI-`)/(2*`rSD-`)) %>%
          dplyr::mutate(SN= (`MFI+`/`MFI-`))
        
        
        write.csv(df_stats,paste0(parentpath, Sys.Date(), "_DF_plate", count, ".csv"), row.names = FALSE)
        df_stats <- df_stats %>%
                      flextable::flextable() %>%
                      flextable::colformat_num() %>%
                      flextable::bold(bold = TRUE, part = "header") %>%
                      flextable::fontsize(size  = 5, part = "body") %>%
                      flextable::fontsize(size  = 6.5, part = "header")
        doc %>%
          add_slide(layout = "Blank") %>%
          ph_with(value=df_stats, location = ph_location(left = 0, top= 0, width = 14))
        
        message('created dataframe for stats')
      }
    }

    
      #tryCatch({
      #  doc <- add_slide(doc) %>%
      #    ph_with(value = dml(grid.arrange(grobs = plist, ncol = intDefaultNumColms)), location = ph_location_fullsize())
      #  },
      #error = function(cond) {
      #  message(cond)
      #  message("passing embedding of scatter plot in ppt slide")
      #},
      #finally={
      #  print(doc, target = paste0(parentpath, Sys.Date(), "_dataSlides_", count, ".pptx"))
      #}
    #)
      print(doc, target = paste0(parentpath, Sys.Date(), "_dataSlides_", count, ".pptx"))
}



param_list = list("1-12"="V-450-A",
                  "13-24"="UV-379-A",
                  "25-36"="B-525-A",
                  "37-48"="YG-582-A",
                  "49-60"="V-450-A",
                  "61-72"="UV-379-A",
                  "73-84"="B-525-A",
                  "85-96"="YG-582-A",
                  #"gate_fsc" = c(1e5, 2e5),
                  #"gate_ssc" = c(3e4, 1e5),
                  "sct_x" = "FSC-A",
                  "sct_y" = "SSC-A",
                  "gate_yadj" = 1e6,
                  "gate_tgt" = c(1e5, 6e4, 0.8, 4), #fcs-a end, start, quantile, K
                  "gate_hst_1"= c(1.7e3, 3.8e3),
                  "gate_hst_2"= c(2.4e3, 3.8e3),
                  "gate_hst_3"= c(2e3, 3.8e3),
                  "gate_hst_4"= c(2.2e3, 3.8e3),
                  "gate_hst_5"= c(1.7e3, 3.8e3),
                  "gate_hst_6"= c(2.4e3, 3.8e3),
                  "gate_hst_7"= c(2e3, 3.8e3),
                  "gate_hst_8"= c(2.2e3, 3.8e3),
                  "colour_1"="#071e3d", 
                  "colour_2"="#1f4287", 
                  "colour_3"="#278ea5", 
                  "colour_4"="#21e6c1",
                  "colour_5"="#071e3d", 
                  "colour_6"="#1f4287", 
                  "colour_7"="#278ea5", 
                  "colour_8"="#21e6c1"
                  )


param_list = list("1-12"="V-450-A",
                  "13-24"="V-450-A",
                  "25-36"="V-450-A",
                  "37-48"="V-450-A",
                  "49-60"="V-450-A",
                  "61-72"="V-450-A",
                  "73-84"="V-450-A",
                  "85-96"="V-450-A",
                  #"gate_fsc" = c(5e4, 1.5e5),
                  #"gate_ssc" = c(3e4, 1e5),
                  #"sct_x" = "V-450-A",
                  "sct_x" = "FSC-A",
                  "sct_y" = "SSC-A",
                  "gate_yadj" = 1e6,
                  "gate_tgt" = c(1e5, 6e4, 0.8, 4), #fcs-a end, start, quantile, K
                  "gate_hst_1"= c(1.7e3, 3.8e3),
                  "gate_hst_2"= c(1.7e3, 3.8e3),
                  "gate_hst_3"= c(1.7e3, 3.8e3),
                  "gate_hst_4"= c(1.7e3, 3.8e3),
                  "gate_hst_5"= c(1.7e3, 3.8e3),
                  "gate_hst_6"= c(1.7e3, 3.8e3),
                  "gate_hst_7"= c(1.7e3, 3.8e3),
                  "gate_hst_8"= c(1.7e3, 3.8e3),
                  "colour_1"="#98a0ab", 
                  "colour_2"="#98a0ab", 
                  "colour_3"="#98a0ab", 
                  "colour_4"="#98a0ab",
                  "colour_5"="#98a0ab", 
                  "colour_6"="#98a0ab", 
                  "colour_7"="#98a0ab", 
                  "colour_8"="#98a0ab"
)


parentpath = "C:/Users/10322096/Documents/fcs_data/30Jun0221 8 Old vs 8 New Test/"
subdirName = "Old 8 - Plate "
# (1) heatmap (2) scatter (3) histogram (4) ridgeplot
trg = list(T,T,T,T)
trg = list(T,F,T,F)

generatePlots(dirname = subdirName, 
              count = 1, 
              fileNameType = "counter",
              autogate = T,
              autogate_index = 1,
              replaceString = "Specimen_00\\d{1}_", 
              pltsave = T, 
              trigger = trg, 
              filter_param = param_list)
  