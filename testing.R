setwd("C:/Users/10322096/Documents/gitrepos/metadata-file-shiny-app/")
install.packages("renv")
renv::restore()

renv::modify()
renv::install("colourpicker")
renv::record(records = 'colourpicker')

renv::snapshot()



files = list.files("./LWB/P1/FCS Files/", pattern = "*.fcs$")
files_ = sapply(files, function(x) grep("fcs$", x, value=T), USE.NAMES = F)
order_files = order(as.integer(sub(".*?(0[0-9]{2}).fcs$", "\\1", files_)))
order_files[!is.na(order_files)]
files_[order_files]


letterMatrix <- sapply(LETTERS[1:8], function(x) rep(x,12), USE.NAMES = F)
orderedWellID <- mapply(zeroPadpaste0, as.vector(letterMatrix), rep(seq(1,12),8), USE.NAMES = F)



fcs_files <- list.files(paste0(parentpath, subdirName, 1), pattern = ".fcs")
fcs_files
orderedIndex <- sapply(fcs_files, getCounter, USE.NAMES = F)

orderedIndex <- sapply(fcs_files, function(x) {
  match(getAlpha(x,F), orderedWellID)
}, USE.NAMES = F)
orderedIndex
#fcs_files <- list.files(paste0(dir_path), pattern = ".fcs")
#orderIndex = sapply(fcs_files, getCounter, USE.NAMES = F)
fcs_files_ordered <- fcs_files[order(orderedIndex, fcs_files)]
fs <- flowCore::read.flowSet(fcs_files_ordered, path=(paste0(parentpath, subdirName, 1)))
fs

dat <- fs[[2]]
x <- seq(1:12)
y <- LETTERS[1:8]
blanks <- setdiff(seq(96),orderedIndex)
fsOutput = sapply(seq_along(fs), function(x) {
  filter_res = flowCore::filter(fs[[x]], set_gate)
  summary(filter_res)$true
})

  data <- expand.grid(X=x, Y=y)
  tmpVector <- c()
  cnt = 1
  for (i in seq(96)) {
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
  validWells <- validWells[2:length(validWells), ]

set_gate <- openCyto::gate_flowclust_2d(dat, 
                                        xChannel = "FSC-A", 
                                        yChannel = "SSC-A", 
                                        K = 3, 
                                        target = c(1e5, 6e4), 
                                        quantile=0.8)


#fs_sub <- Subset(fs, rect.g)
tf <- estimateLogicle(gs[[1]], channels = test)
#OR
biexpTrans <- flowjo_biexp_trans(channelRange=256, maxValue=262144
                                 , pos=4.41854,neg=0, widthBasis=-100)
test <- "V-450-A"

rect.g <- rectangleGate(filterId = "main",list("FSC-A" = c(1e5, 2e5),"SSC-A" = c(3e4, 1e5)))
rect.g2 <- rectangleGate(filterId = "sec", "V-450-A"=c(0,1200))
#rect.gates <- sapply(sampleNames(fs), function(sn) rect.g)
fsApply(fs, each_col, median)

fs[[7]]@exprs[,'V-450-A']
quantile(fs[[7]]@exprs[,'V-450-A'], probs=0.5)

MFI_FN <- function(fr) {
  res <- quantile(fr@exprs[, 'V-450-A'], probs = 0.5)
  names(res) = "MFI"
  res
}

RSD_RN <- function(fr) {
  col = fr@exprs[, "V-450-A"]
  diff = 0.3413
  lowerUpper = quantile(col, c(0.5-diff, 0.5+diff))
  cat(lowerUpper)
  res = (lowerUpper[[2]] - lowerUpper[[1]]) /2
  cat(res)
  names(res) = 'rSD'
  res
}

RSD_FN <- function(df) {
  diff = 0.3413
  lowerUpper = quantile(df, c(0.5-diff, 0.5+diff))
  res = (lowerUpper[[2]] - lowerUpper[[1]]) /2
  message(paste0("lowerUpper 1: ", lowerUpper[[1]],"lowerUpper 2: ", lowerUpper[[2]], " res: ", res))
  res
}



idx = 8

setlims <- ggcyto_par_set(limits = "instrument")
dat <- fs[idx]
g <- openCyto::gate_flowclust_2d(dat[[1]], xChannel = "FSC-A", yChannel = "SSC-A", K = 4, target = c(1e5, 4e4), quantile=0.8)
p <- autoplot(dat, x="FSC-A", y="SSC-A", bins = 200) 

p

p + geom_gate(g) 

ggcyto(dat, aes(x = !!test, y = "SSC-A")) + geom_hex(bins=150)

gs_sub <- Subset(fs[idx], g)
gs <- GatingSet(gs_sub)
tf <- transformerList(test, biexpTrans)
tfd <- transform(gs, tf)
da <- gs_cyto_data(tfd)
g_ <- openCyto:::.mindensity(da[[1]], channels = test)
max_y <- max(gs@data[[1]]@exprs[, test])
max_y

ggcyto(da, aes(x = !!test)) +
  geom_histogram(stat = "bin", bins = 100) +
  #geom_density(aes(y = ..count..), fill='black') +
  #ggcyto_par_set(limits = list(y =c(0, 500), x = c(50, 250))) +
  geom_gate(g_) +
  geom_stats(size = 8,
             adjust = c(0.5, 1e7),
             label.padding = unit(0.05, "lines"))




d_low <- gs_sub[[1]]@exprs[, 'V-450-A'][gs_sub[[1]]@exprs[, 'V-450-A'] < g_@min]
d_up <-gs_sub[[1]]@exprs[, 'V-450-A'][gs_sub[[1]]@exprs[, 'V-450-A'] > g_@min]
RSD_FN(d_up)
median(d_low)
median(d_up)



da = dat[rowMeans(exprs(dat[,c("FSC-A","SSC-A")])>0) == 1,]
as.ggplot(ggcyto(da, aes(x=`FSC-A`, y=`SSC-A`)) +
            geom_hex(bins=128) +
            ylab("") +
            xlab("") +
            ggtitle("TEST") +
            theme(
              strip.text = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
            ) +
            geom_gate(g) +
            geom_stats(size = 5)
          )
###


filter_res = flowCore::filter(fs[[1]], rect.g)
summary(filter_res)$true





tf <- transformerList(test, biexpTrans)
gs_sub1 <- Subset(fs[7], rect.g)
gs_sub2 <- Subset(gs_sub1, rect.g2)

gs_sub1 <- Subset(fs[1], g)
gs_sub2 <- Subset(gs_sub1, g_)

gsm1 <- GatingSet(gs_sub1)
gsm2 <- GatingSet(gs_sub2)
tfd2 <- transform(gsm2, tf)
tfd1 <- transform(gsm1, tf)

gs_pop_get_stats(tfd2, type = RSD_RN, inverse.transform = T)
gs_pop_get_stats(tfd2, type = MFI_FN, inverse.transform = T)


ggcyto(gs_cyto_data(tfd1), aes(x=!!test)) +geom_histogram(bins=500) + geom_gate(rect.g2, size=1) + geom_stats(size = 5,
                                                                                                  adjust = c(0.5,1.5e6),
                                                                                                  label.padding = unit(0.05, "lines"))

ggcyto(fs[[7]], aes(x='FSC-A', y='SSC-A')) + geom_hex(bins=300)
#ggcyto(gs_cyto_data(tfd)[[1]], aes(x='V-450-A')) + geom_histogram(bins=300) + geom_gate(rect.g2) + geom_stats()

gs <- Subset(fs[7], rect.g)
rs <- filter(fs[[7]], rect.g)
summary(rs)$true
summary(rs)$n
summary(rs)$p


gs <- GatingSet(gs)
tf <- transformerList(test, biexpTrans)
tfd <- transform(gs, tf)
dd <- data.frame(exprs(gs_cyto_data(tfd)[[1]])[, test])
names(dd) <- "x"
head(dd)
nrow(dd)
#gs_pop_get_stats(gs, type = "percent")
ggplot(dd, aes(x=x)) + geom_histogram(bins = 200)

gs <- GatingSet(fs[7])
tfd <- transform(gs, tf)



fr <- gh_pop_get_data(obj = tfd1)#, y = test)
p<-ggcyto(fr, aes(x=!!test)) + geom_histogram(bins=300) + geom_gate(g_, size=1) + geom_stats(size = 5,
                                                                                     adjust = c(0.5,1.5e6),
                                                                                     label.padding = unit(0.05, "lines"))

p<-ggcyto(fr, aes(x=!!test)) + geom_density(aes(y = ..count..)) + geom_gate(g_, size=1) + geom_stats(size = 5,
                                                                                             adjust = c(0.5,1.5e6),
                                                                                             label.padding = unit(0.05, "lines"))

p

p<-as.ggplot(ggcyto(gs_cyto_data(tfd)[[1]], aes(x='V-450-A')) + geom_density())
p
dp<- data.frame(ggplot_build(p)$data[[1]])


library(pracma)
v <- findpeaks(-dp$y)
x_valley <- dp$x[v[,2]]
dv <- data.frame("valleys_x"=x_valley, "valleys_y"=dp$y[v[,2]])
ggplot(dp, aes(x=x, y=y)) +
  geom_line() + 
  geom_point (data = dv, mapping = aes(x = valleys_x, y= valleys_y)) +
  geom_vline(xintercept = x_valley)
#library(dplyr)
#dp %>% 
#  mutate(c = if_else(x < x_valley, 'NEG', 'POS'))



dp$gate <- with(dp, ifelse(x<x_valley, 'NEG', 'POS'))

#transform(dp, gate= ifelse(x<x_valley, 'NEG', 'POS'))

#nrow(dp %>% filter(gate == 'NEG')) cant use dplyr cause it conflicts with flowcore etc.

neg_pop <- nrow(dp[dp$gate == 'NEG',])
all_pop <- nrow(dp)
pct_neg <- (neg_pop/all_pop)*100
pct_neg
pct_pos <- 100 - pct_neg
pct_pos

mfi_neg <- median(dp$x[dp$gate=='NEG'])
mfi_pos <- median(dp$x[dp$gate=='POS'])

upper_rng <- mfi_neg + mfi_neg*0.3413
lower_rng <- mfi_neg - mfi_neg*0.3413

(upper_rng + lower_rng)/2

pct75 <- quantile(dp$x[dp$gate=='NEG'], 0.75)
pct25 <- quantile(dp$x[dp$gate=='NEG'], 0.25)

rCV_neg = (pct75[[1]] - pct25[[1]])/mfi_neg*100
rCV_neg

rSD_neg = mfi_neg*(rCV_neg/50)
rSD_neg


df <- data.frame()

# A06
d2 <- data.frame(exprs(gs_cyto_data(tfd)[[1]])[, test])
d2 <- cbind(d2, "A06")
names(d2) <- c(test, "well_id")
# A12
d0 <- data.frame(exprs(gs_cyto_data(tfd)[[1]])[, test])
d0 <- cbind(d0, "A12")
names(d0) <- c(test, "well_id")

df <- rbind(d0,d2)
da_list <- list()

for (i in seq(1,12)) {
  gs <- GatingSet(fs[i])
  tf <- transformerList(test, biexpTrans)
  tfd <- transform(gs, tf)
  da <- gs_cyto_data(tfd)
  dtmp <- data.frame(exprs(da[[1]][, test]))
  names(dtmp) <- test
  da_list[[i]] <- dtmp
}

for (i in seq(1,12)) {
  d <- da_list[[i]]
  well_id <- fcs_files_ordered[i]
  well_id <- regmatches(well_id, regexpr("[A-H]\\d{2}", well_id))
  d <- cbind(d, data.frame("well_id" = well_id))
  df <- rbind(df, d)
}

names(df)[1] <- test
head(df)

d <- data.frame(x = rep(1:5, 3), y = c("A", "B", "C"),
                height = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1))
ggplot(d, aes(x,y)) + geom_density_ridges(fill="lightblue")#geom_ridgeline(fill="lightblue")

d <- data.frame(x = rep(1:5, 3), y = c(rep(0, 5), rep(1, 5), rep(3, 5)),
                height = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1))
ggplot(d, aes(x, y, height = height, group = y)) + geom_ridgeline(fill="lightblue")

ggplot(df, aes(x = !!test, y = well_id, fill = ..x..)) + #, height = ..density..)) + 
  geom_density_ridges_gradient(show.legend = F, panel_scaling = F) +
  theme_ridges() +
  theme(
    panel.spacing = unit(0.1, "lines")
  )


p0 <-as.ggplot(ggcyto(gs_cyto_data(tfd), aes(x= !!test))) 
p1 <-as.ggplot(ggcyto(gs_cyto_data(tfd), aes(x= !!test)))  
p
p_d0 <- p0 + geom_density(fill='blue')
p_d1 <- p1 + geom_density(fill='blue')
#p_h <- p + geom_histogram(stat='bin', bins = 400)

d_d0 <- data.frame(ggplot_build(p_d0)$data[[1]])
d_d1 <- data.frame(ggplot_build(p_d1)$data[[1]])

dr0 <- data.frame(x = d_d0$x, y = d_d0$y/d_d0$n)
dr1 <- data.frame(x = d_d1$x, y = d_d1$y)

dr01 <- cbind(dr1[c('x','y')], data.frame("well_id"="A06", "ypos"=2e-4))
dr01 
dr00 <- cbind(dr0[c('x','y')], data.frame("well_id"="A12", "ypos"=0))
dr00


df <- rbind(dr00, dr01) 


ggplot(df, aes(x, ypos, height = y, group = ypos)) + geom_ridgeline(fill="blue")


ggplot(df, aes(x=x, y=y, fill=well_id)) +
  geom_area() +
  geom_line()

parlist <- list("V-450-A",
                "UV-379-A",
                "B-525-A",
                "YG-582-A",
                "V-450-A",
                "UV-379-A",
                "B-525-A",
                "YG-582-A",
                "V-450-A",
                "V-450-A",
                "V-450-A",
                "V-450-A")

parlist
df <- data.frame()
ypos = 0

for (i in seq(1,12)) {
  gs <- GatingSet(fs[i])
  tf <- transformerList(test, biexpTrans)
  tfd <- transform(gs, tf)
  da <- gs_cyto_data(tfd)
  p <-as.ggplot(ggcyto(da, aes(x= !!parlist[[i]])))
  pd <- p + geom_density()
  dd <- data.frame(ggplot_build(pd)$data[[1]])
  if (i > 8) {
    dr <- data.frame(x = dd$x, y = dd$y/dd$n)
  } else {
    dr <- data.frame(x = dd$x, y = dd$y)
  }
  well_id <- fcs_files_ordered[i]
  well_id <- regmatches(well_id, regexpr("[A-H]\\d{2}", well_id))
  dr <- cbind(dr[c('x','y')], data.frame("well_id"=well_id, "ypos"=ypos))
  names(dr)[2] <- 'height'
  df <- rbind(df, dr) 
  ypos <- ypos + 2e-4
}



ggplot(df, aes(x, ypos, height = height, group = ypos)) + geom_ridgeline(fill="blue") + ylim(c(0,0.003)) + xlim(c(0, 1e4))









library(pracma)
# Returns a matrix where each row represents one peak found. 
# (1) The first column gives the height, 
# (2) the second the position/index where the maximum is reached, 
# (3) the third and forth the indices of where the peak begins and ends 
# --- in the sense of where the pattern starts and ends.
p <- findpeaks(dr00$y)
v <- findpeaks(-dr00$y)
dp <- data.frame("peaks_x"=dr00$x[p[,2]], "peaks_y"=dr00$y[p[,2]])
dv <- data.frame("valleys_x"=dr00$x[v[,2]], "valleys_y"=dr00$y[v[,2]])
ggplot(dr00, aes(x=x, y=y)) +
  geom_line() + 
  geom_point(data = dp, mapping = aes(x = peaks_x, y = peaks_y)) +
  geom_point (data = dv, mapping = aes(x = valleys_x, y= valleys_y))



p<-as.ggplot(ggcyto(gs_cyto_data(tfd), aes(x= !!test)) +
               geom_density(fill="blue") +
               #geom_histogram(stat="bin", bins = 400, fill='blue') +
               #geom_gate(rect.g.hist.list[[1]]) + 
               #geom_stats(size = 2.5, adjust = c(0.5,2e6), label.padding = unit(0.08, "lines")) + 
               #ggcyto_par_set(limits = list(x = c(0, 4e3),y = c(0, 2e2))) +
               #ggcyto_par_set(limits = "instrument") +
               ylab("") +
               xlab(test) +
               ggtitle("A01") +
               theme(
                 strip.text = element_blank(),
                 plot.title = element_text(size = 5, hjust = 0.5, vjust = -5),
                 #axis.ticks = element_blank(),
                 axis.text.y = element_text(size = 5),
                 #axis.text.x=element_blank(),
                 #axis.ticks.x = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 axis.title.x = element_text(size=5,vjust = 10)
               ) +
               scale_y_continuous(breaks = scales::pretty_breaks(n = 2)) #+
               #scale_x_continuous(breaks = scales::pretty_breaks(n = 2))
)
p

d <-exprs(fs[[6]])[,"V-450-A"]
transform(d, tf)
build_gs_pop_get_data(tfd)[[1]]
x1 <- data.frame(ggplot_build(p)$data[[1]])
x1$xmin[1]
x1$xmax[400]

cmpr_max <- function(value, curr_max) {
  if (value > curr_max) {
    return(c(T,value))
  } 
    return(c(F,curr_max))
}


reset_lims = c()
plist2 = list()
max_xvals = c(10.0)
max_yvals = c(100.0)
nbr_bins = 400
tf <- transformerList("UV-379-A", biexpTrans)
biexpTrans <- flowjo_biexp_trans(channelRange=4096, maxValue=262144
                                 , pos=4.4,neg=0, widthBasis=-100)

rect.g.hist.list <- c()
param_name_list = c()
gate_vtr = c()
non_mapvar = sapply(names(param_list), function(x) {grep("gate|lim",x)}, USE.NAMES = F)
exclude_vals = sum(unlist(non_mapvar))
lenDiffparams = length(param_list)
message(paste0('exclude vals: ', exclude_vals))

for (i in seq(1,lenDiffparams-exclude_vals)) {
  idxRange = names(param_list)[i]
  pname = param_list[[i]]
  strSplitRange = strsplit(idxRange, "-")
  endNbr = as.numeric(strSplitRange[[1]][2])
  startNbr = as.numeric(strSplitRange[[1]][1])
  param_name_list = c(param_name_list, rep(pname, (endNbr+1-startNbr)))
  gate_vtr = c(gate_vtr, rep(i, endNbr+1-startNbr))
  param_val_list = list()
  param_val_list[[pname]] = param_list[[ paste0("gate_hst_", i) ]]
  rect.g.hist.list <- c(rect.g.hist.list, rectangleGate(param_val_list) )
}



for (i in seq(1,12)){
  tryCatch({
  gs <- GatingSet(fs[i])
  tf <- transformerList(param_name_list[i], biexpTrans)
  tfd <- transform(gs, tf)
  da <- gs_cyto_data(tfd)
  xtest = sym(param_name_list[i])
  p <- ggplot(da, aes(x= !!xtest)) +
    geom_histogram(stat="bin", bins = nbr_bins)
  df <- data.frame(ggplot_build(p)$data[[1]])
  # get max x value
  max_x <- max(max_xvals)
  curr_xmax <- (df$xmax[nbr_bins])
  max_xvals <- c(max_xvals, curr_xmax)
  curr_xmax <- cmpr_max(curr_xmax, max_x)
  # get max y value and increase it by a little
  max_y <- max(max_yvals)
  curr_ymax <- max(df$count)*1.1
  max_yvals <- c(max_yvals, curr_ymax)
  curr_ymax <- cmpr_max(curr_ymax, max_y)
  p <- ggcyto(da, aes(x= !!param_name_list[i])) +
        geom_histogram(stat="bin", bins = nbr_bins, fill='blue', alpha=0.5) +
        geom_gate(rect.g.hist.list[[gate_vtr[i]]]) + 
        geom_stats(size = 2.5, adjust = c(0.5,0.5)) 
  
  if (curr_xmax[2] > max_x | curr_ymax[2] > max_y) {
    reset_lims <- c(reset_lims, i)
  }
  plist2[[i]] <- gcy_plot(p, max_x, max_y, 1,1,paste0('well',i), param_name_list[i]) 
  },
error= function(cond) {
  plist2[[i]] <- createBlankPlot(data.frame(), orderedWellID[i], 0, 0)
  message(paste("Error in producing histogram: ", i, " -", cond))
}
  )}

po <- arrangeGrob(grobs=plist2, top = "test", bottom = "Param", left = "Count", ncol=intDefaultNumColms)
ggsave(
  filename = paste0(parentpath, Sys.Date(), "_HST_plate", 1, ".png"),
  plot = po,
  scale = 1,
  width = 300,
  height = 225,
  units = "mm",
  dpi = 300,
  limitsize = TRUE
)
do.call(grid.arrange,c(plist2,ncol=12))
#plist2[[19]]

max_x <- max(max_xvals)
max_y <- max(max_yvals)

if (length(reset_lims) > 0) {
  for (i in reset_lims) {
    gs <- GatingSet(fs[i])
    tfd <- transform(gs, tf)
    da <- gs_cyto_data(tfd)
    p <- ggcyto(da, aes(x= `UV-379-A`)) +
      geom_histogram(stat="bin", bins = nbr_bins, fill='blue', alpha=0.5) +
      geom_gate(rectangleGate("UV-379-A"=c(2.4e3,3.8e3))) + 
      geom_stats(size = 6, adjust = c(0.5,0.5))
      plist2[[i]] <- gcy_plot(p, max_x, max_y)
  }
}

do.call(grid.arrange,c(plist2,ncol=12))
plist2[[2]]


p2 <- autoplot(gs_cyto_data(gs)[[1]], "UV-379-A") #+
#ggcyto_par_set(limits = "instrument")
p2

df <- as.data.frame(fsApply(fs[16], range)[[1]])

df[, 'UV-379-A']

gs_pop_add(gs, rect.g)
gs_pop_add(gs, rect.g2)

recompute(gs)
autoplot(gs, "main", bins = 128)

gs_pop_get_stats(gs, "sec", type = pop.MFI)

summary(filter(fs[[15]],
               kmeansFilter("FSC-H"=c("Low", "Medium", "High"),
                            filterId="myKMeans")))

pname = "B-525-A"
val <- c(1.3e4,1e5)
L <- list()
L[[pname]] <- val
L
rect.g.hist.list <- rectangleGate(L)
filter_param = param_list
lenDiffparams = length(filter_param)
param_name_list = c()
rect.g.hist.list <- c()
gate_vtr <- c()




da <- flowJo_biexp_trans(channelRange=4096, maxValue=262144
                           , pos=4.5,neg=0, widthBasis=-10)
p <- ggcyto(fs_sub[[64]], aes(x= !!param_name_list[64])) + 
  #geom_gate(rect.g.hist.list[[2]]) +
  geom_gate(rectangleGate("B-525-A" = c(500,2000))) + 
  geom_density(fill='black') +
  ggcyto_par_set(limits = list(x = c(-100, 2e3), y = c(0,2e-3))) +
  geom_stats(size = 10)
p

p <- ggcyto(fs[[15]], aes(x= `UV-379-A`)) +
  geom_density(fill='black', alpha = 0.4) +
  #geom_histogram(stat="bin", bins = 900, fill='blue', alpha=0.5) +
  ggcyto_par_set(limits = list(x = c(-1e3, 7e4), y = c(0, 6e-5)))
  #geom_gate(rectangleGate("UV-379-A" = c(1.5e4, 6e4))) + 
  #geom_stats(size = 6, adjust = c(0.5,0.6))
p

gs <- gs_pop_get_stats(gs, nodes, type = pop.MFI)


plist = list()
for (i in seq(1,8)) {
  plist[[i]] = as.ggplot(
                        ggcyto(fs_sub[[i]], aes(x = `V-450-A`)) + 
                          geom_density(fill='black') + 
                          geom_gate(rect.g.hist) +
                          geom_stats(size = 3, type = "count", adjust = c(0.2,0.2))
)
}
#den.gates.y <- fsApply(fs, openCyto::gate_mindensity, channel = "V-450-A", gate_range = c(100, 1e6))


ggplot(fs_sub[[8]], aes(x = `V-450-A`, y = `SSC-A`)) + geom_hex(bins=128)

as.ggplot(ggcyto(fs_sub[[5]], aes(x = `B-525-A`)) + 
            geom_density(fill = "black") + 
            geom_gate(rect.g.hist.list) + 
            geom_stats(type = "count", size = 3, adjust = c(0.5,1)) +
            #params_
            xlim(c(-100,2e5))
          )

#ggcyto_par_set(limits = list(x = c(-1e4,5e4)))


params_ <- ggcyto_par_set(limits = list(x = c(-100,2e5)))


do.call(grid.arrange,c(plist,ncol=12))

as.ggplot(ggcyto(fs[[93]],
                 #aes(x = "V-450-A")  
                 #aes(x = "B-525-A")
) + 
  xlab("") +
  ylab("") +
  theme(
    strip.text = element_blank(),
  ) +
  params_
) + geom_density(fill="#3288BD")
  #xlim(c(0,1000))


getMinMax <- function(fcsobj, cname){
  min <- summary(fcsobj)["Min.", cname]
  max <- summary(fcsobj)["Max.", cname]
  min <- min - min*0.15
  max <- max*0.75
  return (c(min, max))
}

setLimits <- function(vals) {
  if (vals[1] < 0) {
    vals[1] <- 0
  }
  return (vals)
}

yvals = getMinMax(fs[[2]], "SSC-A")
xvals = getMinMax(fs[[2]], "FSC-A")


xvals = setLimits(xvals)
yvals = setLimits(yvals)

as.ggplot(ggcyto(fs[[2]],
              aes(x = "FSC-A", 
              y = "SSC-A")  
              ) + 
  geom_hex(bins = 128) +
  xlab("") +
  ylab("") +
  theme(
    strip.text = element_blank(),
  )
)
  #scale_x_continuous(labels = function(x) formatC(x, format="e", digits =1), limits=c(1000, 1000000)) +
  #scale_y_continuous(breaks = c(yvals[1], yvals[2]), labels = function(x) formatC(x, format="e", digits =1))
  #scale_x_flowjo_biexp(maxValue = xvals[2], widthBasis = -10)  


da = fs[[2]]
da = da[rowMeans(exprs(da[,c("FSC-A","SSC-A")])>0) == 1,]

ggplot(da,aes(x = `FSC-A`,y = `SSC-A`)) + 
  geom_hex(bins = 128) +
  scale_x_continuous(breaks = c(-Inf, Inf)) +
  scale_fill_distiller(palette = "Spectral")


p <- ggcyto(fs, aes(x = `FSC-H`, y =  `SSC-H`))
p <- p + geom_hex(bins = 128)

c(formatC(vals[1], format="e", digits =2), formatC(vals[2], format="e", digits = 2))


p + ggtitle("Core Head") +
  theme(
      strip.text = element_text(size = 5),
      axis.text = element_text(size=8),
      plot.title = element_text(size=24, colour="#113F6A", hjust=0.5)
      )
  
  


rmSuffix <- function(filename) {
  if (nchar(filename) == 27) {
    split_fn = strsplit(filename, "\\.")
    #print(split_fn)
    name = split_fn[[1]][1]
    #print(name)
    lengthName = nchar(name)
    #print(lengthName)
    name = substr(name, 1, lengthName -4)
    #print(name)
    newFn = paste0(name, ".fcs")
    #print(newFn)
    newFn
  } else {
    filename
  }
}



df_stats <- read.csv("C:\\Users\\10322096\\Documents\\fcs_data\\07Jun2021\\2021-06-09_DF_plate2.csv")
df_stats <- df_stats %>% 
  flextable::flextable() %>% 
  flextable::colformat_num() %>%
  flextable::bold(bold = TRUE, part = "header") %>%
  flextable::fontsize(size  = 5, part = "body") %>%
  flextable::fontsize(size  = 6.5, part = "header")

doc <- read_pptx() %>%
  #add_slide(layout = "Title and Content") %>%
  add_slide(layout = "Blank") %>%
  ph_with(value=df_stats, location = ph_location(left = 0, top= 0, width = 14))

print(doc, target = paste0(parentpath, "test.pptx"))


library(flowCore)
library(flowWorkspace)
library(ggcyto)
data(GvHD)
fs <- GvHD[1:4]
fs
param <- "FL2-A"
g <- openCyto::gate_flowclust_2d(fs[[1]], xChannel = "FSC-H", yChannel = "SSC-H", K = 3, target = c(450, 300), quantile=0.8)
gs_sub <- Subset(fs[1], g)
gs <- GatingSet(gs_sub)
g_ <- openCyto:::.mindensity(gs@data[[1]], channels = param)
ggcyto(fs[[1]], aes("FSC-H", "SSC-H")) +
  geom_hex(bins = 100) +
  geom_gate(g) +
  geom_stats(size = 5, abs= T, adjust = c(0.5,0.1))



ggcyto(gs@data[[1]], aes(x = !!param)) +
  geom_histogram(stat = "bin", bins = 100) +
  geom_gate(g_) +
  geom_stats(size = 8,
             adjust = c(0.5, 5e6),
             label.padding = unit(0.05, "lines"))
