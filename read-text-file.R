params = read.csv('C:/Users/10322096/Documents/R/Rscripts/test_run_param_output.txt', header=F, sep=',')
params

LMG_NAMES = c()
POP_MINS = list() # 2k, 300, 1500 are the orig spec'd numbers


LMG_NAMES <- sapply(unname(params[1:3,1]), function(x) c(LMG_NAMES, as.character(x)))
LMG_NAMES

for (i in seq(1:length(LMG_NAMES))) {
  name = LMG_NAMES[i]
  POP_MINS[name] = as.character(params[i+3,1])
}

POP_MINS


WIDTH_BASIS = as.character(params[7,1])
WIDTH_BASIS


save(LMG_NAMES, file='C:/Users/10322096/Documents/R/Rscripts/LMG_NAMES.RData')
save(POP_MINS, file='C:/Users/10322096/Documents/R/Rscripts/POP_MINS.RData')
save(WIDTH_BASIS, file='C:/Users/10322096/Documents/R/Rscripts/WIDTH_BASIS.RData')


filepath<- "C:/Users/10322096/Documents/R/Rscripts/test_run_param_output.txt"
write(LMG_NAMES, file=filepath)
write(unlist(POP_MINS, use.names = F), file=filepath, append=T)
write(WIDTH_BASIS, file=filepath, append=T)


load(file='C:/Users/10322096/Documents/R/Rscripts/WIDTH_BASIS.RData')
load(file='C:/Users/10322096/Documents/R/Rscripts/POP_MINS.RData')
load(file='C:/Users/10322096/Documents/R/Rscripts/LMG_NAMES.RData')

INT_WIDTH_BASIS <- as.numeric(WIDTH_BASIS)
INT_WIDTH_BASIS


LMG_NAMES



lapply(POP_MINS, function(x)as.numeric(x))


POP_MINS


setNames(POP_MINS, c("test", "a", "ok"))
