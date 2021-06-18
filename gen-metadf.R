library(dplyr)
library(stringr)
library(flowCore)

repeat_inputs = function(input_str, nbr_repeats) {
  rep(input_str, nbr_repeats/length(input_str)) 
}

convert_to_list = function(input_matrix) {
  unlist(split(input_matrix, 
               rep(1:ncol(input_matrix), 
                   each= nrow(input_matrix))), use.names = F)
}

apply_reps = function(input_list, nbr_repeats) {
  sapply(input_list, function(x) rep(x, nbr_repeats))
}

unq_vals = function(input_vals) {
  unqvals = input_vals[!grepl("NA", input_vals)]
  unqvals = unique(unqvals)
  unqvals
}


main_repeat_call = function(input_vals, nbr_repeats) {
  input_vals = unq_vals(input_vals)
  vals = apply_reps(input_vals, 12)
  nbr_empty = nbr_repeats - (12*length(input_vals))
  if (length(input_vals) == 1) {
    to_repeat = input_vals
  } else {
    to_repeat = 'NA'
  }
  vals = c(rep(to_repeat, nbr_empty), vals)
  vals
}


#concatPadding = function(inputIndex, inputString) {
#  padding = '_0'
#  if (as.numeric(inputIndex) < 10) {
#    padding = paste0(padding, '0')
#  }
#  paste0(inputString, padding, inputIndex, '.fcs')
#}

#extractFilenames = function(stringInput) {
#  theString = str_match(stringInput, '(_[A-H])([0-9]{1,2}).fcs')
#  nbr = as.numeric(theString[,3])
#  if (nbr < 9) {
#    stringInput
#  }
#}

# rename files
#directory = 'C:/Users/10322096/Documents/gitrepos/romiq-meta-docker-setup/common_drive/reports/20211602STFY21W3P13/'
#setwd(directory)
#newNames = unlist(sapply(filenames, function(fi) strsplit(fi, '\\.')[[1]][1], USE.NAMES = F))
#newNames = unlist(lapply(seq_along(newNames), function(i) concatPadding(i, newNames[[i]])))
#list.files('.')
#file.rename(filenames, newNames)


# number of titration pts: 7
nbr_titr_pts = 7

#UNITS 
input_units = 'ug'

# FILENAMES
parentpath = "C:/Users/10322096/Documents/gitrepos/romiq-meta-docker-setup/common_drive/reports/20210318-JC-FY21w3p18/"
filenames = list.files(parentpath, pattern=".*fcs$")
#filenames = list.files('C:/Users/10322096/Documents/gitrepos/romiq-meta-docker-setup/common_drive/reports/20210701SDFY21W3P7//', pattern="^Specimen.*fcs$")

regexcompile = ".*?(0[0-9]{2}).fcs$"
filenames = filenames[grepl(regexcompile, filenames)]

order_filenames = order(as.integer(sub(regexcompile, "\\1", filenames)))
order_filenames = order_filenames[!is.na(order_filenames)]
filenames = filenames[order_filenames]
nbr_repeats = length(filenames)
nbr_repeats
order_filenames
filenames


# WELL ID
well_id = sapply(LETTERS[1:8], function(x) paste0(x,seq(1,8)))
well_id = convert_to_list(well_id)
well_id = well_id[1:nbr_repeats]
well_id


# PLATE ID
input_plate_id = '20191218-10299178-01'

# STAIN DATE
input_stain_date = '20191218-10299178'

# PLATE ROW
plate_rows = sapply(LETTERS[1:8], function(x) rep(x, 8))
plate_rows = convert_to_list(plate_rows)
plate_rows = plate_rows[1:nbr_repeats]
plate_rows
length(plate_rows)

# PLATE COLUMN
plate_colms = rep(seq(1:12), ceiling(nbr_repeats/12))
length(plate_colms)
# CONTROL ROW
input_ctrl_row = c('NA', 'A') #'A', 'A', 'A', 'A', 'A', 'A')

# TARGET SPECIES
input_tgt_species = rep('Hu', 8)

# SPECIFICITY
#input_specif = c("NA", 'CD56 (NCAM-1)', 'CD314', 'Dog CD8', "CD163", "CD64", "CD21", "CD5")
input_specif = c("NA", "CD8")
specif = unq_vals(input_specif)
specif = apply_reps(specif, 12)
nbr_empty = nbr_repeats - (12*length(specif[1,]))
specif = convert_to_list(specif)
specif = c(rep("NA", nbr_empty), specif)
specif

length(specif)
# UG TEST
tmp_ugtest = sapply(seq_along(rep(2,7)), function(x) signif(4/(2**(x)), 1))
# determine how many NA's to insert for ug.test
ug_test = rep(NA, nbr_repeats)
indices = seq(from=2, to=nbr_repeats-nbr_titr_pts, by=12)
lst_indices = lapply(indices, function(x) seq(from=x,to=x+6))
for (i in seq(1:length(lst_indices))) {
  ug_test[lst_indices[[i]]] = tmp_ugtest
}
length(ug_test)

# BATCH NUMBER
rdn_batch_nbr = sample.int(10000,1)
input_batchno = rep(as.character(rdn_batch_nbr, 7))

# PARAMETER
input_param = rep('UV-515-A', 8)

# FLUOROCHROME
input_fluor = rep('BUV496', 8)

# CLONE
# input_clones = c('X40', 'ICRF44', 'B-ly6', 'M-L233', "NA", "NA", "NA")
#input_clones = c('BD159', '1D11', 'HIT8a', 'GHI_61', 'CW10.1', '1048',)
input_clones = c('X40', 'RPA-T8')
input_clones = unq_vals(input_clones)

reps = 12
if (nbr_repeats == 12) {
  reps = 6
}
clones  = apply_reps(input_clones, reps)
clones = convert_to_list(clones)
nbr_empty = abs(nbr_repeats - (reps*length(input_clones)))
clones = c(rep("NA", nbr_empty), clones)
length(clones)

# TYPE HOST SPECIES
#input_iso_host_sp = c('Ms Ig', 'Hu Ig', 'Ms Ig', 'Ms Ig', 'NA', 'NA', 'NA')
input_iso_host_sp = rep('Ms Ig',8)
input_iso_host_sp = unq_vals(input_iso_host_sp)
iso_host_sp = apply_reps(input_iso_host_sp, 12)
nbr_empty = nbr_repeats - (12*length(input_iso_host_sp))
iso_host_sp = convert_to_list(iso_host_sp)
iso_host_sp = c(rep("NA", nbr_empty), iso_host_sp)
length(iso_host_sp)
# SAMPLE TYPE
input_sample_type = 'LWB'

# DONOR ID
input_donorid = 'DN22'

# SAMPLE SPECIES
input_sample_species = 'Hu'

# SPEC 1
input_spec1 = c('30-40','30-40','30-40','30-40','30-40','30-40','30-40')

# SPEC 2
input_spec2 = c('20-30','20-30','20-30','20-30','20-30','20-30','20-30')

# SPEC 3
input_spec3 = c('24-42','24-42','24-42','24-42','24-42','24-42','24-42')

# GATING METHOD
input_gm = c('sd','sd','sd','sd','sd','sd','sd')

# GATING ARG
input_ga = c('3','3','3','3','3','3','3')


# FLUOROCHROME
fluor = main_repeat_call(input_fluor, nbr_repeats)
fluor
# PARAMETER
param = main_repeat_call(input_param, nbr_repeats)
param
# BATCH NUMBER
batchno = main_repeat_call(input_batchno, nbr_repeats)
batchno

# SPEC1
spec1 = main_repeat_call(input_spec1, nbr_repeats)
spec1
# SPEC2
spec2 = main_repeat_call(input_spec2, nbr_repeats)
spec2
# SPEC3
spec3 = main_repeat_call(input_spec3, nbr_repeats)
spec3
# GATING METHOD
gm = main_repeat_call(input_gm, nbr_repeats)
gm
# GATING ARG
ga = main_repeat_call(input_ga, nbr_repeats)
length(ga)


df = data.frame('Filename'=filenames,             #1
					 'Well.ID'=well_id,               #2
					 'Plate.ID'=rep(input_plate_id, nbr_repeats),#3
					 'Stain.Date'=rep(input_stain_date, nbr_repeats),#4
					 'Plate.Row'=plate_rows,          #5
					 'Plate.Column'=plate_colms,      #6
					 'Control.Row'=main_repeat_call(input_ctrl_row,nbr_repeats),#7
					 'Target.Species'=main_repeat_call(input_tgt_species, nbr_repeats),#8
					 'Specificity (CD)'=specif,         #9
					 'Isotype.Host.Species'=iso_host_sp,#10
					 'Clone'=clones,                    #11
					 'Fluorochrome'=main_repeat_call(input_fluor, nbr_repeats),#12
					 'Parameter'=main_repeat_call(input_param, nbr_repeats),  #13
					 'Batch.Number'=main_repeat_call(rdn_batch_nbr, nbr_repeats),#14
					 'ug.test'=ug_test,                    #15
					 'units'=rep(input_units, nbr_repeats),#16
					 'Sample.Types'=rep(input_sample_type, nbr_repeats),#17
					 'Sample.Species'=rep(input_sample_species, nbr_repeats),#18
					 'Sample.Strain'=rep(NA, nbr_repeats),#19
					 'Donor.ID'=rep(input_donorid, nbr_repeats), #20
					 'spec1_range'=main_repeat_call(input_spec1, nbr_repeats),#21
					 'spec2_range'=main_repeat_call(input_spec2, nbr_repeats),#22
					 'spec3_range'=main_repeat_call(input_spec3, nbr_repeats),#23
					 'gating_method'=main_repeat_call(input_gm, nbr_repeats), #24
					 'gating_argument'=main_repeat_call(input_ga, nbr_repeats) #25
					 , stringsAsFactors = FALSE)



populate_NA_cols = c('Control.Row', 
                     'Target.Species',
                     'Specificity..CD.',
                     'Isotype.Host.Species',
                     'Clone',
                     'Fluorochrome',
                     'Parameter',
                     'Batch.Number',
                     'ug.test',
                     'units',
                     'Sample.Types',
                     'Sample.Species',
                     'Sample.Strain',
                     'Donor.ID')

df[is.na(df$ug.test), populate_NA_cols] = NA

unq_param = unique(na.omit(df$Parameter))
unq_tgt_species = unique(na.omit(df$Target.Species))

cols_to_replace_NA = c("Target.Species", 
                       "Parameter", 
                       "Sample.Types",
                       "Sample.Species", 
                       "Donor.ID")
dff <- df

for (cols in cols_to_replace_NA) {
  unq_value = unique(na.omit(dff[[cols]]))
  print(paste0('uniq value of, ', cols, ': ', unq_value))
  dff <- dff %>%
    mutate(!!sym(cols) := replace(
      !!sym(cols), str_detect(Well.ID, "^[A-H]1$"), unq_value) ) %>%
        as.data.frame()
  }
        

# set NA for first row on Control Row & specif
colname = paste0("^[", input_ctrl_row, "1-9]{1,2}$")
for (cols in c("Control.Row", "Specificity..CD.")) {
  dff <- dff %>% 
    mutate(!!sym(cols) := replace(
      !!sym(cols), str_detect(Well.ID, colname), NA) ) %>%
        as.data.frame()
  }  

head(dff)

write.csv(dff, 'C:/Users/10322096/Downloads/output.csv', row.names = F)


fi <- file.info('C:/Users/10322096/Documents/gitrepos/romiq-meta-docker-setup/common_drive/reports/20210701SDFY21W3P7/all_stats.csv')

difftime(Sys.time(), fi$mtime, units = 'secs')
