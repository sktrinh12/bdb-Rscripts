library(RPostgres)

# Connect to the default postgres database
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "postgres",
                 host = "hostname",
                 port = 5432,
                 user = "user",
                 password = "pass"
)

COL = "TARGET_SPECIES"

query = paste0("SELECT * FROM DEVMETADB WHERE ", COL)
query = "SELECT COSTAIN_CONCENTRATION_UGTEST[2] FROM TESTDB"
query
#r_ <- dbGetQuery(con, query)

res <- dbSendQuery(con, paste(query, "= $1"))
dbBind(res, list("human"))
res <- dbSendQuery(con, query)
r <- dbFetch(res)
dbHasCompleted(res)
dbClearResult(res)

dbDisconnect(con)

#dbCommit(conn)

r$target_species
r$clone
r$notes

for (n in names(r)) {
  print(paste(n, ":", r[[n]]))
}



filename = "C:/Users/10322096/Downloads/ppt_test.dat"
#raw <- readBin(filename, what= raw(), n=file.info(filename)$size)
raw <- hexView::readRaw(filename)
binary_file <- raw$fileRaw
sql_smt <- "UPDATE PPTFILES SET PPT_FILE = $1" 

rs  <- dbExecute(
            con, 
            statement=paste(sql_smt, "where experiment_id = 'FY21w1p214';"), 
            list(paste0("\\x", paste(binary_file, collapse = "")))
            )


get_res <- dbGetQuery(con, "SELECT PPT_FILE from PPTFILES WHERE experiment_id = 'FY21w1p214'")
identical(get_res$ppt_file[[1]], binary_file)

# open pptx as binary and write as binary to new location
rtn_filename <- "C:/Users/10322096/Downloads/20201222-RB-FY21w3p9 Hu HLA-G (87G) PE.pptx"
read_ppt <- readBin(rtn_filename, what= raw(), n=file.info(rtn_filename)$size)
con_w = file(file.path(dirname(rtn_filename), "ppt_binary.dat"), "wb")
writeBin(object = read_ppt, con = con)
close(con_w)

# read the binary file
filepath <- file.path(dirname(rtn_filename), "ppt_binary.dat")
binary_file <- readBin(filepath, what = raw(), n=file.info(filepath)$size)

sql_smt <- "UPDATE PPTFILES SET PPT_FILE = $1 where experiment_id = 'FY21w1p214';" 

# upload to sql db
rs  <- dbExecute(
  con, 
  statement=sql_smt, 
  list(paste0("\\x", paste(binary_file, collapse = "")))
)

# check that the original file and what is in the db is the same
get_res <- dbGetQuery(con, "SELECT PPT_FILE from PPTFILES WHERE experiment_id = 'FY21w1p214'")
identical(get_res$ppt_file[[1]], binary_file)

#get_res$ppt_file[[1]]

con_w  = file(file.path(dirname(rtn_filename), "ppt_test.pptx"), "wb")
writeBin(object = get_res$ppt_file[[1]], con = con_w)
close(con_w)