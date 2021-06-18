library(httr)

#arg = 'p'
arg = 'g'
dag_run_id = 'testrun14'
VM = "10.29.128.4"
expid_dir = "20211806TEST2"

current_time = Sys.time()
attr(current_time, "tzone") <- "UTC"
current_time <- format(current_time, '%Y-%m-%dT%H:%M:%SZ')

body <- list(conf=list(EXP_ID=expid_dir), dag_run_id=dag_run_id, execution_date=current_time)
link <- paste0("http://", VM, ":8000/api/v1/dags/r_dag/dagRuns")
res <- httr::POST(url = link,
           config = authenticate("airflow", "airflow"),
           body =  jsonlite::toJSON(body, pretty = T, auto_unbox = T),
           httr::add_headers(`accept` = 'application/json'), 
           httr::content_type('application/json'))

content(res,"parsed")

link <- paste0("http://", VM, ":8000/api/v1/dags/r_dag/dagRuns/", dag_run_id)
res <- httr::GET(url = link,
                 config = authenticate("airflow", "airflow"),
                 httr::add_headers(`accept` = 'application/json'), 
                 httr::content_type('application/json'))

res <- content(res, "parsed")
res
