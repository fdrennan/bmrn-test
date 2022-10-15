#' @export
connection_postgres <- function(host = Sys.getenv("POSTGRES_HOST"),
                               port = Sys.getenv("POSTGRES_PORT"),
                               user = Sys.getenv("POSTGRES_USER"),
                               password = Sys.getenv("POSTGRES_PASSWORD"),
                               dbname = Sys.getenv("POSTGRES_DB")) {
  box::use(DBI[dbConnect],RPostgres[Postgres])
  dbConnect(Postgres(),
            host = host,
            port = port,
            user = user,
            password = password,
            dbname = dbname
  )
}

#' @export
table_exists <- function(dataname) {
  box::use(DBI)
  box::use(./postgres[connection_postgres])
  con <- connection_postgres()
  on.exit(DBI$dbDisconnect(con))
  DBI$dbExistsTable(con, dataname)
}

#' @export
table_create <- function(data) {
  box::use(DBI)
  box::use(./postgres[connection_postgres])
  con <- connection_postgres()
  on.exit(DBI$dbDisconnect(con))
  dataname <- deparse1(substitute(data))
  if (isFALSE(DBI$dbExistsTable(con, dataname))) {
    DBI$dbCreateTable(con, dataname, data)
  }
}

#' @export
table_get <- function(dataname) {
  box::use(DBI)
  box::use(dplyr)
  box::use(dbplyr)
  box::use(./postgres[connection_postgres])
  con <- connection_postgres()
  on.exit(DBI$dbDisconnect(con))
  dplyr$tbl(con, dataname) |>
    dplyr$collect()
}
