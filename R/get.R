#' Show top n line of table of database in sqlite
#'
#' @param db Output path of sqlite database
#' @param table.name Table name in sqlite 
#' @param n Top n lines will be selected
#' @export
#' @examples
#' test.sqlite <- sprintf('%s/snp.test.sqlite', tempdir())
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' x <- sqlite.build(filename = test.dat, db = test.sqlite, table.name = 'snp_test')
#' sqlite.head(test.sqlite, 'snp_test')
sqlite.head <- function(db, table.name, n = 10) {
  sqlite.db <- dbConnect(RSQLite::SQLite(), db)
  sql <- sprintf("SELECT * FROM %s LIMIT %s", table.name, n)
  nlines <- dbGetQuery(sqlite.db, sql)
  return(nlines)
}
