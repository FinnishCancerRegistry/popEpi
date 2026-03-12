read_meanpop_fi <- function() {
  requireNamespace("pxweb")
  pxweb_query <- list(
    Vuosi = "*",
    Sukupuoli = as.character(1:2),
    "Ik\U00E4" = formatC(0:112, flag = "0", width = 3),
    Tiedot = "vaesto"
  )
  pxweb_url <- "https://statfin.stat.fi/PXWeb/api/v1/en/StatFin/vaerak/statfin_vaerak_pxt_11rd.px"

  mp <- pxweb::pxweb_get_data(
    url = pxweb_url,
    query = pxweb_query,
    verbose = FALSE
  )
  data.table::setDT(mp)

  mp[, "Sex" := as.integer(factor(Sex, c("Males", "Females")))-1L]
  mp[, "Age" := as.integer(gsub(Age, pattern = "\\D", replacement = ""))]
  mp[, "Year" := as.integer(Year)]
  data.table::setkeyv(mp, c("Sex", "Year", "Age"))
  data.table::setcolorder(mp, c("Sex", "Year", "Age"))
  v <- "Population 31 Dec"
  mp[
    j = "meanpop" :=
      (.SD[[v]] + data.table::shift(.SD[[v]], n = 1L, type = "lag")) / 2,
    by = c("Sex", "Age")
  ]
  mp <- mp[!is.na(meanpop), .SD, .SDcols = c("Sex", "Year", "Age", "meanpop")]
  data.table::setnames(
    mp,
    c("Year", "Sex", "Age"),
    c("year", "sex", "agegroup")
  )
  return(mp[])
}

mp <- read_meanpop_fi()
