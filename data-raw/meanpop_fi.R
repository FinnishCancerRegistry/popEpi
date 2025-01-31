
## how mean population counts were downloaded from FinStat

library("data.table")
library("pxweb")

dims_list <- list(
  Alue = c('SSS'),
  Vuosi = c('*'),
  Sukupuoli = c('1', '2'),
  Ikä = formatC(0:100, flag = "0", width = 3)
)

px_url <- "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/vrm/vaerak/071_vaerak_tau_109.px"

mp <- get_pxweb_data(
  url = px_url,
  dims = dims_list,
  clean = TRUE
)


setDT(mp)
mp[, "Area" := NULL]
mp[, "Sex" := as.integer(factor(Sex, c("Males", "Females")))-1L]
mp[, "Age" := as.integer(gsub(Age, pattern = "\\D", replacement = ""))]
mp[, "Year" := as.integer(levels(Year)[Year])]

setnames(mp, c("Year", "Sex", "Age", "values"),
         c("year", "sex", "agegroup", "meanpop"))
setcolorder(mp, c("sex", "year", "agegroup", "meanpop"))

write.csv(mp, file = "data-raw/meanpop_fi.csv", row.names = FALSE)







