context("Testing aggregation by categories of exposure")

test_that("prepExpo works in the simple case", {
  skip()
  library(Epi)
  
  df <- data.frame(id = "A", birth  = c(1952.4534), 
                   entry = c(1965.4746, 1972.42845, 1991.78643),
                   exit = c(1968.56346, 1979.32478, 1997.32432), fail = 0)
  
  # Define as Lexis object with timescales calendar time and age
  x <- Lexis( entry = list(work = 0, per=entry ),
                 exit = list( per=exit, age=exit-birth ),
                 exit.status = fail, id = id,
                 data = df )
  
  x2 <- prepExpo(x, freezeScales = "work", 
                 cutScale = "per", 
                 entry = 1966, 
                 exit = 2012, by = "lex.id")
  
  x2 <- prepExpo(x, freezeScales = "work", 
                 cutScale = "per", 
                 entry = 1964, 
                 exit = 2012, by = "lex.id")
  
  
  x2 <- prepExpo(x, freezeScales = "work", 
                 cutScale = "per", 
                 entry = 1964, 
                 exit = 2012, by = "lex.id", 
                 breaks = list(work = 0:50, age = c(0,18,Inf), per = 1963:2014))
  ag <- laggre(x2, aggre = list(lex.id, work, per, age))
  setkeyv(ag, c("per", "work"))
  
  
  
})
