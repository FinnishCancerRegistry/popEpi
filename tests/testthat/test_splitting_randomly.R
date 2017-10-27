context(
  "Additional splitting tests with random splitting"
)



test_that("splitting funs congruent with random splitting and fixed data", {

  skip_usually()

  library("Epi")
  library("data.table")

  data("occup", package = "Epi")

  occup <- Epi::Lexis(
    entry = list(age = AoE, per = DoE),
    exit = list(per = DoX),
    entry.status = 0L,
    exit.status = as.integer(Xst == "D"),
    data = occup
  )

  sire <- setDT(copy(popEpi::sire))
  sire[, "dg_yrs" := get.yrs(dg_date, "actual")]
  sire[, "ex_yrs" := get.yrs(ex_date, "actual")]
  sire[, "bi_yrs" := get.yrs(bi_date, "actual")]
  sire[, "id":= 1:.N]
  sire <- Lexis(
    data=sire[dg_date < ex_date],
    entry=list(fot=0, per=dg_yrs, age=dg_age),
    exit=list(per=ex_yrs),
    merge=TRUE,
    exit.status = status,
    entry.status = 0L
  )


  sibr <- setDT(copy(popEpi::sibr))
  sibr[, "dg_yrs" := get.yrs(dg_date, "actual")]
  sibr[, "ex_yrs" := get.yrs(ex_date, "actual")]
  sibr[, "bi_yrs" := get.yrs(bi_date, "actual")]
  sibr[, "id":= 1:.N]
  sibr <- Lexis(
    data=sibr[dg_date < ex_date],
    entry=list(fot=0, per=dg_yrs, age=dg_age),
    exit=list(per=ex_yrs),
    merge=TRUE,
    exit.status = status,
    entry.status = 0L
  )

  lex_list <- list(
    occup = occup,
    sire = sire,
    sibr = sibr
  )

  n_random_splits <- 500
  invisible(lapply(names(lex_list), function(lex_nm) {
    lapply(1:n_random_splits, function(i) {
      used_seed <- get_random_seed()
      set.seed(used_seed)
      l <- random_splitting_on(lex = lex, n.max.breaks = 50)
      # list contents in order: Epi::splitLexis, splitLexisDT, splitMulti

      lapply(2:length(l), function(list_pos) {

        tt_msg <- paste0(
          "Epi::splitLexis and ", c("splitLexisDT", "splitMulti")[list_pos-1],
          " are in agreement in data '", lex_nm, "' using seed ", used_seed
        )

        test_that(tt_msg, {
          expect_equal(l[[1]], l[[list_pos]], check.attributes = FALSE)
        })

      })

      test_that(
        paste0(
          "splitLexisDT and splitMulti are in agreement in data '", lex_nm,
          "' using seed ", used_seed
        ),
        expect_equal(l[[2]], l[[3]], check.attributes = TRUE)
      )

    })
  }))



})






test_that("splitting funs congruent with random splitting and random data", {
  
  skip_usually()
  
  library("Epi")
  library("data.table")
  
  n_datasets <- 100
  n_random_splits <- 50
  lapply(seq_len(n_datasets), function(lex_nm) {
    
    data_seed <- get_random_seed()
    lex <- random_Lexis(
      n.rows = 100:2000,
      n.time.scales = 1:10,
      n.statuses = 2:10, 
      n.other.vars = 1
    )
    
    lapply(seq_len(n_random_splits), function(i) {
      split_seed <- get_random_seed()
      set.seed(split_seed)
      l <- random_splitting_on(lex = lex, n.max.breaks = 50)
      # list contents in order: Epi::splitLexis, splitLexisDT, splitMulti
      
      lapply(2:length(l), function(list_pos) {
        
        tt_msg <- paste0(
          "Epi::splitLexis and ", c("splitLexisDT", "splitMulti")[list_pos-1],
          " are in agreement in data '", lex_nm, 
          "' using data seed ", data_seed, " and splitting seed ", split_seed
        )
        
        test_that(tt_msg, {
          expect_equal(l[[1]], l[[list_pos]], check.attributes = FALSE)
        })
        
      })
      
      test_that(
        paste0(
          "splitLexisDT and splitMulti are in agreement in data '", lex_nm, 
          "' using data seed ", data_seed, " and splitting seed ", split_seed
        ),
        expect_equal(l[[2]], l[[3]], check.attributes = TRUE)
      )
      
    })
  })
  
  
  
})




