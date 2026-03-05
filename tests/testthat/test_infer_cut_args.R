context("infer_cut_args__")

testthat::test_that("infer_cut_args__ works", {
  testthat::expect_equal(
    infer_cut_args__(
      factor(c("[0,2[", "[2,5[", "[5,1e6["))
    )[c("breaks", "right")],
    list(
      breaks = c(0, 2, 5, 1e6),
      right = FALSE
    )
  )
  testthat::expect_equal(
    infer_cut_args__(
      factor(c("]-2,-1]", "]-1,5]"))
    )[c("breaks", "right")],
    list(
      breaks =  c(-2, -1, 5),
      right = TRUE
    )
  )
  testthat::expect_equal(
    infer_cut_args__(
      factor(c("]-2.1,-1.1]", "]-1.1,5.1]"))
    )[c("breaks", "right")],
    list(
      breaks = c(-2.1, -1.1, 5.1),
      right = TRUE
    )
  )
  testthat::expect_equal(
    infer_cut_args__(
      factor(c("[2001,2002[", "[2002,2003[", "[2003,2004["))
    )[c("breaks", "right")],
    list(
      breaks = 2001:2004,
      right = FALSE
    )
  )
  testthat::expect_equal(
    infer_cut_args__(
      factor(c("[50,51[", "[51,52[", "[52,53["))
    )[c("breaks", "right")],
    list(
      breaks = 50:53,
      right = FALSE
    )
  )
  testthat::expect_equal(
    infer_cut_args__(
      factor(c("[0 / 12, 1 / 12[", "[1 / 12, 2 / 12["))
    )[c("breaks", "right")],
    list(
      breaks = 0:2 / 12,
      right = FALSE
    )
  )
  testthat::expect_equal(
    infer_cut_args__(
      factor(c("(0 / 12, 1 / 12]", "(1 / 12, 2 / 12]"))
    )[c("breaks", "right")],
    list(
      breaks = 0:2 / 12,
      right = TRUE
    )
  )
})
