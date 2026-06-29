## winbuilder ------------------------------------------------------------------
devtools::build(binary = FALSE, path = "dev/")
message("Upload the created source file to this address on every R version it ",
        "allows: https://win-builder.r-project.org/. ",
        "Press enter when you have done that.")
readline(": ")

## rhub ------------------------------------------------------------------------
message("Browse to https://github.com/FinnishCancerRegistry/popEpi/actions",
        "and run the action 'R-hub'. Press enter when you have done that.")
readline(": ")

## cran-comments.md ------------------------------------------------------------
message("After winbuilder has e-mailed its passing checks and rhub checks ",
        "have passed, update cran-comments.md. Press enter when you have done ",
        "that.")
readline(": ")
system2("git", c("add", "cran-comments.md"))
system2("git", c("commit", "--m", "\"docs: update cran-comments.md\""))
