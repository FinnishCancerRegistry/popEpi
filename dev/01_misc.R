
s1 <- git2r::status()
stopifnot(
  vapply(s1, length, integer(1L)) == 0L
)
usethis::use_coverage("codecov")
usethis::use_github_action("test-coverage")

usethis::use_mit_license()
usethis::use_github_action(name = "check-release")
desc::desc_normalize()

s2 <- git2r::status()

if (!identical(s1, s2)) {
  git2r::add(path = ".")
  git2r::commit(message = "build: run dev/01_misc.R")
}
