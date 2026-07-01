# steps to do after CRAN has accepted the new release.
du <- new.env()
source("dev/utils.R", local = du)

lines <- readLines("CRAN-SUBMISSION")
cran_version <- sub("Version: ", "", lines[1])
git_ref <- sub("SHA: ", "", lines[3])

du$git_exe_cmd(c("pull", "--tags", "--force"))
du$git_exe_cmd(c("checkout", git_ref))
du$git_exe_cmd(c("tag", sprintf("v%s", cran_version)))
du$git_exe_cmd(c("checkout", "master"))
du$git_exe_cmd(c("push", "--tags", "--force"))

desc::desc_bump_version(which = "dev")
du$git_exe_cmd(c("add", "DESCRIPTION"))
du$git_exe_cmd(c(
  "commit",
  "-m",
  sprintf("\"build: v%s\"", desc::desc_get_version())
))
message("Bumped to development version ", desc::desc_get_version())
