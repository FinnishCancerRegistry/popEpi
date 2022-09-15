# these checks on remote services must pass.

## funs ------------------------------------------------------------------------
du <- new.env()
source("dev/utils.R", local = du)

## winbuilder ------------------------------------------------------------------
# if this fails, you can always upload manually: 
# https://win-builder.r-project.org/
du$run_r_cmd_check_on_winbuilder(c("release", "devel", "oldrelease"))
# insert each URL to raw (NOT html) result. they take some time ---
# but less than 2 hours usually.
wb_release <- du$read_remote_check_results(readline("release url: "))
wb_devel <- du$read_remote_check_results(readline("devel url: "))
wb_oldrel <- du$read_remote_check_results(readline("oldrel url: "))
cat(wb_release, sep = "\n")
cat(wb_devel, sep = "\n")
cat(wb_oldrel, sep = "\n")

## rhub ------------------------------------------------------------------------
# if this fails, you can always upload manually: 
# https://builder.r-hub.io/
du$run_r_cmd_check_on_rhub(
  c("debian-gcc-devel", "debian-gcc-patched", "debian-gcc-release")
)
# insert each URL to raw (NOT html) result. each check takes time ---
# check your email in 2-3 hours.
rh_release <- du$read_remote_check_results(readline("release url: "))
rh_devel <- du$read_remote_check_results(readline("devel url: "))
rh_oldrel <- du$read_remote_check_results(readline("oldrel url: "))
cat(rh_release, sep = "\n")
cat(rh_devel, sep = "\n")
cat(rh_oldrel, sep = "\n")
