# at the time of writing this before releasing 0.3.10,
# there is a NOTE about the submission itself which can be ignored.
# another NOTE is the inability of checking whether URLs work.
# you can try them manually but they virtually certainly are working.
# if the PDF manual fails to build, that is almost certainly a problem
# of the system you are using and not this R package.

chk <- devtools::check()

rdchk <- revdepcheck::revdep_check(bioc = FALSE)
