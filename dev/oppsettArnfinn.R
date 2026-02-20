sship::dec(
  "c://Users/ast046/Downloads/intensiv141abf297.sql.gz__20260219_102507.tar.gz",
  keyfile = "c://Users/ast046/.ssh/id_rsa",
  target_dir = "c://Users/ast046/Downloads/")

devtools::install("../rapbase/.", upgrade = FALSE)
devtools::install(upgrade = FALSE)

source("dev/sysSetenv.R")
intensiv::kjorIntensivApp(browser = TRUE)
