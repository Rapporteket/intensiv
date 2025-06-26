sship::dec(
  "c://Users/ast046/Downloads/intensiv126642fed.sql.gz__20250611_092542.tar.gz",
  keyfile = "p://.ssh/id_rsa",
  target_dir = "c://Users/ast046/Downloads/")

devtools::install("../rapbase/.", upgrade = FALSE)
devtools::install(upgrade = FALSE)

source("dev/sysSetenv.R")
intensiv::kjorIntensivApp(browser = TRUE)
