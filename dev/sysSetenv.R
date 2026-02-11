
Sys.setenv(FALK_EXTENDED_USER_RIGHTS= "[{\"A\":106,\"R\":\"SC\",\"U\":4201313},
           {\"A\":106,\"R\":\"LU\",\"U\":112044},
           {\"A\":106,\"R\":\"LU\",\"U\":700720},
           {\"A\":106,\"R\":\"SC\",\"U\":700720},
           {\"A\":106,\"R\":\"SC\",\"U\":112044}]")
Sys.setenv(MYSQL_DB_LOG="db_log")
Sys.setenv(MYSQL_DB_AUTOREPORT="db_autoreport")
Sys.setenv(MYSQL_DB_DATA="intensiv")
#Sys.setenv(MYSQL_DB_DATA="intensivregisterreportdatastaging")
#Sys.setenv(MYSQL_HOST="db")
Sys.setenv(MYSQL_HOST="localhost")
Sys.setenv(MYSQL_USER="root")
Sys.setenv(MYSQL_PASSWORD="root")
Sys.setenv(FALK_APP_ID="106")
Sys.setenv(FALK_USER_EMAIL="jesus@sky.no")
Sys.setenv(FALK_USER_FULLNAME="Reidar")
Sys.setenv(USERORGID="pilot")
Sys.setenv(SHINYPROXY_USERNAME="sivh")
Sys.setenv(SHINYPROXY_USERGROUPS="pilot")
Sys.setenv(R_RAP_INSTANCE="QAC")
#Sys.setenv(R_RAP_CONFIG_PATH="data-raw/config")
Sys.setenv(R_RAP_CONFIG_PATH=paste0(getwd(), "/data-raw/config"))
#Sys.setenv(R_RAP_CONFIG_PATH="/home/rstudio/intensiv/data-raw/config")
Sys.setenv(MRS_ACCESS_HIERARCHY_URL = "https://app.mrs.nhn.no/intensivregisterservices/AccessHiearchyReport")
Sys.setlocale(locale = 'nb_NO.UTF-8')
