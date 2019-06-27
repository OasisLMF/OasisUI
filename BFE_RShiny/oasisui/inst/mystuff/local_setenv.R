# Zoom link
# https://zoom.us/j/369846338
# Connect to DB
# sudo openvpn --config Downloads/client.ovpn
# use as credentials fvitalini 4M2XJT9np
# #Oasisui Settings
# Sys.setenv(OASIS_DB_IP = "10.10.0.141")
# Sys.setenv(OASIS_DB_PORT = "1433")
# # New DB
# Sys.setenv(OASIS_DB_USERNAME = 'db_ri')
# Sys.setenv(OASIS_DB_PASSWORD = 'test1234')
# Sys.setenv(OASIS_DB_PORT = 1433)
# Sys.setenv(OASIS_DB_NAME = 'Oasisui_db_ri')
# New API
# Sys.setenv(API_IP = "localhost")
Sys.setenv(API_IP = "99.80.10.98")#10.10.0.182; 34.244.212.74; http://34.244.205.121:8000/; 10.10.0.8 99.80.10.98
Sys.setenv(API_PORT = "8000")
Sys.setenv(API_VERSION = "v1")
Sys.setenv(API_SHARE_FILEPATH = "./downloads")
Sys.setenv(OASIS_ENVIRONMENT = "piwind")
Sys.setenv(ADMIN_MODE = "admin")
# # Connect manually to new API
# library(oasisui)
# options(oasisui.settings.api = api_init("10.10.0.182", "8000"))
# options(oasisui.settings.api.version = "v1")
# options(oasisui.settings.api.share_filepath = "./downloads")
# res <- api_access_token("admin", "password")
# if (res$status == "Success") {
#   #result$user <- user # for later
#   options(oasisui.settings.api.token = httr::content(res$result)$access_token)
#   options(oasisui.settings.api.refresh = httr::content(res$result)$refresh_token)
# } else {
#   options(oasisui.settings.api.token = NULL)
# }
# #Local DB
# Sys.setenv(OASIS_DB_IP="localhost")
# Sys.setenv(OASIS_DB_PORT="1433")
# Sys.setenv(OASIS_DB_NAME="Oasisui_db")
# Sys.setenv(OASIS_DB_USERNAME="SA")
# Sys.setenv(OASIS_DB_PASSWORD="Mirai5olution$")
# # Old DB
# Sys.setenv(OASIS_DB_NAME="Oasisui_db")
# Sys.setenv(OASIS_DB_USERNAME="db")
# Sys.setenv(OASIS_DB_PASSWORD="piwind")
# Sys.setenv(OASIS_PORT_RANGE_START="20000")
# Sys.setenv(OASIS_ENVIRONMENT="db")
# Sys.setenv(OASIS_SHARE_FILEPATH_ON_HOST="/home/mirai/Desktop/FV/R-projects/OasisUI/Files/")
#
