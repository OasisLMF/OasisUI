###############################################################################
# note with shiny::runApp this statement isn't needed anymore
# library(shiny, warn.conflicts = FALSE, quietly = TRUE)

library(oasisui, warn.conflicts = FALSE)

#Extend max uploadable file size from default 400MB, with option to read size in MB from env
env_upload = as.integer(Sys.getenv("MAX_UPLOAD_SIZE", unset = 400))
if (is.na(env_upload) || env_upload < 400) {upload_size = 400} else {upload_size = env_upload}
options(shiny.maxRequestSize = upload_size*1024^2)

### logger ---------------------------------------------------------------------
loginfo("testing logger", logger = "oasisui.module")

### Django API -----------------------------------------------------------------

APISettings <- APIgetenv(server = "API_IP",
                         port = "API_PORT",
                         scheme = "API_HTTPS",
                         version = "API_VERSION",
                         share_filepath = "API_SHARE_FILEPATH")

options(oasisui.settings.api.server = APISettings$server)
options(oasisui.settings.api.port = APISettings$port)
if (isTRUE(as.logical(APISettings$scheme))) {
  options(oasisui.settings.api.scheme = "https")
} else {
  options(oasisui.settings.api.scheme = "http")
}
options(oasisui.settings.api.httptype = "application/json")
options(oasisui.settings.api.version = APISettings$version)
options(oasisui.settings.api.share_filepath = APISettings$share_filepath)

options(oasisui.settings.admin.mode = Sys.getenv("ADMIN_MODE"))

options(oasisui.settings.oasis_environment = Sys.getenv("OASIS_ENVIRONMENT"))
