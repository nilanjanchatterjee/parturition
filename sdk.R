library(dotenv)
# You can control your local app development via environment variables.
# You can define things like input-data, app-configuration etc.
# Per default your environment is defined in `/.env`
load_dot_env()

# provide common stuff
source("src/common/logger.R")
source("src/common/runtime_configuration.R")
clearRecentOutput()
# This will parse a JSON file containing the concrete configuration of
# the app run. Per default the file `/app-configuration.json` will be parsed.
args <- configuration()

# Lets simulate running your app on MoveApps
source("src/moveapps.R")
simulateMoveAppsRun(args)
