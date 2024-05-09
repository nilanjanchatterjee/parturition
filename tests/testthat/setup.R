source(file.path("..", "..", "src", "common", "logger.R"))
source(file.path("..", "..", "src", "common", "runtime_configuration.R"))
source(file.path("..", "..", "src", "io", "app_files.R"))
source(file.path("..", "..", "src", "io", "io_handler.R"))
Sys.setenv("USER_APP_FILE_HOME_DIR" = "../../data/auxiliary/user-files")

clearRecentOutput()
# the system under test (sut)
source(file.path("..", "..", "./RFunction.R"))