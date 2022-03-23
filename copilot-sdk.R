library(jsonlite)
library(move)

source("logger.R")
source("RFunction.R")

inputFileName = "input.rds"
outputFileName = "output.rds"

args<-list()

#################################################################
########################### Arguments ###########################

args[["data"]] 
args[["threshold"]]  
  
#################################################################
#################################################################

readInput <- function(sourceFile) {
  input <- NULL
  if(!is.null(sourceFile) && sourceFile != "") {
    logger.debug("Loading file from %s", sourceFile)
    input <- tryCatch({
      # 1: try to read input as move RDS file
      readRDS(file = sourceFile)
    },
    error = function(readRdsError) {
      tryCatch({
        # 2 (fallback): try to read input as move CSV file
        move(sourceFile, removeDuplicatedTimestamps=TRUE)
      },
      error = function(readCsvError) {
        # collect errors for report and throw custom error
        stop(paste(sourceFile, " -> readRDS(sourceFile): ", readRdsError, "move(sourceFile): ", readCsvError, sep = ""))
      })
    })
  } else {
    logger.debug("Skip loading: no source File")
  }
  
  input
}

inputData <- readInput(inputFileName)
# Add the data parameter if input data is available
if (!is.null(inputData)) {
  args[["data"]] <- inputData
}

result <- tryCatch({
  do.call(rFunction, args)
},
error = function(e) {
  logger.error(paste("ERROR:", e))
  stop(e) # re-throw the exception
}
)

if(!is.null(outputFileName) && outputFileName != "" && !is.null(result)) {
  logger.info(paste("Storing file to '", outputFileName, "'", sep = ""))
  save(result, file = outputFileName)
} else {
  logger.warn("Skip store result: no output File or result is missing.")
}
