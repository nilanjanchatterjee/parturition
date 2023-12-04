readInput <- function(sourceFile) {
    # for now every input-type can be read via RDS
    return(readRdsInput(sourceFile))
}

storeResult <- function(result, outputFile) {
    return(storeRdsOutput(result, outputFile))
}

storeToFile <- function(result, outputFile) {
    if(!is.null(outputFile) && outputFile != "" && !is.null(result)) {
        logger.debug("Writing to file %s", outputFile)
        write(paste(result), file = outputFile)
    } else {
        logger.debug("Skip writing to file: no output File or result is missing")
    }
}

sourceFile <- function() {
    result <- Sys.getenv(x = "SOURCE_FILE", "")
    logger.debug("sourceFile: %s", result)
    result
}

outputFile <- function() {
    result <- Sys.getenv(x = "OUTPUT_FILE", "")
    logger.debug("outputFile: %s", result)
    result
}

errorFile <- function() {
  result <- Sys.getenv(x = "ERROR_FILE", "")
  logger.debug("errorFile: %s", result)
  result
}