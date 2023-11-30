library('move2')
library('move')

readRdsInput <- function(sourceFile) {
  if(!is.null(sourceFile) && sourceFile != "") {
    if (file.info(sourceFile)$size == 0) {
        # handle the special `null`-input
        logger.warn("The App has received invalid input! It cannot process NULL-input. Aborting..")
        # codes for exception handling
        # 10: abort consuming null-input
        stop("The App has received invalid input! It cannot process NULL-input. Check the output of the preceding App or adjust the datasource configuration. [code 10]")
    }
    logger.debug("Reading RDS from file '%s'", sourceFile)
    rds <- readRDS(file = sourceFile)
    if (is(rds,"MoveStack")) {
      logger.warn("Received input data in format 'MoveStack'. Will convert it to 'move2'.")
      data <- move2::mt_as_move2(rds)
    } else {
      data <- rds
    }
    return(data)
  } else {
    logger.debug("Skip loading: no source File")
  }

  return(NULL)
}

storeRdsOutput <- function(result, outputFile) {
    if(!is.null(outputFile) && outputFile != "" && !is.null(result)) {
        logger.debug("Storing RDS to file '%s'", outputFile)
        saveRDS(result, file = outputFile)
    } else {
        logger.debug("Storing the null-result to file %s", outputFile)
        file.create(outputFile) # write an empty file (for post-processing etc.)
    }
}