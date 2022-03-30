
FATAL <- 1L
names(FATAL) <- "FATAL"
ERROR <- 2L
names(ERROR) <- "ERROR"
WARN <- 4L
names(WARN) <- "WARN"
INFO <- 6L
names(INFO) <- "INFO"
DEBUG <- 8L
names(DEBUG) <- "DEBUG"
TRACE <- 9L
names(TRACE) <- "TRACE"

logger.layout <- function(level, msg, id='', ...) {
  the.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (length(list(...)) > 0) {
    parsed <- lapply(list(...), function(x) if(is.null(x)) 'NULL' else x )
    msg <- do.call(sprintf, c(msg, parsed))
  }
  sprintf("[%s] %s\n", names(level), msg)
}

logger.log_level <- function(msg, ..., level)
{
  if (level <= logger.threshold)  {
    message <- logger.layout(level, msg, name, ...)
    cat(message)
  }
}

logger.trace <- function(msg, ...) {
  logger.log_level(msg, ..., level=TRACE)
}

logger.debug <- function(msg, ...) {
  logger.log_level(msg, ..., level=DEBUG)
}

logger.info <- function(msg, ...) {
  logger.log_level(msg, ..., level=INFO)
}

logger.warn <- function(msg, ...) {
  logger.log_level(msg, ..., level=WARN)
}

logger.error <- function(msg, ...) {
  logger.log_level(msg, ..., level=ERROR)
}

logger.fatal <- function(msg, ...) {
  logger.log_level(msg, ..., level=FATAL)
}

logger.threshold = TRACE