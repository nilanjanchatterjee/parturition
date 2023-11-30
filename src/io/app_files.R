#' Provides the path to app-files. App-files are files that
#' - are needed by the app to work during run time
#' - gets uploaded by the user of this app during configuration time
#' - there will be no warning if the requested file-set is not available (eg the user did not upload anything and the app developer did not provide any fallback)
#' - can also be provided by the app developer and gets bundled into the app during build time (the fallback)
#' 
#' @param appSpecLocalFileSettingId The ID of the requested set of app-files (see counterpart in `appspec.json` (setting[type=LOCAL_FILE].id))
#' @param fallbackToProvidedFiles Fallback to bundled directory of requested set of app-files (in case the app-developer provided a fallback)?
#' @return Path to the requested set of files (the app-file parent directory). Or `NULL` if user did not upload anything and no fallback was provided
#' 
getAppFilePath <- function(appSpecLocalFileSettingId, fallbackToProvidedFiles=TRUE) {
    if(!is.null(appSpecLocalFileSettingId) && appSpecLocalFileSettingId != "") {
        userUpload <- paste0(Sys.getenv(x = "LOCAL_APP_FILES_DIR"), "/uploaded-app-files/", appSpecLocalFileSettingId, "/")
        if (file.exists(userUpload) && length(list.files(userUpload)) > 0) {
            # directory exists and is not empty: user provided some files
            logger.info(paste0("Detected app-files provided by user for '", appSpecLocalFileSettingId, "'."))
            return(userUpload)
        } else if(fallbackToProvidedFiles) {
            # fallback to directory provided by app developer
            logger.info(paste0("Using fallback files provided by app developer for '", appSpecLocalFileSettingId, "'."))
            return(paste0(Sys.getenv(x = "LOCAL_APP_FILES_DIR"), "/provided-app-files/", appSpecLocalFileSettingId, "/"))
        } else {
            logger.warn(paste0("No files present for app-files '", appSpecLocalFileSettingId, "': User did not upload anything and the app did not provide fallback files."))
            return(NULL)
        }
    }
}

#' Provides the path to an app-artifact.
#' You can write the app generated file to this path and it will become available on MoveApps after each app run.
#' @param artifactName The name of the artifact
#' @return Path for the artifact. Use it to write your data
#'
appArtifactPath <- function(artifactName) {
    return(paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "."), artifactName))
}