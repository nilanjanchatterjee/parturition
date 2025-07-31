# MoveApps R Software Development Kit (SDK)

#### ***NOTE*: this SDK only supports code written for input data of class `move2` and not `moveStack`, as all input data of class `moveStack` will be converted to class `move2`. For all other input/output types, this SDK works as usual. Please contact us under support@moveapps.org if you have any questions.**

This documentation provides a short introduction to the [MoveApps](https://www.moveapps.org) **R SDK**.

As a first step, and before your read this, you should have used this GitHub template to create a copy of it in your personal space and named the repository as your App will be named in MoveApps.

A general overview provides the [MoveApps user manual](https://docs.moveapps.org/#/create_app)

# Overview

This template is designed according to a file structure that is necessary for your App to run in your local development environment similar to the way it will run in the MoveApps environment later. Please contain the structure and only change/add files as necessary for your App's functionality. See below which files can be changed and which should remain as is for simulation of the behaviour on MoveApps on your local system. A stepwise explanation below indicates the function and some background of each file and folder.

## File structure

(truncated)

```
.
├── Dockerfile
├── README.md
├── RFunction.R
├── Template_R_Function_App.Rproj
├── app-configuration.json
├── appspec.json
├── data
│   ├── auxiliary
│   ├── output
│   └── raw
│       ├── input1.rds
│       ├── input2.rds
│       ├── input3.rds
│       └── input4.rds
├── renv.lock
├── sdk.R
├── src
│   ├── common
│   │   ├── logger.R
│   │   └── runtime_configuration.R
│   ├── io
│   │   ├── app_files.R
│   │   ├── io_handler.R
│   │   └── rds.R
│   └── moveapps.R
└── start-process.sh


```

1. `./RFunction.R`: This is the entrypoint for your App logic. MoveApps will call this function during a workflow execution which includes your App. **The file must be named `RFunction.R`, do not alter it!**
1. `./appspec.json`: This file defines the settings and metadata of your App, for details refer to the [MoveApps User Manual](https://docs.moveapps.org/#/appspec)
1. `./renv.lock`: Definition of the dependencies of your App. We use `renv` as library manager. Optional.
1. `./data/**`: Resources of the SDK
   1. `auxiliary/**`: Simulates the usage of [*auxiliary App files*](https://docs.moveapps.org/#/auxiliary). You can put files into this folder to simulate an App run with provided/user-uploaded files. 
   1. `output/**`: If your App produces [*artefacts*](https://docs.moveapps.org/#/copilot-r-sdk?id=artefacts) they will be stored here.
   1. `raw/**`: Collection of sample App input data. You can use these samples to simulate an App run with real input.
1. `./sdk/**`: The (internal) MoveApps R SDK logic.
1. `./sdk.R`: The main entry point of the SDK. Use it to execute your App in your IDE.
1. `./tests/**`: Location for Unit Tests

## SDK Runtime environment

Critical parts of the SDK can be adjusted by `environment variables`. 
Keep in mind that these variables are only changeable during App development and not during an App run on MoveApps.
They are predefined with sensible defaults - they should work for you as they are.

- `CONFIGURATION_FILE`: configuration of your App ([JSON](https://www.w3schools.com/js/js_json_intro.asp) - must correspondent with the `settings` of your `appspec.json`)
- `PRINT_CONFIGURATION`: prints the configuration your App receives (`yes|no`)
- `SOURCE_FILE`: path to an input file for your App during development
- `OUTPUT_FILE`: path to the output file of your App
- `ERROR_FILE`: path to a file collecting error messages
- `APP_ARTIFACTS_DIR`: base directory for writing App artifacts
- `USER_APP_FILE_HOME_DIR`: home aka base directory of your local user App files (*auxiliary*)
- ~~`LOCAL_APP_FILES_DIR`~~: Deprecated! base directory of your local App files (*auxiliary*)
- `CLEAR_OUTPUT`: clears all output of the previously app run at each start of the SDK aka the next app start

You can adjust these environment variables by adjusting the file `./.env`.

The file `./.env` is **hidden** by default in `RStudio`! You can show it by

1. Tab `Files` in `RStudio`
1. `More` Button in the Files-Toolbar
1. Activate _Show Hidden Files_

## MoveApps App Bundle

Which files will be bundled into the final App running on MoveApps?

- the file `./RFunction.R
- all directories defined in your `appspec.json` at `providedAppFiles` 

Nothing else.

Note that many App features will be set and updated with information from the `appspec.json` in each new App version. Thus, even if not bundled into the App, this file is required and must be up to date.


## App development

1. Execute `Rscript sdk.R` (on a terminal) or run/source `sdk.R` in _RStudio_
1. Ensure the sdk executes the vanilla template App code. Everything is set up correctly if no error occurs and you see something like _Welcome to the MoveApps R SDK._
1. Begin with your App development in `./RFunction.R`

## Examples

### Request App configuration from your users

`./appspec.json`: define the settings UI on MoveApps. Users of your App can enter their configuration values.

![img.png](documentation/app-configuration-ui.png)

You can also use our [Settings Editor](https://www.moveapps.org/apps/settingseditor) to generate the App configuration

```
"settings": [
 {
   "id": "line_width",
   "name": "Line width",
   "description": "The width of the lines in the plot.",
   "defaultValue": 2,
   "type": "INTEGER"
 },
 {
   "id": "legend",
   "name": "Include legend?",
   "description": "Should the plot contain a legend?",
   "defaultValue": false,
   "type": "CHECKBOX"
 }
],
```

`./app-configuration.json`: this is only needed during the app development to simulate an App run

```
{
  "line_width": 2,
  "legend": true
}
```

`./RFunction.R`: your App will be called with the user's App configuration

```
rFunction = function(data, line_width, legend, ...) {
}
```

`./tests/testthat/test_RFunction.R`: do not forget to test your App

```
test_data <- test_data("input3_stork.rds")

test_that("happy path", {
  actual <- rFunction(data = test_data, sdk = "unit test", year = 2005)
  expect_equal(unique(lubridate::year(actual@timestamps)), 2005)
})

test_that("year not included", {
  actual <- rFunction(data = test_data, sdk = "unit test", year = 2023)
  expect_null(actual)
})
```

### Produce an App artefact

Your App can write files which the user can download after it has run.

`./appspec.json`

```
  "createsArtifacts": true,
```

`./RFunction.R`

```
pdf(appArtifactPath("MorningReport_overviewTable.pdf"), paper = "a4r")
```

**Notice:** Only files are permitted to act as MoveApps App artifact! If your app produces a directory as an App artificat you have to bundle it eg. by zipping it. In other words: at the moment your App completes its work there must be only files present in `APP_ARTIFACTS_DIR`.

#### example for zipping:
```
library('zip')
dir.create(targetDirFiles <- tempdir())
...
# add any files to targetDirFiles
...
zip_file <- appArtifactPath(paste0("myfiles.zip"))
zip::zip(zip_file, 
    files = list.files(targetDirFiles, full.names = TRUE),
    mode = "cherry-pick")
```

## Include files to your App

[Details and examples about _auxiliary files_](https://docs.moveapps.org/#/auxiliary).

This template also implements in `./RFunction.R` a showcase about this topics.

---

## R packages management / renv (optional)

The template is prepared to use [`renv` as a dependency manager](https://rstudio.github.io/renv/articles/renv.html) - but is disabled by default (_opt-in_).
You can [activate `renv` with `renv::activate()`](https://rstudio.github.io/renv/articles/renv.html#uninstalling-renv) and then use it in the [usual `renv` workflow](https://rstudio.github.io/renv/articles/renv.html#workflow).

### Docker support (optional)

- at the end your app will be executed on MoveApps in a Docker container.
- if you like you can test your app in the almost final environment by running your app locally in a docker container:

1. enable `renv` (see above)
1. set a working title for your app by `export MY_MOVEAPPS_APP=hello-world` (in your terminal)
1. build the Docker image locally by `docker build --platform=linux/amd64 -t $MY_MOVEAPPS_APP .` (in your terminal)
1. execute the image with `docker run --platform=linux/amd64 --rm --name $MY_MOVEAPPS_APP -it $MY_MOVEAPPS_APP` (in your terminal)
1. you will get a `bash` terminal of the running container. There you can get a R console by `R` or simply start your app by invoking `/home/moveapps/co-pilot-r/start-process.sh` (in the `bash` of the running container)

## Synchronisation of your fork with this template

This template includes a _GitHub action_ to keep your fork synchronized with the original template (aka the MoveApps R SDK). The synchronization action creates a _GitHub pull request_ in your fork from time to time in case the original template has changed.
