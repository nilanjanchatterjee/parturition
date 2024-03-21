# Changelog SDK

## 2024-03 `v3.2.0`

- introduce app-setting-type `USER_FILE`
- deprecate app-setting-type `LOCAL_FILE`

## 2024-02 `v3.1.0`

- introduce `appspec.json` version `1.2`
    - removed `createsArtifacts`. It is safe to remove it completely from your `appspec.json` - MoveApps tries now to fetch artifacts for every running App.
    - verify to include the `null` option for setting types `DROPDOWN` and `RADIOBUTTONS` if `defaultValue` is set to `null`.
- drop `move1` dependency

## 2023-12 `v3.0.5`

- update of input files for testing apps. Files now include `move2_loc` and `telemetry.list` I/O types, and projected and not projected data. All details included in the README.txt in the data/raw folder.

## 2023-11 `v3.0.4`

- Upgrade `R` framework to `4.3.2`

## 2023-09 `v3.0.3`

- introduce `appspec.json` version `1.1`
    - documentation link is not required any more

## 2023-08 `v3.0.2`

- removed MoveApps IO-Type dependency

## 2023-07 `v3.0.1`

- provide new `move2` input files
- minor fixes in src/io files

## 2023-06 `v3.0.0`

- introduces template versioning (starting w/ `v3.0.0` as this is the third major iteration)
- introduces `dotenv` to control/adjust local app-development
- updates developer readme
- introduces a _Template Synchronization_ GH action. Use it to synchronize your forked app with template updates. If you already forked from the template _before_ SDK `v3.0.0` you can (*only use this option if your app is written for input data of class `move2`*): 
    1. manually add the files `.github/workflows/template-sync.yml` and `.github/.templatesyncignore` to your fork. 
    1. with these files you can manually execute the GH action named _.github/workflows/template-sync.yml_. 
    1. after merging the generated pull request add the file `app-configuration.json` manually to the root directory of your App.

- fix app-configuration for execution on moveapps.org
- clear app output of previously app run at each start of the SDK

- SDK supports [`move2`](https://gitlab.com/bartk/move2/), if input is of class [`move`](https://gitlab.com/bartk/move/) it will be converted into class `move2`.
- output is always move2
- Upgrade `R` framework to `4.3.1`
