# Partuition

Moveapps 
Github repository: https://github.com/nilanjanchatterjee/partuition

## Description

The app predicts time, location and length of parturition events from radio-telemetry data. It uses the input as *move/movestack* as the output is given in the form of step-length plots with the marked threshold (red-dashed line) and the partuition time (blue-dotted line) and csv with the individual id, partution time and the lat-long of the partutition event.

## Documentation

The app should be ran with the app *Filter data by season*  and *Distance between locations* for convenience. It uses the input of step-length calculated from the *Distance between locations* app. This app uses an individual filter and calcualtes the stepwise speed for each relocation. Then it filters the location based on the threshold provided by the user or calculates using the average from the dataset. There are pros and cons for both the choices and user discretion and knowledge should be used. 
Based on the filter it calcualtes the partuition interval, location of the partuition and number of relocations in the partution inerval. We used the maximum interval from the data that satisfies the threshold condiiton. 

## Input data

*move/moveStack* in Movebank format and optional threshold speed and window length

## Output data
### Artefacts
 - Partuition_vel.pdf: pdf with the step-length plots with the marked threshold (red-dashed line) and partuitin intervel (blue-dotted line)
 - Partuition_date_output.csv: csv with the individual id, partution time and partuiton lat-long

## Parameters

*threshold*: A speed threshold (user specified) or averge calculated using input dataset
*window*: A window specified by user for calculation of moving average or **72 hours** used as default
