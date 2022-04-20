# Parturition

Moveapps 
Github repository: https://github.com/nilanjanchatterjee/parturition

## Description

The app predicts time, location and length of parturition events from tracking data. It uses the input as *move/movestack* as the output is given in the form of step-length plots with the marked threshold (red-dashed line) and the parturition time (blue-dotted line) and csv with the individual id, parturition time and the lat-long of the parturition event.

## Documentation

The app must be ran in a workflow with the app *Distance between locations* preceding it, becuase it uses the input of step-length calculated by this App. Furthermore, the App *Filter data by season* allows filtering to the parturition season before. This app uses an individual filter and calculates the step-wise speed for each relocation. Then it filters the location based on the threshold provided by the user or calculates using the average from the dataset. There are pros and cons for both the choices and user discretion and knowledge should be used. 
Based on the filter it calculates the parturition interval, location of the parturition and number of relocations in the parturition interval. We used the maximum interval from the data that satisfies the threshold condition. 

## Input data

*move/moveStack* in Movebank format and optional threshold speed and window length

## Output data

*move/moveStack* in Movebank format

### Artefacts
 - Parturition_vel.pdf: pdf with the distance/time_interval plots with the marked threshold (red-dashed line) and parturition interval (blue-dotted line)    
 
 - Parturition_date_output.csv: csv with the individual id, parturition time and parturition lat-long

## Parameters

*threshold*: A speed threshold (user specified) or average calculated using input dataset
   
*window*: A window specified by user for calculation of moving average or **72 hours** used as default
