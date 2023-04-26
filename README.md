# Parturition

MoveApps 
Github repository: https://github.com/nilanjanchatterjee/parturition

## Description

The app estimates time, location and length of parturition events from tracking data, based on the prediction that females reduce their speed abruptly and for a period following calving (for example, as described in [DeMars et al., 2013](https://doi.org/10.1002/ece3.785)). It uses the input as *move/movestack*, and the output is given in the form of a file containing three plots for each event and a csv summarizing estimated parturitions (see [Artefacts](#artefacts)). The calving date and location are selected when the moving average of speed in the time window drops below the speed threshold (see [Parameters](#parameters)).

## Documentation

The app should be run in a workflow following the App *Filter Data by Season* that allows filtering to the parturition season. The App *Filter by Animal Data* can likewise be used to limit the analysis to females. This app uses an individual filter and calculates the step-wise speed for each relocation. Then it filters the location based on the threshold provided by the user or calculates using the average from the dataset. There are pros and cons for both the choices and user discretion and knowledge should be used.

Based on the filter it calculates the parturition interval, location of the parturition and number of relocations in the parturition interval. The app selects the maximum time interval from the data that satisfies the threshold condition. The calving time and location are identified as the start of the parturition interval and individual's location at that time. Up to one parturition event can be calculated from each data segment, identified as trackId. Therefore, if prior apps break an individual's movements up into seasonal segments over multiple years, the app will run the algorithm separately for each animal-year.

The results are provided as a set of plots per track (`Parturition_vel.pdf`) and a csv summarizing parturition events (`Parturition_output.csv`). In addition, the attribute `run.positive` is added to the moveStack output, indicating whether the locations satisfy the clustering criteria or not.

## Input data

*move/moveStack* in Movebank format and optional threshold speed and window length

## Output data

*move/moveStack* in Movebank format

### Artefacts
`Parturition_vel.pdf`: pdf with three plots per individual; 
1. The distance/time_interval plots with the marked speed threshold (red dashed line) and parturition interval (blue dotted lines, which may appear as a single line depending on the time range of the x-axis)    
2. The spatial locations of the individual with the identified parturition location marked with a blue cross
3. The net-squared displacement plot for the individual with the identified parturition interval marked with blue dotted lines 
 
`Parturition_output.csv`: csv containing the following attributes:
`Track_id`: The ID for the track segment
`Individual_id`: The animal ID
`Number_of_max_reloc`: The number of relocations in the parturition interval
`Threshold_speed(m/h)`: The threshold speed used
`Start_date`: The timestamp of the start of the parturition event (yyyy-MM-dd HH:mm:ss UTC)
`End_date`: The timestamp of the end of the parturition event (yyyy-MM-dd HH:mm:ss UTC)
`Numbers_of_detected_events`: the total number of events in the track that satisfy the threshold criteria
`location_long`: The longitude of the individual's location at the start of the parturition interval (decimal degrees, WGS84)
`location_lat`: The latitude of the individual's location at the start of the parturition interval (decimal degrees, WGS84)

## Settings

**Threshold speed (`threshold`):** A speed threshold, specified by the user or else the average individual speed calculated from the input dataset is used as default
   
**Time window used to calculate change in movement (`window`):** A time window for calculating the moving average, specified by user or else **72 hours** is used as default
