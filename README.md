# Parturition

MoveApps 
Github repository: https://github.com/nilanjanchatterjee/parturition

## Description

The app estimates time, location and length of parturition events from tracking data, based on the prediction that females reduce their speed abruptly and for a period following calving (for example, as described in [DeMars et al., 2013](https://doi.org/10.1002/ece3.785)). The calving date and location are selected when the moving average of speed in the time window drops below the speed threshold (see [Settings](#settings)). Output includes a file containing three plots for each event and a csv summarizing estimated parturitions (see [Artefacts](#artefacts)). 

## Documentation

See the data preparation steps below to prepare your workflow. This app is run across each track separately and calculates the step-wise speed for each relocation. Then it filters the location based on the threshold provided by the user or calculated using the average for each track. There are pros and cons for both the choices and user discretion and knowledge should be used.

Based on the filter it calculates the parturition interval, location of the parturition and number of relocations in the parturition interval. The app selects all time intervals from each data segment, identified as trackID, that satisfy the threshold condition. The calving time and location are identified as the start of the parturition interval and individual's location at that time. If prior apps break an individual's movements up into seasonal segments over multiple years, the app will run the algorithm separately for each animal-year. To prevent errors caused by tracks with insufficient data, the app will remove any track segments containing 10 or fewer records.

The results are provided as a set of plots per track (`Parturition_vel.pdf`) and a csv summarizing parturition events (`Parturition_output.csv`). In addition, the attribute `run.positive` is added to the moveStack output, indicating whether the locations satisfy the clustering criteria or not.

**Data preparation:** For realistic results, expert input is needed to prepare data segments appropriate for parturition analysis. First, ensure that females in the population are known or expected to reduce their movements (exhibit spatial clustering) during calving. If not, pursue an alternative method for analysis. This behavior is common across many ungulate populations, but not all. Prior to running the app, take the following steps to prepare the data, as needed:

* **Reduce the data to the expected calving season.** (Similar clustering behavior may occur for different reasons during other seasons.) You can extract data for the calving season using the *Filter/Annotate by Season* App. In this app, you can select to filter the data to a select date range, and to split tracks by season, in order to identify parturitions across animal-years.

* **Reduce the data to females only.** The *Filter by Track Data* can be used to limit the analysis to females. Note that this will only work if the sex is present in the data. If you access data in Movebank, ensure that [animal-sex](http://vocab.nerc.ac.uk/collection/MVB/current/MVB000023/) has been provided for all animals in the study. If information is missing, see [these instructions](https://www.movebank.org/cms/movebank-content/upload-qc#add_deployments_and_reference_data) to add it or contact support@movebank.org for assistance.

* **Remove outliers.** This method is sensitive to outliers and low accuracy points that imply high movement rates, which can prevent the app from detecting periods of low movement. To assess whether there are outliers in your dataset, view the tracks on a map, for example using the *Interactive Map (leaflet)* app or on Movebank, or assess speeds between location (several apps are available for this). If you access data in Movebank, see [these instructions](https://www.movebank.org/cms/movebank-content/upload-qc#flag_outliers) to flag outliers directly in the study, or contact support@movebank.org for assistance. Alternatively, you can use the *Remove Outliers* App to remove outliers based on speed or other quality indicators in the dataset.

* **Assess variability in fix rates.** The *Track Summary Statistics* App provides a summary of fix intervals per track. The results of app settings are affected by differences in fix rates. For example, as fix rate decreases, the movement speed calculated as a straight line between consecutive fixes will tend to increasingly underestimate the true movement speed. Calculating the threshold speed as the average speed of each track (the default `threshold speed` setting) is one way to account for these differences. Also, the minimum parturition period represented by the time window defined in the settings cannot be shorter than the time between fixes. If fix rates vary greatly across the dataset, consider creating separate workflow instances, so you can use optimal settings for each fix schedule.

* **Remove very short tracks.** Attempting to run the analysis across track segments without enough data to calculate or identify a change in movement rate over the chosen time window can result in errors or unexpected results. After completing other filtering, you can use the *Filter by Track Duration* App to remove tracks too short to include in the analysis. The app will remove segments with 10 or fewer records, however in some cases you may need to remove additional short segments.

**Interpreting results:** Results are summarized for each track segment, allowing estimates of parturition events across animal-years. Note that *all* periods meeting the user-defined threshold conditions will be reported in the results, in some cases suggesting multiple calving events for one animal-year, although this is not biologically realistic. This is done to support the user's expert assessment of choice of settings and interpretation of the results. Once optimal settings are chosen, the PDF output and other local information can be used to choose the most likely calving event in cases that multiple events are identified, or to conclude that no parturition took place. For example, the net-squared displacement plot can be used to identify whether an identified calving event follows a significant movement representing travel to calving grounds, which is expected for many ungulate populations. 

**Choosing settings:** If your results indicate more or fewer calving events than expected, revisit the settings. For example, if too many parturition events are identified, you can either increase the time window or decrease the speed threshold. If you are unsure what speed setting is reasonable, first try using the average speed per track (the default `threshold speed` setting).

**Comparing results:** Often, exact or approximate calving dates for some animals in a dataset are known from aerial surveys, VITs, collar cameras, or other sources. To assess confidence in the automated analysis and settings applied in this app, it can be helpful to quickly compare the results with ground-truthed calving events where available. To compare the results of this app with calving dates from other sources, you can upload a file containing these dates, and they will be included in the output artefacts. They will be appended to `Parturition_output.csv` as `known_birthdate`, and displayed as grey vertical lines on the distance/time_interval and net-squared displacement plots in `Parturition_vel.pdf`. The file of known calving dates must be in .csv format and contain two columns:  
* `track_id`: An identifier for the track that exactly matches the track ID in your workflow, as shown in the output.csv or listed as `track_names` in the cargo agent. This is usually the animalID.year.  
* `birthdate`: The known or estimated birth date from another source, in format `yyyy-mm-dd`.  
Additional columns, as well as `track_id` values that do not match tracks in the dataset, can be included in the file but will be ignored. The values in this file are not used in the analysis and will not affect your results.

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
	`track_id`: The ID for the track segment   
	`individual_local_identifier`: The animal ID   
	`number_max_reloc`: The number of relocations in the parturition interval   
	`threshold_speed_meters_per_hour`: The threshold speed used   
	`start_date`: The timestamp of the start of the parturition event (yyyy-MM-dd HH:mm:ss UTC)   
	`end_date`: The timestamp of the end of the parturition event (yyyy-MM-dd HH:mm:ss UTC)   
	`number_detected_events`: the total number of events in the track that satisfy the threshold criteria  
	`location_long`: The longitude of the individual's location at the start of the parturition interval (decimal degrees, WGS84)   
	`location_lat`: The latitude of the individual's location at the start of the parturition interval (decimal degrees, WGS84)   
	`known_birthdate`: If a file of known calving dates is uploaded by the user, the date for each track will be added to the results file.


## Settings

**Threshold speed (`threshold`):** A speed threshold specified by the user or else defined as the average speed per track calculated from the input dataset (default). Movements below this speed over the specified time window may indicate a calving event.
   
**Time window used to calculate change in movement (`window`):** A time window for calculating the average movement speed, specified by user. Defaults to **72 hours**.

**BETA: Known calving events (`events_file`):** Option to upload a local file with known calving events to include in outputs. See file requirements above.

### Null or error handling

**Common errors**

ERROR: `App failed because the window is less than median difference in time between locations... Please increase your window size.`  
CAUSE: It is not possible to calculate an average movement speed, or assess the movement behavior of the animal, over a period in which there are no location estimates.  
SOLUTION: Increase "Time window used to calculate change in movement" so that the time window is longer than the median fix rate for the track in the dataset with the lowest fix rate.

ERROR: `attempt to set an attribute on NULL`  
CAUSE: There are no input data, or for one or more tracks, there is insufficient data to run the analysis.  
SOLUTION: Check that the output of the previous app is as expected. Otherwise, remove any very short track segments (see Data Preparation above).