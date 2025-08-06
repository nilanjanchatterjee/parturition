# Parturition Detection

MoveApps

Repository for detecting parturition events from animal movement data  
Github repository: https://github.com/nilanjanchatterjee/parturition

## Description

The app estimates the timing, location, and duration of parturition events from tracking data, based on the prediction that females reduce their movement speed abruptly and for an extended period following calving (for example, as described in [DeMars et al., 2013](https://doi.org/10.1002/ece3.785)). The app identifies parturition events when the rolling average of movement speed drops below a defined threshold for a sustained period.

## Documentation

See the data preparation steps below to prepare your workflow. This app is run across each track separately and calculates the step-wise speed for each relocation. Then it filters the location based on the threshold provided by the user or calculated using the average for each track. There are pros and cons for both the choices and user discretion and knowledge should be used.

Based on the filter it calculates the parturition interval, location of the parturition and number of relocations in the parturition interval. The app selects all time intervals from each data segment, identified as trackID, that satisfy the threshold condition. The calving time and location are identified as the start of the parturition interval and individual's location at that time. If prior apps break an individual's movements up into seasonal segments over multiple years, the app will run the algorithm separately for each animal-year. To prevent errors caused by tracks with insufficient data, the app will remove any track segments containing 10 or fewer records.

The results are provided as a set of plots per track (`parturition_analysis_[threshold]mh_[window]h_threshold.pdf`) and a csv summarizing parturition events (`parturition_output_[threshold]mh_[window]h_threshold.csv`). In addition, movement metrics and parturition indicators are added to the move2 output, indicating whether the locations satisfy the clustering criteria or not.

**Data preparation:** For realistic results, expert input is needed to prepare data segments appropriate for parturition analysis. First, ensure that females in the population are known or expected to reduce their movements (exhibit spatial clustering) during calving. If not, pursue an alternative method for analysis. This behavior is common across many ungulate populations, but not all. Prior to running the app, take the following steps to prepare the data, as needed:

* **Reduce the data to the expected calving season.** (Similar clustering behavior may occur for different reasons during other seasons.) You can extract data for the calving season using the *Filter/Annotate by Season* App. In this app, you can select to filter the data to a select date range, and to split tracks by season, in order to identify parturitions across animal-years.
* **Reduce the data to females only.** The *Filter by Track Data* can be used to limit the analysis to females. Note that this will only work if the sex is present in the data. If you access data in Movebank, ensure that [animal-sex](http://vocab.nerc.ac.uk/collection/MVB/current/MVB000023/) has been provided for all animals in the study. If information is missing, see [these instructions](https://www.movebank.org/cms/movebank-content/upload-qc#add_deployments_and_reference_data) to add it or contact support@movebank.org for assistance.
* **Remove outliers.** This method is sensitive to outliers and low accuracy points that imply high movement rates, which can prevent the app from detecting periods of low movement. To assess whether there are outliers in your dataset, view the tracks on a map, for example using the *Interactive Map (leaflet)* app or on Movebank, or assess speeds between location (several apps are available for this). If you access data in Movebank, see [these instructions](https://www.movebank.org/cms/movebank-content/upload-qc#flag_outliers) to flag outliers directly in the study, or contact support@movebank.org for assistance. Alternatively, you can use the *Remove Outliers* App to remove outliers based on speed or other quality indicators in the dataset.
* **Assess variability in fix rates.** The *Track Summary Statistics* App provides a summary of fix intervals per track. The results of app settings are affected by differences in fix rates. For example, as fix rate decreases, the movement speed calculated as a straight line between consecutive fixes will tend to increasingly underestimate the true movement speed. Calculating the threshold speed as the average speed of each track (the default `threshold speed` setting) is one way to account for these differences. Also, the minimum parturition period represented by the time window defined in the settings cannot be shorter than the time between fixes. If fix rates vary greatly across the dataset, consider creating separate workflow instances, so you can use optimal settings for each fix schedule.
* **Remove very short tracks.** Attempting to run the analysis across track segments without enough data to calculate or identify a change in movement rate over the chosen time window can result in errors or unexpected results. After completing other filtering, you can use the *Filter by Track Duration* App to remove tracks too short to include in the analysis. The app will remove segments with 10 or fewer records, however in some cases you may need to remove additional short segments.

**Interpreting results:** Results are summarized for each track segment, allowing estimates of parturition events across animal-years. Note that *all* periods meeting the user-defined threshold conditions will be reported in the results, in some cases suggesting multiple calving events for one animal-year, although this is not biologically realistic. This is done to support the user's expert assessment of choice of settings and interpretation of the results. Once optimal settings are chosen, the PDF output and other local information can be used to choose the most likely calving event in cases that multiple events are identified, or to conclude that no parturition took place. For example, the net-squared displacement plot can be used to identify whether an identified calving event follows a significant movement representing travel to calving grounds, which is expected for many ungulate populations. 

**Choosing settings:** If your results indicate more or fewer calving events than expected, revisit the settings. For example, if too many parturition events are identified, you can either increase the time window or decrease the speed threshold. If you are unsure what speed setting is reasonable, first try using the average speed per track (the default `threshold speed` setting).

**Comparing results:** Often, exact or approximate calving dates for some animals in a dataset are known from aerial surveys, VITs, collar cameras, or other sources. To assess confidence in the automated analysis and settings applied in this app, it can be helpful to quickly compare the results with ground-truthed calving events where available. To compare the results of this app with calving dates from other sources, you can upload a file containing these dates, and they will be included in the output artefacts. They will be appended to `parturition_output_[threshold]mh_[window]h.csv` as `known_birthdate`, and displayed as blue vertical lines on the speed and net-squared displacement plots in `parturition_analysis_[threshold]mh_[window]h_threshold.pdf`. The file of known calving dates must be in .csv format and contain two columns:  
* `track_id`: An identifier for the track that exactly matches the track ID in your workflow, as shown in the output.csv or listed as `track_names` in the cargo agent.  
* `birthdate`: The known or estimated birth date from another source, in format `yyyy-mm-dd`.  

Additional columns, as well as `track_id` values that do not match tracks in the data set, can be included in the file but will be ignored. The values in this file are not used in the analysis and will not affect your results.

### Application Scope

#### Generality of App Usability

This app was developed primarily for ungulate species (deer, elk, caribou, etc.) that exhibit characteristic movement reduction during parturition. While it may work for other species with similar parturition behaviors, the default settings and analysis approach are optimized for ungulates. The method requires that your study species exhibits spatial clustering behavior during calving.

#### Required Data Properties

- **GPS tracking data** with sufficient temporal resolution (recommended: ≤4 hour fix intervals)
- **Breeding females only** during expected calving seasons
- **Sufficient track duration** to calculate rolling windows (minimum 10 locations per track)
- **Data covering calving periods** rather than full annual cycles
- **WGS84 coordinate system (EPSG:4326)** - the app will error if data is in a different projection

### Data Preparation

For realistic results, expert input is needed to prepare appropriate data segments for parturition analysis:

**Essential Preprocessing Steps:**

1. **Reduce to calving season**: Extract data for expected calving periods using the *Filter/Annotate by Season* App
2. **Filter to females only**: Use the *Filter by Track Data* App to include only breeding females
3. **Remove outliers**: Use the *Remove Outliers* App or flag outliers in Movebank to prevent false negatives
4. **Assess fix rates**: Use *Track Summary Statistics* App to understand temporal resolution and adjust settings accordingly
5. **Remove short tracks**: Use *Filter by Track Duration* App to remove tracks too short for meaningful analysis

**Data Quality Considerations:**

- **Fix Rate Consistency**: Variable fix rates can affect speed calculations and threshold performance
- **Outlier Impact**: High-speed outliers can mask periods of low movement
- **Seasonal Appropriateness**: Analysis should focus on calving seasons when clustering behavior is expected

### Input Type

`move2::move2_loc` in Movebank format

### Output Type

`move2::move2_loc` in Movebank format with additional movement metrics

### Artefacts

**`parturition_analysis_[window]h_threshold_[threshold].pdf`**: Comprehensive visualization document containing:
- **Legend page**: Detailed explanation of all plot elements and symbols
- **Analysis results**: Three plots per track showing:
  1. **Location plot**: Spatial track with elevation background (optional) and detected parturition locations marked
  2. **Speed plot**: Time series of movement speed with rolling averages, thresholds, and detected events highlighted
  3. **Net Squared Displacement plot**: NSD over time showing movement patterns relative to starting location

**`parturition_output_[window]h_threshold_[threshold].csv`**: Detailed results summary containing:
- `track_id`: Track segment identifier
- `individual_local_identifier`: Animal identifier  
- `number_max_reloc`: Number of locations in the parturition interval
- `threshold_speed_meters_per_hour`: Speed threshold used for detection
- `start_date`: Parturition event start timestamp (UTC)
- `end_date`: Parturition event end timestamp (UTC)  
- `number_detected_events`: Total events detected for this track
- `location_long`: Longitude of parturition location (WGS84)
- `location_lat`: Latitude of parturition location (WGS84)
- `known_birthdate`: Known parturition date (if provided by user)

### Settings

**Movement speed threshold (`threshold`)**: Speed threshold in meters per hour below which movement indicates potential parturition behavior. When set to null (default), the rolling average speed for each track is automatically calculated and used as the threshold.

**Rolling window duration (`window`)**: Time window in hours for calculating rolling averages and identifying sustained low-movement periods. Must be larger than the fix rate interval. Default: 72 hours (recommended for ungulates).

**Known parturition events file (`events_file`)**: Optional CSV file containing known parturition dates for validation. Must contain columns `track_id` and `birthdate` (YYYY-MM-DD format). Known events will be displayed in plots and included in output files.

**Speed plot Y-axis limit (`yaxis_limit`)**: Maximum value for speed plot Y-axis in meters per hour. Helps standardize plot scales across tracks. Default: 1000.

**Include elevation background (`include_elevation`)**: Add elevation terrain as background in location plots. Provides geographic context but increases processing time and requires internet connectivity. Default: false.

### Interpreting Results

**Event Validation**: The app reports all periods meeting threshold conditions, which may include multiple events per animal. Use expert knowledge and the visualization outputs to identify the most likely parturition event.

**Plot Interpretation**:
- **Green titles**: Analysis completed successfully
- **Red titles**: Analysis failed due to parameter issues (check logs)
- **Speed plots**: Look for sustained periods below threshold (gray shaded areas)
- **NSD plots**: Parturition often follows significant movements to calving grounds
- **Location plots**: Parturition locations marked with distinctive symbols

**Settings Optimization**: 
- **Too many events detected**: Increase time window or decrease speed threshold
- **Too few events detected**: Decrease time window or increase speed threshold  
- **Start with default settings**: Use rolling average threshold initially, then refine based on results

### Changes in Output Data

The input move2 object is returned with additional columns containing calculated movement metrics and parturition event indicators:

**Movement Metrics:**
- `location_long`: Longitude coordinates (decimal degrees, WGS84)
- `location_lat`: Latitude coordinates (decimal degrees, WGS84)  
- `distance`: Step distance in meters between consecutive locations
- `timediff`: Time difference in hours between consecutive locations
- `speed`: Movement speed in meters per hour (distance/timediff)
- `speed_rolling_mean`: Rolling average of movement speed over the specified window
- `nsd_km`: Net squared displacement in kilometers from the first location
- `nsd_km_rolling_mean`: Rolling average of net squared displacement

**Parturition Detection Variables:**
- `is_below_threshold`: Binary indicator (1/0) showing whether speed is below the threshold
- `run_length`: Sequential counter of consecutive locations within the same speed category
- `run_positive`: Counter of consecutive locations below threshold (0 when above threshold)
- `run_change`: Indicator of changes in movement patterns used for event detection
- `parturition_event`: Binary indicator (1/0) marking locations within detected parturition events
- `valid_window`: Boolean indicating whether the rolling window analysis was successful for this track

These additional columns allow users to conduct further analyses, validate results, and understand the movement patterns that led to parturition event detection.

### Most Common Errors

**ERROR**: `Window [X] less than median time between locations [Y] for track [Z]. Increase your window size.`  
**CAUSE**: Time window is shorter than the fix interval, preventing rolling average calculation.  
**SOLUTION**: Increase the "Rolling window duration" setting to exceed the median fix rate.

**ERROR**: `Window [X] too large for median time between locations [Y] for track [Z]. Decrease window size.`  
**CAUSE**: Time window is too large relative to track duration.  
**SOLUTION**: Decrease the "Rolling window duration" or remove short tracks from analysis.

**ERROR**: `attempt to set an attribute on NULL`  
**CAUSE**: No input data or insufficient data for analysis.  
**SOLUTION**: Check previous app output and remove tracks with ≤10 locations.

### Null or Error Handling

**Track Filtering**: Tracks with insufficient data (≤10 locations or duration less than the specified window) are automatically excluded from analysis with appropriate logging.

**Failed Analysis Indication**: Tracks where analysis fails due to parameter issues are clearly marked in plot titles with red text and detailed error messages in logs.

**Missing Data**: The app handles missing values gracefully and continues processing other tracks when individual tracks encounter errors.

**File Validation**: Known events files are validated for proper format, with warnings logged for any formatting issues while continuing analysis with available data.