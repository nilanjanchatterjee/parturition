Set of input data to test apps.

*Content*
- input1: 1 goat, median fix rate = 30mins, tracking duration 7.5 month, gps, local movement
- input2: 3 storks, median fix rate = 1sec, tracking duration 2 weeks, gps, local movement
- input3: 1 stork, median fix rate = 1h | 1day | 1 week, tracking duration 11.5 years, argos, includes migration
- input4: 3 geese, median fix rate = 1h | 4h, tracking duration 1.5 years, gps, includes migration

*I/O types*
- all data sets are provided as 'move2_loc'
- input1 & input2 are also provided as 'telemetry.list'

*Projection*
- for 'move2_loc', data are provided in "lat/long" (EPSG:4326) and projected to "Mollweide" (ESRI:54009) in order to test your app accordingly for not projected and projected data. If your app does not allow projected data or only can deal with projected data, document and either build a automatic transformation in the app or make it fail with an informative error message. The app "Change projection" can be refered to for the user to change the projection of the data acordingly previous to your app.

- the 'telemetry.list' examples are in a "aeqd" projection with 0,0 in the center of the track, as this is a common projection used within the ctmm library


*File names*
input1_move2loc_LatLon.rds
input1_move2loc_Mollweide.rds

input2_move2loc_LatLon.rds
input2_move2loc_Mollweide.rds

input3_move2loc_LatLon.rds
input3_move2loc_Mollweide.rds

input4_move2loc_LatLon.rds
input4_move2loc_Mollweide.rds


input1_telemetrylist_aeqd.rds
input2_telemetrylist_aeqd.rds



