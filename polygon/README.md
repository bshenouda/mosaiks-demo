This directory contains an example of processing polygon label data for merging with MOSAIKS features. The label data set is a shapefile with data on soil erosion propensity in Ecuador from 2018. 

The raw shapefile data can be downloaded here: https://geonode.wfp.org/layers/geonode%3Aecu_ica_landdegradation_geonode_jul2018.

The file `shp_label_predictions.Rmd` is an R markdown document that walks throught the intuition and process for preparing label data, merging the processed label with MOSAIKS features, and making predictions. 

The file `shp_label_predictions.md` shows the rendered output of the `.Rmd` notebook.

The shapefile input data is located in the directory `ecu_soil_erosion_data`. The directory `ECU_wat` contains shapefiles of waterways in Ecuador used in the final plot of the notebook.