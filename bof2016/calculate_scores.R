# load required libraries
suppressWarnings(require(ohicore))

# set working directory to the scenario directory, ie containing conf and layers directories
setwd('bof2016') # if working within the RProject this filepath is enough

# load scenario configuration
conf = Conf('conf')

# run checks on scenario layers
CheckLayers('layers.csv', 'layers', flds_id=conf$config$layers_id_fields)

# load scenario layers
layers = Layers('layers.csv', 'layers')

# calculate scenario scores
scores = CalculateAll(conf, layers, debug=F)
write.csv(scores, 'scores.csv', na='', row.names=F)

## plot maps of scores
source('PrepSpatial.r') # see PrepSpatial.r for install of broom, maptools, rgdal
source('PlotMap.r')     
spatial_regions = PrepSpatial('spatial/MRPAOverallBoundaryPolygon(2014).shp') # can be .geojson or .shp
spatial_regions$region_id = 3 # make BoFEP region_id = 3 to match CAN New Brunswick
PlotMap(scores          = scores,
        spatial_regions = spatial_regions,
        path_figures    = 'reports/figures')
