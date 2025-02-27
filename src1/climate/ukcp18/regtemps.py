import os
import xarray as xr
import xagg as xa
import geopandas as gpd

datapath = os.path.expanduser("~/Dropbox/UK Economic Risks Data")

gdf_regions = gpd.read_file(os.path.join(datapath, "regions/ukcp-spatial-files-master/spatial-files/ukcp18-uk-land-region-hires/ukcp18-uk-land-region-hires-latlon.shp"))
weightmap = None

for filename in os.listdir(os.path.join(datapath, "climate/worldclim/uk-future")):
    print(filename)
    ds_tas = xr.open_dataset(os.path.join(datapath, "climate/worldclim/uk-future", filename))
    
    if weightmap is None:
        weightmap = xa.pixel_overlaps(ds_tas, gdf_regions, subset_bbox=False)

    aggregated = xa.aggregate(ds_tas, weightmap)
    aggregated.to_csv(os.path.join(datapath, "climate/ukcp18/worldclim/uk-future", filename.replace('.nc', '.csv')))

ds_tas = xr.open_dataset(os.path.join(datapath, "climate/worldclim/uk-present", "uk_bio.nc"))    
aggregated = xa.aggregate(ds_tas, weightmap)
aggregated.to_csv(os.path.join(datapath, "climate/ukcp18/worldclim/uk-present", "uk_bio.nc".replace('.nc', '.csv')))
