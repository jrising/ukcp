import os
import numpy as np
import xarray as xr
import xagg as xa
import geopandas as gpd

datapath = os.path.expanduser("~/Dropbox/UK Economic Risks Data")

gdf_regions = gpd.read_file(os.path.join(datapath, "regions/ukcp-spatial-files-master/spatial-files/ukcp18-uk-land-region-hires/ukcp18-uk-land-region-hires-latlon.shp"))

filename = "Europe_TAVG_LatLong0.25.nc"

ds = xr.open_dataset(os.path.join(datapath, "climate/best/", filename))

## Construct total temperatures

# Expand climatology to span all days
clim_tmp = xr.DataArray(dims=('time','latitude','longitude'),
                        coords={'time':ds.time,'latitude':ds.latitude,'longitude':ds.longitude},
                        data=np.zeros((ds.dims['time'],ds.dims['latitude'],ds.dims['longitude']))*np.nan)

# Sub in variables one year at a time
for yr in np.unique(np.floor(ds.time)):
    if yr == 2013:
        break
    clim_tmp.values[np.floor(ds.time)==yr, :, :] = ds.climatology.values

ds['climatology'] = clim_tmp

ds['tas'] = ds['temperature'] + ds['climatology']
ds = ds.drop(['temperature','climatology'])

weightmap = xa.pixel_overlaps(ds, gdf_regions, subset_bbox=False)
aggregated = xa.aggregate(ds, weightmap)
aggregated.to_csv(os.path.join(datapath, "climate/ukcp18/best", filename.replace('.nc', '.csv')))
