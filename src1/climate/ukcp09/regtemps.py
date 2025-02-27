import os, glob
import numpy as np
import xarray as xr
import xagg as xa
import geopandas as gpd

datapath = os.path.expanduser("~/Open Modeling Group Dropbox/UK Economic Risks")

gdf_regions = gpd.read_file(os.path.join(datapath, "regions/ukcp09/ukcp09-merged01.shp"))
weightmap = None

for filename in glob.glob(os.path.join(datapath, "climate/ukcp09/meanprob*.nc")):
    print(filename)
    ds_tas = xr.open_dataset(os.path.join(datapath, "climate/ukcp09", filename))
    lon_b0 = ds_tas.lon_b[0, :-1]
    lon_b1 = ds_tas.lon_b[0, 1:]
    lat_b0 = ds_tas.lat_b[:-1, 0]
    lat_b1 = ds_tas.lat_b[1:, 0]

    ds_tas2 = ds_tas.drop(['lon_b', 'lat_b'], dim=None)
    ds_tas2['lon'] = ds_tas2.lon[0, :]
    ds_tas2['lat'] = ds_tas2.lat[:, 0]
    ds_tas3 = xa.fix_ds(ds_tas2)

    ds_tas3['lon_bnds'] = xr.DataArray(data=np.transpose([lon_b0, lon_b1]), dims=['lon','bnds'], coords={'lon': ds_tas3['lon'], 'bnds': np.arange(0,2)})
    ds_tas3['lat_bnds'] = xr.DataArray(data=np.transpose([lat_b0, lat_b1]), dims=['lat','bnds'], coords={'lat': ds_tas3['lat'], 'bnds': np.arange(0,2)})

    if weightmap is None:
        weightmap = xa.pixel_overlaps(ds_tas3, gdf_regions, subset_bbox=False)

    aggregated = xa.aggregate(ds_tas3, weightmap)
    aggregated.to_csv(os.path.join(datapath, "climate/ukcp09/", filename.replace('.nc', '.csv')))
