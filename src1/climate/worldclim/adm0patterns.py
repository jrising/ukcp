import os, shutil, pickle, glob
import numpy as np
import xarray as xr
import xagg as xa
import geopandas as gpd
from pathlib import Path
import rioxarray

gdf_regions = gpd.read_file("../../regions/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
## weightmap = None
with open("adm0patterns_weights.pkl", 'rb') as saver:
    weightmap = pickle.load(saver)

for filename in os.listdir("future"):
    print(filename)
    os.system("unzip \"" + os.path.join("future", filename) + "\"")
    tiffiles = list(Path("share").rglob("*.tif"))
    
    ds_tas = xr.open_dataset(tiffiles[0], engine="rasterio")
    #ds_tas2 = xa.fix_ds(ds_tas)
    ds_tas2 = xa.fix_ds(ds_tas.isel(band=0))
    ds_tas3 = xa.get_bnds(ds_tas2)
        
    if weightmap is None:
        ds_pop = xr.open_dataset("../../socioeconomics/LandScan Global 2015/lspop2015.nc")
        ds_pop2 = xa.fix_ds(ds_pop)
        ds_pop3 = xa.get_bnds(ds_pop2)
        
        weightmap = xa.pixel_overlaps(ds_tas3, gdf_regions, weights=ds_pop3.Population, subset_bbox=False)
        with open("adm0patterns_weights.pkl", 'wb') as saver:
            pickle.dump(weightmap, saver)
            
    aggregated = xa.aggregate(ds_tas3, weightmap)
    aggregated.to_csv(os.path.join("adm0", filename.replace('.zip', '.csv')))
    shutil.rmtree('share')

os.system("unzip present/wc2.1_10m_bio.zip")

# ds_tases = []
# for band in range(1, 20):
#     ds_tas = xr.open_dataset("wc2.1_10m_bio_" + str(band) + ".tif", engine="rasterio")
#     ds_tases.append(ds_tas)
# ds_tas = xr.concat(ds_tases, 'band')
# ds_tas['band'] = range(1, 20)

ds_tas = xr.open_dataset("wc2.1_10m_bio_1.tif", engine="rasterio")
ds_tas2 = xa.fix_ds(ds_tas.isel(band=0))
ds_tas3 = xa.get_bnds(ds_tas2)

aggregated = xa.aggregate(ds_tas3, weightmap)
aggregated.to_csv(os.path.join("adm0", "wc2.1_10m_bio.csv"))
