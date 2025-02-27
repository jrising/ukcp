import numpy as np
import pandas as pd
import pyproj

df = pd.read_csv("data/Ag_model_grid_2km.csv")

## https://epsg.io/27700
proj4_bng = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs "
proj4_lls = "+proj=longlat +datum=WGS84"

crs_bng = pyproj.CRS.from_proj4(proj4_bng)
crs_lls = pyproj.CRS.from_proj4(proj4_lls)

transformer = pyproj.Transformer.from_crs(crs_bng, crs_lls)

df['lon'] = np.nan
df['lat'] = np.nan

for ii, row in df.iterrows():
    lonlat = transformer.transform(row.Easting, row.Northing)
    df.lon[ii] = lonlat[0]
    df.lat[ii] = lonlat[1]

df.to_csv("data/Ag_model_grid_2km.v2.csv")
