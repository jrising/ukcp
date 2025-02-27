import os, datetime
import xarray as xr
import pandas as pd

ukmst_chunks = []
for filename in os.listdir("."):
    if os.path.splitext(filename)[1] != '.nc':
        continue
    if "-dailytemp-" in filename:
        continue
    print(filename)
    with xr.open_dataset(filename) as ds:
        t2mmin = ds.t2m.groupby('time.date').min() - 273.15
        t2mmax = ds.t2m.groupby('time.date').max() - 273.15
        t2mavg = ds.t2m.groupby('time.date').mean() - 273.15

        dsout = xr.Dataset({"tavg": t2mavg, "tmin": t2mmin, "tmax": t2mmax})
        dsout['date'] = pd.to_datetime(dsout.date) - datetime.datetime(1900, 1, 1)
        dsout['date'].attrs['unit'] = "days since 1990-01-01 00:00:00" ;
        dsout['date'].attrs['calendar'] = "proleptic_gregorian" 
        dsout.to_netcdf(filename.replace("-temp-", "-dailytemp-"))

        ukmst_chunk = ds.t2m.groupby('time.year').mean(['time', 'latitude', 'longitude'], skipna=True)
        ukmst_chunks.append(ukmst_chunk)

        ukmst = xr.concat(ukmst_chunks, dim='year')
        ukmst.to_dataframe().to_csv("ukmst.csv")

ukmst = xr.concat(ukmst_chunks, dim='year')
ukmst.to_dataframe().to_csv("ukmst.csv")
