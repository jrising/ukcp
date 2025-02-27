import xarray as xr
import numpy as np

for filename in ['Temp_35K_A1B_CO2.nc', 'Temp_35K_Const_CO2.nc', 'Temp_71K_A1B_CO2.nc', 'Temp_71K_Const_CO2.nc']:
    print(filename)
    ds = xr.open_dataset(filename)
    ds['year'] = np.floor(ds.time)

    counts = ds.groupby('year').count(...).Temp

    ds2 = ds.groupby('year').mean('time')
    ds3 = ds2.where(counts == np.max(counts), drop=True)

    ds3.to_netcdf("~/Open Modeling Group Dropbox/UK Economic Risks/channels/agriculture/Boulton/" + filename)
    
