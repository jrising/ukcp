import numpy as np
import xarray as xr
import xesmf as xe
from scipy import interpolate

global_scens = ["B1-03236a-global-ann-2070-2099.nc", "A1B-03236a-global-ann-2070-2099.nc", "A1FI-03236a-global-ann-2070-2099.nc"]
global_desired_temps = {2: global_scens[0], 4: global_scens[2], 6: global_scens[2]}

indmap = {}
for temp in global_desired_temps.keys():
    ds = xr.open_dataset(global_desired_temps[temp])
    inds = np.where(np.abs(ds.temp_dmean_tmean_abs - temp) < .1)
    print(len(inds[2]))
    indmap[temp] = inds[2]

for filename in ['prob_land_cc_grid_box_25km_ann_2010-2039_temp_dmean_tmean_abs.nc', 'prob_land_cc_grid_box_25km_ann_2070-2099_temp_dmean_tmean_abs.nc']:
    ds = xr.open_dataset(filename)
    for temp in global_desired_temps.keys():
        ds_mu = ds.isel(sample=indmap[temp]).mean('sample')
        ds_mu_sc = ds_mu.isel(em_scen=global_scens.index(global_desired_temps[temp]))

        temps = ds_mu_sc.temp_dmean_tmean_abs.values.flatten()
        ifunc = interpolate.NearestNDInterpolator(list(zip(ds_mu_sc.lon.values.flatten()[~np.isnan(temps)],
                                                           ds_mu_sc.lat.values.flatten()[~np.isnan(temps)])),
                                                  temps[~np.isnan(temps)])

        ds_out = xe.util.grid_2d(np.min(ds_mu.lon), np.max(ds_mu.lon), np.min(np.diff(ds_mu.lon)) / 2,
                                 np.min(ds_mu.lat), np.max(ds_mu.lat), np.min(np.diff(ds_mu.lat)) / 2)

        # temp_sc = ifunc(np.unique(ds_out.lon), np.unique(ds_out.lat))
        temp_sc = ifunc(ds_out.lon.values.flatten(), ds_out.lat.values.flatten())

        ifunc_nan = interpolate.NearestNDInterpolator(list(zip(ds_mu_sc.lon.values.flatten(),
                                                               ds_mu_sc.lat.values.flatten())),
                                                      np.isnan(temps) + 0)
        nan_sc = ifunc_nan(ds_out.lon.values.flatten(), ds_out.lat.values.flatten())

        temp_sc[nan_sc == 1.] = np.nan
        
        ds_out['temp'] = (('y', 'x'), temp_sc.reshape(ds_out.lon.shape))
        ds_out.to_netcdf(filename.replace('prob_', 'meanprob_').replace('.nc', 'warm' + str(temp) + '.nc'))

## Also compute baseline

ds = xr.open_dataset('B1-03236a-global-ann-2010-2039.nc')
inds = np.where(np.abs(ds.temp_dmean_tmean_abs - 0.9272394) < .1)[2] # Average from find-2012.R
ds = xr.open_dataset('prob_land_cc_grid_box_25km_ann_2010-2039_temp_dmean_tmean_abs.nc')
ds_mu = ds.isel(sample=inds).mean('sample')
ds_mu_sc = ds_mu.isel(em_scen=0)

temps = ds_mu_sc.temp_dmean_tmean_abs.values.flatten()
ifunc = interpolate.NearestNDInterpolator(list(zip(ds_mu_sc.lon.values.flatten()[~np.isnan(temps)],
                                                   ds_mu_sc.lat.values.flatten()[~np.isnan(temps)])),
                                          temps[~np.isnan(temps)])

ds_out = xe.util.grid_2d(np.min(ds_mu.lon), np.max(ds_mu.lon), np.min(np.diff(ds_mu.lon)) / 2,
                         np.min(ds_mu.lat), np.max(ds_mu.lat), np.min(np.diff(ds_mu.lat)) / 2)

temp_sc = ifunc(ds_out.lon.values.flatten(), ds_out.lat.values.flatten())

ifunc_nan = interpolate.NearestNDInterpolator(list(zip(ds_mu_sc.lon.values.flatten(),
                                                       ds_mu_sc.lat.values.flatten())),
                                              np.isnan(temps) + 0)
nan_sc = ifunc_nan(ds_out.lon.values.flatten(), ds_out.lat.values.flatten())

temp_sc[nan_sc == 1.] = np.nan
        
ds_out['temp'] = (('y', 'x'), temp_sc.reshape(ds_out.lon.shape))
ds_out.to_netcdf('meanprob_land_cc_grid_box_25km_ann_2010-2039_temp_dmean_tmean_abs2012.nc')
