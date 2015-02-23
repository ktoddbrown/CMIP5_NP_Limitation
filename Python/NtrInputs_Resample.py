'''
Created on Jul 12, 2013
Author: William Kolby Smith

Inputs: 
1. MODIS Landcover (MOD12Q1) data. Reference: Friedl, M.A. et al. Remote Sens. Environ. 114, 168-182 (2010). 
2. Nitrogen Fixation data. Reference: Wang, Y.P. & Houlton, B.Z. Geophys. Res. Lett. 36, doi:10.1029/2009gl041009 (2009).
3. Nitrogen Deposition data. Reference: i) Lamarque, J. F. et al. Atmos. Chem. Phys. 10, 7017-7039 (2010); ii) Lamarque, J. F. et al. Climatic Change 109, 191-212 (2011).
4. Phosphorus weathing data. Reference: Wang, Y. P. et al. Biogeosciences 7, 2261-2282, (2010).
5. Phosphorus deposition data. Reference: i) Mahowald, N. M. in Ecological Systems (ed Rik Leemans) Ch. 2, 7-29 (Springer, 2013); ii) Mahowald, N. et al. Glob. Biogeochem. Cy. GB026 (2008).

Outputs:
Resampled versions of the input data at 1 degree spatial resolution 
'''
from input_raster import input_raster
import osgeo.gdal as gdal
import osgeo.gdalconst as gdalconst
import osgeo.osr as osr
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import netCDF4 as pycdf
import numpy as np

####Resample function
def smooth(src,rows_dst,cols_dst,method):
    #get src rows/cols/res
    rows_src=src.shape[0]
    cols_src=src.shape[1]
    cols_res_src=360.0/cols_src
    rows_res_src=180.0/rows_src
    
    #Calculate area of each gridcell m2
    radians=0.0174532925
    radius=6378137 #m
    lats_src=np.linspace((-rows_src/2)*rows_res_src-((rows_res_src)/2),(rows_src/2)*rows_res_src+((rows_res_src)/2),rows_src)
    lons_src=np.linspace(0,(cols_src)*cols_res_src-cols_res_src,cols_src)
    lons_grid_src,lats_grid_src = np.meshgrid(lons_src,lats_src)
    area_src=(np.sin(lats_grid_src*radians+0.5*rows_res_src*radians)-np.sin(lats_grid_src*radians-0.5*rows_res_src*radians))*(cols_res_src*radians)*radius*radius
    total_area_src=src*area_src
    
    #get dst res
    cols_res_dst=360.0/cols_dst
    rows_res_dst=180.0/rows_dst
    
    #geotransform
    geot_src=[(-cols_src/2)*cols_res_src,cols_res_src,0.0,(rows_src/2)*rows_res_src,0.0,-rows_res_src]
    geot_dst=[(-cols_dst/2)*cols_res_dst,cols_res_dst,0.0,(rows_dst/2)*rows_res_dst,0.0,-rows_res_dst]
    sr_wgs84 = osr.SpatialReference()
    sr_wgs84.ImportFromEPSG(4326) #4326 is the EPSG code for WGS84
        
    #set up src data
    source = gdal.GetDriverByName('MEM').Create('',cols_src,rows_src,1,gdalconst.GDT_Float32)
    band_source = source.GetRasterBand(1)
    #band_source.Fill(0) #no data value
    #band_source.SetNoDataValue(0)
    source.SetProjection(sr_wgs84.ExportToWkt()) #convert to well-known text format for setting projection on output raster
    source.SetGeoTransform(geot_src)
    band_source.WriteArray(src,0,0)
           
    #set up dst data
    dest=gdal.GetDriverByName('MEM').Create('',cols_dst,rows_dst,1,gdal.GDT_Float32)
    dest.SetProjection(sr_wgs84.ExportToWkt())
    dest.SetGeoTransform(geot_dst)
    
    #re-project
    gdal.ReprojectImage(source, dest,sr_wgs84.ExportToWkt(),sr_wgs84.ExportToWkt(), method)
    output=dest.GetRasterBand(1).ReadAsArray()
    
    #Calculate area of each gridcell m2
    lats_dst=np.linspace((-rows_dst/2)*rows_res_dst-((rows_res_dst)/2),(rows_dst/2)*rows_res_dst+((rows_res_dst)/2),rows_dst)
    lons_dst=np.linspace(0,(cols_dst)*cols_res_dst-cols_res_dst,cols_dst)
    lons_grid,lats_grid = np.meshgrid(lons_dst,lats_dst)
    area_output=(np.sin(lats_grid*radians+0.5*rows_res_dst*radians)-np.sin(lats_grid*radians-0.5*rows_res_dst*radians))*(cols_res_dst*radians)*radius*radius
    total_area_output=output*area_output
    print (total_area_src.sum())/1e+12, (total_area_output.sum())/1e+12 #convert to Tg

    return output

####Write to NetCDF function
def gTifToNcdf_v2(data,dtype,lats,lons,fpathOut,varName,varAttrs,zlib=False,compLevel=4):
    '''
    Converts a dataset to NetCDF
    :param data: data array
    :param dtype: data type
    :param lats: latitude array
    :param lons: longitude array
    :param varName: variable name for the data in the output NetCDF
    :param varAttrs: a dictionary of attributes(name/value pairs) to set on the NetCDF variable
    :param zlib: internally compress with gzip (True/False)
    :param compLevel: level of zlib compression (1-9; default = 4; ignored if zlib=False)
    ''' 
    if (data.ndim==2):
        #Determine fill value
        if "_FillValue" in varAttrs.keys():
        
            fillValue = varAttrs["_FillValue"]
            #Remove from attribute dict, as python netcdf sets this automatically
            varAttrs = varAttrs.copy()
            varAttrs.pop("_FillValue")
        
        #Create output NetCDF file
        dsOut = pycdf.Dataset(fpathOut,'w')
        
        #Create lat/lon dimensions and variables
        dsOut.createDimension('lat',lats.size)
        dsOut.createDimension('lon',lons.size)

        latitudes = dsOut.createVariable('lat','f8',('lat',))
        latitudes.long_name = "latitude"
        latitudes.units = "degrees_north"
        latitudes.standard_name = "latitude"
        latitudes[:] = lats

        longitudes = dsOut.createVariable('lon','f8',('lon',))
        longitudes.long_name = "longitude"
        longitudes.units = "degrees_east"
        longitudes.standard_name = "longitude"
        longitudes[:] = lons
    
        #Create main variable and write out data
        ncdfVar = dsOut.createVariable(varName,dtype,('lat','lon',),fill_value=fillValue,zlib=zlib,complevel=compLevel)
        ncdfVar[:] = data
        dsOut.sync()
        ncdfVar.setncatts(varAttrs)
        dsOut.close()
        
    else:
        #Determine fill value
        if "_FillValue" in varAttrs.keys():
        
            fillValue = varAttrs["_FillValue"]
            #Remove from attribute dict, as python netcdf sets this automatically
            varAttrs = varAttrs.copy()
            varAttrs.pop("_FillValue")
            
        #Create output NetCDF file
        dsOut = pycdf.Dataset(fpathOut,'w')
        
        #Create lat/lon dimensions and variables
        dsOut.createDimension('time',data.shape[0])
        dsOut.createDimension('lat',lats.size)
        dsOut.createDimension('lon',lons.size)

        latitudes = dsOut.createVariable('lat','f8',('lat',))
        latitudes.long_name = "latitude"
        latitudes.units = "degrees_north"
        latitudes.standard_name = "latitude"
        latitudes[:] = lats

        longitudes = dsOut.createVariable('lon','f8',('lon',))
        longitudes.long_name = "longitude"
        longitudes.units = "degrees_east"
        longitudes.standard_name = "longitude"
        longitudes[:] = lons
    
        #Create main variable and write out data
        ncdfVar = dsOut.createVariable(varName,dtype,('time','lat','lon',),fill_value=fillValue,zlib=zlib,complevel=compLevel)
        ncdfVar[:] = data
        dsOut.sync()
        ncdfVar.setncatts(varAttrs)
        dsOut.close()
        
######################MAIN################################
if __name__ == '__main__':
    data_dir='/Users/Bill/Data/NFIX/'
    ###global variables###
    cols_dst=360
    rows_dst=180
    res_dst=1
    lats_dst=np.linspace((rows_dst/2)*res_dst-((res_dst)/2),(-rows_dst/2)*res_dst+((res_dst)/2),rows_dst)
    lons_dst=np.linspace((-cols_dst/2)*res_dst+((res_dst)/2),(cols_dst/2)*res_dst-((res_dst)/2),cols_dst)
    lats_dst_ncdf=np.linspace((-rows_dst/2)*res_dst-((res_dst)/2),(rows_dst/2)*res_dst+((res_dst)/2),rows_dst)
    lons_dst_ncdf=np.linspace(0,(cols_dst)*res_dst-res_dst,cols_dst)
    
    ###read data###
    #n fixation symbiotic
    nfix_nat_5min=np.fromfile("".join([data_dir,'Original_Files/fix_nat_geo.flt']),dtype='<f64',count=-1)
    nfix_nat_5min[nfix_nat_5min<0]=0
    nfix_nat_5min=np.reshape(nfix_nat_5min,(1822,3768),order="C")
    print nfix_nat_5min.shape
    #n fixation asymbiotic
    anfix = np.loadtxt("".join([data_dir,'Original_Files/fixation_asym.asc']),skiprows=6)
    anfix[anfix<0]=0
    #n deposition
    f=pycdf.Dataset("".join([data_dir,'Original_Files/fndep_clm_rcp8.5_simyr1849-2106_1.9x2.5_c100428.nc']))
    ndep=f.variables['NDEP_year'][:]
    lats_src_nc=f.variables['lat'][:]
    lons_src_nc=f.variables['lon'][:]
    f.close()
    n_ndep=ndep.shape[0]
    print ndep.shape
    #p weathering
    data=input_raster("".join([data_dir,'Original_Files/pweather_geo_1dgr.tif']))
    pwth=data.readEntireRaster()
    pwth[pwth<0]=0
    print pwth.shape
    #p deposition
    f=pycdf.Dataset("".join([data_dir,'Original_Files/phos.nc']))
    p=f.variables['pdep-RCP8.5'][:]
    po4=f.variables['po4dep-RCP8.5'][:]
    pdep=(p+po4)*31536000*1000 #sec to years, kg to g
    lats_src_nc=f.variables['lat'][:]
    lons_src_nc=f.variables['lon'][:]
    f.close()
    n_pdep=pdep.shape[0]
    print pdep.shape
    
    ###Correct Fixation Data Extent (-90,90,-180,180)###
    nfix_nat2=np.zeros((1884,3768))
    anfix2=np.zeros((180,360))
    nfix_nat2[62:1884,:]=nfix_nat_5min
    anfix2[6:180,:]=anfix
    
    ###Resample the data###
    #n fixation symbiotic
    print "N Fix Sym totals:"
    nfix_nat_1dgr=smooth(nfix_nat2,180,360, gdal.GRA_Bilinear)
    #n fixation asymbiotic
    print "N Fix Asym totals:"
    anfix_1dgr=smooth(anfix2,180,360, gdal.GRA_Bilinear)
    #n deposition
    print "N Dep totals:"
    ndep_1dgr=np.zeros((n_ndep,rows_dst,cols_dst))
    for i in np.arange(n_ndep):
        ndep_1dgr[i,:,:]=smooth(ndep[i,:,:],rows_dst,cols_dst, gdal.GRA_Bilinear)
    #p weathering
    print "P Wther totals:"
    pwth_1dgr=smooth(pwth,180,360, gdal.GRA_Average)
    #p deposition
    print "P Dep totals:"
    pdep_1dgr=np.zeros((n_pdep,rows_dst,cols_dst))
    for i in np.arange(n_pdep):
        pdep_1dgr[i,:,:]=smooth(pdep[i,:,:],rows_dst,cols_dst, gdal.GRA_Bilinear)
    
    ###Write to Netcdf###
    #n fixation symbiotic
    gTifToNcdf_v2(nfix_nat_1dgr,np.float,lats_dst,lons_dst,"".join([data_dir,'Dgr_Grids/Nfixation_nat_sym_1dgr.nc']),'N_Fix_Sym',
                      {'long_name':"Annual N Fixation Symbiotic",'units':"gN m-2 y-1","_FillValue":False},zlib=False,compLevel=4)
    #n fixation asymbiotic
    gTifToNcdf_v2(anfix_1dgr,np.float,lats_dst,lons_dst,"".join([data_dir,'Dgr_Grids/Nfixation_nat_asym_1dgr.nc']),'N_Fix_Asym',
                      {'long_name':"Annual N Fixation Asymbiotic",'units':"gN m-2 y-1","_FillValue":False},zlib=False,compLevel=4)
    #n deposition
    gTifToNcdf_v2(ndep_1dgr,np.float,lats_dst_ncdf,lons_dst_ncdf,"".join([data_dir,'Dgr_Grids/ndep_1dgr.nc']),'N_Dep',
                  {'long_name':"Annual N Deposition",'units':"gN m-2 y-1","_FillValue":0},zlib=False,compLevel=4)
    #p weathering
    gTifToNcdf_v2(pwth_1dgr,np.float,lats_dst,lons_dst,"".join([data_dir,'Dgr_Grids/Pweathering_1dgr.nc']),'P_Weathering',
                      {'long_name':"Annual P Weathering",'units':"gP m-2 y-1","_FillValue":False},zlib=False,compLevel=4)
    #p deposition
    gTifToNcdf_v2(pdep_1dgr,np.float,lats_dst_ncdf,lons_dst_ncdf,"".join([data_dir,'Dgr_Grids/pdep_1dgr.nc']),'P_Dep',
                  {'long_name':"Annual P Deposition",'units':"gP m-2 y-1","_FillValue":0},zlib=False,compLevel=4)
    
    
    ###Test Plot###
    plt.imshow(nfix_nat2,cm.get_cmap('rainbow',20),interpolation='nearest')
    #plt.clim(0,12)
    plt.colorbar()
    plt.savefig('test.png', dpi=300)
    plt.show()
