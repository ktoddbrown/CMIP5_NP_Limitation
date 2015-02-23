; a_cmip5_maps.ncl
; Created by Will Wieder
; Modified Feb 2015
; Draws Figs. 2, S2-S5 & S8-S10 from: i
; Wieder et al. Future productivity and carbon storage are limited 
;     by terrestrial nutrient availability. Nature Geosci. (2015).
; reads in summary file that includes cumulative N & P demand from 11 models
; makes postage stamp maps of cumulative N & P demand
; firts MASKS out ag lands from native model grid 
; ** Modified here to read in C projections w/ adjusted soil decomposition
; /project/tss/wweider/cmip5/CMIP5_01032015/*
  
load "$NCARG_LIB/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_LIB/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_LIB/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_LIB/ncarg/nclscripts/csm/shea_util.ncl"

begin
 print ("=========================================")
 print ("Start Time: "+systemfunc("date") )
 print ("=========================================")

 ;-------------------------------------------------------------
 ;-----------------read in .nc files---------------------------
 ;-------------------------------------------------------------
 path     = (/"/set_your_working_directory_here/"/)                  ;directory where files are found
 pathin   = (/path + "directory_w/_data/"/)                          ;subdirectory w/ CMIP5 model results
 landpath = (/path + "landarea/Mean_Landarea_1dgr.nc"/)
 NPdata   = (/path + "NPinputs/model_cum_NPbalance.nc"/)
 l        = addfile(NPdata, "r")
 lat      = l->lat
 lon      = l->lon
 landarea = l->landarea
 biome    = l->biome  
 nlat     = dimsizes(lat)
 nlon     = dimsizes(lon)
 nbiome   = 13
 
 printVarSummary(landarea)

 nmod     = 11
 model    = new(nmod, string) ; same models as in Todd-Brown BG 2013
 model    = new(nmod, string) ; same models as in Todd-Brown BG 2013
 model(0) = "bcc-csm1-1-m" 
 model(1) = "BNU-ESM"
 model(2) = "CanESM2"
 model(3) = "CESM1-BGC"
 model(4) = "GFDL-ESM2G"
;model(5) = "GISS-E2-H"                                      ;omitted from this analysis
 model(5) = "HadGEM2-ES"
 model(6) = "inmcm4"
 model(7) = "IPSL-CM5A-MR"  ;should be "MR"
 model(8) = "MIROC-ESM"
 model(9) = "MPI-ESM-MR"
 model(10)= "NorESM1-M"

 ntime    = 3
 time     = new(ntime, string) 
 time(0)  = "start"
 time(1)  = "modern"
 time(2)  = "end"

 ;--------------------------------------------------
 ;--------------------------------------------------
 ; -- Read out NPP, Veg C, Soil C for:
 ;         each treatment (control, N-limit, NP-limit)
 ;         each cmip5 model (11 total)
 ;         at each time (1860, 2005, 2100)
 ;--------------------------------------------------
 ;--------------------------------------------------

 ntype    = 3
 type     = new(ntime, string) 
 type(0)  = "C"
 type(1)  = "N"
 type(2)  = "NP"

 nlat     = 180
 nlon     = 360

 NPP1860 = new((/ntype, nmod, nlat, nlon/), float)
 NPP2005 = new((/ntype, nmod, nlat, nlon/), float)
 NPP2100 = new((/ntype, nmod, nlat, nlon/), float)

 VEGC1860 = new((/ntype, nmod, nlat, nlon/), float)
 VEGC2005 = new((/ntype, nmod, nlat, nlon/), float)
 VEGC2100 = new((/ntype, nmod, nlat, nlon/), float)

 SOILC1860 = new((/ntype, nmod, nlat, nlon/), float)
 SOILC2005 = new((/ntype, nmod, nlat, nlon/), float)
 SOILC2100 = new((/ntype, nmod, nlat, nlon/), float)

 ;-------------initial (1860)------------------------------
 do t = 0,2

   fils = systemfunc ("ls " +pathin+type(t)+ "/*NPP_start.nc")
   f    = addfiles(fils, "r")
   ListSetType (f, "join")

   vils = systemfunc ("ls " +pathin+type(t)+ "/*cVeg_start.nc")
   v    = addfiles(vils, "r")
   ListSetType (v, "join")

   sils = systemfunc ("ls " +pathin+type(t)+ "/*cSoil_start.nc")
   s    = addfiles(sils, "r")
   ListSetType (s, "join")

   if (t.eq.0) 
     NPP1860(t,:,:,:)   = f[:]->$"ESM NPP"$
     VEGC1860(t,:,:,:)  = v[:]->$"ESM cVeg+"$
     SOILC1860(t,:,:,:) = s[:]->$"ESM cSoil+"$
   else
     NPP1860(t,:,:,:)   = f[:]->$"Limited NPP"$
     VEGC1860(t,:,:,:)  = v[:]->$"Limited cVeg+"$
     SOILC1860(t,:,:,:) = s[:]->$"Limited cSoil+"$
   end if

   delete([/f, v, s/])
   delete([/fils, vils, sils/])

 end do

 printVarSummary(NPP1860)

 ;-------------modern (2005)------------------------------
 do t = 0,2

   fils = systemfunc ("ls " +pathin+type(t)+ "/*NPP_modern.nc")
   f    = addfiles(fils, "r")
   ListSetType (f, "join")

   vils = systemfunc ("ls " +pathin+type(t)+ "/*cVeg_modern.nc")
   v    = addfiles(vils, "r")
   ListSetType (v, "join")

   sils = systemfunc ("ls " +pathin+type(t)+ "/*cSoil_modern.nc")
   s    = addfiles(sils, "r")
   ListSetType (s, "join")

   if (t.eq.0) 
     NPP2005(t,:,:,:)   = f[:]->$"ESM NPP"$
     VEGC2005(t,:,:,:)  = v[:]->$"ESM cVeg+"$
     SOILC2005(t,:,:,:) = s[:]->$"ESM cSoil+"$
   else
     NPP2005(t,:,:,:)   = f[:]->$"Limited NPP"$
     VEGC2005(t,:,:,:)  = v[:]->$"Limited cVeg+"$
     SOILC2005(t,:,:,:) = s[:]->$"Limited cSoil+"$
   end if

   delete([/f, v, s/])
   delete([/fils, vils, sils/])

 end do
 printVarSummary(NPP2005)

 ;-------------rcp 8.5 (2100)------------------------------
 do t = 0,2

   fils = systemfunc ("ls " +pathin+type(t)+ "/*NPP_end.nc")
   f    = addfiles(fils, "r")
   ListSetType (f, "join")

   vils = systemfunc ("ls " +pathin+type(t)+ "/*cVeg_end.nc")
   v    = addfiles(vils, "r")
   ListSetType (v, "join")

   sils = systemfunc ("ls " +pathin+type(t)+ "/*cSoil_end.nc")
   s    = addfiles(sils, "r")
   ListSetType (s, "join")

   if (t.eq.0) 
     NPP2100(t,:,:,:)   = f[:]->$"ESM NPP"$
     VEGC2100(t,:,:,:)  = v[:]->$"ESM cVeg+"$
     SOILC2100(t,:,:,:) = s[:]->$"ESM cSoil+"$
   else
     NPP2100(t,:,:,:)   = f[:]->$"Limited NPP"$
     VEGC2100(t,:,:,:)  = v[:]->$"Limited cVeg+"$
     SOILC2100(t,:,:,:) = s[:]->$"Limited cSoil+"$
   end if

   delete([/f, v, s/])
   delete([/fils, vils, sils/])

 end do
 printVarSummary(NPP2100)

 ;-----------replace NaN w/ FillValue--------------
 value = 1.e36

 if (any(isnan_ieee(NPP1860))) then
   replace_ieeenan (NPP1860, value, 0)
   replace_ieeenan (VEGC1860, value, 0)
   replace_ieeenan (SOILC1860, value, 0)
   NPP1860@_FillValue = value
   VEGC1860@_FillValue = value
   SOILC1860@_FillValue = value
   replace_ieeenan (NPP2005, value, 0)
   replace_ieeenan (VEGC2005, value, 0)
   replace_ieeenan (SOILC2005, value, 0)
   NPP2005@_FillValue = value
   VEGC2005@_FillValue = value
   SOILC2005@_FillValue = value
   replace_ieeenan (NPP2100, value, 0)
   replace_ieeenan (VEGC2100, value, 0)
   replace_ieeenan (SOILC2100, value, 0)
   NPP2100@_FillValue = value
   VEGC2100@_FillValue = value
   SOILC2100@_FillValue = value

 end if

 printVarSummary(SOILC2100)

 ;fill in missing values as in inm4
 missingmask  = where(ismissing(NPP1860(0,7,:,:)), 0, 1)
 missingmaskC = conform_dims(dimsizes(NPP1860), missingmask, (/2,3/))
 
 NPP1860      = where(missingmaskC .eq. 0, value, NPP1860)
 NPP2005      = where(missingmaskC .eq. 0, value, NPP2005)
 NPP2100      = where(missingmaskC .eq. 0, value, NPP2100)

 TOTC1860     = VEGC1860 + SOILC1860  ;land C storage
 TOTC2005     = VEGC2005 + SOILC2005
 TOTC2100     = VEGC2100 + SOILC2100

 TOTC1860     = where(missingmaskC .eq. 0, value, TOTC1860)
 TOTC2005     = where(missingmaskC .eq. 0, value, TOTC2005)
 TOTC2100     = where(missingmaskC .eq. 0, value, TOTC2100)

 ;--------------------------------------------------
 ; Calculate gridcell C fluxes / pools 
 ;--------------------------------------------------

 landareaC = conform_dims(dimsizes(NPP1860), landarea, (/2,3/))
 biomeC    = conform_dims(dimsizes(NPP1860), biome,    (/2,3/))

 NPP1860_grid  = NPP1860 * landareaC   ;gridcell C flux 
 NPP2005_grid  = NPP2005 * landareaC  
 NPP2100_grid  = NPP2100 * landareaC  
 TOTC1860_grid = TOTC1860 * landareaC   ;gridcell C pools 
 TOTC2005_grid = TOTC2005 * landareaC  
 TOTC2100_grid = TOTC2100 * landareaC  

 NPP1860_glob    = new((/ntype, nmod/), double) ;vectors to store global output
 NPP2005_glob    = new((/ntype, nmod/), double)
 NPP2100_glob    = new((/ntype, nmod/), double)

 TOTC1860_glob   = new((/ntype, nmod/), double)
 TOTC2005_glob   = new((/ntype, nmod/), double)
 TOTC2100_glob   = new((/ntype, nmod/), double)

 NPP1860_glob!0     = "type"
 NPP1860_glob!1     = "model"
 NPP1860_glob&type  = type
 NPP1860_glob&model = model
 NPP1860_glob@units = "Pg C"
 copy_VarMeta (NPP1860_glob, NPP2005_glob)
 copy_VarMeta (NPP1860_glob, NPP2100_glob)
 copy_VarMeta (NPP1860_glob, TOTC1860_glob)
 copy_VarMeta (NPP1860_glob, TOTC2005_glob)
 copy_VarMeta (NPP1860_glob, TOTC2100_glob)

 do t = 0, ntype - 1
   do m = 0, nmod - 1

     NPP1860_glob(t,m)  = sum(NPP1860_grid(t,m,:,:)) / 1e12 ;Pg C
     NPP2005_glob(t,m)  = sum(NPP2005_grid(t,m,:,:)) / 1e12 ;Pg C
     NPP2100_glob(t,m)  = sum(NPP2100_grid(t,m,:,:)) / 1e12 ;Pg C

     TOTC1860_glob(t,m) = sum(TOTC1860_grid(t,m,:,:)) / 1e12 ;Pg C
     TOTC2005_glob(t,m) = sum(TOTC2005_grid(t,m,:,:)) / 1e12 ;Pg C
     TOTC2100_glob(t,m) = sum(TOTC2100_grid(t,m,:,:)) / 1e12 ;Pg C

   end do
 end do


; write out preliminary table of values
; Table 1 & 2 calcuated with full time series of global pools & fluxes in 
; Table1_2_Wieder_NatGeo2015.R

  ;---------global NPP at each time, for each model-------------
  rows = nmod
  cols = 7 
  data1 = new((/rows,cols/),"double")
  data1(:,0)  = ((/NPP1860_glob(0,:)/))
  data1(:,1)  = ((/NPP2005_glob(0,:)/))
  data1(:,2)  = ((/NPP2005_glob(1,:)/))
  data1(:,3)  = ((/NPP2005_glob(2,:)/))
  data1(:,4)  = ((/NPP2100_glob(0,:)/))
  data1(:,5)  = ((/NPP2100_glob(1,:)/))
  data1(:,6)  = ((/NPP2100_glob(2,:)/))

  fname   = pathin + "NatGeo_Revised_Figs/global_NPP_table"
  ascfile = fname + ".asc"
  system("/bin/rm -f "+ ascfile)
  opt = True
  opt@fout = ascfile
  eWidth = 17
  eDec   = 7
  emtx   = cols + "e" + eWidth + "." + eDec
  write_matrix(data1,emtx,opt)
  print("-- " + ascfile + " written--")
  delete([/data1, fname, ascfile/])

  ;---------model total ecosystem C at each time-------------
  rows = nmod
  cols = 7 
  data1 = new((/rows,cols/),"double")
  data1(:,0)  = ((/TOTC1860_glob(0,:)/))
  data1(:,1)  = ((/TOTC2005_glob(0,:)/))
  data1(:,2)  = ((/TOTC2005_glob(1,:)/))
  data1(:,3)  = ((/TOTC2005_glob(2,:)/))
  data1(:,4)  = ((/TOTC2100_glob(0,:)/))
  data1(:,5)  = ((/TOTC2100_glob(1,:)/))
  data1(:,6)  = ((/TOTC2100_glob(2,:)/))

  fname = pathin + "NatGeo_Revised_Figs/global_TOTC_table"
  ascfile = fname + ".asc"
  system("/bin/rm -f "+ ascfile)
  opt = True
  opt@fout = ascfile
  eWidth = 17
  eDec   = 7
  emtx   = cols + "e" + eWidth + "." + eDec
  write_matrix(data1,emtx,opt)
  print("-- " + ascfile + " written--")
  delete([/data1, fname, ascfile, rows, cols/])
 
 ;--------------------------------------------------
 ; Calculate change in fluxes / pools (relative to 1860)
 ;--------------------------------------------------

 ; check to make sure initial values are identical
 dNPP_N1860  = NPP1860(1,:,:,:)  - NPP1860(0,:,:,:)
 dNPP_NP1860 = NPP1860(2,:,:,:)  - NPP1860(0,:,:,:)

 print("tot_N_diff  = " + sum(abs(dNPP_N1860)))
 print("tot_NP_diff = " + sum(abs(dNPP_NP1860)))


 dNPP2005   = NPP2005   - NPP1860
 dVEGC2005  = VEGC2005  - VEGC1860
 dSOILC2005 = SOILC2005 - SOILC1860
 dTOTC2005  = TOTC2005 - TOTC1860

 dNPP2100   = NPP2100   - NPP1860
 dVEGC2100  = VEGC2100  - VEGC1860
 dSOILC2100 = SOILC2100 - SOILC1860
 dTOTC2100  = TOTC2100 - TOTC1860


 dNPP2005!0 = "type"
 dNPP2005!1 = "model"
 dNPP2005!2 = "lat"
 dNPP2005!3 = "lon"
 dNPP2005&type  = type
 dNPP2005&model = model
 dNPP2005&lat   = lat
 dNPP2005&lon   = lon

 copy_VarCoords (dNPP2005, dVEGC2005)
 copy_VarCoords (dNPP2005, dSOILC2005)
 copy_VarCoords (dNPP2005, dTOTC2005)
 copy_VarCoords (dNPP2005, dNPP2100)
 copy_VarCoords (dNPP2005, dVEGC2100)
 copy_VarCoords (dNPP2005, dSOILC2100)
 copy_VarCoords (dNPP2005, dTOTC2100)

 copy_VarCoords( dNPP2005, NPP1860)
 copy_VarCoords( dNPP2005, TOTC1860)

 printVarSummary(dNPP2005)

;-----------------------------------------------------
; mean changes in NPP TOT_C
;-----------------------------------------------------
 mean_NPP_1860     = dim_avg_n_Wrap(NPP1860(0,:,:,:),  0)
 mean_dNPP_2100_C  = dim_avg_n_Wrap(dNPP2100(0,:,:,:), 0)
 mean_dNPP_2100_N  = dim_avg_n_Wrap(dNPP2100(1,:,:,:), 0)
 mean_dNPP_2100_NP = dim_avg_n_Wrap(dNPP2100(2,:,:,:), 0)

 mean_TOTC_1860     = dim_avg_n_Wrap(TOTC1860(0,:,:,:),  0)
 mean_dTOTC_2100_C  = dim_avg_n_Wrap(dTOTC2100(0,:,:,:), 0)
 mean_dTOTC_2100_N  = dim_avg_n_Wrap(dTOTC2100(1,:,:,:), 0)
 mean_dTOTC_2100_NP = dim_avg_n_Wrap(dTOTC2100(2,:,:,:), 0)

 mean_NPP_1860@long_name = "Mean CMIP5 NPP (1860)"
 mean_dNPP_2100_C@long_name = "Mean Change CMIP5 NPP (2100)"
 mean_dNPP_2100_N@long_name = "Mean Change CMIP5 NPP-N limited (2100)"
 mean_dNPP_2100_NP@long_name = "Mean Change CMIP5 NPP-NP limited (2100)"

 mean_TOTC_1860@long_name = "Mean CMIP5 TOTC (1860)"
 mean_dTOTC_2100_C@long_name = "Mean Change CMIP5 TOTC (2100)"
 mean_dTOTC_2100_N@long_name = "Mean Change CMIP5 TOTC-N limited (2100)"
 mean_dTOTC_2100_NP@long_name = "Mean Change CMIP5 TOTC-NP limited (2100)"

 ;mask out low land fraction areas for figure
 mean_NPP_1860     = where(landarea.lt.2.e9, value, mean_NPP_1860 )
 mean_dNPP_2100_C  = where(landarea.lt.2.e9, value, mean_dNPP_2100_C )
 mean_dNPP_2100_N  = where(landarea.lt.2.e9, value, mean_dNPP_2100_N )
 mean_dNPP_2100_NP = where(landarea.lt.2.e9, value, mean_dNPP_2100_NP)

 mean_TOTC_1860     = where(landarea.lt.2.e9, value, mean_TOTC_1860 )
 mean_dTOTC_2100_C  = where(landarea.lt.2.e9, value, mean_dTOTC_2100_C )
 mean_dTOTC_2100_N  = where(landarea.lt.2.e9, value, mean_dTOTC_2100_N )
 mean_dTOTC_2100_NP = where(landarea.lt.2.e9, value, mean_dTOTC_2100_NP)

;****************************************************
; start making graphs
;****************************************************

 plot = new(4,graphic)
 plotout = pathin + "NatGeo_Revised_Figs/mean_CMIP5_results"
 wks   = gsn_open_wks("ps" , plotout+"_FigS5")
 wks2  = gsn_open_wks("ps" , plotout+"_Fig2")

 res                        = True
 res@gsnDraw                = False
 res@gsnFrame               = False
 res@cnSmoothingOn          = False
 res@mpProjection           = "Robinson"
 res@mpOutlineOn            = True
 res@lbOrientation          = "Vertical"
 res@mpPerimOn              = False
 res@mpGridAndLimbOn        = True         ;draws Limb line around plots, w/o lat-lon lines
 res@mpGridLatSpacingF      = 180
 res@mpGridLonSpacingF      = 180
 res@mpGridLineThicknessF   = 0.
 res@mpGridLineColor        = "transparent"
 res@mpGridMaskMode         = "MaskLand"

  res@cnFillOn               = True
  res@cnFillPalette          = "WhBlGrYeRe"
  res@cnLinesOn              = False         ; Turn lines off
  res@cnLineLabelsOn         = False         ; Turn labels off
  res@cnLevelSelectionMode   = "ManualLevels"
  res@cnMinLevelValF         = 0.
  res@cnMaxLevelValF         = 1.3
  res@cnLevelSpacingF        = 0.1

  res@vpXF            = 0.1                 ; make plot bigger
  res@vpYF            = 0.9         
  res@vpWidthF        = 0.8
  res@vpHeightF       = 0.8
  plot(0)    = gsn_csm_contour_map(wks,mean_NPP_1860,res)

  res@cnFillPalette      = "MPL_YlGn"
  res@cnMinLevelValF         = 0.
  res@cnMaxLevelValF         = 0.45
  res@cnLevelSpacingF        = 0.05

  plot(2)    = gsn_csm_contour_map(wks,mean_dNPP_2100_C, res)
  plot(1)    = gsn_csm_contour_map(wks,mean_dNPP_2100_N, res)
  plot(3)    = gsn_csm_contour_map(wks,mean_dNPP_2100_NP,res)
  resP                = True                ; panel only resources
  resP@gsnMaximize    = True                ; maximize plots
  gsn_panel(wks,plot,(/2,2/),resP)          ; now draw as one
  
  print("--------npp plots made (Fig. S5) -----")
; draw figure 2

  res@cnMinLevelValF         = 0.
  res@cnMaxLevelValF         = 33
  res@cnLevelSpacingF        = 3

  plot(0)    = gsn_csm_contour_map(wks2,mean_TOTC_1860,res)
 
  res@cnFillPalette      = "MPL_BrBG"
  res@cnMinLevelValF         = -4.
  res@cnMaxLevelValF         =  4.
  res@cnLevelSpacingF        =  0.5

  plot(2)    = gsn_csm_contour_map(wks2,mean_dTOTC_2100_C, res)
  plot(1)    = gsn_csm_contour_map(wks2,mean_dTOTC_2100_N, res)
  plot(3)    = gsn_csm_contour_map(wks2,mean_dTOTC_2100_NP,res)
  resP                = True                ; panel only resources
  resP@gsnMaximize    = True                ; maximize plots
  gsn_panel(wks2,plot,(/2,2/),resP)          ; now draw as one


 delete([/wks, wks2, res, plot/])
 print("---finished "+ pathin +"NatGeo_Revised_Figs/mean_CMIP5_results_Fig2.ps--")

;-----------------------------------------------------
; postage stamp figures for all models
;-----------------------------------------------------

 ;set up files to graph
 wks  = gsn_open_wks("ps", pathin + "NatGeo_Revised_Figs/npp_maps")

 res                        = True
 res@gsnDraw                = False
 res@gsnFrame               = False

 res@mpProjection           = "robinson"
 res@mpOutlineOn            = True
 res@mpPerimOn              = False
 res@mpGridAndLimbOn        = True         ;draws Limb line around plots, w/o lat-lon lines
 res@mpGridLatSpacingF      = 180
 res@mpGridLonSpacingF      = 180
 res@mpGridLineThicknessF   = 0.
 res@mpGridLineColor        = "transparent"
 res@mpGridMaskMode         = "MaskLand"

 res@lbLabelBarOn           = False
 res@cnLinesOn              = False
 res@cnFillOn               = True
 
 res@cnLevelSelectionMode   = "ManualLevels"
 res@cnFillPalette   = "BlueYellowRed"   ; change the color palette

 plot1860_NPP_C     = new(nmod,graphic)
 plot2100_difNPP_C  = new(nmod,graphic)
 plot2100_difNPP_N  = new(nmod,graphic)
 plot2100_difNPP_NP = new(nmod,graphic)

 plot1860_TOTC_C     = new(nmod,graphic)
 plot2100_difTOTC_C  = new(nmod,graphic)
 plot2100_difTOTC_N  = new(nmod,graphic)
 plot2100_difTOTC_NP = new(nmod,graphic)

 do i = 0, nmod-1

   ;----NPP maps----------
   ;----need to set common label bar for each plot type------
   res@cnFillPalette      = "WhBlGrYeRe"
   res@cnMinLevelValF         =  0.
   res@cnMaxLevelValF         = 1.3
   res@cnLevelSpacingF        = 0.1
   npp_1860                   = NPP1860(0,i,:,:)
   npp_1860@long_name         = model(i)
   npp_1860@units             = ""
   plot1860_NPP_C(i)    = gsn_csm_contour_map(wks,npp_1860,res)

   res@cnFillPalette      = "MPL_YlGn"
   res@cnMinLevelValF         = -0.2
   res@cnMaxLevelValF         = 0.8
   res@cnLevelSpacingF        = 0.1
   DIF_npp_2100C              = dNPP2100(0,i,:,:)
   DIF_npp_2100C@long_name    = model(i)
   plot2100_difNPP_C(i) = gsn_csm_contour_map(wks,DIF_npp_2100C,res)

   DIF_npp_2100N              = dNPP2100(1,i,:,:)
   DIF_npp_2100N@long_name    = model(i)
   plot2100_difNPP_N(i) = gsn_csm_contour_map(wks,DIF_npp_2100N,res)

   DIF_npp_2100NP              = dNPP2100(2,i,:,:)
   DIF_npp_2100NP@long_name    = model(i)
   plot2100_difNPP_NP(i) = gsn_csm_contour_map(wks,DIF_npp_2100NP,res)

   ;----TOTC maps----------
   ;----need to set common label bar for each plot type------
   res@cnFillPalette      = "WhBlGrYeRe"
   res@cnMinLevelValF         = 0.
   res@cnMaxLevelValF         = 36.
   res@cnLevelSpacingF        =  3.
   totc_1860                   = TOTC1860(0,i,:,:)
   totc_1860@long_name         = model(i)
   totc_1860@units             = ""
   plot1860_TOTC_C(i)    = gsn_csm_contour_map(wks,totc_1860,res)

   res@cnFillPalette      = "MPL_BrBG"
   res@cnMinLevelValF         = -6.
   res@cnMaxLevelValF         =  6.
   res@cnLevelSpacingF        =  1.0
   DIF_totc_2100C              = dTOTC2100(0,i,:,:)
   DIF_totc_2100C@long_name    = model(i)
   plot2100_difTOTC_C(i) = gsn_csm_contour_map(wks,DIF_totc_2100C,res)

   DIF_totc_2100N              = dTOTC2100(1,i,:,:)
   DIF_totc_2100N@long_name    = model(i)
   plot2100_difTOTC_N(i) = gsn_csm_contour_map(wks,DIF_totc_2100N,res)

   DIF_totc_2100NP              = dTOTC2100(2,i,:,:)
   DIF_totc_2100NP@long_name    = model(i)
   plot2100_difTOTC_NP(i) = gsn_csm_contour_map(wks,DIF_totc_2100NP,res)


   delete([/npp_1860, DIF_npp_2100C, DIF_npp_2100N, DIF_npp_2100NP/])
   delete([/totc_1860, DIF_totc_2100C, DIF_totc_2100N, DIF_totc_2100NP/])

 end do

  ;-------Make global thumbnail plots----------
  pres                  = True
  pres@gsnMaximize      = True
  pres@gsnFrame         = False
  pres@gsnPanelLabelBar = True
  pres@pmLabelBarWidthF = 0.8

;
; Panel the first set of plots without drawing them, so we
; can retrieve the bounding boxes and calculate the height.
;
  pres@gsnDraw = False
;
; "gsn_panel_return" is an unadvertised function. It behaves
; the same as "gsn_panel", except it returns all the objects 
; being paneled, including the labelbar if there is one.
;
  pplots = gsn_panel_return(wks,plot1860_NPP_C(0:2),(/1,3/),pres)

;
; Retrieve bounding boxes of all objects created by gsn_panel call.
;
; max(bb(:,0)) is the topmost location of all combined objects,
; and min(bb(:,1)) is the bottommost.
;
  bb     = NhlGetBB(pplots)
  height = max(bb(:,0)) - min(bb(:,1))

;---Using height value, now we can panel all sets of plots.
  pres@gsnDraw                          = True
  pres@gsnPanelTop                      = 0.9   ; leave room for title
  pres@gsnPanelBottom                   = pres@gsnPanelTop - height

;---figure string resources
  pres@gsnPanelFigureStringsPerimOn     = False
  pres@gsnPanelFigureStringsFontHeightF = 0.02
  pres@amJust                           = "TopLeft"
;  pres@gsnPanelLabelBar = False

  fig_strs = (/"a","b","c","d","e","f","g","h","i","j","k","l"/) + ")"

;--------------------------------------------
;     NPP 1860
;--------------------------------------------
;---Main title, only on first set of plots
  print("--------drawing plots-----")
  pres@txString = "1860 NPP"

  do n=0,nmod-1,3
    pres@gsnPanelFigureStrings = fig_strs(n:n+2)
    if (n .le.8)
      gsn_panel(wks,plot1860_NPP_C(n:n+2),(/1,3/),pres)
    else
      gsn_panel(wks,plot1860_NPP_C(n:n+1),(/1,3/),pres)
    end if

;---Set for the next panel call.
    pres@txString = ""
    pres@gsnPanelTop    = pres@gsnPanelBottom
    pres@gsnPanelBottom = pres@gsnPanelTop-height
    print("finnished 1860 NPP plot " + fig_strs(n))

 ;   if (n .ge. 8)
 ;     pres@gsnPanelLabelBar = True
 ;   end if  

 end do
 frame(wks)

;--------------------------------------------
;     NPP CHANGE C
;--------------------------------------------
;---Main title, only on first set of plots 
;---Using height value, now we can panel all sets of plots.
  pres@gsnDraw                          = True
  pres@gsnPanelTop                      = 0.9   ; leave room for title
  pres@gsnPanelBottom                   = pres@gsnPanelTop - height

 print("--------drawing plots-----")

  pres@txString = "Change NPP C (2100 - 1860)"


  do n=0,nmod-1,3
    pres@gsnPanelFigureStrings = fig_strs(n:n+2)
    if (n .le.8)
      gsn_panel(wks,plot2100_difNPP_C(n:n+2),(/1,3/),pres)
    else
      gsn_panel(wks,plot2100_difNPP_C(n:n+1),(/1,3/),pres)
    end if
;---Set for the next panel call.
    pres@txString = ""
    pres@gsnPanelTop    = pres@gsnPanelBottom
    pres@gsnPanelBottom = pres@gsnPanelTop-height
    print("finnished 2100 NPP plot C " + fig_strs(n))

 end do
 frame(wks)

;--------------------------------------------
;     NPP CHANGE N
;--------------------------------------------
;---Main title, only on first set of plots 
;---Using height value, now we can panel all sets of plots.
  pres@gsnDraw                          = True
  pres@gsnPanelTop                      = 0.9   ; leave room for title
  pres@gsnPanelBottom                   = pres@gsnPanelTop - height

 print("--------drawing plots-----")

  pres@txString = "Change NPP N (2100 - 1860)"


  do n=0,nmod-1,3
    pres@gsnPanelFigureStrings = fig_strs(n:n+2)
    if (n .le.8)
      gsn_panel(wks,plot2100_difNPP_N(n:n+2),(/1,3/),pres)
    else
      gsn_panel(wks,plot2100_difNPP_N(n:n+1),(/1,3/),pres)
    end if
;---Set for the next panel call.
    pres@txString = ""
    pres@gsnPanelTop    = pres@gsnPanelBottom
    pres@gsnPanelBottom = pres@gsnPanelTop-height
    print("finnished 2100 NPP plot N " + fig_strs(n))

 end do
 frame(wks)

;--------------------------------------------
;     NPP CHANGE NP
;--------------------------------------------
;---Main title, only on first set of plots 
;---Using height value, now we can panel all sets of plots.
  pres@gsnDraw                          = True
  pres@gsnPanelTop                      = 0.9   ; leave room for title
  pres@gsnPanelBottom                   = pres@gsnPanelTop - height

 print("--------drawing plots-----")

  pres@txString = "Change NPP NP (2100 - 1860)"


  do n=0,nmod-1,3
    pres@gsnPanelFigureStrings = fig_strs(n:n+2)
    if (n .le.8)
      gsn_panel(wks,plot2100_difNPP_NP(n:n+2),(/1,3/),pres)
    else
      gsn_panel(wks,plot2100_difNPP_NP(n:n+1),(/1,3/),pres)
    end if
;---Set for the next panel call.
    pres@txString = ""
    pres@gsnPanelTop    = pres@gsnPanelBottom
    pres@gsnPanelBottom = pres@gsnPanelTop-height
    print("finnished 2100 NPP plot NP " + fig_strs(n))

 end do
 frame(wks)


;*******************************************************************
;  Total land C (kg C / m2)
;*******************************************************************


;--------------------------------------------
;     TOTC 1860
;--------------------------------------------
;---Main title, only on first set of plots 
;---Using height value, now we can panel all sets of plots.
  pres@gsnDraw                          = True
  pres@gsnPanelTop                      = 0.9   ; leave room for title
  pres@gsnPanelBottom                   = pres@gsnPanelTop - height

  print("--------drawing plots-----")
  pres@txString = "1860 TOTC"

  do n=0,nmod-1,3
    pres@gsnPanelFigureStrings = fig_strs(n:n+2)
    if (n .le.8)
      gsn_panel(wks,plot1860_TOTC_C(n:n+2),(/1,3/),pres)
    else
      gsn_panel(wks,plot1860_TOTC_C(n:n+1),(/1,3/),pres)
    end if

;---Set for the next panel call.
    pres@txString = ""
    pres@gsnPanelTop    = pres@gsnPanelBottom
    pres@gsnPanelBottom = pres@gsnPanelTop-height
    print("finnished 1860 TOTC plot " + fig_strs(n))

 ;   if (n .ge. 8)
 ;     pres@gsnPanelLabelBar = True
 ;   end if  

 end do
 frame(wks)

;--------------------------------------------
;     TOTC CHANGE C
;--------------------------------------------
;---Main title, only on first set of plots 
;---Using height value, now we can panel all sets of plots.
  pres@gsnDraw                          = True
  pres@gsnPanelTop                      = 0.9   ; leave room for title
  pres@gsnPanelBottom                   = pres@gsnPanelTop - height

 print("--------drawing plots-----")

  pres@txString = "Change TOTC C (2100 - 1860)"


  do n=0,nmod-1,3
    pres@gsnPanelFigureStrings = fig_strs(n:n+2)
    if (n .le.8)
      gsn_panel(wks,plot2100_difTOTC_C(n:n+2),(/1,3/),pres)
    else
      gsn_panel(wks,plot2100_difTOTC_C(n:n+1),(/1,3/),pres)
    end if
;---Set for the next panel call.
    pres@txString = ""
    pres@gsnPanelTop    = pres@gsnPanelBottom
    pres@gsnPanelBottom = pres@gsnPanelTop-height
    print("finnished 2100 TOTC plot C " + fig_strs(n))

 end do
 frame(wks)

;--------------------------------------------
;     TOTC CHANGE N
;--------------------------------------------
;---Main title, only on first set of plots 
;---Using height value, now we can panel all sets of plots.
  pres@gsnDraw                          = True
  pres@gsnPanelTop                      = 0.9   ; leave room for title
  pres@gsnPanelBottom                   = pres@gsnPanelTop - height

 print("--------drawing plots-----")

  pres@txString = "Change TOTC N (2100 - 1860)"


  do n=0,nmod-1,3
    pres@gsnPanelFigureStrings = fig_strs(n:n+2)
    if (n .le.8)
      gsn_panel(wks,plot2100_difTOTC_N(n:n+2),(/1,3/),pres)
    else
      gsn_panel(wks,plot2100_difTOTC_N(n:n+1),(/1,3/),pres)
    end if
;---Set for the next panel call.
    pres@txString = ""
    pres@gsnPanelTop    = pres@gsnPanelBottom
    pres@gsnPanelBottom = pres@gsnPanelTop-height
    print("finnished 2100 TOTC plot N " + fig_strs(n))

 end do
 frame(wks)

;--------------------------------------------
;     TOTC CHANGE NP
;--------------------------------------------
;---Main title, only on first set of plots 
;---Using height value, now we can panel all sets of plots.
  pres@gsnDraw                          = True
  pres@gsnPanelTop                      = 0.9   ; leave room for title
  pres@gsnPanelBottom                   = pres@gsnPanelTop - height

 print("--------drawing plots-----")

  pres@txString = "Change TOTC NP (2100 - 1860)"


  do n=0,nmod-1,3
    pres@gsnPanelFigureStrings = fig_strs(n:n+2)
    if (n .le.8)
      gsn_panel(wks,plot2100_difTOTC_NP(n:n+2),(/1,3/),pres)
    else
      gsn_panel(wks,plot2100_difTOTC_NP(n:n+1),(/1,3/),pres)
    end if
;---Set for the next panel call.
    pres@txString = ""
    pres@gsnPanelTop    = pres@gsnPanelBottom
    pres@gsnPanelBottom = pres@gsnPanelTop-height
    print("finnished 2100 TOTC plot NP " + fig_strs(n))

 end do
 frame(wks)

 print("---finished "+ pathin +"NatGeo_Revised_Figs/npp_maps.ps--")




exit
;---Main title, only on first set of plots 
;---Using height value, now we can panel all sets of plots.
  pres@gsnDraw                          = True
  pres@gsnPanelTop                      = 0.9   ; leave room for title
  pres@gsnPanelBottom                   = pres@gsnPanelTop - height

 print("--------drawing plots-----")
  pres@txString = "Cumulative N demand (1861-2099)"


  do n=0,nmod-1,3
    pres@gsnPanelFigureStrings = fig_strs(n:n+2)
    gsn_panel(wks,plot2100_cumN(n:n+2),(/1,3/),pres)

;---Set for the next panel call.
    pres@txString = ""
    pres@gsnPanelTop    = pres@gsnPanelBottom
    pres@gsnPanelBottom = pres@gsnPanelTop-height
    print("finnished 2100 N plot " + fig_strs(n))

 end do
 frame(wks)

;---Main title, only on first set of plots 
;---Using height value, now we can panel all sets of plots.
  pres@gsnDraw                          = True
  pres@gsnPanelTop                      = 0.9   ; leave room for title
  pres@gsnPanelBottom                   = pres@gsnPanelTop - height

 print("--------drawing plots-----")
  pres@txString = "Cumulative P demand (1861-2099)"


  do n=0,nmod-1,3
    pres@gsnPanelFigureStrings = fig_strs(n:n+2)
    gsn_panel(wks,plot2100_cumP(n:n+2),(/1,3/),pres)

;---Set for the next panel call.
    pres@txString = ""
    pres@gsnPanelTop    = pres@gsnPanelBottom
    pres@gsnPanelBottom = pres@gsnPanelTop-height
    print("finnished 2100 P plot " + fig_strs(n))

 end do
 frame(wks)

 print("---finished "+ pathin +"npp_maps.ps--")
end

 
