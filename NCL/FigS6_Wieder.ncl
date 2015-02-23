; b_biome_results.ncl
; Created by Will Wieder
; Modified Feb 2015
; Draws Figs. S6 from: 
; Wieder et al. Future productivity and carbon storage are limited
;     by terrestrial nutrient availability. Nature Geosci. (2015).
; reads in summary file that includes cumulative N & P demand from 12 models
; looks at biome aggregated averages of changes in NPP from CMIP5 models w/ & w/out nutrient limitation
; ** Modified here to read in C projections w/ adjusted soil decomposition

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
 path     = (/"/project/tss/wwieder/cmip5/"/)
 pathin   = (/path + "CMIP5_01032015/"/)

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
 biomeName = new(nbiome, string)
 biomeName(0) = "ocean"
 biomeName(1) = "ENF"
 biomeName(2) = "EBF"
 biomeName(3) = "DNF"
 biomeName(4) = "BDF"
 biomeName(5) = "MXF"
 biomeName(6) = "CSH"
 biomeName(7) = "OSH"
 biomeName(8) = "WSV"
 biomeName(9) = "SVN"
 biomeName(10) = "GRS"
 biomeName(12) = "AGR"
 biome         = where(biome.eq.7, 6, biome)
 biomeName(6) = "SHB" ; combine open and closed shrublands
 
 printVarSummary(landarea)

 nmod     = 11
 model    = new(nmod, string) ; same models as in Todd-Brown BG 2013
 model(0) = "bcc-csm1-1-m" 
 model(1) = "BNU-ESM"
 model(2) = "CanESM2"
 model(3) = "CESM1-BGC"
 model(4) = "GFDL-ESM2G"
; model(5) = "GISS-E2-H"
 model(5) = "HadGEM2-ES"
 model(6) = "inmcm4"
 model(7) = "IPSL-CM5A-MR"  ;should be "MR"
 model(8) = "MIROC-ESM"
 model(9)= "MPI-ESM-MR"
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

 biome_NPP1860_glob = new((/ntype, nmod, nbiome/), double)
 biome_NPP2005_glob = new((/ntype, nmod, nbiome/), double)
 biome_NPP2100_glob = new((/ntype, nmod, nbiome/), double)

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

 biome_NPP1860_glob!0 ="type"
 biome_NPP1860_glob!1 ="model"
 biome_NPP1860_glob!2 ="biome"
 biome_NPP1860_glob&type = type
 biome_NPP1860_glob&model = model
 biome_NPP1860_glob&biome = biomeName
 copy_VarCoords(biome_NPP1860_glob, biome_NPP2100_glob)
 copy_VarCoords(biome_NPP1860_glob, biome_NPP2005_glob)

 do t = 0, ntype - 1
   do m = 0, nmod - 1

     NPP1860_glob(t,m)  = sum(NPP1860_grid(t,m,:,:)) / 1e12 ;Pg C
     NPP2005_glob(t,m)  = sum(NPP2005_grid(t,m,:,:)) / 1e12 ;Pg C
     NPP2100_glob(t,m)  = sum(NPP2100_grid(t,m,:,:)) / 1e12 ;Pg C

     TOTC1860_glob(t,m) = sum(TOTC1860_grid(t,m,:,:)) / 1e12 ;Pg C
     TOTC2005_glob(t,m) = sum(TOTC2005_grid(t,m,:,:)) / 1e12 ;Pg C
     TOTC2100_glob(t,m) = sum(TOTC2100_grid(t,m,:,:)) / 1e12 ;Pg C

     do b = 0, nbiome -1 

       temp1860_NPP    = where(biome.eq.b, NPP1860_grid(t,m,:,:), value)
       temp2005_NPP    = where(biome.eq.b, NPP2005_grid(t,m,:,:), value)
       temp2100_NPP    = where(biome.eq.b, NPP2100_grid(t,m,:,:), value)

       biome_NPP1860_glob(t,m,b) = sum(temp1860_NPP)  / 1e12 ;Pg C
       biome_NPP2005_glob(t,m,b) = sum(temp2005_NPP)  / 1e12 ;Pg C
       biome_NPP2100_glob(t,m,b) = sum(temp2100_NPP)  / 1e12 ;Pg C

     end do

   end do
 end do

;print(AG_NPP2100_glob(0,:))
;print(AG_NPP2100_glob(2,:))
print("1860 all tx, model 0, all biomes" )
print(biome_NPP1860_glob(:,0,:))
print(biome_NPP2100_glob(:,0,:))

; write out table of values
  ;---------biome totals for each model at each time--------------
  rows = nbiome
  cols = 9       ;3 treatments (c, n, np) at 3 times (1860, 2005, 2100) 
  do m = 0, nmod-1
    data1 = new((/rows,cols/),"double")
    data1(:,0)  = ((/biome_NPP1860_glob(0,m,:)/))
    data1(:,1)  = ((/biome_NPP1860_glob(1,m,:)/))
    data1(:,2)  = ((/biome_NPP1860_glob(2,m,:)/))
    data1(:,3)  = ((/biome_NPP2005_glob(0,m,:)/))
    data1(:,4)  = ((/biome_NPP2005_glob(1,m,:)/))
    data1(:,5)  = ((/biome_NPP2005_glob(2,m,:)/))
    data1(:,6)  = ((/biome_NPP2100_glob(0,m,:)/))
    data1(:,7)  = ((/biome_NPP2100_glob(1,m,:)/))
    data1(:,8)  = ((/biome_NPP2100_glob(2,m,:)/))

    fname = pathin + "biome_table/biome_table_" + model(m) 
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
  end do


  d_biome2005 = biome_NPP2005_glob - biome_NPP1860_glob
  d_biome2100 = biome_NPP2100_glob - biome_NPP1860_glob
  copy_VarCoords( biome_NPP2100_glob, d_biome2005)
  copy_VarCoords( biome_NPP2100_glob, d_biome2100)

  printVarSummary(d_biome2100)
  print("---------% Ag change of total NPP-----------------")
  Ag_C  = 100* d_biome2100(0,:,12) / dim_sum_n(d_biome2100(0,:,:), 1)
  Ag_N  = 100* d_biome2100(1,:,12) / dim_sum_n(d_biome2100(1,:,:), 1)
  Ag_NP = 100* d_biome2100(2,:,12) / dim_sum_n(d_biome2100(2,:,:), 1)

  printMinMax(Ag_C, True)
  print("mean CMIP5  = " + avg(Ag_C))
  printMinMax(Ag_N, True)
  print("mean Nlimit = " + avg(Ag_N))
  printMinMax(Ag_NP, True)
  print("mean NPlimit= " + avg(Ag_NP))

  
;****************************************************
; start making graphs
;****************************************************

;-----------------------------------------------------
; postage stamp figures for all models
;-----------------------------------------------------

 ;set up files to graph
 wks  = gsn_open_wks("ps", pathin + "NatGeo_Revised_Figs/biome_scatter")

 plot = new(nmod+1,graphic)

 res                        = True
 res@gsnDraw                = False
 res@gsnFrame               = False
 res@gsnMaximize            = True                     ; maximize plot
 res@gsnPaperOrientation    = "portrait"


 do m = 0, nmod-1

  xval = biome_NPP1860_glob(0,m,:)
  yval = d_biome2100(0:2,m,:)
  res@trYMinF           = -2.
  res@trYMaxF           = 15.
  res@trXMinF           = -1.
  res@trXMaxF           = 18.
  res@gsnYRefLine       = 0.0                                 ; adds horizontal line
  res@gsnYRefLineDashPattern = 1                              ; dashed
  res@xyMarkLineModes   = (/"Markers", "Markers", "Markers"/) ; choose to use markers
  res@xyMarkers         = (/ 1, 1, 1 /)                       ; choose type of marker  
  res@xyMarkerColors    = (/"black","red","blue"/)            ; Marker color
  res@xyMarkerSizeF     = 0.1                                 ; Marker size (default 0.01)


;  plot=gsn_csm_xy(wks,biome_NPP2100_glob(0,m,:),biome_NPP2100_glob(0:2,m,:),res)
  plot(m)=gsn_csm_xy(wks,xval,d_biome2100(0:2,m,:),res)


; Attach some text strings next to the markers. 
  labels             = biomeName
  txres               = True
  txres@txFontHeightF = 0.03
  txres@txJust        = "TopCenter"
  txres@txFontColor   = "black"
  txid = gsn_add_text(wks,plot(m),labels,xval,d_biome2100(0,m,:)+1.2,txres)


 end do


  ;-------Make global thumbnail plots----------
  pres                  = True
  pres@gsnMaximize      = True
  pres@gsnFrame         = False


; Panel the first set of plots without drawing them, so we
; can retrieve the bounding boxes and calculate the height.

  pres@gsnDraw = False

; "gsn_panel_return" is an unadvertised function. It behaves
; the same as "gsn_panel", except it returns all the objects 
; being paneled, including the labelbar if there is one.

  pplots = gsn_panel_return(wks,plot(0:2),(/1,4/),pres)


; Retrieve bounding boxes of all objects created by gsn_panel call.

; max(bb(:,0)) is the topmost location of all combined objects,
; and min(bb(:,1)) is the bottommost.
;
  bb     = NhlGetBB(pplots)
  height = max(bb(:,0)) - min(bb(:,1))

;---Using height value, now we can panel all sets of plots.
  pres@gsnDraw                          = True
  pres@gsnPanelTop                      = 1.0   ; leave room for title
  pres@gsnPanelBottom                   = pres@gsnPanelTop - height

;---figure string resources
  pres@gsnPanelFigureStringsPerimOn     = False
  pres@gsnPanelFigureStringsFontHeightF = 0.01
  pres@amJust                           = "TopLeft"
;  pres@gsnPanelLabelBar = False

  fig_strs = (/"a","b","c","d","e","f","g","h","i","j","k","l"/) + ")"
  m2 = new(12, string)
  do m = 0,10
    m2(m) = model(m)
  end do

;--------------------------------------------
;--------------------------------------------
;---Main title, only on first set of plots
  print("--------drawing plots-----")
;  pres@txString = "NPP Change"

  do n=0,nmod-1,3
    pres@gsnPanelFigureStrings = fig_strs(n:n+2) + " " + m2(n:n+2)
    gsn_panel(wks,plot(n:n+2),(/1,4/),pres)

;---Set for the next panel call.
    pres@txString = ""
    pres@gsnPanelTop    = pres@gsnPanelBottom
    pres@gsnPanelBottom = pres@gsnPanelTop-height
    print("finnished change NPP plot " + fig_strs(n))

 end do
 frame(wks)

 print("---finished "+ pathin +"biome_scatter.ps--")

end

 
