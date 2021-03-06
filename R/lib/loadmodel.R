##Programer: K Todd-Brown ktoddbrown@gmail.com
##Date: May 2014
##Purpose: Load requested variables from the specified
##         model-variable-experiment raster files.
##Inputs: modelStr - must be specified in the workspace to specify the model



##set the librarys
library('raster')
library('ncdf')
library('abind')

source('lib/rasterToArray.R') #convert raster files to our array format

##modelStr is previously defined
cat('running loadmodel.R\n')

##Where is everything located
CMIPDir <- '/Volumes/DATAFILES/downloads'
anuRstDir <- '/Volumes/DATAFILES/anuNCDF'

CToK <- 273.15 ##Convert Celcius to Kalvin
yrToSec <- 3.15569e7 ##convert seconds to years using this constant
RstSaveExt <- 'grd' ##which raster format is everything saved to?

if(! 'commonStr' %in% ls()){
    commonStr <- '' #alternative is '_commmon' (yes, it's misspelled)
}else if(! commonStr %in% ''){
    commonStr <- '_commmon'
}


if(! 'varStrArr' %in% ls()){
    varStrArr <- c('npp', 'gpp', 'ra', 'rh',
                   'cSoil', 'cVeg', 'tsl10')
}

cat('loading: ', varStrArr, '\n')
cat('common string is: [', commonStr, ']\n')

multiFactor <- list(npp=yrToSec, gpp=yrToSec, ra=yrToSec, rh=yrToSec,
                         cSoil=1, cLitter=1, cCwd=0, cVeg=1, tsl10=1)
addFactor <-  list(npp=0, gpp=0, ra=0, rh=0,
                         cSoil=0, cLitter=0, cCwd=0, cVeg=0, tsl10=-1*CToK)
yrStr <- NULL
for(varStr in varStrArr){#, c('cLitter', 'cCwd'))){
    cat('loading',varStr,'for', modelStr, '...')
    eval(parse(text=sprintf('%s <- NULL', varStr)))
    varFilename <- sprintf('%s/%s_%s%s.%s',
                       anuRstDir, varStr, modelStr,  commonStr, RstSaveExt)
    if(file.exists(varFilename)){
        tempVar <- brick(varFilename)
        Rstlandarea <- raster::area(tempVar)
        yrStr.var <- names(tempVar)
        tempVar <- rasterToArray(tempVar)*multiFactor[[varStr]]+addFactor[[varStr]]
    }else if(varStr %in% 'npp'){
        ##Load gpp and ra to construct NPP
        cat('constructing from gpp...')
        gppFilename <- sprintf('%s/%s_%s%s.%s', anuRstDir, 'gpp', modelStr,  commonStr, RstSaveExt)
        gpp <- brick(gppFilename)
        Rstlandarea <- raster::area(gpp)
        yrStr.var <- names(gpp)
        gpp <- rasterToArray(gpp)
        if(all(gpp <=0, na.rm=TRUE)){
            gpp <- gpp*-1
        }
        cat('rh...')
        raFilename <- sprintf('%s/%s_%s%s.%s', anuRstDir, 'ra', modelStr,  commonStr, RstSaveExt)
        ra <- brick(raFilename)
        ra <- rasterToArray(ra)
        cat('differences is npp...')
        tempVar <- (gpp-ra)*multiFactor[[varStr]]+addFactor[[varStr]]

    }else{
        cat('no file found, moving on\n')
        next
    }

    cat('applying absolute tollerenances of 1e10 and 1e-4...')
    tempVar[abs(tempVar) > 1e10] <- NA
    tempVar[abs(tempVar) < 1e-4] <- 0
    cat('done\n')

    tempVar[!is.finite(tempVar)] <- NA

    eval(parse(text=sprintf('%s <- tempVar', varStr)))
    eval(parse(text=sprintf('yrStr.%s <- yrStr.var', varStr)))

    if(is.null(yrStr)){
        yrStr <- yrStr.var
    }else{
        yrStr <- intersect(yrStr, yrStr.var)
    }
}

if('cCwd' %in% ls() && !is.null(cCwd)){
    cVeg <- cVeg + cCwd
}

if('cLitter' %in% ls() & !is.null(cLitter)){
    cSoil <- cSoil + cLitter
}

##Take the intersecting years only
#yrStr <- intersect(intersect(intersect(intersect(yrStr.npp, yrStr.rh),
#                                       yrStr.cSoil), yrStr.cVeg), yrStr.tsl10)
cat('loading area...')
years <- as.numeric(substr(yrStr, 2,5))

areaFile <- sprintf('%s/areacella_fx_%s_historical_r0i0p0.nc', CMIPDir,  modelStr)
sftlfFile <- sprintf('%s/sftlf_fx_%s_historical_r0i0p0.nc', CMIPDir,  modelStr)
if(commonStr %in% '' &&
   file.exists(sftlfFile) && file.exists(areaFile)){
    cat('loading land area from CMIP output...')
    cellarea <- raster( sprintf('%s/areacella_fx_%s_historical_r0i0p0.nc', CMIPDir,  modelStr), varname='areacella')
    modelRes <- res(cellarea)

    sftlf <- rasterToArray(raster( sprintf('%s/sftlf_fx_%s_historical_r0i0p0.nc', CMIPDir,  modelStr), varname='sftlf'))
    if(max(as.vector(sftlf), na.rm=TRUE)==100){
        cat('correcting percentage to land fraction...')
        sftlf <- sftlf/100
    }
    landarea <- rasterToArray(cellarea)*sftlf
    if(is.null(dim(landarea)) ){
        cat('load failed for some reason... trying to ')
    }
}else{

    modelRes <- res(Rstlandarea)

    if(any(res(Rstlandarea) != dim(landarea)/c(360, 180))){
        cat('using raster::area for land area...')
        landarea <- rasterToArray(Rstlandarea)
        cat('converting from km to m...')
        landarea <- landarea * 1e6
    }else{
        cat('land area already calculated...')
    }
}

lon <- seq(0, 360, length=dim(landarea)[1] + 1)
lon <- (lon[2:length(lon)]+lon[(2:length(lon))-1])/2
lat <- seq(0, 360, length=dim(landarea)[2] + 1)
lat <- (lat[2:length(lat)]+lat[(2:length(lat))-1])/2

cat('done\n')

for(varStr in  varStrArr){
    if(varStr %in% ls() && eval(parse(text=sprintf('!is.null(%s)', varStr)))){
        eval(parse(text=sprintf('%s <- %s[,,yrStr.%s %%in%% yrStr]', varStr, varStr, varStr)))
        eval(parse(text=sprintf('yrStr.%s <- yrStr.%s[yrStr.%s %%in%% yrStr]',
                   varStr, varStr, varStr)))
    }
}

if('rh' %in% varStrArr && 'cSoil' %in% varStrArr){
    k <- rh/cSoil
    k[!is.finite(k)] <- NA
    yrStr.k <- yrStr.rh
}

cat('calculating dC...')
if('cSoil' %in% ls()){
    dCs <- abind(cSoil[,,2:(dim(cSoil)[3])] - cSoil[,,(2:(dim(cSoil)[3])) - 1], array(NA, dim(landarea)), along=3)
    yrStr.dCs <- yrStr.cSoil
}
if('cVeg' %in% ls()){
    dCv <-  abind(cVeg[,,2:(dim(cSoil)[3])] - cVeg[,,(2:(dim(cSoil)[3])) - 1], array(NA, dim(landarea)), along=3)
    yrStr.dCv <- yrStr.cVeg
}

cat('done\n')

