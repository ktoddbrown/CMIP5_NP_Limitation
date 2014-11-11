##Programer: K Todd-Brown ktoddbrown@gmail.com
##Data: May 2014
##Purpose: Load the biome mask and gridded new NPP from nutrient inputs.
##         Use these to construct the new globally gridded nutrient inputs
##         at each year.
##
##Output: Dump out the biome map to 'biomeMap.png'
##        ===annual gridded arrays=======
##        NnewNPP - new N supported NPP
##        PnewNPP - new P supported NPP
##        NInRate - new N inputs
##        PInRate - new P inputs
##        ===gridded arrays==============
##        biomeMask - index for biome masks
##        landarea - gridded land area
##        ===time array==================
##        yr - year for *newNPP (numeric)
##        yrStr.InRate - string version of yr
##        NIn_tot PIn_tot - total annual N and P inputs
##        newNNPP_tot newPNPP_tot - total new NPP inputs
##
##Inputs: nutrientMaps/MOD12Q1_fill_1dgr.nc - biome index map
##        nutrientMaps/ann_Input_data.nc - N and P supported new NPP
##        lib/rasterToArray.R - convert the raster file to our array format
##


library('ncdf4')
source('lib/rasterToArray.R')

#nutrientDir <- 'nutrientMaps/'
nutrientDir <- '/Volumes/DATAFILES/anuNCDF/ncarTransfer'

###############################
##Load the biome mask
###############################
cat('loading biome mask...')
#biomeMask <- raster('nutrientMaps/MOD12Q1_fill_1dgr.nc') #12 is Agg, 13 urban
biomeMask <- raster(sprintf('%s/MOD12Q1_fill_1dgr.nc', nutrientDir))
extent(biomeMask) <- c(-180, 180, -90, 90)
res(biomeMask) <- c(1,1)
biomeMask <- rasterToArray(biomeMask)
cat('done\n')

################################
##Load the C:N:P ratios
################################
cat('constructing C:N:P for soils...')
CtoNsoil.arr <- c(
                  Inf, #0
                  16.1,#1
                  12.8,#2
                  24.8,#3
                  30,  #4
                  10.1,#5
                  19.3,#7
                  15.0,#8
                  15.0,#9
                  13.1,#10
                  13.2,#12
                  13.2,#13 repeat of cropland
                  26.8)#16
soilBiomeStr <- c('water', 'enf', 'ebf', 'dnf', 'dbf', 'mixed', 'shrub land', 'woody savannah', 'savanna', 'grassland', 'cropland', 'urban', 'barren')
soilBiomeIndex <- c(0,1,2,3,4,5,7,8,9,10,12,13,16) ##tied to biome mask values
names(CtoNsoil.arr) <- soilBiomeIndex

cat('construct C:N:P ratios for NPP...')
CtoNutrient.mtx <- matrix(
                          c(Inf,   Inf, #water 0
                            0.1021217E+03,    0.1328857E+04, #enf 1
                            0.4674353E+02,    0.1136019E+04, #ebf 2
                            0.1031860E+03,    0.1063311E+04, #dnf 3
                            0.5831366E+02,    0.8882278E+03, #dbf 4
                            0.9045517E+02,    0.1051518E+04, #mixed 5
                            0.5830027E+02,    0.7149689E+03, #closed shrub 6
                            0.5830027E+02,    0.7149689E+03, #open shrub 7
                            0.4603385E+02,    0.8965328E+03, #woody savana 8
                            0.4796446E+02,    0.8703600E+03, #savana 9
                            0.6093211E+02,    0.9928398E+03, #grassland 10
                            Inf,    Inf, #cropland 12
                            Inf,    Inf, #urban 13 repeat of cropland
                            0.6093211E+02,    0.9928398E+03) ,#16 barren
                          nrow=14, ncol=2, byrow=TRUE)
biomeStr <- c('water', 'enf', 'ebf', 'dnf', 'dbf', 'mixed', 'closed shrub',
              'open shrub', 'woody savana', 'savana', 'grassland',
              'cropland', 'urban', 'barren')
biomeIndex <- c(0,1,2,3,4,5,6,7,8,9,10,12,13,16)##tied to biome mask values
row.names(CtoNutrient.mtx) <- biomeIndex

#########################################
## Set up C:N:P for NPP and C:N for soils
#########################################
CtoN <- biomeMask
CtoP <- biomeMask
CtoNsoil <- biomeMask

##plot the biome maps and generate the C:N, C:P, and soils C:N maps
png('fig/biomeMap.png', height=4*300, width=4*400)
par(mfrow=c(4,4))
for(ii in unique(as.vector(biomeMask))){
    if(is.na(ii)) next;

    image(biomeMask==ii, main=biomeStr[biomeIndex==ii])
    CtoN[biomeMask==ii] <- CtoNutrient.mtx[toString(ii),1]
    CtoP[biomeMask==ii] <- CtoNutrient.mtx[toString(ii),2]
    CtoNsoil[biomeMask == ii] <- CtoNsoil.arr[toString(ii)]
}
#image(biomeMask > 0 & biomeMask < 13, main='biome 1:12')
dev.off()
cat('done\n')


cat('loading N|P new NPP, N|P input rates, land area, and associated year. (Ignore the errors generated here)...\n')
################################################
##Load nutrient supported new NPP for each year
################################################
#data.nc <- nc_open('nutrientMaps/ann_Input_data.nc')
data.nc <- nc_open(sprintf('%s/ann_Input_data.nc', nutrientDir))

NnewNPP <- ncvar_get(data.nc, varid='input_npp_N')
##set the NA values
NnewNPP[NnewNPP > 1e20] <- NA
##don't constrain aggriculture or urban
NnewNPP[array(biomeMask, dim=dim(NnewNPP)) == 12] <- Inf
NnewNPP[array(biomeMask, dim=dim(NnewNPP)) == 13] <- Inf

##Do the same with P
PnewNPP <- ncvar_get(data.nc, varid='input_npp_P')
PnewNPP[PnewNPP > 1e20] <- NA
PnewNPP[array(biomeMask, dim=dim(PnewNPP)) == 12] <- Inf
PnewNPP[array(biomeMask, dim=dim(PnewNPP)) == 13] <- Inf

##Back out the nutrient input rates from the C:N:P ratios
NInRate <- NnewNPP/array(CtoN, dim=dim(NnewNPP))
PInRate <- PnewNPP/array(CtoP, dim=dim(PnewNPP))

##Again be careful about the aggriculture and urban
NInRate[array(biomeMask, dim=dim(NInRate)) == 12] <- Inf
PInRate[array(biomeMask, dim=dim(PInRate)) == 12] <- Inf
NInRate[array(biomeMask, dim=dim(NInRate)) == 13] <- Inf
PInRate[array(biomeMask, dim=dim(PInRate)) == 13] <- Inf

##Calculate the land area cover to get the global totals
landarea <- ncvar_get(data.nc, varid='landarea')
##Pull the years for the nutrient inputs
yr <- ncvar_get(data.nc, varid='year')
yrStr.InRate <- as.character(yr)

nc_close(data.nc) ##close out the nc file
cat('done\n')

##Get the global totals
cat('calculating global totals...')
NIn_tot <- apply(NInRate, c(3), function(x){sum(as.vector(x*landarea)[is.finite(x)], na.rm=TRUE)})/1e12
PIn_tot <- apply(PInRate, c(3), function(x){sum(as.vector(x*landarea)[is.finite(x)], na.rm=TRUE)})/1e12
cat('done\n')

##Record the total annual addition supported by N and P
cat('finding annual N and P supported NPP...')
NnewNPP_tot <-  apply(NnewNPP, c(3), function(x){sum(as.vector(x*landarea)[is.finite(x)], na.rm=TRUE)})/1e12
PnewNPP_tot <-  apply(PnewNPP, c(3), function(x){sum(as.vector(x*landarea)[is.finite(x)], na.rm=TRUE)})/1e12
cat('making figures...')
#pdf('fig/FinalN.pdf', height=3, width=5)
png('fig/FinalN.png', height=300, width=500)
world.plot(1:360, -90:89, NInRate[,,251]*1e3, title='Total New N [g m^2]')
dev.off()
#pdf('fig/FinalP.pdf', height=3, width=5)
png('fig/FinalP.png', height=300, width=500)
world.plot(1:360, -90:89, PInRate[,,251]*1e3, title='Total New P [g m^2]')
dev.off()
##Record the total annual addition supported by N and P
cat('finding annual N and P supported NPP...')
NnewNPP_tot <-  apply(NnewNPP, c(3), function(x){sum(as.vector(x*landarea)[is.finite(x)], na.rm=TRUE)})/1e12
PnewNPP_tot <-  apply(PnewNPP, c(3), function(x){sum(as.vector(x*landarea)[is.finite(x)], na.rm=TRUE)})/1e12
cat('making figures...')
#pdf('fig/FinalN.pdf', height=3, width=5)
png('fig/FinalN.png', height=300, width=500)
world.plot(1:360, -90:89, NInRate[,,251]*1e3, title='Total New N [g m^2]')
dev.off()
#pdf('fig/FinalP.pdf', height=3, width=5)
png('fig/FinalP.png', height=300, width=500)
world.plot(1:360, -90:89, PInRate[,,251]*1e3, title='Total New P [g m^2]')
dev.off()

pdf('fig/cumulativeN.pdf', height=3, width=5)
plot(yr, NIn_tot, xlab='', ylab='Pg', main='Cumulative global new N', type='l')
dev.off()
pdf('fig/cumulativeP.pdf', height=3, width=5)
plot(yr, PIn_tot, xlab='', ylab='Pg', main='Cumulative global new P', type='l')
dev.off()
cat('done\n')
