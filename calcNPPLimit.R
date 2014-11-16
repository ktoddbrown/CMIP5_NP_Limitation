##Programer: K Todd-Brown ktoddbrown@gmail.com
##Date:Update - November 2014 - added lower/upper bounds
##     Orginal - May 2014
##Purpose:Calculate added nutrient limintation on NPP and soil/vegetation
##        carbon stocks
##Dependencies:
##     =========scripts====
##      lib/loadBiomeConst.R - load biome constants and nutrient inputs
##      lib/loadmodel.R - load specific model
##      writeNPPLimitSummary.R - process output into netcdf files to
##               show 10yr mean maps and csv tables for global annual totals
##      lib/world.plot.R - make pretty maps
##      lib/rasterToArray.R - convert raster files to our array format
##     ==========directory structure==========
##     ./lib - library for supporting scripts listed above
##     ./data - save directory for internal NPP limitation files
##     ./fig - dump certain figure files to sanity check script
##     ./output - repository for final output written by writeNPPLimitSummary


##set the librarys
library('raster')
library('ncdf4')
library('abind')

source('lib/world.plot.R') #plot the maps nicely
source('lib/rasterToArray.R') #convert raster files to our array format

loadPrevious <- TRUE #Do we build off of previous runs?

##Where is everything located this will change
#CMIPDir <- '/Volumes/DATAFILES/downloads'
#anuRstDir <- '/Volumes/DATAFILES/anuNCDF'
CMIPDir <- '/Volumnes/DATAFILES/anuNCDF/ncarTransfer'
anuRstDir <- CMIPDir

for(boundStr in c('mean', 'lower', 'upper')){

  boundSD <- 0.2 #relative lower bound on new N, new P, C:N, and C:P estimates
  
  if(boundStr %in% 'lower'){
    cat('Processing lower bound\n')
    boundSD <- -1*boundSD
    halfStrArr <- c('', 'In', 'Ratio')
  }else if(boundStr %in% 'upper'){
    cat('Processing upper bound\n')
    boundSD <- boundSD
    halfStrArr <- c('', 'In', 'Ratio')
    } else {#otherwise don't do anything
    cat('Processing mean\n')
    boundSD <- 0
    halfStrArr <- c('')
  }
  
  for(halfStr in halfStrArr){
    
  source('lib/loadBiomeConst.R')
  
  if(halfStr %in% c('', 'In')){
  #To find the maximum raise the inputs 
    cat('reducing Inputs\n')
    NInRate <- NInRate*(1+boundSD)
    PInRate <- PInRate*(1+boundSD)
  }
  
  if(halfStr %in% c('', 'Ratio')){
  #...and the required C/N or C/P ratio
    cat('reducing Ratio\n')
    CtoN <- CtoN*(1+boundSD)
    CtoP <- CtoP*(1+boundSD)
    CtoNsoil <- CtoNsoil*(1+boundSD)
  }
  
  boundStr <- sprintf('%s%s', boundStr, halfStr)

  #are we constraining based on N, P or NP, anything else will be a test run where NPP is unconstrained
  for(limitStr in c('N', 'P', 'NP')){
    #Do we add N lost from the soil to the NPP N pool?
    if(boundStr %in% 'lower' | limitStr %in% 'P'){
      addSoilNarr <- c(FALSE)
    }else{
      addSoilNarr <- c(TRUE, FALSE)
    }
    for(addSoilN in addSoilNarr){
      
      ##Intialize the variables we will save
      orginalTot <- NULL ##orginal global totals
      adjustedTot <- NULL ##constrained global totals
      startMaps <- list() ##1860's maps of orginal and constrained results
      modernMaps <- list() ##1995-2004 maps
      endMaps <- list() ##2090's maps
      
      if(addSoilN){
        cat('Adding soil N back\n')
      }else{
        cat('No added N into the soil\n')
      }
      ##Pick the save file name intellegently
      saveFilename <- sprintf('data/%s%s%scalcNPPLimit.RData', boundStr, limitStr, c('_withCsn', '_noCsn')[c(addSoilN, !addSoilN)])
      
      
      ##Models we are running
      modelToConsider <- c( "bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CESM1-BGC", "GFDL-ESM2G", "HadGEM2-ES", "inmcm4", "IPSL-CM5A-MR", "MIROC-ESM", "MPI-ESM-MR", "NorESM1-M")#, "NorESM1-ME") #what models are we using?
      
      ##If we build off of pervious runs then load those here
      if(loadPrevious & file.exists(saveFilename)){
        load(saveFilename) #loads initalized saved variables
      }
      
      ##Save reloading time when we are debugging the code
      currentModelStr <- NA
      
      if('modelStr' %in% ls()){
        currentModelStr <- modelStr
      }
      
      cat('processing', limitStr, ' limited system\n')
      ##Only go through models that aren't already processed c("inmcm4")
      for(modelStr in setdiff(modelToConsider, unique(orginalTot$model))){
        
        cat('\n**********', modelStr, '*************\n')
        ## ####################################
        ## Load model variables
        ## ####################################
        
        if(is.null(currentModelStr) || !(currentModelStr %in% modelStr)) {
          ##Loadd the 1x1 degree merge of historcal and rcp85
          commonStr <- '_commmon'
          
          ##Variables of to load for a given model
          ##...rh is used to calculate the inputs
          varStrArr <- c('npp',  'rh', 'cSoil', 'cLitter', 'cVeg', 'cCwd')
          
          ##Load the variables for the model
          source('lib/loadmodel.R')
          ##yrStr comes out of loadmodel and is the intersection
          ##...of all loaded variables
          
          ##Make sure that all the variables are clean
          npp[!is.finite(rh)] <- NA
          rh[!is.finite(npp)] <- NA
          cSoil[!is.finite(npp)] <- NA
          cVeg[!is.finite(npp)] <- NA
          dCs[!is.finite(npp)] <- NA
          dCv[!is.finite(npp)] <- NA
          
          #back out the SOC inputs from change and losses
          socInputs <- dCs + rh
          vcInputs <- dCv + dCs + rh
          #back out veg inputs, difference between
          #...NPP and vcInputs will the
          #...losses due to fire or grazing
          #...or land use change or
          #...something similar
          
          
          ##loading a model's variables takes a while, keep track of
          ##...the curent model loaded so we don't have to do it again
          ##...during development
          currentModelStr <- modelStr
        }
        
        ##Pull the demand for the specific model
        if(FALSE){
          ##This takes a long time and was added for debugging
          cat('pull N and P demand and ratio of the model...')
          Ndemand <- npp/array(CtoN, dim=dim(npp))
          Ndemand[array(biomeMask, dim=dim(npp)) == 12] <- 0
          Pdemand <- npp/array(CtoP, dim=dim(npp))
          Pdemand[array(biomeMask, dim=dim(npp)) == 12] <- 0
          
          Ndemand_tot <- apply(Ndemand, c(3), function(x){sum(as.vector(x*landarea)[is.finite(x)], na.rm=TRUE)})/1e12
          Pdemand_tot <- apply(Pdemand, c(3), function(x){sum(as.vector(x*landarea)[is.finite(x)], na.rm=TRUE)})/1e12
          
          CtoN_tot <- apply(npp/Ndemand, c(3), function(x){weighted.mean(x=as.vector(x[is.finite(x) & x != 0]), w=as.vector(landarea[is.finite(x) & x != 0]), na.rm=TRUE)})
          CtoP_tot <- apply(npp/Pdemand, c(3), function(x){weighted.mean(x=as.vector(x[is.finite(x) & x != 0]), w=as.vector(landarea[is.finite(x) & x != 0]), na.rm=TRUE)})
          cat('done\n')
        }
        
        ## ##############################################
        ## Calculate the NP limited NPP and carbon stocks
        ## ##############################################
        cat('pull the starting NPP...')
        ##Assume that the models start out able to support all NPP in the
        ##...1860's and at a minumum 0 NPP
        startFlag <- grepl('X186', yrStr)
        startNPP <- apply(npp[,,startFlag], c(1,2), max)
        startNPP[startNPP < 0] <- 0
        cat('done\n')
        
        cat('calculate new soil and veg carbon...')
        ##Initialize all the arrays we'll use later
        newSoil <- array(NA, dim=dim(cSoil)) #store the new soil carbon
        newVeg <- array(NA, dim=dim(cVeg)) #new veg carbon
        modelN <- array(NA, dim=dim(cSoil)) #N pool for NPP
        modelP <- array(NA, dim=dim(cSoil)) #P pool for NPP
        nppAdj <- array(NA, dim=dim(npp)) #new NPP
        nppAdjRatio <- array(1, dim=dim(npp)) #adjustment ratio to scale
        #...NPP change into soil inputs
        #...and veg losses
        soilNloss <- array(NA, dim=dim(npp)) #N loss from the soil (negative)
        nppMax <-array(NA, dim=dim(npp[,,1])) #maximum NPP allowed by the N|P
        
        ##Assume that all inital NPP is supported by the N|P pools
        modelN0 <- startNPP/CtoN
        modelP0 <- startNPP/CtoP
        
        ##...but don't limit the ag biome
        modelP0[biomeMask == 12] <- Inf
        modelN0[biomeMask == 12] <- Inf
        
        cat('initalize pools...')
        newSoil[,,1] <- cSoil[,,1]# apply(cSoil[,,startFlag], c(1,2), mean)
        newVeg[,,1] <- cVeg[,,1]#apply(cVeg[,,startFlag], c(1,2), mean)
        nppAdj[,,1] <- npp[,,1]
        
        ##Start the N|P pools out with their inital support plus the new inputs
        modelN[,,1] <- modelN0 + NInRate[,,1]
        modelP[,,1] <- modelP0 + PInRate[,,1]
        
        ##Main for-loop that goes through and calculates the nutrient limited
        ##..fluxes and pools. This take s a while.
        cat('step through years...')
        for(ii in 2:(dim(npp)[3])){
          ##Tell the user where you are
          if(ii %% 10 == 0){
            cat(ii, ' ')
          }
          
          ##Calculate the N|P pools based on the old pool size and the
          ##... change in nutrient input rates
          modelP[,,ii] <- modelP[,,ii-1]+(PInRate[,,ii] - PInRate[,,ii-1])
          modelN[,,ii] <- modelN[,,ii-1]+(NInRate[,,ii] - NInRate[,,ii-1])
          
          ##Calcluae the maximum NPP supported by the N|P pools
          if(limitStr %in% 'N'){
            nppMax <- modelN[,,ii]*CtoN
          }else if(limitStr %in% 'P'){
            nppMax <- modelP[,,ii]*CtoP
          }else if(limitStr %in% 'NP'){
            if(TRUE){
              ##Call anywhere that N supports more NPP then P N-limited
              NLimit <- (modelN[,,ii]*CtoN <= modelP[,,ii]*CtoP)
              ##likewise for P
              PLimit <- (modelP[,,ii]*CtoP < modelN[,,ii]*CtoN)
              ##Check out the ratio between to resolve co-limited sites
              ##ratioCut <- modelN[,,ii]/modelP[,,ii] < CtoP/CtoN
              ratioCut <- modelN[,,ii]/modelP[,,ii] < 16
              
              ##Since there are NA's involved code up their compliments
              ##..explicitly
              notNLimit <- !NLimit
              notPLimit <- !PLimit
              
              ##...then set everything not a boolean to false
              NLimit[!is.finite(NLimit)] <- FALSE
              notNLimit[!is.finite(notNLimit)] <- FALSE
              PLimit[!is.finite(PLimit)] <- FALSE
              notPLimit[!is.finite(notPLimit)] <- FALSE
              ratioCut[!is.finite(ratioCut)] <- FALSE
              
              ##Set the default maximum NPP to Inf
              nppMax <- array(Inf, dim=dim(npp[,,ii]))
              
              ##Call a location N Limited if it's strictly N limited
              ##... or co-limited and meets the ratio cut-off
              NFlag <-  (NLimit & notPLimit) | (NLimit & PLimit & ratioCut)
              ##similar stuff for P
              PFlag <-  (notNLimit & PLimit) | (NLimit & PLimit & !ratioCut)
              ##Set the maximum NPP based on those flags
              nppMax[NFlag] <-(modelN[,,ii]*CtoN)[NFlag]
              nppMax[PFlag] <-(modelP[,,ii]*CtoP)[PFlag]
            }else{
              ##Alternatively use the strict minimum nutrient supported NPP
              nppMax <- pmin(modelN[,,ii]*CtoN, modelP[,,ii]*CtoP)
            }
          }else{
            ##don't constrain the NPP, use this as a test case to make
            ##...sure everything is in balance
            nppMax <- npp[,,ii]
          }
          ##If we are in the ag biome don't constrain NPP
          nppMax[biomeMask == 12] <- Inf
          ##If we don't have nutrient info, don't constrain NPP
          nppMax[!is.finite(nppMax)] <- Inf
          
          ##Take the minimum between what the model says should be there and
          ##... what can be supported with nutrients.
          nppAdj[,,ii] <- pmin(npp[,,ii], nppMax)
          
          ##Calculate the scale for the inputs from the adjusted NPP values
          nppAdjRatio[,,ii] <- nppAdj[,,ii]/npp[,,ii]
          nppAdjRatio[,,ii][is.infinite(nppAdjRatio[,,ii])] <- 0
          
          ##soil is equal to the old soil times the loss due to heter resp
          ##...plus the scaled inputs
          newSoil[,,ii]<- newSoil[,,ii-1]*(1-k[,,ii-1]) +
            nppAdjRatio[,,ii-1]*socInputs[,,ii-1]
          newSoil[newSoil < 0] <- 0
          
          ##Keep track of the new soil losses
          soilNloss[,,ii] <- newSoil[,,ii]-newSoil[,,ii-1]
          soilNloss[soilNloss > 0] <- 0
          soilNloss[,,ii] <- soilNloss[,,ii]/CtoNsoil
          
          ##Should we add them to the NPP N pools
          if(addSoilN){
            modelN[,,ii] <- modelN[,,ii] - soilNloss[,,ii]
          }
          
          ##veg carbon is equal to the previous veg carbon plus the scaled
          ##... inputs minus outputs
          newVeg[,,ii] <- newVeg[,,ii-1] + nppAdjRatio[,,ii-1]*
            (vcInputs[,,ii-1]-socInputs[,,ii-1])
        }#for-loop timesteps
        cat('done\n')
        
        ## #############################################
        ## record the results
        ## #############################################
        
        cat('calculating orginal input totals...')
        Is_tot <- apply(socInputs, c(3), function(x){sum(as.vector(x*landarea), na.rm=TRUE)})/1e12
        Is_tot[length(Is_tot)] <- NA
        Iv_tot <- apply(vcInputs, c(3), function(x){sum(as.vector(x*landarea), na.rm=TRUE)})/1e12
        Iv_tot[length(Iv_tot)] <- NA
        
        cat('calculating adjusted NPP, I_s, soil, and veg global totals...')
        N_tot <- apply(modelN, c(3), function(x){sum(as.vector(x*landarea)[is.finite(x)], na.rm=TRUE)})/1e12
        P_tot <- apply(modelP, c(3), function(x){sum(as.vector(x*landarea)[is.finite(x)], na.rm=TRUE)})/1e12
        soilNLoss_tot <- apply(soilNloss, c(3), function(x){sum(as.vector(x*landarea), na.rm=TRUE)})/1e12
        
        adjNPP_tot <- apply(nppAdj, c(3), function(x){sum(as.vector(x*landarea), na.rm=TRUE)})/1e12
        
        adjIs_tot <- apply(nppAdjRatio*socInputs, c(3), function(x){sum(as.vector(x*landarea), na.rm=TRUE)})/1e12
        adjIs_tot[length(adjIs_tot)] <- NA
        
        adjIv_tot <- apply(nppAdjRatio*vcInputs, c(3), function(x){sum(as.vector(x*landarea), na.rm=TRUE)})/1e12
        adjIv_tot[length(adjIv_tot)] <- NA
        
        adjSoil_tot <- apply(newSoil, c(3), function(x){sum(as.vector(x*landarea), na.rm=TRUE)})/1e12
        adjSoil_tot[length(adjSoil_tot)] <- NA
        
        
        adjVeg_tot <- apply(newVeg, c(3), function(x){sum(as.vector(x*landarea), na.rm=TRUE)})/1e12
        adjVeg_tot[length(adjVeg_tot)] <- NA
        cat('done\n')
        
        ##SHow the user what's going on
        cat('make user plot...')
        par(mfrow=c(2,2))
        plot(cSoil_tot, ylim = range(c(cSoil_tot, adjSoil_tot), na.rm=TRUE), type = 'l')
        points(adjSoil_tot, type='l', col='red')
        
        plot(cVeg_tot, ylim = range(c(cVeg_tot, adjVeg_tot), na.rm=TRUE), type = 'l')
        points(adjVeg_tot, type='l', col='red')
        
        plot(npp_tot, ylim=range(c(npp_tot, adjNPP_tot), na.rm=TRUE), type='l')
        points(adjNPP_tot, type='l', col='red')
        
        plot(Is_tot, ylim=range(c(Is_tot, adjIs_tot), na.rm=TRUE), lty=2, type='l')
        points(adjIs_tot, type='l', lty=2, col='red')
        
        cat('done\n')
        ##Store the orginal and adjusted global totals
        
        tempTot <- data.frame(model=modelStr, varStr=rbind('soil', 'veg', 'npp', 'Is', 'Ib', 'k'), rbind(cSoil_tot, cVeg_tot, npp_tot, Is_tot, Iv_tot, k_tot))
        names(tempTot) <- c('model', 'var', yrStr)
        if(is.null(orginalTot)){
          orginalTot <- tempTot
        }else{
          tempTot[,setdiff(names(orginalTot), names(tempTot))] <- NA
          orginalTot[,setdiff(names(tempTot), names(orginalTot))] <- NA
          orginalTot <- rbind(orginalTot, tempTot)
        }
        
        tempTot <- data.frame(model=modelStr, varStr=rbind('soil', 'veg', 'npp', 'Is', 'Iv'), rbind(adjSoil_tot, adjVeg_tot, adjNPP_tot, adjIs_tot, adjIv_tot))
        names(tempTot) <- c('model', 'var', yrStr)
        if(is.null(adjustedTot)){
          adjustedTot <- tempTot
        }else{
          tempTot[,setdiff(names(adjustedTot), names(tempTot))] <- NA
          adjustedTot[,setdiff(names(tempTot), names(adjustedTot))] <- NA
          adjustedTot <- rbind(adjustedTot, tempTot)
        }
        
        startFlag <- grepl('X186', yrStr)
        modernFlag <- grepl('(X199[5-9]|X200[0-4])', yrStr)
        endFlag <- grepl('X209', yrStr)
        
        startMaps[[modelStr]] <- list(npp=apply(npp[,,startFlag], c(1,2), mean, na.rm=TRUE),
                                      cSoil=apply(cSoil[,,startFlag], c(1,2), mean, na.rm=TRUE),
                                      cVeg=apply(cVeg[,,startFlag], c(1,2), mean, na.rm=TRUE),
                                      adjnpp=apply(nppAdj[,,startFlag], c(1,2), mean, na.rm=TRUE),
                                      adjcSoil=apply(newSoil[,,startFlag], c(1,2), mean, na.rm=TRUE),
                                      adjcVeg=apply(newVeg[,,startFlag], c(1,2), mean, na.rm=TRUE))
        
        modernMaps[[modelStr]] <- list(npp=apply(npp[,,modernFlag], c(1,2), mean, na.rm=TRUE),
                                       cSoil=apply(cSoil[,,modernFlag], c(1,2), mean, na.rm=TRUE),
                                       cVeg=apply(cVeg[,,modernFlag], c(1,2), mean, na.rm=TRUE),
                                       adjNPP=apply(nppAdj[,,modernFlag], c(1,2), mean, na.rm=TRUE),
                                       adjcSoil=apply(newSoil[,,modernFlag], c(1,2), mean, na.rm=TRUE),
                                       adjcVeg=apply(newVeg[,,modernFlag], c(1,2), mean, na.rm=TRUE))
        endMaps[[modelStr]] <- list(npp=apply(npp[,,endFlag], c(1,2), mean, na.rm=TRUE),
                                    cSoil=apply(cSoil[,,endFlag], c(1,2), mean, na.rm=TRUE),
                                    cVeg=apply(cVeg[,,endFlag], c(1,2), mean, na.rm=TRUE),
                                    adjnpp=apply(nppAdj[,,endFlag], c(1,2), mean, na.rm=TRUE),
                                    adjcSoil=apply(newSoil[,,endFlag], c(1,2), mean, na.rm=TRUE),
                                    adjcVeg=apply(newVeg[,,endFlag], c(1,2), mean, na.rm=TRUE))
        
        save(file=saveFilename, orginalTot, adjustedTot, startMaps, modernMaps, endMaps, boundSD)
      } #for-loop modelStr
      
      
      closeAllConnections()
    } #for-loop addSoilN
  } #for-loop limitStr
} #for-loop bondStr
}#for-loop halfStr
source('writeNPPLimitSummary.R')
