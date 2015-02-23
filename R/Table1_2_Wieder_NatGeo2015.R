# Created by Kathe Todd-Brown & Will Wieder
# Modified Feb 2015
# Writes data presented in Tables 1 & 2 from :
# Wieder et al. Future productivity and carbon storage are limited by terrestrial nutrient availability Nature Geosci. (2015).
# reads in NPP & Terrestrial C estimates from CMIP5 models
# compares global control pools & fluxes to N limited and NP limited results
# soil respiration fluxes adjusted to give same NEP & land C sink over 20th Century
# calculates mean NPP & land C at:
	#start of historical period (1860-1869)
	#end   of historical period (1996-2005)
	#end   of RCP 8.5 (2090-2099)
# plots individual model changes over entire time series for NPP, Terrestrail C, k, etc.
# looks at sensitivity analysis (w/ high land low N/P inputs, CNP ratios)

dir <- "/set_your_directory_here/"
setwd(dir)


# --------------- matrixes to store results----------------------

annual_NPP_C_mean          <- matrix(NA, nrow = 252, ncol = 11 )
annual_NPP_N_mean          <- matrix(NA, nrow = 252, ncol = 11 )
annual_NPP_N_lower         <- matrix(NA, nrow = 252, ncol = 11 )
annual_NPP_N_lowerIn       <- matrix(NA, nrow = 252, ncol = 11 )
annual_NPP_N_lowerRatio    <- matrix(NA, nrow = 252, ncol = 11 )
annual_NPP_N_upper         <- matrix(NA, nrow = 252, ncol = 11 )
annual_NPP_N_upperIn       <- matrix(NA, nrow = 252, ncol = 11 )
annual_NPP_N_upperRatio    <- matrix(NA, nrow = 252, ncol = 11 )

annual_NPP_NP_mean         <- matrix(NA, nrow = 252, ncol = 11 )
annual_NPP_NP_lower        <- matrix(NA, nrow = 252, ncol = 11 )
annual_NPP_NP_lowerIn      <- matrix(NA, nrow = 252, ncol = 11 )
annual_NPP_NP_lowerRatio   <- matrix(NA, nrow = 252, ncol = 11 )
annual_NPP_NP_upper        <- matrix(NA, nrow = 252, ncol = 11 )
annual_NPP_NP_upperIn      <- matrix(NA, nrow = 252, ncol = 11 )
annual_NPP_NP_upperRatio   <- matrix(NA, nrow = 252, ncol = 11 )

annual_EcoC_C_mean          <- matrix(NA, nrow = 252, ncol = 11 )
annual_EcoC_N_mean          <- matrix(NA, nrow = 252, ncol = 11 )
annual_EcoC_N_lower         <- matrix(NA, nrow = 252, ncol = 11 )
annual_EcoC_N_lowerIn       <- matrix(NA, nrow = 252, ncol = 11 )
annual_EcoC_N_lowerRatio    <- matrix(NA, nrow = 252, ncol = 11 )
annual_EcoC_N_upper         <- matrix(NA, nrow = 252, ncol = 11 )
annual_EcoC_N_upperIn       <- matrix(NA, nrow = 252, ncol = 11 )
annual_EcoC_N_upperRatio    <- matrix(NA, nrow = 252, ncol = 11 )

annual_EcoC_NP_mean         <- matrix(NA, nrow = 252, ncol = 11 )
annual_EcoC_NP_lower        <- matrix(NA, nrow = 252, ncol = 11 )
annual_EcoC_NP_lowerIn      <- matrix(NA, nrow = 252, ncol = 11 )
annual_EcoC_NP_lowerRatio   <- matrix(NA, nrow = 252, ncol = 11 )
annual_EcoC_NP_upper        <- matrix(NA, nrow = 252, ncol = 11 )
annual_EcoC_NP_upperIn      <- matrix(NA, nrow = 252, ncol = 11 )
annual_EcoC_NP_upperRatio   <- matrix(NA, nrow = 252, ncol = 11 )

annual_k_C_mean          <- matrix(NA, nrow = 252, ncol = 11 )
annual_k_N_mean          <- matrix(NA, nrow = 252, ncol = 11 )
annual_k_N_lower         <- matrix(NA, nrow = 252, ncol = 11 )
annual_k_N_lowerIn       <- matrix(NA, nrow = 252, ncol = 11 )
annual_k_N_lowerRatio    <- matrix(NA, nrow = 252, ncol = 11 )
annual_k_N_upper         <- matrix(NA, nrow = 252, ncol = 11 )
annual_k_N_upperIn       <- matrix(NA, nrow = 252, ncol = 11 )
annual_k_N_upperRatio    <- matrix(NA, nrow = 252, ncol = 11 )

annual_k_NP_mean         <- matrix(NA, nrow = 252, ncol = 11 )
annual_k_NP_lower        <- matrix(NA, nrow = 252, ncol = 11 )
annual_k_NP_lowerIn      <- matrix(NA, nrow = 252, ncol = 11 )
annual_k_NP_lowerRatio   <- matrix(NA, nrow = 252, ncol = 11 )
annual_k_NP_upper        <- matrix(NA, nrow = 252, ncol = 11 )
annual_k_NP_upperIn      <- matrix(NA, nrow = 252, ncol = 11 )
annual_k_NP_upperRatio <- matrix(NA, nrow = 252, ncol = 11 )

#limitToConsider <-  c('N', 'P', 'NP')[c(1,3)]
Bound <- c('mean','lower','lowerIn','lowerRatio', 
		   'upper','upperIn','upperRatio')
Nutrient <- c('C','N','NP')		   


# --------------- Read in data ------------------------------------

data_all   <- read.csv('limittedTot.csv')
data_noCsN <- data_all[ which(data_all$soilNReturn == FALSE) , ]
attach(data_noCsN)
names(data_noCsN)

for (tx in 1:3) {
  if (tx == 1) { finB <- 1
    } else {     finB <- 7
  }

  for (B in 1:finB) {
	cat('reading', Nutrient[tx], Bound[B],'\n')
    datain <- data_noCsN[ which(data_noCsN$nutrient == Nutrient[tx]) , ] 
    datain <- datain[ which(datain$bound == Bound[B]) ,] 
	modelToConsider <- unique(datain$model)  #read in model names

     yrIndex <- grepl('X', names(datain))
        yrStartIndex <- grepl('X186', names(datain))
        yrEndIndex <- grepl('X209', names(datain))
        yrs <- as.numeric(substring(names(datain)[yrIndex], 2,5))
        modelsToPlot <- unique(datain$model)
        numRow <- ceiling(sqrt(length(modelsToPlot)))
         
        for(varStr in c('npp', 'veg', 'soil', 'land','k')){
	        mnumber <- 0  # counter to index model number

            for(modelStr in modelToConsider){
	        mnumber <- mnumber + 1
                if(varStr %in%  'land'){ #read in data for total land C

                    rowFlag <- datain$model %in% modelStr &
                                datain$var %in% 'soil'

                    orgSoil <- unlist(datain[rowFlag,yrIndex])
                    rowFlag <- datain$model %in% modelStr &
                                datain$var %in% 'veg'

                    orgVeg <- unlist(datain[rowFlag,yrIndex])
                    orgVals <- orgSoil+orgVeg

				    if (tx == 1 & B==1) {annual_EcoC_C_mean[,mnumber]  <- orgVals }

				    if (tx == 2 & B==1) {annual_EcoC_N_mean[,mnumber]  <- orgVals }
				    if (tx == 2 & B==2) {annual_EcoC_N_lower[,mnumber]  <- orgVals }
				    if (tx == 2 & B==3) {annual_EcoC_N_lowerIn[,mnumber]  <- orgVals }
				    if (tx == 2 & B==4) {annual_EcoC_N_lowerRatio[,mnumber]  <- orgVals }
				    if (tx == 2 & B==5) {annual_EcoC_N_upper[,mnumber]  <- orgVals }
				    if (tx == 2 & B==6) {annual_EcoC_N_upperIn[,mnumber]  <- orgVals }
				    if (tx == 2 & B==7) {annual_EcoC_N_upperRatio[,mnumber]  <- orgVals }

				    if (tx == 3 & B==1) {annual_EcoC_NP_mean[,mnumber]  <- orgVals }
				    if (tx == 3 & B==2) {annual_EcoC_NP_lower[,mnumber]  <- orgVals }
				    if (tx == 3 & B==3) {annual_EcoC_NP_lowerIn[,mnumber]  <- orgVals }
				    if (tx == 3 & B==4) {annual_EcoC_NP_lowerRatio[,mnumber]  <- orgVals }
				    if (tx == 3 & B==5) {annual_EcoC_NP_upper[,mnumber]  <- orgVals }
				    if (tx == 3 & B==6) {annual_EcoC_NP_upperIn[,mnumber]  <- orgVals }
				    if (tx == 3 & B==7) {annual_EcoC_NP_upperRatio[,mnumber]  <- orgVals }


                }else if (varStr %in%  'npp'){ #read in npp data 
                    rowFlag <- datain$model %in% modelStr &
                                datain$var %in% varStr

                    orgVals <- unlist(datain[rowFlag,yrIndex]) 
					if (varStr %in% 'npp') { #save matrix of results

				    	if (tx == 1 & B==1) {annual_NPP_C_mean[,mnumber]  <- orgVals }
				    	if (tx == 2 & B==1) {annual_NPP_N_mean[,mnumber]  <- orgVals }
				    	if (tx == 2 & B==2) {annual_NPP_N_lower[,mnumber]  <- orgVals }
				    	if (tx == 2 & B==3) {annual_NPP_N_lowerIn[,mnumber]  <- orgVals }
				    	if (tx == 2 & B==4) {annual_NPP_N_lowerRatio[,mnumber]  <- orgVals }
				    	if (tx == 2 & B==5) {annual_NPP_N_upper[,mnumber]  <- orgVals }
				    	if (tx == 2 & B==6) {annual_NPP_N_upperIn[,mnumber]  <- orgVals }
				    	if (tx == 2 & B==7) {annual_NPP_N_upperRatio[,mnumber]  <- orgVals }

				    	if (tx == 3 & B==1) {annual_NPP_NP_mean[,mnumber]  <- orgVals }
				    	if (tx == 3 & B==2) {annual_NPP_NP_lower[,mnumber]  <- orgVals }
				    	if (tx == 3 & B==3) {annual_NPP_NP_lowerIn[,mnumber]  <- orgVals }
				    	if (tx == 3 & B==4) {annual_NPP_NP_lowerRatio[,mnumber]  <- orgVals }
				    	if (tx == 3 & B==5) {annual_NPP_NP_upper[,mnumber]  <- orgVals }
				    	if (tx == 3 & B==6) {annual_NPP_NP_upperIn[,mnumber]  <- orgVals }
				    	if (tx == 3 & B==7) {annual_NPP_NP_upperRatio[,mnumber]  <- orgVals }
					}
				} else { 	
					rowFlag <- datain$model %in% modelStr &
                                datain$var %in% varStr

                    orgVals <- unlist(datain[rowFlag,yrIndex])

					if (varStr %in% 'k') { #save matrix of results

				    	if (tx == 1 & B==1) {annual_k_C_mean[,mnumber]  <- orgVals }
				    	if (tx == 2 & B==1) {annual_k_N_mean[,mnumber]  <- orgVals }
				    	if (tx == 2 & B==2) {annual_k_N_lower[,mnumber]  <- orgVals }
				    	if (tx == 2 & B==3) {annual_k_N_lowerIn[,mnumber]  <- orgVals }
				    	if (tx == 2 & B==4) {annual_k_N_lowerRatio[,mnumber]  <- orgVals }
				    	if (tx == 2 & B==5) {annual_k_N_upper[,mnumber]  <- orgVals }
				    	if (tx == 2 & B==6) {annual_k_N_upperIn[,mnumber]  <- orgVals }
				    	if (tx == 2 & B==7) {annual_k_N_upperRatio[,mnumber]  <- orgVals }

				    	if (tx == 3 & B==1) {annual_k_NP_mean[,mnumber]  <- orgVals }
				    	if (tx == 3 & B==2) {annual_k_NP_lower[,mnumber]  <- orgVals }
				    	if (tx == 3 & B==3) {annual_k_NP_lowerIn[,mnumber]  <- orgVals }
				    	if (tx == 3 & B==4) {annual_k_NP_lowerRatio[,mnumber]  <- orgVals }
				    	if (tx == 3 & B==5) {annual_k_NP_upper[,mnumber]  <- orgVals }
				    	if (tx == 3 & B==6) {annual_k_NP_upperIn[,mnumber]  <- orgVals }
				    	if (tx == 3 & B==7) {annual_k_NP_upperRatio[,mnumber]  <- orgVals }
				    	
					}		
                }
            }  # closes varStr loop
	    }  # closes pool loop
	remove(datain)
    } # closes bound loop
  } # closes tx loop


#-----------------------------------------------------
# (2) visualize infividual models results
#-----------------------------------------------------
quartz() 
Ylim <- c(40,110)
plot(yrs,annual_NPP_N_mean[,1], type="l", ylim=Ylim, lwd=3,
  ylab =expression(paste("NPP (Pg C ", y^-1,")")),
  main = "N limited", xlab="")
i <- 10
 for (i in 1:11) {
 	lines(yrs,annual_NPP_N_upper[,i], col=i)
 	lines(yrs,annual_NPP_N_lower[,i], col=i)
 	lines(yrs,annual_NPP_N_mean[,i], col=i, lwd=3)
 	lines(yrs,annual_NPP_N_lowerIn[,i], col=i)
 	lines(yrs,annual_NPP_N_lowerRatio[,i], col=1)

 }

#----------NPP estiamtes across models---------------------
quartz() 
par(mfrow=c(3,3), mar=c(3,4,1,1))
Ylim <- c(40,150)
plot(yrs,annual_NPP_C_mean[,1], type="l", ylim=Ylim, lwd=3,
  ylab =expression(paste("NPP (Pg C ", y^-1,")")),
  main = "CMIP5_orig", xlab="")

 for (i in 1:11) {
 	lines(yrs,annual_NPP_C_mean[,i], col=i, lwd=3)
 }

plot(yrs,annual_NPP_N_mean[,1], type="l", ylim=Ylim, lwd=3,
  ylab =expression(paste("NPP (Pg C ", y^-1,")")),
  main = "N limited", xlab="")
#i <- 10
 for (i in 1:11) {
 	lines(yrs,annual_NPP_N_upper[,i], col=i)
 	lines(yrs,annual_NPP_N_lower[,i], col=i)
 	lines(yrs,annual_NPP_N_mean[,i], col=i, lwd=3)
# 	lines(yrs,annual_NPP_N_lowerIn[,i], col=i)
# 	lines(yrs,annual_NPP_N_lowerRatio[,i], col=1)

 }

plot(yrs,annual_NPP_NP_mean[,1], type="l", ylim=Ylim, lwd=3,
  ylab =expression(paste("NPP (Pg C ", y^-1,")")),
  main = "NP limited", xlab="")

 for (i in 1:11) {
 	lines(yrs,annual_NPP_NP_upper[,i], col=i)
 	lines(yrs,annual_NPP_NP_lower[,i], col=i)
 	lines(yrs,annual_NPP_NP_mean[,i], col=i, lwd=3)
 }

#-------------Ecosystem C across models------------------------------------
Ylim <- c(800,3800)
par(mar=c(4,4,0,1))
plot(yrs,annual_EcoC_C_mean[,1], type="l", ylim=Ylim, lwd=3,
  ylab ="EcoC (Pg C)")
 for (i in 1:11) {
 	lines(yrs,annual_EcoC_C_mean[,i], col=i, lwd=3)
 }

plot(yrs,annual_EcoC_N_mean[,1], type="l", ylim=Ylim, lwd=3,
  ylab ="EcoC (Pg C)")
 for (i in 1:11) {
 	lines(yrs,annual_EcoC_N_upper[,i], col=i)
 	lines(yrs,annual_EcoC_N_lower[,i], col=i)
 	lines(yrs,annual_EcoC_N_mean[,i], col=i, lwd=3)
 }

plot(yrs,annual_EcoC_NP_mean[,1], type="l", ylim=Ylim, lwd=3,
  ylab ="EcoC (Pg C)")
 for (i in 1:11) {
 	lines(yrs,annual_EcoC_NP_upper[,i], col=i)
 	lines(yrs,annual_EcoC_NP_lower[,i], col=i)
 	lines(yrs,annual_EcoC_NP_mean[,i], col=i, lwd=3)
 }


#-------------soil decomp constant-------------------------------------
Ylim <- c(0, 1e-1)
par(mar=c(4,4,0,1))
plot(yrs,annual_k_C_mean[,1], type="l", ylim=Ylim, lwd=3,
  ylab ="k (y^-1)")
 for (i in 1:11) {
 	lines(yrs,annual_k_C_mean[,i], col=i, lwd=3)
 }

plot(yrs,annual_k_N_mean[,1], type="l", ylim=Ylim, lwd=3,
  ylab ="k (y^-1)")
 for (i in 1:11) {
 	lines(yrs,annual_k_N_upper[,i], col=i)
 	lines(yrs,annual_k_N_lower[,i], col=i)
 	lines(yrs,annual_k_N_mean[,i], col=i, lwd=3)
 }

plot(yrs,annual_k_NP_mean[,1], type="l", ylim=Ylim, lwd=3,
  ylab ="k (y^-1)")
 for (i in 1:11) {
 	lines(yrs,annual_k_NP_upper[,i], col=i)
 	lines(yrs,annual_k_NP_lower[,i], col=i)
 	lines(yrs,annual_k_NP_mean[,i], col=i, lwd=3)
 }


yrs[12:21]   #initial
yrs[147:156] #end of historical
yrs[242:251] #enf of RCP8.5

# --------------- matrixes to store mean values for each time period----------------------

cnames <- c('initial','historical','rcp8.5')
rnames <- unique(data_noCsN$model)
nmod   <- length(rnames)
ntime  <- length(cnames)
trange <- c('12:21','148:157','242:251')
syear  <- c(12, 147, 242)   #start of each time period to average
eyear  <- c(21, 156, 251)   #  end of each time period to average

NPP_C_mean   <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
NPP_N_mean   <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
NPP_N_lower  <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
NPP_N_upper  <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
NPP_NP_mean  <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
NPP_NP_lower <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
NPP_NP_upper <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))

EcoC_C_mean   <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
EcoC_N_mean   <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
EcoC_N_lower  <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
EcoC_N_upper  <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
EcoC_NP_mean  <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
EcoC_NP_lower <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
EcoC_NP_upper <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))

k_C_mean   <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
k_N_mean   <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
k_N_lower  <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
k_N_upper  <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
k_NP_mean  <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
k_NP_lower <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))
k_NP_upper <- matrix(NA, nrow=nmod, ncol=ntime, dimnames=list(rnames, cnames))

# loop over each model, for each time period

for (m in 1:nmod) {
	for (t in 1:ntime) {

		NPP_C_mean[m,t]   <- mean(  annual_NPP_C_mean[syear[t]:eyear[t],m], na.rm=T)
		NPP_N_mean[m,t]   <- mean(  annual_NPP_N_mean[syear[t]:eyear[t],m], na.rm=T)
		NPP_N_lower[m,t]  <- mean( annual_NPP_N_lower[syear[t]:eyear[t],m], na.rm=T)
		NPP_N_upper[m,t]  <- mean( annual_NPP_N_upper[syear[t]:eyear[t],m], na.rm=T)
		NPP_NP_mean[m,t]  <- mean( annual_NPP_NP_mean[syear[t]:eyear[t],m], na.rm=T)
		NPP_NP_lower[m,t] <- mean(annual_NPP_NP_lower[syear[t]:eyear[t],m], na.rm=T)
		NPP_NP_upper[m,t] <- mean(annual_NPP_NP_upper[syear[t]:eyear[t],m], na.rm=T)

		EcoC_C_mean[m,t]   <- mean(  annual_EcoC_C_mean[syear[t]:eyear[t],m], na.rm=T)
		EcoC_N_mean[m,t]   <- mean(  annual_EcoC_N_mean[syear[t]:eyear[t],m], na.rm=T)
		EcoC_N_lower[m,t]  <- mean( annual_EcoC_N_lower[syear[t]:eyear[t],m], na.rm=T)
		EcoC_N_upper[m,t]  <- mean( annual_EcoC_N_upper[syear[t]:eyear[t],m], na.rm=T)
		EcoC_NP_mean[m,t]  <- mean( annual_EcoC_NP_mean[syear[t]:eyear[t],m], na.rm=T)
		EcoC_NP_lower[m,t] <- mean(annual_EcoC_NP_lower[syear[t]:eyear[t],m], na.rm=T)
		EcoC_NP_upper[m,t] <- mean(annual_EcoC_NP_upper[syear[t]:eyear[t],m], na.rm=T)

		k_C_mean[m,t]   <- mean(  annual_k_C_mean[syear[t]:eyear[t],m], na.rm=T)
		k_N_mean[m,t]   <- mean(  annual_k_N_mean[syear[t]:eyear[t],m], na.rm=T)
		k_N_lower[m,t]  <- mean( annual_k_N_lower[syear[t]:eyear[t],m], na.rm=T)
		k_N_upper[m,t]  <- mean( annual_k_N_upper[syear[t]:eyear[t],m], na.rm=T)
		k_NP_mean[m,t]  <- mean( annual_k_NP_mean[syear[t]:eyear[t],m], na.rm=T)
		k_NP_lower[m,t] <- mean(annual_k_NP_lower[syear[t]:eyear[t],m], na.rm=T)
		k_NP_upper[m,t] <- mean(annual_k_NP_upper[syear[t]:eyear[t],m], na.rm=T)

	} #close time loop
} #close model loop

#-----------calculate sensitivty ranges----------------------------

NPP_N_lower_dif  <- NPP_N_lower[,2:3]  - NPP_N_mean[,2:3]
NPP_N_upper_dif  <- NPP_N_upper[,2:3]  - NPP_N_mean[,2:3]
NPP_NP_lower_dif <- NPP_NP_lower[,2:3] - NPP_NP_mean[,2:3]
NPP_NP_upper_dif <- NPP_NP_upper[,2:3] - NPP_NP_mean[,2:3]

NPP_N_lower_percent  <- NPP_N_lower_dif  / NPP_N_mean[,2:3] * 100
NPP_N_upper_percent  <- NPP_N_upper_dif  / NPP_N_mean[,2:3] * 100
NPP_NP_lower_percent <- NPP_NP_lower_dif / NPP_NP_mean[,2:3] * 100
NPP_NP_upper_percent <- NPP_NP_upper_dif / NPP_NP_mean[,2:3] * 100

#print % chagne in NPP w/ sensitivity analyses
  colMeans(NPP_N_lower_percent)
  colMeans(NPP_N_upper_percent)
  colMeans(NPP_NP_lower_percent)
  colMeans(NPP_NP_upper_percent)

EcoC_N_lower_dif  <- EcoC_N_lower[,2:3]  - EcoC_N_mean[,2:3]
EcoC_N_upper_dif  <- EcoC_N_upper[,2:3]  - EcoC_N_mean[,2:3]
EcoC_NP_lower_dif <- EcoC_NP_lower[,2:3] - EcoC_NP_mean[,2:3]
EcoC_NP_upper_dif <- EcoC_NP_upper[,2:3] - EcoC_NP_mean[,2:3]

EcoC_N_lower_percent  <- EcoC_N_lower_dif  / EcoC_N_mean[,2:3] * 100
EcoC_N_upper_percent  <- EcoC_N_upper_dif  / EcoC_N_mean[,2:3] * 100
EcoC_NP_lower_percent <- EcoC_NP_lower_dif / EcoC_NP_mean[,2:3] * 100
EcoC_NP_upper_percent <- EcoC_NP_upper_dif / EcoC_NP_mean[,2:3] * 100

#print % chagne in EcoC w/ sensitivity analyses
  colMeans(EcoC_N_lower_percent)
  colMeans(EcoC_N_upper_percent)
  colMeans(EcoC_NP_lower_percent)
  colMeans(EcoC_NP_upper_percent)

k_N_lower_dif  <- k_N_lower[,2:3]  - k_N_mean[,2:3]
k_N_upper_dif  <- k_N_upper[,2:3]  - k_N_mean[,2:3]
k_NP_lower_dif <- k_NP_lower[,2:3] - k_NP_mean[,2:3]
k_NP_upper_dif <- k_NP_upper[,2:3] - k_NP_mean[,2:3]

k_N_lower_percent  <- k_N_lower_dif  / k_N_mean[,2:3] * 100
k_N_upper_percent  <- k_N_upper_dif  / k_N_mean[,2:3] * 100
k_NP_lower_percent <- k_NP_lower_dif / k_NP_mean[,2:3] * 100
k_NP_upper_percent <- k_NP_upper_dif / k_NP_mean[,2:3] * 100

#---------------------------------------------------------------
#    Write out Tables 1 & 2
#---------------------------------------------------------------

NPP_out <- data.frame(NPP_C_mean[,1:2],NPP_N_mean[,2], NPP_NP_mean[,2], 
                      NPP_C_mean[,3],  NPP_N_mean[,3], NPP_NP_mean[,3])  
NPP_N_range  <- data.frame(NPP_N_lower_dif,  NPP_N_upper_dif)
NPP_NP_range <- data.frame(NPP_NP_lower_dif,NPP_NP_upper_dif)

EcoC_out <- data.frame(EcoC_C_mean[,1:2],EcoC_N_mean[,2], EcoC_NP_mean[,2], 
                      EcoC_C_mean[,3],  EcoC_N_mean[,3], EcoC_NP_mean[,3])  
EcoC_N_range  <- data.frame(EcoC_N_lower_dif,  EcoC_N_upper_dif)
EcoC_NP_range <- data.frame(EcoC_NP_lower_dif,EcoC_NP_upper_dif)


write.table(NPP_out,  file="global_NPP_table1.csv", sep=",")   	#writes out Table 1
																#column heading incorrect in .csv
																#ssee NPP_out (above)
write.table(NPP_N_range,   file="global_NPP_N_range.csv" , sep=",")
write.table(NPP_NP_range,  file="global_NPP_NP_range.csv", sep=",")

write.table(EcoC_out, file="global_EcoC_table2.csv", sep=",")   #writes out table 2
																#column heading incorrect in .csv
																#ssee EcoC_out (above)
write.table(EcoC_N_range,   file="global_EcoC_N_range.csv" , sep=",")
write.table(EcoC_NP_range,  file="global_EcoC_NP_range.csv", sep=",")
