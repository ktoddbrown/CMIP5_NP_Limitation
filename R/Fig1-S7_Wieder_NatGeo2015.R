# Created by Kathe Todd-Brown & Will Wieder
# Modified Feb 2015
# Draws Fig. 1 & S7 from: Wieder et al. Future productivity and carbon storage are limited 
#     by terrestrial nutrient availability. Nature Geosci. (2015).
# reads in NPP & Terrestrial C estimates from CMIP5 models
# compares global control pools & fluxes to N limited and NP limited results
# soil respiration fluxes adjusted to give same NEP & land C sink over 20th Century

dir <- "/set_your_directory_here/"
setwd(dir)

soilNreturn <- "False" #False for Fig. 1, True for Fig. S7
#------------ matrixes to store results-----------------------------

annual_NPP_C   <- matrix(NA, nrow = 252, ncol = 11 )
annual_NPP_N   <- matrix(NA, nrow = 252, ncol = 11 )
annual_NPP_NP  <- matrix(NA, nrow = 252, ncol = 11 )
annual_EcoC_C  <- matrix(NA, nrow = 252, ncol = 11 )
annual_EcoC_N  <- matrix(NA, nrow = 252, ncol = 11 )
annual_EcoC_NP <- matrix(NA, nrow = 252, ncol = 11 )

# I manually broke up large .csv file into individual files for each before running this code
if (soilNreturn == "False") {
	files <- c('orginalTot.csv', 'N_limitedTot.csv', 'NP_limitedTot.csv')
} else if (soilNreturn == "True") {
	files <- c('orginalTot.csv', 'N_limitedTot_soilNReturn.csv', 'NP_limitedTot_soilNReturn.csv')
}
#------------ read in data ---------------------------------------

for (tx in 1:3) {
	cat('reading', files[tx], '\n')
	datain <- read.csv(files[tx])
	loadFilename <- files[tx]
#	load(loadFilename)
	modelToConsider <- unique(datain$model)  #read in model names

     yrIndex <- grepl('X', names(datain))
        yrStartIndex <- grepl('X186', names(datain))
        yrEndIndex <- grepl('X209', names(datain))
        yrs <- as.numeric(substring(names(datain)[yrIndex], 2,5))
        modelsToPlot <- unique(datain$model)
        numRow <- ceiling(sqrt(length(modelsToPlot)))
         
        for(varStr in c('npp', 'veg', 'soil', 'land')){
	        mnumber <- 0  # counter to index model number

            for(modelStr in modelToConsider){
	        mnumber <- mnumber + 1
                if(varStr %in%  'land'){ #read in data for total land C

                    rowFlag <- datain$model %in% modelStr &
                                datain$var %in% 'soil'

                    orgSoil <- unlist(datain[rowFlag,yrIndex]) -
                        mean(unlist(datain[rowFlag,yrStartIndex]), na.rm=TRUE)
                    rowFlag <- datain$model %in% modelStr &
                                datain$var %in% 'veg'

                    orgVeg <- unlist(datain[rowFlag,yrIndex]) -
                        mean(unlist(datain[rowFlag,yrStartIndex]), na.rm=TRUE)
                    orgVals <- orgSoil+orgVeg

	 			    if (tx == 1) {annual_EcoC_C[,mnumber]  <- orgVals } 
				    if (tx == 2) {annual_EcoC_N[,mnumber]  <- orgVals }
				    if (tx == 3) {annual_EcoC_NP[,mnumber] <- orgVals }

                }else{
                    rowFlag <- datain$model %in% modelStr &
                                datain$var %in% varStr

                    orgVals <- unlist(datain[rowFlag,yrIndex]) -
                        mean(unlist(datain[rowFlag,yrStartIndex]), na.rm=TRUE)

					if (varStr %in% 'npp') { #save matrix of results
	 			        if (tx == 1) {annual_NPP_C[,mnumber]  <- orgVals } 
				        if (tx == 2) {annual_NPP_N[,mnumber]  <- orgVals }
				        if (tx == 3) {annual_NPP_NP[,mnumber] <- orgVals }
					}	
                }
            }  # closes varStr loop
	    }  # closes pool loop
	remove(datain)
    } # closes tx loop


#---------------------------------------------------------------
#    Change in NPP C figure (Fig 1a)
#---------------------------------------------------------------

NPP_C_mean  <- rowMeans(annual_NPP_C,  na.rm = TRUE, dims = 1)
NPP_N_mean  <- rowMeans(annual_NPP_N,  na.rm = TRUE, dims = 1)
NPP_NP_mean <- rowMeans(annual_NPP_NP, na.rm = TRUE, dims = 1)
NPP_C_sd    <- apply(annual_NPP_C, 1,  sd, na.rm = TRUE)
NPP_N_sd    <- apply(annual_NPP_N, 1,  sd, na.rm = TRUE)
NPP_NP_sd   <- apply(annual_NPP_NP, 1, sd, na.rm = TRUE)
xx   <- c(yrs, rev(yrs))
yyC  <- c(NPP_C_mean  + NPP_C_sd,  rev(NPP_C_mean  - NPP_C_sd )) 
yyN  <- c(NPP_N_mean  + NPP_N_sd,  rev(NPP_N_mean  - NPP_N_sd )) 
yyNP <- c(NPP_NP_mean + NPP_NP_sd, rev(NPP_NP_mean - NPP_NP_sd)) 

fyr = 251
#calculate mean dNPP for final 10 years of each model for boxplot
finalMean_C <-colMeans(annual_NPP_C[242:251,], na.rm=TRUE, dims=1)  
finalMean_N <-colMeans(annual_NPP_N[242:251,], na.rm=TRUE, dims=1)
finalMean_NP <-colMeans(annual_NPP_NP[242:251,], na.rm=TRUE, dims=1)

collist <- c(rgb(0, 0, 0,0.4), rgb(1, 0, 0,0.4), rgb(0, 0, 1,0.5))
NPPmin_max <- c(-05, 65) 

#make NPP plot
quartz()
par(fig = c(0,0.9, 0.4, 1.0), cex = 1.2, mar=c(4.5,5,1,2))
plot(yrs, NPP_C_mean, xlim=c(1860,2100), type="n", lwd=4, ylim=NPPmin_max,
	xlab="", ylab =expression(paste("Change NPP (Pg C ", y^-1,")")), 
	xaxt='n', xaxs="i", cex.lab=1.3, cex.axis=1.2)

  polygon(xx, yyC,  col=collist[1], border = NA)
  polygon(xx, yyN,  col=collist[2], border = NA)
  polygon(xx, yyNP, col=collist[3], border = NA)
#  for (m in 1:11) {
#	lines(yrs, annual_NPP_C[,m],  col = 1, lwd = 1)
#	lines(yrs, annual_NPP_N[,m],  col = 2, lwd = 1)
#	lines(yrs, annual_NPP_NP[,m], col = 4, lwd = 1)
#  }
  lines(yrs, NPP_NP_mean, col = 4, lwd = 4)
  lines(yrs, NPP_N_mean,  col = 2, lwd = 4)
  lines(yrs, NPP_C_mean,  col = 1, lwd = 4)
abline(0,0, lty=2)  
text(1873, 61,"(a)", cex=1.2)
axis(1, at=seq(1900, 2100, 50), labels=NA)
axis(4, at=seq(0,60,10), labels=NA)

# add box plot
frange = data.frame(finalMean_C, finalMean_N, finalMean_NP)
par (fig= c(0.60, 1.0, 0.4, 1.0), cex = 1.2, mar=c(4.5,8,1,0), new=TRUE)
boxplot(frange, col = collist, ylim = NPPmin_max, axes=FALSE)

print(sum(NPP_C_mean))
print(sum(NPP_N_mean))
print(sum(NPP_NP_mean))

#---------------------------------------------------------------
#    Change in land C (Fig. 1b)
#---------------------------------------------------------------
fyr = 250
EcoC_C_mean  <- rowMeans(annual_EcoC_C[1:fyr,],  na.rm = TRUE, dims = 1)
EcoC_N_mean  <- rowMeans(annual_EcoC_N[1:fyr,],  na.rm = TRUE, dims = 1)
EcoC_NP_mean <- rowMeans(annual_EcoC_NP[1:fyr,], na.rm = TRUE, dims = 1)
EcoC_C_sd    <- apply(annual_EcoC_C[1:fyr,], 1,  sd, na.rm = TRUE)
EcoC_N_sd    <- apply(annual_EcoC_N[1:fyr,], 1,  sd, na.rm = TRUE)
EcoC_NP_sd   <- apply(annual_EcoC_NP[1:fyr,], 1, sd, na.rm = TRUE)
xx   <- c(yrs[1:fyr], rev(yrs[1:fyr]))
yyC  <- c(EcoC_C_mean  + EcoC_C_sd,  rev(EcoC_C_mean  - EcoC_C_sd )) 
yyN  <- c(EcoC_N_mean  + EcoC_N_sd,  rev(EcoC_N_mean  - EcoC_N_sd )) 
yyNP <- c(EcoC_NP_mean + EcoC_NP_sd, rev(EcoC_NP_mean - EcoC_NP_sd)) 

#calculate mean dEcoC for final 10 years of each model for boxplot
finalMean_C <-colMeans(annual_EcoC_C[242:250,], na.rm=TRUE, dims=1)  
finalMean_N <-colMeans(annual_EcoC_N[242:250,], na.rm=TRUE, dims=1)
finalMean_NP <-colMeans(annual_EcoC_NP[242:250,], na.rm=TRUE, dims=1)

EcoCmin_max <- c(-500, 500) 

#make EcoC plot
par(fig = c(0,0.9, 0, 0.6), cex = 1.2, mar=c(3,5,2.5,2), new=TRUE)
plot(yrs[1:fyr], EcoC_C_mean, xlim=c(1860,2100), type="n", lwd=4, ylim= EcoCmin_max,
	xlab="", ylab ="Change Land C (Pg)", xaxs="i", cex.lab=1.3, cex.axis=1.2,
	yaxt='n', yaxs="i", xaxt='n', xaxs="i")

  polygon(xx, yyC,  col=collist[1], border = NA)
  polygon(xx, yyNP, col=collist[3], border = NA)
  polygon(xx, yyN,  col=collist[2], border = NA)
#  for (m in 1:11) {
#	lines(yrs, annual_EcoC_C[,m],  col = 1, lwd = 1)
#	lines(yrs, annual_EcoC_N[,m],  col = 2, lwd = 1)
#	lines(yrs, annual_EcoC_NP[,m], col = 4, lwd = 1)
#  }
  lines(yrs[1:fyr], EcoC_NP_mean, col = 4, lwd = 4)
  lines(yrs[1:fyr], EcoC_N_mean,  col = 2, lwd = 4)
  lines(yrs[1:fyr], EcoC_C_mean,  col = 1, lwd = 4)
abline(0,0, lty=2)  
text(1873, 410,"(b)", cex=1.2)
axis(1, at=seq(1900, 2100, 50))
axis(2, at=seq(-400,400,200))
axis(4, at=seq(-400,400,200), labels=NA)

# add boxplot to margin
frange = data.frame(finalMean_C, finalMean_N, finalMean_NP)
par (fig= c(0.60, 1.0, 0, 0.6), cex = 1.2, mar=c(3,8,2.5,0), new=TRUE)
boxplot(frange, col = collist, ylim = EcoCmin_max, axes=FALSE)



print((EcoC_C_mean[fyr]))
print((EcoC_N_mean[fyr]))
print((EcoC_NP_mean[fyr]))
