# TODO: Add comment
# 
# Author: triffe
###############################################################################

# run these if you're only interested
#source("PrepareData.R")
#source("Functions.R")

# ---------------------------------
# Index calculations for IPUMS samples:
samples <- unique(DATA$SAMPLE)
N <- length(samples)
MalesInd <- FemalesInd <- TotalInd <- matrix(nrow=N,ncol=8)
colnames(MalesInd) <- colnames(FemalesInd) <- colnames(TotalInd) <- c("Whipple","Myers","Wtot","Wtot_mod","Wtot_mod_notrans","Wtot_mod_nomean","Wtot_mod_no2","Wtot_mod_log")
rownames(MalesInd) <- rownames(FemalesInd) <- rownames(TotalInd) <- samples
# let's make an indicator for pyramidy-shaped pyramids:
# proportion 20-40 of population aged 20-60
shape_m <- shape_f <- shape_tot <- vector(length=length(samples))
names(shape_m) <- names(shape_f) <- names(shape_tot) <- samples

for (i in 1:length(samples)){
	males 			<- DATA$population[DATA$SAMPLE==samples[i] & DATA$SEX=="Male"][1:90]
	females 		<- DATA$population[DATA$SAMPLE==samples[i] & DATA$SEX=="Female"][1:90]
	males[is.na(males)] 	<- 0
	females[is.na(females)] <- 0
	Total <- males+females
	# calculate each index for each population group
	MalesInd[i,] 	<- c(Whipple(males),MyersI(males),Wtot_calc(males),Wtot_mean_calc(males),Wtot_mean_calc_notrans(males),Wtot_calc_nomean(males),Wtot_mean_calc_no2(males),Wtot_mean_calc_log(males))
	FemalesInd[i,] 	<- c(Whipple(females),MyersI(females),Wtot_calc(females),Wtot_mean_calc(females),Wtot_mean_calc_notrans(females),Wtot_calc_nomean(females),Wtot_mean_calc_no2(females),Wtot_mean_calc_log(females))
	TotalInd[i,] 	<- c(Whipple(Total),MyersI(Total),Wtot_calc(Total),Wtot_mean_calc(Total),Wtot_mean_calc_notrans(Total),Wtot_calc_nomean(Total),Wtot_mean_calc_no2(Total),Wtot_mean_calc_log(Total))
	shape_m[i] 		<- sum(males[21:41])/sum(males[21:61])
	shape_f[i] 		<- sum(females[21:41])/sum(females[21:61])
	shape_tot[i] 	<- sum(Total[21:41])/sum(Total[21:61])
}
# the idea is to use shape as a regressor

#---------------------
# Properties
# all population on 0s:
POPtest <- rep(c(10,rep(0,9)),9)
Whipple(POPtest) 			# 5
MyersI(POPtest)  			# 90
Wtot_calc(POPtest) 			# 13
Wtot_mean_calc(POPtest) 	# 6.5
Wtot_mean_calc_log(POPtest) # 0.804719

# all population on 0s and 5s:
POPtest <- rep(c(10,0,0,0,0,10,0,0,0,0),9)
Whipple(POPtest) 			# 5
MyersI(POPtest)  			# 80
Wtot_calc(POPtest) 			# 16
Wtot_mean_calc(POPtest) 	# 8
Wtot_mean_calc_log(POPtest) # 1.609438

# population perfectly uniform:
POPtest <- rep(10,91)
Whipple(POPtest) 			# 1
MyersI(POPtest)  			# 0 
Wtot_calc(POPtest) 			# 0
Wtot_mean_calc(POPtest) 	# 0
Wtot_mean_calc_log(POPtest) # 0

# other strange situations:

# staggered every other age:
POPtest <- c(rep(c(0,1),45),0)
Whipple(POPtest) 			# 1 (not detected)
MyersI(POPtest) 			# 50 (turns up strongly)
Wtot_calc(POPtest) 			# 8.333 (turns up strongly)
Wtot_mean_calc(POPtest) 	# 4.1666 (exactly 1/2 of Wtot)
Wtot_mean_calc_log(POPtest) # 1.277064

POPtest <- c(rep(c(.5,1),45),.5)
Whipple(POPtest) 			# 1 (not detected)
MyersI(POPtest) 			# 16.66667 (turns up strongly)
Wtot_calc(POPtest) 			# 2.678571 (turns up strongly)
Wtot_mean_calc(POPtest) 	# 1.625 (not half)
Wtot_mean_calc_log(POPtest) # 1.399039

POPtest <- c(rep(c(.9,1),45),.9)
Whipple(POPtest) 			# 1
MyersI(POPtest) 			# 2.631579
Wtot_calc(POPtest) 			# 0.4210993
Wtot_mean_calc(POPtest) 	# 0.2152778 (not half, but almost)- smaller preferences lead to smaller discrepancies 
Wtot_mean_calc_log(POPtest) # 0.2107678
#------------------------------------
# .000001 is an extreme value... it gets caught because of the transform: when in the numerator it will
# cause a number less than 1 to be amplified by a degree of 1/number, which can get infintely large
# as the number approaches zero...
Wtot_mean_calc(rep(c(20,.0001,.0001,.0001,.0001,20,.0001,.0001,.0001,.0001),9)) # asymptotically infinity
# except this is not a problem since people are counted in whole numbers,
# so, beware, only use with integers

# approaches its maximum because it has no such transform
Wtot_calc(rep(c(20,.0001,.0001,.0001,.0001,20,.0001,.0001,.0001,.0001),9))

# does moving which digit is 20 have uniform results?
for (i in 1:10){
	digits <- rep(1,10)
	digits[i] <- 20
	POPtest <- rep(digits,9)  
	cat("\n--------------")
	cat("\n",c(0:10)[i],":")
	cat("\nWhipple: ",Whipple(POPtest))		
	cat("\nMyers: ",MyersI(POPtest))	
	cat("\nWtot:",Wtot_calc(POPtest))	
	cat("\nWtot_mod:",Wtot_mean_calc(POPtest))	
}
# Whipple is the only one that changes because it depends entirely on 5 and 0, not unrealistic

POPtest <- rep(c(5,1,1,1,1),18)
Whipple(POPtest) 			# 2.777778 # more than half of max value
MyersI(POPtest) 			# 35.55556 # less than half of max value
Wtot_calc(POPtest) 			# 7.111111 # less than half of max value
Wtot_mean_calc(POPtest) 	# 4.977778 # near max value of 8
Wtot_mean_calc_log(POPtest) # 3.372798


POPtest <- rep(c(10,1,1,1,1),18)
Whipple(POPtest) 			# 3.571429 # more than half of max value
MyersI(POPtest) 			# 51.42857 # more than half of max value
Wtot_calc(POPtest) 			# 10.28571 # more than half of max value
Wtot_mean_calc(POPtest) 	# 9.771429 # higher than when intermediate values are 0
Wtot_mean_calc_log(POPtest) # 5.391443

POPtest <- rep(c(10,3,3,3,3),18)
Whipple(POPtest) 			# 2.272727 # more than half of max value
MyersI(POPtest) 			# 25.45455 # more than half of max value
Wtot_calc(POPtest) 			# 5.090909 # more than half of max value
Wtot_mean_calc(POPtest) 	# 3.139394 # higher than when intermediate values are 0
Wtot_mean_calc_log(POPtest) # 2.35295

library(MASS)
# show be the bad ones function
badones <- function(mod,dat){
	d1 <- cooks.distance(mod)
	r <- stdres(mod)
	a <- cbind(dat, d1, r)
	a[d1 > 4/51, ]
}

library(car)
spm(cbind(shape_tot,TotalInd[,1:4]))
plot(shape_tot,TotalInd[,2]) #Myers
plot(shape_tot,TotalInd[,3]) #Wtot
plot(shape_tot,TotalInd[,4]) #Wtotmod -> shows much less dispersion (due to use of mean I presume)
# the shape indicator explains a large amount of the difference
# M1
M1tot <- lm(TotalInd[,4]~TotalInd[,3]+shape_tot)
M1m <- lm(MalesInd[,4]~MalesInd[,3]+shape_m)
M1f <- lm(FemalesInd[,4]~FemalesInd[,3]+shape_f)
summary(M1tot)
summary(M1m)
summary(M1f)
cdata <- cbind(Wtot=TotalInd[,4],WtotMod=TotalInd[,3],Shape=shape_tot)
badones(M1tot,cdata)
rm1 <- rownames(badones(M1tot,cdata))
# these all show Pakistan as an outlier with too much leverage: 
#  4 plots for each model, one model at a time
par(mfrow=c(2,2))
plot(M1tot) 
plot(M1m) 
plot(M1f) 
# what about if we pick out the extreme high values?
# I'll remove all Pakistan samples
# M2
keep1 <- !(rownames(TotalInd)%in% rm1)
M2tot <- lm(TotalInd[keep1,4]~TotalInd[keep1,3]+shape_tot[keep1])
M2m <- lm(MalesInd[keep1,4]~MalesInd[keep1,3]+shape_m[keep1])
M2f <- lm(FemalesInd[keep1,4]~FemalesInd[keep1,3]+shape_f[keep1])
# looks like Pakistan was determining everything
summary(M2tot)
summary(M2m)
summary(M2f)
cdata <- cbind(Wtot=TotalInd[keep1,4],WtotMod=TotalInd[keep1,3],Shape=shape_tot[keep1])
badones(M2tot,cdata)
rm2 <- c(rm1,rownames(badones(M2tot,cdata)))
#  4 plots for each model, one model at a time
par(mfrow=c(2,2))
plot(M2tot) 
plot(M2m) 
plot(M2f) 

# and removing 3 India samples? : moves shape regressor more
keep2 <- !(rownames(TotalInd) %in% rm2)
M3tot <- lm(TotalInd[keep2,4]~TotalInd[keep2,3]+shape_tot[keep2])
M3m <- lm(MalesInd[keep2,4]~MalesInd[keep2,3]+shape_m[keep2])
M3f <- lm(FemalesInd[keep2,4]~FemalesInd[keep2,3]+shape_f[keep2])
summary(M3tot)
summary(M3m)
summary(M3f)
cdata <- cbind(Wtot=TotalInd[keep2,4],WtotMod=TotalInd[keep2,3],Shape=shape_tot[keep2])
badones(M3tot,cdata)
#  flip through disgnostics
plot(M3tot) 
plot(M3m) 
plot(M3f) 
# then the shape parameter magically regains its power...
# this is getting interesting: let's make a colored scatterplot
# reds for high prop
# blues for low prop
propcols <- grDevices:::colorRampPalette(c("blue","red"))
propcol <- paste(propcols(length(samples)),60,sep="")
proptot_col <- propcol[order(shape_tot)]
par(mfrow=c(1,1))
plot(log(TotalInd[keep_ind2,4]),log(TotalInd[keep_ind2,3]),col=proptot_col[keep_ind2],main="I see no pattern to the colors...",pch=19)

# lets just do robust regression compare huber vs bisquare weights:
DatRedux <- data.frame(WtotMod = TotalInd[keep2,4], Wtot = TotalInd[keep2,3], Shape = shape_tot[keep2])
RLM1tot.hub <- rlm(WtotMod ~ Wtot + Shape,data=DatRedux)
RLM1tot.bisq <- rlm(WtotMod ~ Wtot + Shape,data=DatRedux, psi = psi.bisquare)
plot(RLM1tot.hub)
# function to show the 15 lowest weights used (the largest resids)
lookat15weights <- function(rlmmod){
	Weights <- data.frame(resid = rlmmod$resid, weight = rlmmod$w)
	Weights2 <- Weights[order(rlmmod$w), ]
	Weights2[1:15, ]
}
lookat15weights(RLM1tot.hub)
lookat15weights(RLM1tot.bisq)
anova(RLM1tot.hub,RLM1tot.bisq)