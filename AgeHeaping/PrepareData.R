# 
# 
# Author: triffe
###############################################################################

# code for reading in and preparing data
cat("\n#------------------------------------------------------#")
cat("\nBegin Data Prep\n")

DATA <- read.table("DATA\\IPUMS_Samples.txt",header=TRUE,sep=",")

# get column classes right:
DATA[,1] <- as.character(DATA[,1])
DATA[,3] <- as.character(DATA[,3])
DATA[,4] <- as.character(DATA[,4])
DATA[,5] <- as.character(DATA[,5])

# get ages numeric:
DATA$AGE[DATA$AGE=="Less than 1 year"] <- "0"
DATA$AGE[DATA$AGE=="1 year"] <- "1"
DATA$AGE[DATA$AGE=="2 years"] <- "2"
DATA$AGE[DATA$AGE=="100+"] <- "100"
DATA$AGE <- as.integer(DATA$AGE) # puts NAs in all the "Not reported/missing" (warning OK)

# ---------------------------
# remove some samples because ages are grouped:
# remove all: Iran, Ireland, Israel, Italy, Slovenia
DATA <- DATA[! DATA$CNTRY %in% c("Iran", "Ireland", "Israel", "Italy", "Slovenia","Palestine"),]

#-----------
samples <- unique(DATA$SAMPLE)
# used for iterating
# ----------------------------------------------------------------------
# impute missing or unknown sex:
# based on declared sex ratio within age
DATA2 <- data.frame(matrix(ncol=6,nrow=0))
colnames(DATA2) <- colnames(DATA)
tot <- 0; howbad <- c()
for (i in 1:length(samples)){
	Sampi <- DATA[DATA$SAMPLE==samples[i],]
	if (any(Sampi$SEX=="Unknown")){
		indall <- 1:nrow(Sampi)
		ind <- which(Sampi$SEX=="Unknown")
		ages <- Sampi$AGE[ind]
		males <- Sampi$population[Sampi$AGE%in%ages & Sampi$SEX=="Male"]
		females <- Sampi$population[Sampi$AGE%in%ages & Sampi$SEX=="Female"]
		# proportion males
		pm <- males/(males+females)
		Sampi$population[Sampi$AGE %in% ages & Sampi$SEX == "Male"]  <- males + pm * Sampi$population[ind]
		Sampi$population[Sampi$AGE %in% ages & Sampi$SEX == "Feale"]  <- females + (1-pm) * Sampi$population[ind]
		toti <- sum(Sampi$population[ind])
		tot <- tot + toti
		howbad[i] <- toti/(sum(Sampi$population)-toti)
		Sampi <- Sampi[!indall %in% ind,]
	}
	# this is a slow way to do things, but the data aren't that big:
	DATA2 <- rbind(DATA2,Sampi)
}

cat("\nsex imputed for a total of",nrow(DATA)-nrow(DATA2),"specific ages (between all samples)")
cat("\nfor a population of unknown sex of",tot,"or",round(100*tot/sum(DATA$population),digits=4),"percent of the entire population in question")
cat("\nthe worst sample for sex imputation was",samples[which.max(howbad)],"with",round(100*max(howbad,na.rm=TRUE),digits=4),"percent imputed.\n")

# ----------------------------------------------------------------------
# Impute missing or unknown ages according to sex-specific pdf of declared ages
DATA3 <- data.frame(matrix(ncol=6,nrow=0))
colnames(DATA3) <- colnames(DATA2)
tot <- 0; howbad <- c()

for (i in 1:length(samples)){
	Sampi <- DATA2[DATA2$SAMPLE==samples[i],]
	NASm <- NASf <- 0
	if (any(is.na(Sampi$AGE[Sampi$SEX=="Male"]))){
		NASm <- Sampi$population[Sampi$SEX=="Male" & is.na(Sampi$AGE)]
		indm <- Sampi$SEX=="Male" & !is.na(Sampi$AGE)
		males <- Sampi$population[indm]
		pdfm <- males/sum(males)
		Sampi$population[indm] <- males + pdfm * NASm
	}
	if (any(is.na(Sampi$AGE[Sampi$SEX=="Female"]))){
		NASf <- Sampi$population[Sampi$SEX=="Female" & is.na(Sampi$AGE)]
		indf <- Sampi$SEX=="Female" & !is.na(Sampi$AGE)
		females <- Sampi$population[indf]
		pdff <- females/sum(females)
		Sampi$population[indf] <- females + pdff * NASf
	}
	
	# redo for ages
	toti <- sum(NASm,NASf)
	tot <- tot + toti
	howbad[i] <- toti/(sum(Sampi$population)-toti)
	Sampi <- Sampi[!is.na(Sampi$AGE),]
	# this is a slow way to do things, but the data aren't that big:
	DATA3 <- rbind(DATA3,Sampi)
}

cat("\nage imputed a total of",nrow(DATA2)-nrow(DATA3),"times (between all samples)")
cat("\nfor a population of unknown sex of",tot,"or",round(100*tot/sum(DATA2$population),digits=4),"percent of the entire population in question")
cat("\nthe worst sample for sex imputation was",samples[which.max(howbad)],"with",round(100*max(howbad,na.rm=TRUE),digits=4),"percent imputed.\n")

# ---------------------------------------
# AGE and SEX should now be ready
DATA <- DATA3 ; 
# clean up:
rm(DATA2,DATA3,i,ind,indall,males,females,NASf,NASm,pm,samples,tot,toti,Sampi,indm,indf,pdfm,pdff,howbad)
gc()
cat("\n\nEnd Data Prep\nsee data.frame named 'DATA'")
cat("\n#------------------------------------------------------#\n")
cat("\nWarning here not a problem:\n")
