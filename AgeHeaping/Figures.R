# 
# 
# Author: triffe
###############################################################################
DATA[DATA$SAMPLE=="Senegal 1988",]
setwd("E:\\CODE\\AgeHeaping\\") # the only thing you'd actually need to change

# these need to be run in order prior to producing any plots, might take a minute to run
source("PrepareData.R")
source("Functions.R")
source("Analysis.R") # worth looking at on its own for properties

# necessary to produce big awesome pdf: (latest version uploaded a week ago)
library(Pyramid)
ls()
# makes a 150-page pdf of ALL 149 population pyramids, starting with a scatterplot
samples <- unique(DATA$SAMPLE)
panel.hist <- function(x,y,...){
	usr <- par("usr"); on.exit(par(usr))
	par(usr = c(usr[1:2], 0, 1.5) )
	h <- hist(x, plot = FALSE,breaks=25)
	breaks <- h$breaks; nB <- length(breaks)
	y <- h$counts; y <- y/max(y)
	rect(breaks[-nB], 0, breaks[-1], y, col="grey", ...)
}

panel.plot <- function(x, y, ...)
{
	points(x,y,col="black",...)
	#points(x[i],y[i],pch=19,col="red")
}
pdf(height=7,width=9,"IPUMS Pyramids.pdf")
pairs(TotalInd[,c(1,2,3,4,8)],diag.panel=panel.hist,panel=panel.plot)
for (i in 1:length(samples)){
	# just ages 1:89
	Sampli <- DATA[DATA$SAMPLE==samples[i],]
	males <- Sampli$population[Sampli$SEX=="Male"][1:90]
	females <- Sampli$population[Sampli$SEX=="Female"][1:90]
	males[is.na(males)] <- 0
	females[is.na(females)] <- 0
	x <- round(TotalInd[i,],digits=3)
	Pyramid(males,
			females,
			verbose=FALSE,
			main=samples[i],
			border.males="black",
			border.females="black",
			fill.males="cadetblue2",
			fill.females="bisque1",
			xlim=c(-2.5,2.5),
			mar = c(5,5,5,12))
	segments(2.7,95,4.5,95,xpd=TRUE)
	text(3.5,(10:5)*10,c("METHOD","Whipple","Myers","Wtot","Wmod","Wmon log"),pos=2,xpd=TRUE)
	text(3.45,(10:5)*10,c("VALUE",x[c(1,2,3,4,8)]),pos=4,xpd=TRUE)
	text(4,(10:5)*10,c("RANK",as.numeric(rank(TotalInd[,1])[i]),
					as.numeric(rank(TotalInd[,2])[i]),
					as.numeric(rank(TotalInd[,3])[i]),
					as.numeric(rank(TotalInd[,4])[i]),
					as.numeric(rank(TotalInd[,8])[i])),pos=4,xpd=TRUE)
	text(c(-.5,.5),-10,c("Males","Females"),xpd=TRUE,pos=c(2,4))
}
dev.off()

# same PDF ordered by Myers rank
samplesm <- names(sort(TotalInd[,2]))

pdf(height=7,width=9,"IPUMS Pyramids_MyersRank.pdf")
pairs(TotalInd[,c(1,2,3,4,8)],diag.panel=panel.hist,panel=panel.plot)
for (i in 1:length(samplesm)){
	# just ages 1:89i
	Sampli <- DATA[DATA$SAMPLE==samplesm[i],]
	males <- Sampli$population[Sampli$SEX=="Male"][1:90]
	females <- Sampli$population[Sampli$SEX=="Female"][1:90]
	males[is.na(males)] <- 0
	females[is.na(females)] <- 0
	ind <- rownames(TotalInd)==samplesm[i]
	x <- round(TotalInd[ind,],digits=3)
	Pyramid(males,
			females,
			verbose=FALSE,
			main=samplesm[i],
			border.males="black",
			border.females="black",
			fill.males="cadetblue2",
			fill.females="bisque1",
			xlim=c(-2.5,2.5),
			mar = c(5,5,5,12))
	segments(2.7,95,4.5,95,xpd=TRUE)
	text(3.5,(10:5)*10,c("METHOD","Whipple","Myers","Wtot","Wmod","Wmon log"),pos=2,xpd=TRUE)
	text(3.45,(10:5)*10,c("VALUE",x[c(1,2,3,4,8)]),pos=4,xpd=TRUE)
	text(4,(10:5)*10,c("RANK",as.numeric(rank(TotalInd[,1])[ind]),
					as.numeric(rank(TotalInd[,2])[ind]),
					as.numeric(rank(TotalInd[,3])[ind]),
					as.numeric(rank(TotalInd[,4])[ind]),
					as.numeric(rank(TotalInd[,8])[ind])),pos=4,xpd=TRUE)
	text(c(-.5,.5),-10,c("Males","Females"),xpd=TRUE,pos=c(2,4))
}
dev.off()

# same PDF ordered by Wtot rank
samplesw <- names(sort(TotalInd[,3]))
pdf(height=7,width=9,"IPUMS Pyramids_WtotRank.pdf")
pairs(TotalInd[,c(1,2,3,4,8)],diag.panel=panel.hist,panel=panel.plot)
for (i in 1:length(samplesw)){
	# just ages 1:89i
	Sampli <- DATA[DATA$SAMPLE==samplesw[i],]
	males <- Sampli$population[Sampli$SEX=="Male"][1:90]
	females <- Sampli$population[Sampli$SEX=="Female"][1:90]
	males[is.na(males)] <- 0
	females[is.na(females)] <- 0
	ind <- rownames(TotalInd)==samplesw[i]
	x <- round(TotalInd[ind,],digits=3)
	Pyramid(males,
			females,
			verbose=FALSE,
			main=samplesw[i],
			border.males="black",
			border.females="black",
			fill.males="cadetblue2",
			fill.females="bisque1",
			xlim=c(-2.5,2.5),
			mar = c(5,5,5,12))
	segments(2.7,95,4.5,95,xpd=TRUE)
	text(3.5,(10:5)*10,c("METHOD","Whipple","Myers","Wtot","Wmod","Wmon log"),pos=2,xpd=TRUE)
	text(3.45,(10:5)*10,c("VALUE",x[c(1,2,3,4,8)]),pos=4,xpd=TRUE)
	text(4,(10:5)*10,c("RANK",as.numeric(rank(TotalInd[,1])[ind]),
					as.numeric(rank(TotalInd[,2])[ind]),
					as.numeric(rank(TotalInd[,3])[ind]),
					as.numeric(rank(TotalInd[,4])[ind]),
					as.numeric(rank(TotalInd[,8])[ind])),pos=4,xpd=TRUE)
	text(c(-.5,.5),-10,c("Males","Females"),xpd=TRUE,pos=c(2,4))
}
dev.off()
# same PDF ordered by Wtot mod rank
sampleswm <- names(sort(TotalInd[,4]))
pdf(height=7,width=9,"IPUMS Pyramids_WtotModRank.pdf")
pairs(TotalInd[,c(1,2,3,4,8)],diag.panel=panel.hist,panel=panel.plot)
for (i in 1:length(sampleswm)){
	# just ages 1:89i
	Sampli <- DATA[DATA$SAMPLE==sampleswm[i],]
	males <- Sampli$population[Sampli$SEX=="Male"][1:90]
	females <- Sampli$population[Sampli$SEX=="Female"][1:90]
	males[is.na(males)] <- 0
	females[is.na(females)] <- 0
	ind <- rownames(TotalInd)==sampleswm[i]
	x <- round(TotalInd[ind,],digits=3)
	Pyramid(males,
			females,
			verbose=FALSE,
			main=sampleswm[i],
			border.males="black",
			border.females="black",
			fill.males="cadetblue2",
			fill.females="bisque1",
			xlim=c(-2.5,2.5),
			mar = c(5,5,5,12))
	segments(2.7,95,4.5,95,xpd=TRUE)
	text(3.5,(10:5)*10,c("METHOD","Whipple","Myers","Wtot","Wmod","Wmon log"),pos=2,xpd=TRUE)
	text(3.45,(10:5)*10,c("VALUE",x[c(1,2,3,4,8)]),pos=4,xpd=TRUE)
	text(4,(10:5)*10,c("RANK",as.numeric(rank(TotalInd[,1])[ind]),
					as.numeric(rank(TotalInd[,2])[ind]),
					as.numeric(rank(TotalInd[,3])[ind]),
					as.numeric(rank(TotalInd[,4])[ind]),
					as.numeric(rank(TotalInd[,8])[ind])),pos=4,xpd=TRUE)
	text(c(-.5,.5),-10,c("Males","Females"),xpd=TRUE,pos=c(2,4))
}
dev.off()
# same PDF ordered by Wtot mod rank
sampleswml <- names(sort(TotalInd[,8]))
pdf(height=7,width=9,"IPUMS Pyramids_WtotmodlogRank.pdf")
pairs(TotalInd[,c(1,2,3,4,8)],diag.panel=panel.hist,panel=panel.plot)
for (i in 1:length(sampleswml)){
	# just ages 1:89i
	Sampli <- DATA[DATA$SAMPLE==sampleswml[i],]
	males <- Sampli$population[Sampli$SEX=="Male"][1:90]
	females <- Sampli$population[Sampli$SEX=="Female"][1:90]
	males[is.na(males)] <- 0
	females[is.na(females)] <- 0
	ind <- rownames(TotalInd)==sampleswml[i]
	x <- round(TotalInd[ind,],digits=3)
	Pyramid(males,
			females,
			verbose=FALSE,
			main=sampleswml[i],
			border.males="black",
			border.females="black",
			fill.males="cadetblue2",
			fill.females="bisque1",
			xlim=c(-2.5,2.5),
			mar = c(5,5,5,12))
	segments(2.7,95,4.5,95,xpd=TRUE)
	text(3.5,(10:5)*10,c("METHOD","Whipple","Myers","Wtot","Wmod","Wmon log"),pos=2,xpd=TRUE)
	text(3.45,(10:5)*10,c("VALUE",x[c(1,2,3,4,8)]),pos=4,xpd=TRUE)
	text(4,(10:5)*10,c("RANK",as.numeric(rank(TotalInd[,1])[ind]),
					as.numeric(rank(TotalInd[,2])[ind]),
					as.numeric(rank(TotalInd[,3])[ind]),
					as.numeric(rank(TotalInd[,4])[ind]),
					as.numeric(rank(TotalInd[,8])[ind])),pos=4,xpd=TRUE)
	text(c(-.5,.5),-10,c("Males","Females"),xpd=TRUE,pos=c(2,4))
}
dev.off()
# distributions:
# males
par(mfrow=c(2,2))
for (i in 1:4){
	hist(MalesInd[,i],
			breaks=20,
			main=colnames(MalesInd)[i],
			xlab="Index Value",
			ylab="frequency")
}

# females
par(mfrow=c(2,2))
for (i in 1:4){
	hist(FemalesInd[,i],
			breaks=20,
			main=colnames(FemalesInd)[i],
			xlab="Index Value",
			ylab="frequency")
}

# total
par(mfrow=c(2,2))
for (i in 1:4){
	hist(TotalInd[,i],
			breaks=20,
			main=colnames(TotalInd)[i],
			xlab="Index Value",
			ylab="frequency")
}

# pairs:
pairs(TotalInd[,1:4])
pairs(FemalesInd[,1:4])
pairs(MalesInd[,1:4])

# population pyramid shape variable: does the use of mean make a difference?
# they're all bimodal
hist(shape_m,breaks=20,main="distribution of pyramid shape indicator, (20P20)/(40P20), males")
hist(shape_f,breaks=20,main="distribution of pyramid shape indicator, (20P20)/(40P20), females")
hist(shape_tot,breaks=20,main="distribution of pyramid shape indicator, (20P20)/(40P20), total pop")


# from now on, all comparisons will just involve the Total pop because otherwise this will get tedious...
#


