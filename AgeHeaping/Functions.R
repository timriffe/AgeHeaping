# TODO: Add comment
# 
# Author: triffe
###############################################################################
# Whipple

Whipple <- function(Pop,Age=0:90,Minage=23,Maxage=62){
	if (missing(Age)) { 
		Age <- 0:(length(Pop)-1)
	}
	denom <- sum(Pop[Age <= Maxage & Age >= Minage])
	num <- 5*sum(Pop[Age %% 10 %in% c(0,5) & Age <= Maxage & Age >= Minage])
	num/denom
}

# Myers blended:
MyersI <- function(Pop,ages){
	if (missing(ages)) { 
		ages <- 0:(length(Pop)-1)
	}
	# ul = upper limit = largest age evenly divisible by 10 up to 100
	# i.e if highest age is 85, then it spits back 80
	ul <- min(c(max(ages[ages %% 10 == 0]), 100))
	# indices to pick out 2 tabulations
	# ul-10 inserted to ensure possibility of acheiving minimum of zero
	ind1 <- ages >= 10 & ages < (ul-10)
	ind2 <- ages >= 20 & ages < ul
	# sum by digits, in this case picked out by modulars (%%)
	tab1 <- tapply(Pop[ind1],ages[ind1] %% 10,sum)
	tab2 <- tapply(Pop[ind2],ages[ind2] %% 10,sum)
	# weighted tabulation
	TAB <- tab1 * 1:10 + tab2 * 9:0
	# interpret as % that would need to be redistributed...
	sum(abs(TAB/sum(TAB) - .1)) * 50
}

# from Spoorenberg:
Wtot_calc <- function(Pop,Ages=0:90,Minage=23,Maxage=62){
	
	# used to shift the indices in question in order to
	# grab the relevant 5-year age groups
	lagger <- function(series,lag){
		if (lag != 0){
			if (lag > 0){
				series <- c(rep(FALSE,lag),series[-c((length(series)-lag+1):length(series))])
			}
			if (lag < 0){
				series <- c(series[-c(1:abs(lag))],rep(FALSE,abs(lag)))
			}
		}
		series
	}
	
	# I couldn't remember the better way to vectorize indexing
	Getthe5s <- function(ind,x){
		x[ind]
	}
	
	# calculates W for a given i (using the proportion centered on the given digit +/-2
	Wi_calc <- function(i,ages=Ages,pop=Pop,minage=Minage,maxage=Maxage){
		if (missing(ages)) { 
			ages <- 0:(length(pop)-1)
		}
		ind_i  <- ages %% 10 == i & ages >= minage & ages <= maxage
		ind_I <- sapply(-2:2,lagger,series=ind_i)
		out <- (5*sum(pop[ind_i]))/sum(apply(ind_I,2,Getthe5s,x=pop))
		if (is.na(out)){
			out <- 0
		}
		out
	}
	
	# so if we do Wi_calc for each digit, take the absolute value of the difference from 1, and then sum:
	sum(abs(sapply(0:9,Wi_calc,minage=Minage,maxage=Maxage,ages=Ages)-1))
	# this number is Spoorenberg's Wtot
}

# Riffe, modified Wtot in 3 ways:
Wtot_mean_calc <- function(Pop,Ages=0:90,Minage=23,Maxage=62){
	
	# used to shift the indices in question in order to
	# grab the relevant 5-year age groups
	lagger <- function(series,lag){
		if (lag != 0){
			if (lag > 0){
				series <- c(rep(FALSE,lag),series[-c((length(series)-lag+1):length(series))])
			}
			if (lag < 0){
				series <- c(series[-c(1:abs(lag))],rep(FALSE,abs(lag)))
			}
		}
		series
	}
	# I couldn't remember the better way to vectorize indexing
	Getthe5s <- function(ind,x){
		x[ind]
	}
	# changes: 1) assure equal magnitude for ratios <1 and >1 
	#          2) take each ratio separately and calculate the mean
	#          3) take deviation from 1 (always positive) 
	
	Wi_mean_calc <- function(i,ages=Ages,pop=Pop,minage=Minage,maxage=Maxage){
		
		if (missing(ages)) { 
			ages <- 0:(length(pop)-1)
		}
		ind_i  <- ages %% 10 == i & ages >= minage & ages <= maxage
		ind_I <- sapply(-2:2,lagger,series=ind_i)
		ratio <- (5*pop[ind_i])/rowSums(apply(ind_I,2,Getthe5s,x=pop))
		trans <- exp(abs(log(ratio)))
		# infintes means that the function tried to take the log of zero, in which case a zero
		# should ultimately come back: messes up due to internals : exp(log(0)) returns zero
		# but exp(abs(log(0))) returns Inf
		trans[is.infinite(trans)|is.na(trans)] <- 0
		mean(trans)-1
	}
	# take sum of these (strictly positive) Wi's and divide by 2
	sum(abs(sapply(0:9,Wi_mean_calc,minage=Minage,maxage=Maxage,ages=Ages,pop=Pop)))/2
	# this number is my modified Wtot
}

# Riffe, modified so that Wi can be compared with Wtot Wi's
Wtot_mean_calc2 <- function(Pop,Ages=0:90,Minage=23,Maxage=62){
	
	# used to shift the indices in question in order to
	# grab the relevant 5-year age groups
	lagger <- function(series,lag){
		if (lag != 0){
			if (lag > 0){
				series <- c(rep(FALSE,lag),series[-c((length(series)-lag+1):length(series))])
			}
			if (lag < 0){
				series <- c(series[-c(1:abs(lag))],rep(FALSE,abs(lag)))
			}
		}
		series
	}
	# I couldn't remember the better way to vectorize indexing
	Getthe5s <- function(ind,x){
		x[ind]
	}
	# changes: 1) assure equal magnitude for ratios <1 and >1 
	#          2) take each ratio separately and calculate the mean
	#          3) take deviation from 1 (always positive) 
	Wi_mean_calc <- function(i,ages=Ages,pop=Pop,minage=Minage,maxage=Maxage){
		if (missing(ages)) { 
			ages <- 0:(length(pop)-1)
		}
		ind_i  <- ages %% 10 == i & ages >= minage & ages <= maxage
		ind_I <- sapply(-2:2,lagger,series=ind_i)
		ratio <- (5*pop[ind_i])/rowSums(apply(ind_I,2,Getthe5s,x=pop))
		sign(mean(ratio)-1)*(mean(exp(abs(log(ratio))))-1)
	}
	# take sum of these (strictly positive) Wi's and divide by 2
	sum(abs(sapply(0:9,Wi_mean_calc,minage=Minage,maxage=Maxage,ages=Ages,pop=Pop)))/2
	# this number is my modified Wtot
}

# Riffe, modified with no exp(abs(log(ratio))) transform
Wtot_mean_calc_notrans <- function(Pop,Ages=0:90,Minage=23,Maxage=62){
	
	# used to shift the indices in question in order to
	# grab the relevant 5-year age groups
	lagger <- function(series,lag){
		if (lag != 0){
			if (lag > 0){
				series <- c(rep(FALSE,lag),series[-c((length(series)-lag+1):length(series))])
			}
			if (lag < 0){
				series <- c(series[-c(1:abs(lag))],rep(FALSE,abs(lag)))
			}
		}
		series
	}
	# I couldn't remember the better way to vectorize indexing
	Getthe5s <- function(ind,x){
		x[ind]
	}
	# changes: 1) assure equal magnitude for ratios <1 and >1 
	#          2) take each ratio separately and calculate the mean
	#          3) take deviation from 1 (always positive) 
	Wi_mean_calc <- function(i,ages=Ages,pop=Pop,minage=Minage,maxage=Maxage){
		if (missing(ages)) { 
			ages <- 0:(length(pop)-1)
		}
		ind_i  <- ages %% 10 == i & ages >= minage & ages <= maxage
		ind_I <- sapply(-2:2,lagger,series=ind_i)
		ratio <- (5*pop[ind_i])/rowSums(apply(ind_I,2,Getthe5s,x=pop))
		mean(ratio)-1
	}
	# take sum of these (strictly positive) Wi's and divide by 2
	sum(abs(sapply(0:9,Wi_mean_calc,minage=Minage,maxage=Maxage,ages=Ages,pop=Pop)))/2
	# this number is my modified Wtot
}

# Riffe, modified with no mean (only transform and divide by 2)
Wtot_calc_nomean <- function(Pop,Ages=0:90,Minage=23,Maxage=62){
	
	# used to shift the indices in question in order to
	# grab the relevant 5-year age groups
	lagger <- function(series,lag){
		if (lag != 0){
			if (lag > 0){
				series <- c(rep(FALSE,lag),series[-c((length(series)-lag+1):length(series))])
			}
			if (lag < 0){
				series <- c(series[-c(1:abs(lag))],rep(FALSE,abs(lag)))
			}
		}
		series
	}
	# I couldn't remember the better way to vectorize indexing
	Getthe5s <- function(ind,x){
		x[ind]
	}
	# changes: 1) assure equal magnitude for ratios <1 and >1 
	#          2) take each ratio separately and calculate the mean
	#          3) take deviation from 1 (always positive) 
	Wi_mean_calc <- function(i,ages=Ages,pop=Pop,minage=Minage,maxage=Maxage){
		if (missing(ages)) { 
			ages <- 0:(length(pop)-1)
		}
		ind_i  <- ages %% 10 == i & ages >= minage & ages <= maxage
		ind_I <- sapply(-2:2,lagger,series=ind_i)
		ratio <- sum(5*pop[ind_i])/sum(apply(ind_I,2,Getthe5s,x=pop))
		mean(exp(abs(log(ratio))))-1
	}
	# take sum of these (strictly positive) Wi's and divide by 2
	sum(sapply(0:9,Wi_mean_calc,minage=Minage,maxage=Maxage,ages=Ages,pop=Pop))/2
	# this number is my modified Wtot
}

# Riffe, modified with no division by 2 (onlt transform and mean)
Wtot_mean_calc_no2 <- function(Pop,Ages=0:90,Minage=23,Maxage=62){
	
	# used to shift the indices in question in order to
	# grab the relevant 5-year age groups
	lagger <- function(series,lag){
		if (lag != 0){
			if (lag > 0){
				series <- c(rep(FALSE,lag),series[-c((length(series)-lag+1):length(series))])
			}
			if (lag < 0){
				series <- c(series[-c(1:abs(lag))],rep(FALSE,abs(lag)))
			}
		}
		series
	}
	# I couldn't remember the better way to vectorize indexing
	Getthe5s <- function(ind,x){
		x[ind]
	}
	# changes: 1) assure equal magnitude for ratios <1 and >1 
	#          2) take each ratio separately and calculate the mean
	#          3) take deviation from 1 (always positive) 
	Wi_mean_calc <- function(i,ages=Ages,pop=Pop,minage=Minage,maxage=Maxage){
		if (missing(ages)) { 
			ages <- 0:(length(pop)-1)
		}
		ind_i  <- ages %% 10 == i & ages >= minage & ages <= maxage
		ind_I <- sapply(-2:2,lagger,series=ind_i)
		ratio <- (5*pop[ind_i])/rowSums(apply(ind_I,2,Getthe5s,x=pop))
		mean(exp(abs(log(ratio))))-1
	}
	# take sum of these (strictly positive) Wi's and divide by 2
	sum(sapply(0:9,Wi_mean_calc,minage=Minage,maxage=Maxage,ages=Ages,pop=Pop))
	# this number is my modified Wtot
}

# Riffe, modified only log transform (equal weight for above and below parity, dampening of extremes)
Wtot_mean_calc_log <- function(Pop,Ages=0:90,Minage=23,Maxage=62){
	
	# used to shift the indices in question in order to
	# grab the relevant 5-year age groups
	lagger <- function(series,lag){
		if (lag != 0){
			if (lag > 0){
				series <- c(rep(FALSE,lag),series[-c((length(series)-lag+1):length(series))])
			}
			if (lag < 0){
				series <- c(series[-c(1:abs(lag))],rep(FALSE,abs(lag)))
			}
		}
		series
	}
	# I couldn't remember the better way to vectorize indexing
	Getthe5s <- function(ind,x){
		x[ind]
	}
	# changes: 1) assure equal magnitude for ratios <1 and >1 
	#          2) take each ratio separately and calculate the mean
	#          3) take deviation from 1 (always positive) 
	Wi_mean_calc <- function(i,ages=Ages,pop=Pop,minage=Minage,maxage=Maxage){
		if (missing(ages)) { 
			ages <- 0:(length(pop)-1)
		}
		ind_i  <- ages %% 10 == i & ages >= minage & ages <= maxage
		ind_I <- sapply(-2:2,lagger,series=ind_i)
		ratio <- (5*pop[ind_i])/rowSums(apply(ind_I,2,Getthe5s,x=pop))
		logged <- log(ratio)
		logged[is.na(logged) | is.infinite(logged)] <- 0
		mean(logged)
	}
	# take sum of these (strictly positive) Wi's and divide by 2
	sum(abs(sapply(0:9,Wi_mean_calc,minage=Minage,maxage=Maxage,ages=Ages,pop=Pop)))/2
	# this number is my modified Wtot
}
