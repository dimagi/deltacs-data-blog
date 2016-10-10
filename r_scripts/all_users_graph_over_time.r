
fullDataset <- read.csv("./data/deltacs_stats_by_user_month.csv")


# FUNCTIONS FOR PLOT BY PROJECT

getLengthOfProjectInDays <- function(domainName) {
	sortedMonthsForDomain <- sort(unique(fullDataset[fullDataset$domain == domainName,]$month))
	lastMonth <- sortedMonthsForDomain[length(sortedMonthsForDomain)]
	firstMonth <- sortedMonthsForDomain[1] 
	return(as.numeric(as.Date(lastMonth) - as.Date(firstMonth)))
}

getLongestRunningProjects <- function(numProjects) {
	aggregatedByProjectLength <- aggregate(fullDataset$domain, list(Domain=fullDataset$domain), getLengthOfProjectInDays)
	sorted <- aggregatedByProjectLength[order(aggregatedByProjectLength$x),]
	return(tail(sorted, n=numProjects))
}

plotDeltaCSByMonthForLongestRunningProjects <- function(useMean) {
}

plotDeltaCSByMonthForProject <- function(domainName) {
	dataForDomain <- fullDataset[fullDataset$domain == domainName,]
	yAxisData <- as.numeric(t(getMedianDeltaCSByMonth(dataForDomain)[2]))
	xAxisData <- seq(1, length(yAxisData))
	plot(xAxisData, yAxisData, pch = 20, col="blue", xlab="Month", ylab="Median DeltaCS Over A Month For Longest-Running Projects")
}


# FUNCTIONS FOR PLOT OF ALL USERS

plotDeltaCSOverMonthForAllUsers <- function(useMean) {
	xAxisData <- seq(1,63)
	if (useMean) {
		yAxisData <- as.numeric(t(getMeanDeltaCSByMonth()[2]))
		yLabel <- "Mean DeltaCS over a Month (in hours)"
	} else {
		yAxisData <- as.numeric(t(getMedianDeltaCSByMonth()[2]))
		yLabel <- "Median DeltaCS over a Month (in hours)"
	}
	plot(xAxisData, yAxisData, pch = 20, col="blue", xlab="Month", ylab=yLabel)
	#make custom axis
	#make trendline -- abline(lm(xAxisData ~ yAxisData)) ?
}


# BASIC FUNCTIONS

# in hours
getMeanDeltaCSByMonth <- function(dataframe = fullDataset) {
	aggregatedByMonth <- aggregate(dataframe$mean_hours, list(Month=dataframe$month), mean)
	return(aggregatedByMonth)
}

# in hours
getMedianDeltaCSByMonth <- function(dataframe = fullDataset) {
	aggregatedByMonth <- aggregate(dataframe$median_hours, list(Month= dataframe$month), median)
	return(aggregatedByMonth)
}

getNumUniqueUsers <- function() {
	return(length(unique(fullDataset$user_id)))
}

getNumUniqueDomains <- function() {
	return(length(unique(fullDataset$domain)))
}

getTotalForms <- function() {
	return(sum(fullDataset$form_count))
}

getOverallMean <- function() {
	return(mean(fullDataset$mean_hours))
}

getOverallMedian <- function() {
	return(median(fullDataset$median_hours))
}