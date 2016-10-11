
fullDataset <- read.csv("./data/deltacs_stats_by_user_month.csv")
datasetStartDate <- as.Date("2011-01-01")
datasetEndDate <- as.Date("2016-03-01")


# FUNCTIONS FOR PLOT BY PROJECT

getLengthOfProjectInMonths <- function(domainName) {
	sortedMonthsForDomain <- sort(unique(fullDataset[fullDataset$domain == domainName,]$month))
	lastMonth <- sortedMonthsForDomain[length(sortedMonthsForDomain)]
	firstMonth <- sortedMonthsForDomain[1] 
	return(getDurationInMonths(firstMonth, lastMonth))
}

getLongestRunningProjects <- function(numProjects) {
	aggregatedByProjectLength <- aggregate(fullDataset$domain, list(Domain=fullDataset$domain), getLengthOfProjectInMonths)
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


# FUNCTIONS FOR PLOT OF LONG-RUNNING USERS

getDurationOfUserActivityInMonths <- function(userID) {
    sortedMonthsForUser <- sort(unique(fullDataset[fullDataset$user_id == userID,]$month))
    lastMonth <- sortedMonthsForUser[length(sortedMonthsForUser)]
    firstMonth <- sortedMonthsForUser[1]
    return(getDurationInMonths(firstMonth, lastMonth))
}

getUsersMeetingPercentageActivityThreshold <- function(percentageThreshold) {
    totalMonthsInDataset <- getDurationInMonths(datasetStartDate, datasetEndDate)
    aggregatedByUserDuration <- aggregate(fullDataset$user_id, list(User=fullDataset$user_id), getDurationOfUserActivityInMonths)
    return(aggregatedByUserDuration[(aggregatedByUserDuration$x/totalDays) >= percentageThreshold,])
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


# UTIL FUNCTIONS

# Assumes that startMonth and endMonth are dates in the form of yyyy-mm-01, such that the date really
# represents a month in which a project or user was active, as opposed to a specific date on which they started/ended
getDurationInMonths <- function(startMonth, endMonth) {
	diffInWeeks <- as.numeric(difftime(as.Date(endMonth), as.Date(startMonth), units=c("weeks")))
	diffInMonths <- round(diffInWeeks/(52/12))  # divide by approx no. of weeks in a month
	# Add 1 month because the endMonth should count as a full month, even though the date used to represent it is the first of the month
	return(diffInMonths + 1) 
}

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