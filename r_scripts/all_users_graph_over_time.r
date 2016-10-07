
fullDataset <- read.csv("./data/deltacs_stats_by_user_month.csv")


plotScatterPlot <- function(useMean) {
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

# FUNCTIONS FOR PLOT BY PROJECT

getLengthOfProjectInDays <- function(domainName) {
	sortedMonthsForDomain <- sort(unique(fullDataset[fullDataset$domain == domainName,]$month))
	lastMonth <- sortedMonthsForDomain[length(sortedMonthsForDomain)]
	firstMonth <- sortedMonthsForDomain[1] 
	return(as.numeric(as.Date(lastMonth) - as.Date(firstMonth)))
}

getLongestRunningProjects <- function(numProjects) {
	projectsInOrderOfLength <- fullDataset[order(getLengthOfProjectInDays(fullDataset$domain))]
	return(projectsInOrderOfLength)
}


# FUNCTIONS FOR ALL PLOT OF ALL USERS

getMeanDeltaCSByMonth <- function() {
	aggregatedByMonth <- aggregate(fullDataset$mean_hours, list(Month=fullDataset$month), mean)
	return(aggregatedByMonth)
}

getMedianDeltaCSByMonth <- function() {
	aggregatedByMonth <- aggregate(fullDataset$median_hours, list(Month=fullDataset$month), median)
	return(aggregatedByMonth)
}

# BASIC FUNCTIONS

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