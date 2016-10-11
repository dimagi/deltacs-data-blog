
fullDataset <- read.csv("./data/deltacs_stats_by_user_month.csv")
primaryDataset <- fullDataset[fullDataset$form_count >= 3,]
datasetStartDate <- as.Date("2011-01-01")
datasetEndDate <- as.Date("2016-03-01")

# FUNCTIONS FOR ANALYSIS BY GEOGRAPHIC REGION

getDomainToLocationMapping <- function() {
	projectSpaceDataset <- read.csv("./data/project-space-list-from-hq.csv")
	
}

getCountryForDomain <- function(domainName) {
	projectSpaceDataset <- read.csv("./data/project-space-list-from-hq.csv")
	return(parseCountries(projectSpaceDataset[projectSpaceDataset$domain == domainName,]$deployment_countries))
}

# START HERE
parseCountries <- function(deploymentCountriesString) {
	indicesOfSingleQuotes <- 
	
	bracketsRemoved <- substring(deploymentCountriesString, 2, nchar(deploymentCountriesString)-1)
	separatorRemoved <- gsub("u", "", gsub(" u", "", bracketsRemoved))
	return(separatorRemoved)
}

getPercentageOfDomainsWithCountryData <- function() {
	projectSpaceDataset <- read.csv("./data/project-space-list-from-hq.csv")
	allDomainsFromFormsDataset <- unique(primaryDataset$domain)
	domainsInPrimaryDataset <- projectSpaceDataset[projectSpaceDataset$domain %in% allDomainsFromFormsDataset,]
	domainsWithCountryData <- domainsInPrimaryDataset[domainsInPrimaryDataset$deployment_countries != "No countries",]
	return(nrow(domainsWithCountryData) / length(allDomainsFromFormsDataset))
}


# FUNCTIONS FOR PLOT BY PROJECT

getLengthOfProjectInMonths <- function(domainName) {
	monthsForDomain <- primaryDataset[fullDataset$domain == domainName,]$month
	lastMonth <- max(as.Date(sortedMonthsForDomain))
	firstMonth <- min(as.Date(sortedMonthsForDomain))
	return(getDurationInMonths(firstMonth, lastMonth))
}

getLongestRunningProjects <- function(numProjects) {
	aggregatedByProjectLength <- aggregate(primaryDataset$domain, list(Domain=primaryDataset$domain), getLengthOfProjectInMonths)
	sorted <- aggregatedByProjectLength[order(aggregatedByProjectLength$x),]
	return(tail(sorted, n=numProjects))
}

plotDeltaCSByMonthForProject <- function(domainName) {
	dataForDomain <- primaryDataset[primaryDataset$domain == domainName,]
	yAxisData <- as.numeric(t(getMedianDeltaCSByMonth(dataForDomain)[2]))
	xAxisData <- seq(1, length(yAxisData))
	plot(xAxisData, yAxisData, pch = 20, col="blue", xlab="Month", ylab="Median DeltaCS Over A Month For Longest-Running Projects")
}


# FUNCTIONS FOR PLOT OF LONG-RUNNING USERS

getDurationOfUserActivityInMonths <- function(userID) {
    monthsForUser <- unique(primaryDataset[primaryDataset$user_id == userID,]$month)
    lastMonth <- max(as.Date(monthsForUser))
    firstMonth <- min(as.Date(monthsForUser))
    return(getDurationInMonths(firstMonth, lastMonth))
}

getUsersMeetingPercentageActivityThreshold <- function(percentageThreshold) {
    totalMonthsInDataset <- getDurationInMonths(datasetStartDate, datasetEndDate)
    aggregatedByUserDuration <- aggregate(primaryDataset$user_id, list(User=primaryDataset$user_id), getDurationOfUserActivityInMonths)
    return(aggregatedByUserDuration[(aggregatedByUserDuration$x/totalMonthsInDataset) >= percentageThreshold,])
}


# FUNCTIONS FOR PLOT OF ALL USERS

plotDeltaCSOverMonthForAllUsers <- function() {
	xAxisData <- seq(1,63)
	yAxisData <- as.numeric(t(getMedianDeltaCSByMonth()[2]))
	plot(xAxisData, yAxisData, pch = 20, col="blue", xlab="Month", ylab="Median DeltaCS over a Month (in hours)")
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
getMeanDeltaCSByMonth <- function(dataframe = primaryDataset) {
	aggregatedByMonth <- aggregate(dataframe$mean_hours, list(Month=dataframe$month), mean)
	return(aggregatedByMonth)
}

# in hours
getMedianDeltaCSByMonth <- function(dataframe = primaryDataset) {
	aggregatedByMonth <- aggregate(dataframe$median_hours, list(Month= dataframe$month), median)
	return(aggregatedByMonth)
}

getNumUniqueUsers <- function(dataframe = primaryDataset) {
	return(length(unique(dataframe$user_id)))
}

getNumUniqueDomains <- function(dataframe = primaryDataset) {
	return(length(unique(dataframe$domain)))
}

getTotalForms <- function(dataframe = primaryDataset) {
	return(sum(dataframe$form_count))
}

getOverallMean <- function(dataframe = primaryDataset) {
	return(mean(dataframe$mean_hours))
}

getOverallMedian <- function(dataframe = primaryDataset) {
	return(median(dataframe$median_hours))
}