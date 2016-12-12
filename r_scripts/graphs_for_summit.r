fullDataset <- read.csv("./data/deltacs_stats_by_user_month.csv")
hqProjectSpaceDataset <- read.csv("./data/project-space-list-from-hq.csv")
primaryDataset <- fullDataset[fullDataset$form_count >= 3,]
attritionDatasetOrig <- read.csv("./data/attritionStats.csv")
attritionDataset <- attritionDatasetOrig[attritionDatasetOrig$domain %in% unique(primaryDataset$domain) & attritionDatasetOrig$totalUserMonths.12months >= 50,]
attritionDataset[["domain"]] <- unlist(lapply(attritionDataset[["domain"]], toString))
datasetStartDate <- as.Date("2011-01-01")
datasetEndDate <- as.Date("2016-03-01")

# Based upon UN Geoscheme
NORTHERN_AFRICA <- c("SUDAN", "ALGERIA","EGYPT")
EASTERN_AFRICA <- c("MOZAMBIQUE", "KENYA", "MALAWI", "UGANDA", "ZIMBABWE", "MADAGASCAR", "ETHIOPIA", "RWANDA", "TANZANIA, UNITED REPUBLIC OF", "ZAMBIA", "BURUNDI", "SOUTH SUDAN", "MAURITIUS")
WESTERN_AFRICA <- c("MALI", "TOGO", "GUINEA", "SENEGAL", "BENIN", "GHANA", "LIBERIA", "NIGERIA", "SIERRA LEONE", "NIGER", "BURKINA FASO", "GAMBIA", "C\\xd4TE D'IVOIRE")
MIDDLE_AFRICA <- c("ANGOLA", "CAMEROON", "CHAD")
SOUTHERN_AFRICA <- c("SOUTH AFRICA", "BOTSWANA", "NAMIBIA", "LESOTHO", "SWAZILAND")
SOUTHERN_ASIA <- c("NEPAL", "INDIA", "BANGLADESH", "PAKISTAN", "SRI LANKA", "AFGHANISTAN")
WESTERN_ASIA <- c("IRAQ", "SYRIAN ARAB REPUBLIC", "JORDAN", "LEBANON", "TURKEY")
SOUTHEASTERN_ASIA <- c("THAILAND", "VIET NAM", "LAO PEOPLE'S DEMOCRATIC REPUBLIC", "MYANMAR", "CAMBODIA", "PHILIPPINES", "MALAYSIA", "TIMOR-LESTE")
EASTERN_ASIA <- c("CHINA", "KOREA (THE REPUBLIC OF)")
SOUTH_AMERICA <- c("BRAZIL", "PERU", "BOLIVIA (PLURINATIONAL STATE OF)", "COLOMBIA", "ECUADOR")
NORTHERN_AMERICA <- c("UNITED STATES OF AMERICA", "CANADA")
CENTRAL_AMERICA <- c("MEXICO", "HONDURAS", "GUATEMALA", "NICARAGUA", "BELIZE", "EL SALVADOR")
CARIBBEAN <- c("HAITI", "GRENADA", "DOMINICAN REPUBLIC")
EUROPE <- c("UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND", "SPAIN", "FRANCE")
OCEANIA <- c("PAPUA NEW GUINEA", "VANUATU")

# FUNCTIONS FOR ATTRITION ANALYSIS

getBucketForAttritionAnalysis <- function(bucketNumber) {
	sortedEligibleProjects <- projectsEligibleForAttritionAnalysisSortedByDeltaCS()$domain
	if (bucketNumber == 1) {
		return(sortedEligibleProjects[1:87])
	} else if (bucketNumber == 2) {
		return(sortedEligibleProjects[88:174])
	} else if (bucketNumber == 3) {
		return(sortedEligibleProjects[175:262])
	} else {
		return(sortedEligibleProjects[263:350])
	}
}

getProjectsWithHighestXPercentDeltaCS <- function(percentage) {
	sortedEligibleProjects <- projectsEligibleForAttritionAnalysisSortedByDeltaCS()$domain
	count <- 350 * percentage
	return(tail(sortedEligibleProjects, count))
}

getProjectsWithLowestXPercentDeltaCS <- function(percentage) {
	sortedEligibleProjects <- projectsEligibleForAttritionAnalysisSortedByDeltaCS()$domain
	count <- 350 * percentage
	return(head(sortedEligibleProjects, count))
}

projectsEligibleForAttritionAnalysisSortedByDeltaCS <- function() {
	eligibleProjectsWithAttritionData <- attritionDataset[attritionDataset$monthsFromStartToCurrent >= 12,]$domain
	deltaCSValues <- unlist(lapply(eligibleProjectsWithAttritionData, getMedianDeltaCSForProject))
	combinedDataFrame <- data.frame(domain=eligibleProjectsWithAttritionData, deltacs=deltaCSValues)
	sorted <- combinedDataFrame[order(combinedDataFrame$deltacs),]
	return(sorted)
}

get12MonthSurvivalRateByBucket <- function(bucketNumber) {
	projectsInBucket <- getBucketForAttritionAnalysis(bucketNumber)
	return(get12MonthSurvivalRate(projectsInBucket))
}

get12MonthSurvivalRate <- function(setOfProjects) {
	survivedProjects <- attritionDataset[attritionDataset$domain %in% setOfProjects & attritionDataset$activeLastQuarter.12months == 1,]
	return(nrow(survivedProjects)/length(setOfProjects))
}

get24MonthSurvivalRateByBucket <- function(bucketNumber) {
	projectsInBucket <- getBucketForAttritionAnalysis(bucketNumber)
	return(get24MonthSurvivalRate(projectsInBucket))
}

get24MonthSurvivalRate <- function(setOfProjects) {
	eligibleProjects <- attritionDataset[attritionDataset$domain %in% setOfProjects & attritionDataset$monthsFromStartToCurrent >= 24,]
	return(toString(nrow(eligibleProjects)))
	#survivedProjects <- eligibleProjects[eligibleProjects$activeLastQuarter.24months == 1,]
	#return(nrow(survivedProjects)/nrow(eligibleProjects))
}


# FUNCTIONS FOR HISTOGRAMS

plotAllUsersHistogram <- function(dataset, title, bucketsVector) {
	hist(dataset$median_hours, 
		breaks=bucketsVector, 
		plot=TRUE, freq=TRUE, col="lightblue",
		main=title,
		xlab="Median DeltaCS (in hours)",
		ylab="Count")
}

histForLastNMonths <- function(numMonths) {
	plotAllUsersHistogram(getDataForLastNMonths(numMonths), paste("Median DeltaCS over Last", numMonths, "Months"), getFullRangeBucketsVector())
}

ecdfForDataFromLastNMonths <- function(numMonths, title, xmax) {
	P = ecdf(getDataForLastNMonths(numMonths)$median_hours)
	plot(P, xlim=c(0, xmax), main=title)
}

histForLastNMonthsWithMaxDeltaCS <- function(numMonths, maxDeltaCSValue) {
	plotAllUsersHistogram(
		getDataForLastNMonthsWithMaxDeltaCS(numMonths, maxDeltaCSValue), 
		paste("Median DeltaCS over Last", numMonths, "Months (Capped at DeltaCS of", maxDeltaCSValue, "Hours)"), 
		getBucketsVectorUpTo(maxDeltaCSValue)
	)
}

histForLastNMonthsWithMaxDeltaCSOfOneDay <- function(numMonths) {
	plotAllUsersHistogram(
		getDataForLastNMonthsWithMaxDeltaCS(numMonths, 24), 
		paste("Median DeltaCS over Last", numMonths, "Months (Capped at DeltaCS of", 24, "Hours)"), 
		seq(from=0, to=24, by=1)
	)
}

histForLastNMonthsWithMaxDeltaCSOfOneHour <- function(numMonths) {
	plotAllUsersHistogram(
		getDataForLastNMonthsWithMaxDeltaCS(numMonths, 1), 
		paste("Median DeltaCS over Last", numMonths, "Months (Capped at DeltaCS of", 1, "Hour)"), 
		seq(from=0, to=1, by=0.02)
	)
}

histForLastNMonthsExcludeLargest <- function(numMonths, numProjectsToExclude) {
	filteredByMonths <- getDataForLastNMonths(numMonths)
	largestProjects <- getLargestProjects(numProjectsToExclude)
	excludeLargeProjects <- filteredByMonths[!(filteredByMonths$domain %in% largestProjects),]
	plotAllUsersHistogram(excludeLargeProjects, paste("Median DeltaCS over Last", numMonths, "Months (Excluding Largest", numProjectsToExclude, "Projects)"), getFullRangeBucketsVector())
}

histForLastNMonthsExcludeLargestWithMaxDeltaCS <- function(numMonths, maxDeltaCSValue, numProjectsToExclude) {
	filteredByMonths <- getDataForLastNMonths(numMonths)
	largestProjects <- getLargestProjects(numProjectsToExclude)
	excludeLargeProjects <- filteredByMonths[!(filteredByMonths$domain %in% largestProjects),]
	filteredByValue <- excludeLargeProjects[excludeLargeProjects$median_hours <= maxDeltaCSValue,]
	plotAllUsersHistogram(filteredByValue, paste("Median DeltaCS over Last", numMonths), getBucketsVectorUpTo(maxDeltaCSValue))
}

#65,607 data points for last 12 months
getDataForLastNMonths <- function(numMonths) {
	return(primaryDataset[getDurationInMonths(primaryDataset$month, datasetEndDate) <= numMonths,])
}

#55,730 data points for last 12 months w/ 168 hour max
#54,180 data points for last 12 months w/ 144 hour max
#52,255 data points for last 12 months w/ 120 hour max
#49,668 data points for last 12 months w/ 96 hour max
#46,331 data points for last 12 months w/ 72 hour max
#42,135 data points for last 12 months w/ 48 hour max
#35,672 data points for last 12 months w/ 24 hour max
#14,252 data points for last 12 months w/ 2 hour max
#12,699 data points for last 12 months w/ 1 hour max
#11,720 data points for last 12 months w/ 1/2 hour max
#10,713 data points for last 12 months w/ 10 min max
#9,035 data points for last 12 months w/ 1 min max
getDataForLastNMonthsWithMaxDeltaCS <- function(numMonths, maxDeltaCSValue) {
	filteredByMonths <- getDataForLastNMonths(numMonths)
	return(filteredByMonths[filteredByMonths$median_hours <= maxDeltaCSValue,])
}

getBucketsVectorUpTo <- function(maxValue) {
	return(seq(from=0, to=maxValue, by=24))
}

getFullRangeBucketsVector <- function() {
	return(seq(from=0, to=1512, by=24))
}


# FUNCTIONS FOR PLOT OF VARIABILITY V. MEAN

getStandardDevOfMedianDeltaCSForProject <- function(domainName) {
	return(sd(primaryDataset[primaryDataset$domain == domainName,]$median_hours))
}

getMeanOfMedianDeltaCSForProject <- function(domainName) {
	return(mean(primaryDataset[primaryDataset$domain == domainName,]$median_hours))
}

getVarianceOfMedianDeltaCSForProject <- function(domainName) {
	return(var(primaryDataset[primaryDataset$domain == domainName,]$median_hours))
}

getCorrelationBetweenMeanAndVariance <- function() {
	longestRunningProjects <- getLongestRunningProjects(185)
	meanData <- unlist(lapply(longestRunningProjects$Domain, getMeanOfMedianDeltaCSForProject))
	varianceData <- unlist(lapply(longestRunningProjects$Domain, getVarianceOfMedianDeltaCSForProject))
	cor(meanData, varianceData)
}

getCorrelationBetweenMeanAndStdDev <- function() {
	longestRunningProjects <- getLongestRunningProjects(185)
	meanData <- unlist(lapply(longestRunningProjects$Domain, getMeanOfMedianDeltaCSForProject))
	stdDevData <- unlist(lapply(longestRunningProjects$Domain, getStandardDevOfMedianDeltaCSForProject))
	cor(meanData, stdDevData)
}

plotVariabilityVsAverage <- function() {
	longestRunningProjects <- getLongestRunningProjects(185) # this gives us all projects running for >= 18 months
	xAxisData <- unlist(lapply(longestRunningProjects$Domain, getStandardDevOfMedianDeltaCSForProject))
	yAxisData <- unlist(lapply(longestRunningProjects$Domain, getMeanOfMedianDeltaCSForProject))
	plot(xAxisData, yAxisData, pch = 20, col="blue", xlab="Standard Deviation of Median DeltaCS", ylab="Mean of Median DeltaCS")
	title("Plot of Variability vs. Average for Projects Running >= 18 months")
}

# FUNCTIONS FOR PLOT BY PROJECT

getLengthOfProjectInMonths <- function(domainName) {
	monthsForDomain <- primaryDataset[primaryDataset$domain == domainName,]$month
	lastMonth <- max(as.Date(monthsForDomain))
	firstMonth <- min(as.Date(monthsForDomain))
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
	plot(xAxisData, yAxisData, pch = 20, col="blue", xlab="Month", ylab="Median DeltaCS (in hours)")
	title(domainName)
}


# FUNCTIONS FOR PLOT OF LONG-RUNNING USERS

plotDeltaCSByMonthForUsersMeetingPercentageThreshold <- function(percentageThreshold, percentageDisplayString) {
	qualifyingUsers <- getUsersMeetingPercentageActivityThreshold(percentageThreshold)
	dataForQualifyingUsers <- primaryDataset[primaryDataset$user_id %in% qualifyingUsers$User,]
	yAxisData <- as.numeric(t(getMedianDeltaCSByMonth(dataForQualifyingUsers)[2]))
	xAxisData <- seq(1, length(yAxisData))
	plot(xAxisData, yAxisData, pch = 20, col="blue", xlab="Month", ylab=paste("Median DeltaCS Over A Month For ", percentageDisplayString, " Activity Threshold (n = ", nrow(qualifyingUsers), ")", sep=""))
	abline(lm(yAxisData ~ xAxisData), lty=2)
}

getUsersMeetingPercentageActivityThreshold <- function(percentageThreshold) {
    totalMonthsInDataset <- getDurationInMonths(datasetStartDate, datasetEndDate)
    aggregatedByUserDuration <- readAggregationByUserDurationFromFile()
    return(aggregatedByUserDuration[(aggregatedByUserDuration$x/totalMonthsInDataset) >= percentageThreshold,])
}

getDurationOfUserActivityInMonths <- function(userID) {
    monthsForUser <- primaryDataset[primaryDataset$user_id == userID,]$month
    lastMonth <- max(as.Date(monthsForUser))
    firstMonth <- min(as.Date(monthsForUser))
    return(getDurationInMonths(firstMonth, lastMonth))
}

# Very slow, so computed once and then saved results to .csv, which can be read back using the function below
computeAggregationByUserDuration <- function() {
	aggregatedByUserDuration <- aggregate(primaryDataset$user_id, list(User=primaryDataset$user_id), getDurationOfUserActivityInMonths)
	return(aggregatedByUserDuration)
}

readAggregationByUserDurationFromFile <- function() {
	return(read.csv("./data/aggregatedByUserDuration.csv"))
}


# FUNCTIONS FOR PLOT OF ALL USERS

plotDeltaCSOverMonthForAllUsers <- function() {
	xAxisData <- seq(1,63)
	yAxisData <- as.numeric(t(getMedianDeltaCSByMonth()[2]))
	plot(xAxisData, yAxisData, pch = 20, col="blue", xlab="Month", ylab="Median DeltaCS over a Month (in hours)")
	abline(lm(yAxisData ~ xAxisData), lty=2)
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

getLargestProjects <- function(dataset, numProjects) {
	aggregatedByNumForms <- aggregate(dataset$form_count, list(domain=dataset$domain), sum)
	sorted <- aggregatedByNumForms[order(aggregatedByNumForms$x),]$domain
	return(tail(sorted, n=numProjects))
}

getMedianDeltaCSForProject <- function(domainName) {
	sprintf(domainName)
	return(median(primaryDataset[primaryDataset$domain == domainName,]$median_hours))
}

# FUNCTIONS FOR ANALYSIS BY GEOGRAPHIC REGION

getNumDataPointsForCountry <- function(country) {
	projectsLocatedInCountry <- hqProjectSpaceDataset[getBooleanVectorForProjectsInCountry(country, hqProjectSpaceDataset$domain),]$domain
	numDataPoints <- nrow(primaryDataset[primaryDataset$domain %in% projectsLocatedInCountry,])
	return(list(country, numDataPoints))
}

getMedianDeltaCSForCountry <- function(country) {
	projectsLocatedInCountry <- hqProjectSpaceDataset[getBooleanVectorForProjectsInCountry(country, hqProjectSpaceDataset$domain),]$domain
	return(median(primaryDataset[primaryDataset$domain %in% projectsLocatedInCountry,]$median_hours))
}

getMedianDeltaCSForRegion <- function(region) {
	projectsLocatedInRegion <- hqProjectSpaceDataset[getBooleanVectorForProjectsInRegion(region, hqProjectSpaceDataset$domain),]$domain
	return(median(primaryDataset[primaryDataset$domain %in% projectsLocatedInRegion,]$median_hours))
}

getBooleanVectorForProjectsInCountry <- function(countryName, listOfDomains) {
	return(unlist(lapply(listOfDomains, projectIsLocatedInCountry, country=countryName)))
}

getBooleanVectorForProjectsInRegion <- function(regionName, listOfDomains) {
	return(unlist(lapply(listOfDomains, projectIsLocatedInRegion, region=regionName)))
}

projectIsLocatedInCountry <- function(domain, country) {
	return(country %in% getCountriesForDomain(domain))
}

projectIsLocatedInRegion <- function(domain, region) {
	return(region %in% getRegionsForDomain(domain))
}

getRegionsForDomain <- function(domainName) {
	regions <- unique(lapply(unlist(getCountriesForDomain(domainName)), getManuallyTaggedGeographicRegion))
	return(regions)
}

getCountriesForDomain <- function(domainName) {
	deploymentCountriesString <- toString(hqProjectSpaceDataset[hqProjectSpaceDataset$domain == toString(domainName),]$deployment_countries)
    if (deploymentCountriesString == "No countries") {
    	return("")
    }
    removeLeadingAndTrailingChars <- substring(deploymentCountriesString, 4, nchar(deploymentCountriesString)-2)
    countries <- strsplit(removeLeadingAndTrailingChars, "', u'")
    return(countries)
}

getDomainsWithCountryData <- function() {
	allDomainsFromFormsDataset <- unique(primaryDataset$domain)
	domainsInPrimaryDataset <- hqProjectSpaceDataset[hqProjectSpaceDataset$domain %in% allDomainsFromFormsDataset,]
	domainsWithCountryData <- domainsInPrimaryDataset[domainsInPrimaryDataset$deployment_countries != "No countries",]
	return(domainsWithCountryData$domain)
}

# 67 countries
getAllRepresentedCountries <- function() {
	return(unique(unlist(lapply(getDomainsWithCountryData(), getCountriesForDomain))))
}

# 45%
getPercentageOfDomainsWithCountryData <- function() {
	allDomainsFromFormsDataset <- unique(primaryDataset$domain)
	return(length(getDomainsWithCountryData()) / length(allDomainsFromFormsDataset))
}

getCountriesForRegion <- function(region) {
	if (region == "Northern Africa") {
		return(NORTHERN_AFRICA)
	} else if (region == "Eastern Africa") {
		return(EASTERN_AFRICA)
	} else if (region == "Western Africa") {
		return(WESTERN_AFRICA)
	} else if (region == "Middle Africa") {
		return(MIDDLE_AFRICA)
	} else if (region == "Southern Africa") {
		return(SOUTHERN_AFRICA)
	} else if (region == "Southern Asia") {
		return(SOUTHERN_ASIA)
	} else if (region == "Western Asia") {
		return(WESTERN_ASIA)
	} else if (region == "Southeastern Asia") {
		return(SOUTHEASTERN_ASIA)
	} else if (regino == "Eastern Asia") {
		return(EASTERN_ASIA)
	}  else if (region == "South America") {
		return(SOUTH_AMERICA)
	} else if (region == "Northern America") {
		return(NORTHERN_AMERICA)
	} else if (region == "Central America") {
		return(CENTRAL_AMERICA)
	} else if (region == "Caribbean") {
		return(CARIBBEAN)
	} else if (region == "Europe") {
		return(EUROPE)
	} else if (region == "Oceania") {
		return(OCEANIA)
	} 
}

getManuallyTaggedGeographicRegion <- function(country) {
	country <- toupper(country)
	if (country %in% NORTHERN_AFRICA) {
		return("Northern Africa")
	} else if (country %in% EASTERN_AFRICA) {
		return("Eastern Africa")
	} else if (country %in% WESTERN_AFRICA) {
		return("Western Africa")
	} else if (country %in% MIDDLE_AFRICA) {
		return("Middle Africa")
	} else if (country %in% SOUTHERN_AFRICA) {
		return("Southern Africa")
	} else if (country %in% SOUTHERN_ASIA) {
		return("Southern Asia")
	} else if (country %in% WESTERN_ASIA) {
		return("Western Asia")
	} else if (country %in% SOUTHEASTERN_ASIA) {
		return("Southeastern Asia")
	} else if (country %in% EASTERN_ASIA) {
		return("Eastern Asia")
	}  else if (country %in% SOUTH_AMERICA) {
		return("South America")
	} else if (country %in% NORTHERN_AMERICA) {
		return("Northern America")
	} else if (country %in% CENTRAL_AMERICA) {
		return("Central America")
	} else if (country %in% CARIBBEAN) {
		return("Caribbean")
	} else if (country %in% EUROPE) {
		return("Europe")
	} else if (country %in% OCEANIA) {
		return("Oceania")
	} else {
		sprintf("WARNING: No region found for country %s", country)
	}
}

