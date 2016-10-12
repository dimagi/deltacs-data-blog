
fullDataset <- read.csv("./data/deltacs_stats_by_user_month.csv")
hqProjectSpaceDataset <- read.csv("./data/project-space-list-from-hq.csv")
primaryDataset <- fullDataset[fullDataset$form_count >= 3,]
datasetStartDate <- as.Date("2011-01-01")
datasetEndDate <- as.Date("2016-03-01")

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


# FUNCTIONS FOR ANALYSIS BY GEOGRAPHIC REGION

getDomainToLocationMapping <- function() {
	
}

# 67 countries
getAllRepresentedCountries <- function() {
	return(unique(unlist(Map(getCountriesForDomain, getDomainsWithCountryData()))))
}

getCountriesForDomain <- function(domainName) {
	countriesEntryForDomain <- (hqProjectSpaceDataset[hqProjectSpaceDataset$domain == domainName,]$deployment_countries)
	return(parseCountriesFromEntry(toString(countriesEntryForDomain)))
}

parseCountriesFromEntry <- function(deploymentCountriesString) {
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

# 45%
getPercentageOfDomainsWithCountryData <- function() {
	allDomainsFromFormsDataset <- unique(primaryDataset$domain)
	return(length(getDomainsWithCountryData()) / length(allDomainsFromFormsDataset))
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

getManuallyTaggedGeographicRegion <- function(country) {
	if (country %in% NORTHERN_AFRICA) {
		return "Northern Africa"
	} else if (country %in% EASTERN_AFRICA) {
		return "Eastern Africa"
	} else if (country %in% WESTERN_AFRICA) {
		return "Western Africa"
	} else if (country %in% MIDDLE_AFRICA) {
		return "Middle Africa"
	} else if (country %in% SOUTHERN_AFRICA) {
		return "Southern Africa"
	} else if (country %in% SOUTHERN_ASIA) {
		return "Southern Asia"
	} else if (country %in% WESTERN_ASIA) {
		return "Western Asia"
	} else if (country %in% SOUTHEASTERN_ASIA) {
		return "Southeastern Asia"
	} else if (country %in% EASTERN_ASIA) {
		return "Eastern Asia"
	}  else if (country %in% SOUTH_AMERICA) {
		return "South America"
	} else if (country %in% NORTHERN_AMERICA) {
		return "Northern America"
	} else if (country %in% CENTRAL_AMERICA) {
		return "Central America"
	} else if (country %in% CARIBBEAN) {
		return "Caribbean"
	} else if (country %in% EUROPE) {
		return "Europe"
	} else if (country %in% OCEANIA) {
		return "Oceania"
	} 
}