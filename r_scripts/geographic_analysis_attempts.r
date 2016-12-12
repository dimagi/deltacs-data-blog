# FUNCTIONS FOR ANALYSIS BY GEOGRAPHIC REGION -- ATTEMPT 1

plotDeltaCSForRegion <- function(regionName) {
	datasetWithCountryData <- primaryDataset[primaryDataset$domain %in% getDomainsWithCountryData(),]
	dataForRegion <- datasetWithCountryData[getBooleanVectorForProjectsInRegion(regionName, datasetWithCountryData$domain),]
	return(dataForRegion)$domain
}

getBooleanVectorForProjectsInRegion <- function(region, listOfDomains) {
	domainsForRegion <- getDomainsForRegion(region)
	return(lapply(listOfDomains, customContains, domainsForRegion))
}

isProjectDeployedInRegion <- function(regionName, domainName) {
	return(regionName %in% getRegionsForDomain(domainName))
}

getRegionsForDomain <- function(domainName) {
	regions <- unique(lapply(unlist(getCountriesForDomain(domainName)), getManuallyTaggedGeographicRegion))
	return(regions)
}

# return all domains for which there is an intersection between countriesForRegion and getCountriesForDomain() for that domain
getDomainsForRegion <- function(regionName) {
	countriesForRegion <- getCountriesForRegion(regionName)
	domainsWithCountryData <- getDomainsWithCountryData()
	
	listOfCountriesForEachDomain <- lapply(domainsWithCountryData, getCountriesForDomain)
	intersectionVector <- lapply(listOfCountriesForEachDomain, intersect, countriesForRegion)
	booleanVector <- lapply(intersectionVector, customVectorNotEmpty)
	
	# STUCK HERE - figure out how to filter domainsWithCountryData by booleanVector (problem is that domainsWithCountryData is an atomic vector)
}

customVectorNotEmpty <- function(vector) {
	return(length(vector) > 0)
}

customContains <- function(vector, item) {
	return(item %in% vector)
}

# FUNCTIONS FOR ANALYSIS BY GEOGRAPHIC REGION -- ATTEMPT 2

getLargestProjectsWithCountryDataSortedByDeltaCS <- function(numProjects) {
	datasetWithCountryData <- primaryDataset[primaryDataset$domain %in% getDomainsWithCountryData(),]
	largeDomainsWithCountryData <- getLargestProjects(datasetWithCountryData, numProjects)
	filtered <- primaryDataset[primaryDataset$domain %in% largeDomainsWithCountryData,]
	return(sortFrameByMedianDeltaCS(filtered))
}

sortFrameByMedianDeltaCS <- function(dataframe) {
	aggregatedByMedianDeltaCSForProject <- aggregate(dataframe$median_hours, list(Domain=dataframe$domain), median)
	sorted <- aggregatedByMedianDeltaCSForProject[order(aggregatedByMedianDeltaCSForProject$x),]
	return(sorted)
}

getHighestDeltaCSProjectsWithCountryData <- function(numProjects) {
	datasetWithCountryData <- primaryDataset[primaryDataset$domain %in% getDomainsWithCountryData(),]
	return(tail(sortFrameByMedianDeltaCS(datasetWithCountryData), numProjects))
}

getLowestDeltaCSProjectsWithCountryData <- function(numProjects) {
	datasetWithCountryData <- primaryDataset[primaryDataset$domain %in% getDomainsWithCountryData(),]
	return(head(sortFrameByMedianDeltaCS(datasetWithCountryData), numProjects))
}

getHighestDeltaCSLargeProjects <- function(numProjectsToIncludeInLargest, numProjectsToReturn) {
	return(tail(getLargestProjectsWithCountryDataSortedByDeltaCS(numProjectsToIncludeInLargest), numProjectsToReturn))
}

getLowestDeltaCSLargeProjects <- function(numProjectsToIncludeInLargest, numProjectsToReturn) {
	return(head(getLargestProjectsWithCountryDataSortedByDeltaCS(numProjectsToIncludeInLargest), numProjectsToReturn))
}