
plotScatterPlot <- function(useMean) {
	fullDataset <- read.csv("./data/deltacs_stats_by_user_month.csv")
	
	xAxisData <- fullDataset$month
	if (useMean) {
		yAxisData <- fullDataset$mean_hours
		yLabel <- "Mean DeltaCS over a Month"
	} else {
		yAxisData <- fullDataset$median_hours
		yLabel <- "Median DeltaCS over a Month"
	}
	
	#plot(xAxisData, yAxisData, xlab="Month", ylab=yLabel)
}

getMeanAggregatedByMonth <- function() {
	fullDataset <- read.csv("./data/deltacs_stats_by_user_month.csv")
	aggregatedByMonth <- aggregate(fullDataset$mean_hours, list(Month = fullDataset$month), mean)
	return(aggregatedByMonth)
}

# this is definitely doing something weird, Jan and March 2016 don't look right
getMedianAggregatedByMonth <- function() {
	fullDataset <- read.csv("./data/deltacs_stats_by_user_month.csv")
	aggregatedByMonth <- aggregate(fullDataset$median_hours, list(Month = fullDataset$month), median)
	return(aggregatedByMonth)
}

getOverallMean <- function() {
	fullDataset <- read.csv("./data/deltacs_stats_by_user_month.csv")
	return(mean(fullDataset$mean_hours))
}

getOverallMedian <- function() {
	fullDataset <- read.csv("./data/deltacs_stats_by_user_month.csv")
	return(median(fullDataset$median_hours))
}
