
MIN_NUM_FORMS = 100

plotHistogram <- function() {
	fullDataset <- read.csv("./data/deltacs_stats_by_user_month.csv")
	rowsOfInterest <- filterDataset(fullDataset)
	hist(rowsOfInterest$median_hours, 
		breaks=getBucketsVector(), 
		plot=TRUE, freq=TRUE, col="lightblue",
		main="Median DeltaCS by User",
		xlab="Median DeltaCS (in hours)",
		ylab="Count")
}

#' Return a filtered version of the original full dataset, excluding any rows that do not meet the following criteria:
# - no. forms submitted > MIN_NUM_FORMS
filterDataset <- function(dataset) {
	imposeFormsRequirement <- dataset[dataset$form_count >= MIN_NUM_FORMS, ]
	return(imposeFormsRequirement)
}

getBucketsVector <- function() {
	#return(c(seq(from=0, to=100, by=10), 150, 200, seq(from=300, to=500, by=100)))
	return(seq(from=0, to=1500, by=50))
	#return(c(seq(from=0, to=500, by=50), seq(from=500, to=1500, by=200)))
}

