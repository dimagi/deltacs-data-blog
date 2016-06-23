
MIN_NUM_FORMS = 5
BIN_SIZE = 24

plotHistogram <- function() {
	fullDataset <- read.csv("./data/deltacs_stats_by_user_month.csv")
	rowsOfInterest <- filterDataset(fullDataset)
	hist(rowsOfInterest$median_hours, 
		breaks=getBucketsVector(), 
		plot=TRUE, freq=TRUE, col="lightblue",
		main="Median DeltaCS over a Month by User",
		xlab="Median DeltaCS (in hours)",
		ylab="Count")
}

# Return a filtered version of the original full dataset, excluding any rows that do not meet the following criteria:
# - no. forms submitted > MIN_NUM_FORMS
filterDataset <- function(dataset) {
	imposeFormsRequirement <- dataset[dataset$form_count >= MIN_NUM_FORMS, ]
	return(imposeFormsRequirement)
}

getBucketsVector <- function() {
	return(seq(from=0, to=1512, by=BIN_SIZE))
}

