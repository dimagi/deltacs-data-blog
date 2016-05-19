
MAX_DELTACS_IN_DAYS = 20
MAX_DELTACS_IN_HOURS = 24*MAX_DELTACS_IN_DAYS
MIN_NUM_FORMS = 100

plotHistogram <- function() {
	fullDataset <- read.csv("./data/deltacs_stats_by_user_month_full.csv")
	rowsOfInterest <- filterDataset(fullDataset)
	hist(rowsOfInterest$mean_hours, breaks=getBucketsVector(), plot=TRUE, freq=TRUE)
}

#' Return a filtered version of the original full dataset, excluding any rows that do not meet the following criteria:
#'  - deltacs < MAX_DELTACS_IN_HOURS
#-  - no. forms submitted > MIN_NUM_FORMS
filterDataset <- function(dataset) {
	imposeTimeLimit <- dataset[dataset$mean_hours <= MAX_DELTACS_IN_HOURS, ]
	imposeFormsRequirement <- imposeTimeLimit[imposeTimeLimit$form_count >= MIN_NUM_FORMS, ]
	return(imposeFormsRequirement)
}

getBucketsVector <- function() {
	return(c(seq(from=0, to=100, by=10), 150, 200, seq(from=300, to=500, by=100)))
}

