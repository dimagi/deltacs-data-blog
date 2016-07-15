#' See https://docs.google.com/document/d/1GrBJMhBcz6fSoBygUDJxg8Vw2wUrxhXvKpkROUbUYss/edit


#' Write a CSV file with one row per user giving the earliest for received date.
#'
#' File columns:
#'  row_number
#'  domain
#'  user_id
#'  first_date
#'
#' @param filename      The name of the file to write the data to.
writeFirstFormDate <- function(filename='first_form_date_per_user.csv') {
    query <- "
    SELECT
        domain,
        user_id,
        min(received_on) as first_date
    FROM formdata
    GROUP BY domain, user_id;"

    DF <- sqldf(query)
    DF$first_date = as.POSIXlt(df$first_date, tz="GMT")  # convert back to UTC
    write.csv(DF, filename)
    print(sprintf("Data written to: %s", filename))
}

#' Write a CSV file with one row per containing the number of forms
#' completed on each day after their first form submission
#'
#' File columns:
#'  domain
#'  user_id
#'  1...N       the number of forms submitted N-1 days after the first date of form submission
#'
#' @param in_filename       The path to the CSV file containing first date data
#'                          Expected columns: domain, user_id, first_date
#' @param out_filename      The name of the file to write the data to.
#' @param n_days            The number of days after the first submission to include
write90DayData <- function(in_filename, out_filename, n_days=90) {
    domainUserData <- read.csv(in_filename)
    domainUserData[[4]] = as.Date(domainUserData[[4]])

    headers = c(c("domain","user_id"),c(1:n_days))
    cat(paste(headers, collapse=","), '\n', file=out_filename)

    for (index in 1:nrow(domainUserData)) {
        row = domainUserData[index,]
        domain = as.character(row$domain)
        user_id = as.character(row$user_id)
        first_date = as.Date(row$first_date)
        DF <- sqldf(sprintf("
        SELECT
            domain,
            user_id,
            date(received_on) as date,
            count(*) as form_count
        FROM
            formdata
        WHERE
            domain = '%s' AND user_id = '%s' AND received_on < '%s'
        GROUP BY domain, user_id, date(received_on)
        ", domain, user_id, as.Date(first_date) + n_days))

        all_dates = seq(as.Date(row$first_date), as.Date(row$first_date)+(n_days-1), by="days")
        missingDates = as.Date(setdiff(all_dates, DF$date), origin="1970-01-01")
        for (date in missingDates) {
            DF[nrow(DF)+1,]<-list(domain, user_id, as.Date(date, origin="1970-01-01"), 0)
        }
        reshapedData = cast(DF, domain + user_id ~ date, value='form_count')

        write.table(reshapedData, file = out_filename, sep = ",", append=T, col.names=F, row.names=F)
        print(sprintf("Data written to: %s, %s, %s", out_filename, domain, user_id))
    }
}
