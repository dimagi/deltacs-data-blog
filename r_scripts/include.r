#!/usr/bin/env Rscript
# Some useful functions for R
# Usage:
#   > source('r_scripts/include.r')

#' Install some useful pacakges
installPackages <- function() {
    if("RPostgreSQL" %in% rownames(installed.packages()) == FALSE) {
        install.packages("RPostgreSQL")
    }
    if("sqldf" %in% rownames(installed.packages()) == FALSE) {
        install.packages("sqldf")
    }
    if("rjson" %in% rownames(installed.packages()) == FALSE) {
        install.packages("rjson")
    }
    if("reshape" %in% rownames(installed.packages()) == FALSE) {
        install.packages("reshape")
    }
}

#' Load some useful packages
loadPackages <- function() {
    library("RPostgreSQL")
    library("sqldf")
    library("rjson")
    library("reshape")
}

#' Setup the DB connection
setupDb <- function() {
    config <- getConfig()
    options(sqldf.RPostgreSQL.user=config$username,
      sqldf.RPostgreSQL.password=config$password,
      sqldf.RPostgreSQL.dbname=config$db_name,
      sqldf.RPostgreSQL.host="localhost",
      sqldf.RPostgreSQL.port=5432)
}

getConfig <- function() {
    fileConn<-file(".conf")
    if (file.exists(".conf")) {
        conf <- readLines(fileConn)
        return(fromJSON(conf))
    } else {
        username <- readline(prompt="Enter an database username: ")
        password <- readline(prompt="Enter an database password: ")
        db_name <- readline(prompt="Enter an database name: ")
        config <- list(username=username, password=password, db_name=db_name)
        cat(toJSON(config), file = '.conf', sep = '\n')
        return(config)
    }
}

#' Show the SQL table definition for reference
showTableDefinition <- function() {
    cat(paste("Table 'formdata'
       Column    |           Type           | Modifiers
    -------------+--------------------------+-----------
     form_id     | character varying(255)   | not null
     domain      | character varying(255)   | not null
     user_id     | character varying(255)   |
     time_end    | timestamp with time zone | not null
     received_on | timestamp with time zone | not null
     deltacs     | interval                 |
     "))
}

#' Calculate the global average deltacs for all users.
#'
#' @return The global average in hours
getGlobalAverage <- function() {
    return(sqldf("SELECT to_hours(avg(deltacs)) AS deltacs FROM formdata WHERE deltacs < '60 days'::interval")$deltacs)
}

#' Show a histogram of the average deltacs for a sample data set.
#'
#' @param sample_size   The number of rows to use. Default = 10000
demoHistogram <- function(sample_size=10000) {
    DF = sqldf(sprintf("select to_hours(deltacs) as deltacs from formdata where deltacs < '60 days'::interval limit %i", sample_size))
    hist(DF$deltacs)
}


#' Write a CSV file with deltacs stats grouped by domain, user_id, month
#'
#' File columns:
#'  row_number
#'  domain
#'  user_id
#'  month
#'  count       Number of forms included in the dataset for this user
#'  mean        Mean deltacs for this user (hours)
#'  median      Median deltacs for this user (hours)
#'
#' @param filename      The name of the file to write the data to.
#' @param sample_size   The size of the sample you want to compute the averages over. Default = 100000.
#'                      Set to 0 to compute over entire data set.
writeGroupStatsData <- function(filename='deltacs_stats_by_user_month.csv', sample_size=100000) {
    query <- "
    SELECT
        domain,
        user_id,
        date_trunc('month', received_on)::date as month,
        count(*) as form_count,
        avg(EXTRACT(EPOCH from deltacs)) / 3600 AS mean_hours,
        median(EXTRACT(EPOCH from deltacs)) / 3600 AS median_hours
    FROM
        %s
    GROUP BY domain, user_id, date_trunc('month', received_on)
    ORDER BY domain, month;"

    if(sample_size == 0) {
        from <- "formdata WHERE received_on > time_end AND deltacs < '60 days'::interval"
    } else {
        from <- sprintf("(SELECT domain, user_id, received_on, deltacs FROM formdata WHERE received_on > time_end AND deltacs < '60 days'::interval limit %i) AS a", sample_size)
    }
    DF <- sqldf(sprintf(query, from))
    write.csv(DF, filename)
    print(sprintf("Data written to: %s", filename))
}


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
#' @param filename      The name of the file to write the data to.
write90DayData <- function(first_form_date_per_user_file) {
    domainUserData <- read.csv(first_form_data_filename)
    domainUserData[[4]] = as.Date(domainUserData[[4]])

    filename = "90day_data.csv"
    headers = c(c("domain","user_id"),c(1:90))
    cat(paste(headers, collapse=","), '\n', file=filename)

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
        ", domain, user_id, as.Date(first_date) + 90))

        all_dates = seq(as.Date(row$first_date), as.Date(row$first_date)+89, by="days")
        missingDates = as.Date(setdiff(all_dates, DF$date), origin="1970-01-01")
        for (date in missingDates) {
            DF[nrow(DF)+1,]<-list(domain, user_id, as.Date(date, origin="1970-01-01"), 0)
        }
        reshapedData = cast(DF, domain + user_id ~ date, value='form_count')

        write.table(reshapedData, file = filename, sep = ",", append=T, col.names=F, row.names=F)
        print(sprintf("Data written to: %s, %s, %s", filename, domain, user_id))
    }
}
