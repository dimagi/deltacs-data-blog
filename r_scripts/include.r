#!/usr/bin/env Rscript
# Some useful functions for R
# Usage:
#   > source('r_scripts/include.r')
#   > setupDb("username", "passwd", "dbname")
#
#   > installPackages()  # only the first time
#   > loadPackages()
#   > demoHistogram()
#   > global_average_hours <- getGlobalAverage()
#   > print(sprintf("Global average: %s hours", global_average_hours))

#' Install some useful pacakges
installSqlPackages <- function() {
    install.packages("RPostgreSQL")
    install.packages("sqldf")
}

#' Load some useful packages
loadPackages <- function() {
    library("RPostgreSQL")
    library("sqldf")
}

#' Setup the DB connection
setupDb <- function(username, password, db_name) {
    options(sqldf.RPostgreSQL.user=username,
      sqldf.RPostgreSQL.password=password,
      sqldf.RPostgreSQL.dbname=db_name,
      sqldf.RPostgreSQL.host="localhost",
      sqldf.RPostgreSQL.port=5432)
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

#' Write a CSV file with some basic deltacs stats grouped by user_id
#'
#' File columns:
#'  row_number
#'  user_id
#'  count       Number of forms included in the dataset for this user
#'  mean        Mean deltacs for this user (hours)
#'  stddev      Standard deviation of the deltacs for this user (hours)
#'
#' @param filename      The name of the file to write the data to.
#' @param sample_size   The size of the sample you want to compute the averages over. Default = 100000.
#'                      Set to 0 to compute over entire data set.
writeGroupStatsData <- function(filename='deltacs_stats_by_user.csv', sample_size=100000) {
    query <- "
    SELECT s.user_id, s.count, s.mean / 3600, s.stddev / 3600 FROM (
        SELECT
            row_number() OVER w AS rk,
            user_id,
            count(*) OVER w AS count,
            avg(EXTRACT(EPOCH from deltacs)) OVER w AS mean,
            stddev_pop(EXTRACT(EPOCH from deltacs)) OVER w AS stddev
        FROM
            %s
        WINDOW w AS (PARTITION BY user_id)
        ) AS s
    WHERE s.rk = 1;"

    if(sample_size == 0) {
        from <- "formdata WHERE deltacs < '60 days'::interval"
    } else {
        from <- sprintf("(SELECT user_id, deltacs FROM formdata WHERE deltacs < '60 days'::interval limit %i) AS a", sample_size)
    }
    DF <- sqldf(sprintf(query, from))
    write.csv(DF, filename)
    print(sprintf("Data written to: %s", filename))
}