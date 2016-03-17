#!/usr/bin/env Rscript
# Some useful functions for R
# Usage:
#   > source('r_scripts/include.r')
#   > options(sqldf.RPostgreSQL.user="commcarehq",
#      sqldf.RPostgreSQL.password="commcarehq",
#      sqldf.RPostgreSQL.dbname="datablog",
#      sqldf.RPostgreSQL.host="localhost",
#      sqldf.RPostgreSQL.port=5432)
#
#   > installPackages()  # only the first time
#   > loadPackages()
#   > demoHistogram()
#   > global_average_hours <- getGlobalAverage()
#   > print(sprintf("Global average: %s hours", global_average_hours))


#' Add a 'deltacs' column to the table containing the
#' interval between 'received_on' and 'time_end'.
#'
#' You should only run this once.
addDeltacsColumn <- function() {
    sqldf("ALTER TABLE formdata ADD COLUMN deltacs interval")
    sqldf("CREATE INDEX formdata_deltacs on formdata (deltacs)")
    sqldf("UPDATE TABLE formdata SET deltacs = received_on - time_end")
}

#' Clean the SQL data.
#' You only need to run this once.
cleanData <- function() {
    sqldf("DELETE FROM formdata where time_end < '2011-01-01'")
    sqldf("DELETE from formdata where user_id = '' or user_id is NULL")
    sqldf("DELETE from formdata where user_id in ('demo_user', 'commtrack-system', 'system', 'admin', 'bihar-system')")
    sqldf("DELETE from formdata where char_length(user_id) < 10")
    sqldf("DELETE from formdata where deltacs <= '0 days'::interval")
}

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
    return(sqldf("SELECT EXTRACT(EPOCH FROM avg(deltacs)) / (60*60) AS deltacs FROM formdata WHERE deltacs < '60 days'::interval")$deltacs)
}

#' Show a histogram of the average deltacs for a sample data set.
#'
#' @param sample_size   The number of rows to use. Default = 10000
demoHistogram <- function(sample_size=10000) {
    DF = sqldf(sprintf("select EXTRACT(EPOCH FROM deltacs) / (60*60) as deltacs from formdata where deltacs < '60 days'::interval limit %i", sample_size))
    hist(DF$deltacs)
}

#' Write a CSV file of the average deltacs (in hours) grouped by user_id
#'
#' @param filename      The name of the file to write the data to.
#' @param sample_size   The size of the sample you want to compute the averages over. Default = 100000.
#'                      Set to 0 to compute over entire data set.
writeGroupAverageData <- function(filename='average_deltacs_by_user.csv', sample_size=100000) {
    query <- "SELECT user_id, EXTRACT(EPOCH FROM avg(deltacs)) / (60*60) as deltacs FROM %s GROUP BY user_id"
    if(sample_size == 0) {
        from <- "formdata WHERE deltacs < '60 days'::interval"
    } else {
        from <- sprintf("(SELECT user_id, deltacs FROM formdata WHERE deltacs < '60 days'::interval limit %i) AS a", sample_size)
    }
    DF <- sqldf(sprintf(query, from))
    write.csv(DF, filename)
    print(sprintf("Data written to: %s", filename))
}
