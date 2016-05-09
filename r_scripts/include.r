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
}

#' Load some useful packages
loadPackages <- function() {
    library("RPostgreSQL")
    library("sqldf")
    library("rjson")
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
