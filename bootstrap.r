source('r_scripts/include.r')
installPackages()
loadPackages()
setupDb()

cat(paste("
Environment setup. Some useful functions:

showTableDefinition - Show the SQL table definition for reference
getGlobalAverage - Calculate the global average deltacs for all users.
demoHistogram - Show a histogram of the average deltacs for a sample data set
writeGroupStatsData - Write a CSV file with deltacs stats grouped by domain, user_id, month
"))
