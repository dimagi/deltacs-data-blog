# deltacs-data-blog
Scripts, queries, and data analysis for examining the time elapsed between form completion and submission across all CommCare form submissions.

## dumping data for analysis
```
  $ cat query.json
  {}
  $ python -m dumper.main http://my.elasticsearch.url my_index query.json
```

# loading dumped data to PostgreSQL
```
  $ loader/loader.sh [/path/to/dump.csv] [DATABAES NAME] [POSTGRES USERNAME]
```

* User must be an admin user.
* CSV should have the following columns (in this order)
    * domain,instance_id,user_id,time_end,received_on

# Using R

$ R
> source('bootstrap.r')
