# deltacs-data-blog
Scripts, queries, and data analysis for examining the time elapsed between form completion and submission across all CommCare form submissions.

## dumping data for analysis
```
  $ cat query.json
  {}
  $ python -m dumper.main http://my.elasticsearch.url my_index query.json 
```
