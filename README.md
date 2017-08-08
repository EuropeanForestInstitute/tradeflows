---
title: "Forest products trade flows database"
output:
  html_document:
    toc : true
---

[![Travis](https://api.travis-ci.org/paulrougieux/tradeflows.svg?branch=master)](https://travis-ci.org/paulrougieux/tradeflows)

This R package prepares data for a forest products trade flow database.
It contains functions that automatically grab data from the 
COMTRADE and COMEXT databases, feed it through algorithms to compare 
unit values, to mirror export and import values.
Data will be delivered to an online user-interface.

Main package documentation

Examples and demonstration:

* [Main package documentation](http://europeanforestinstitute.github.io/tradeflows/index.html)
* `vignettes` contains the vignettes in raw format
* `inst/templates` contains templates to generate various reports
* `docs/development/overview` contains development version of plots
* `docs/development/cleaning` contain unit price calculations 
  and a procedure to prepare volume and weight data

## Installation
This R package can be installed on a server or
desktop with the devtools package.
```
devtools::install_github("EuropeanForestInstitute/tradeflows")
# Optionally build vignettes containing tutorials:
devtools::install_github("EuropeanForestInstitute/tradeflows", build_vignettes = TRUE)
```

## Input

### Loading data from the Comtrade API
The function `loadcomtradebycode`, 
loads data from the comtrade Application Programming Interface (API)
in JSON format and convert it to data frames.
Further information by looking at `help(loadcomtradebycode)`,
`help(loadcomtradeallreporters)` and `help(loadcomtradewithpause)`.

Data is saved to a RDATA file and optionally to a csv file.

Example use, load sawnwood of HS code 440799 data for France :
```
library(tradeflows)
sawnwood99France <- loadcomtradebycode(productcode = 440799, reportercode = 251, year = "recent")
```

Load sawnwood data for all countries in the last 5 years:
The next command will take several minutes to more than an hour 
to complete, because of the [Comtrade API usage limits](http://comtrade.un.org/data/doc/api/) of one request per second and 100 requests per hour.
```
sawnwood99 <- loadcomtradewithpause(440799,path="/tmp")
```

* [Comtrade data extraction interface](http://comtrade.un.org/data/) beta
* [Comtrade API release notes](http://comtrade.un.org/data/doc/releasenotes/)
 first release in February 2014. 


### Loading data from a MySQL database
The Comtrade API limits downloads to a few products per hour. 
As a result, downloading all forest products can take several days.
EFI created a data harvester that loads trade flows 
data from the Comtrade API into a MySQL database. 
Other database engines such as SQLite might be added in the future.
All forest based products are available in a database dump.
Many functions in this tradeflows package are intended 
to read and write data from a MySQL database. 
MySQL server can be downloaded from [dev.mysql.com/](http://dev.mysql.com/downloads/).
After you have installed mysql on your system and created a database called "tradeflows", this shell command can be used to load a database dump into the MySQL server:
```
cat raw_flow_yearly.sql | mysql -u username -p tradeflows
```
Then from the R command prompt, load the package and 
display the location of the database configuration file:
```
library(tradeflows)
setdatabaseconfig()
```
You can edit the given configuration file 
to enter your user name, host, password and database name.
Once you have edited the database configuration file
it can reloaded with: `setdatabaseconfig(reload=TRUE)` 


## Cleaning 
Data modification steps are implemented in the clean function.
See `help(clean)` for more information.
Based on the data loaded from comtrade above:
```
sawnwood99 <- renamecolumns(sawnwood99)
sawnwood99_validated <- clean(sawnwood99)
```

You can create an Excel file containing 
conversion factors, unit prices,
choice description and the impact of the cleaning procedure 
on the total volume of world trade flows.
This function requires the [xlsx package](https://cran.r-project.org/web/packages/xlsx/index.html).
```
clean2excel(sawnwood99,"sawnwood99.xlsx",tempdir())
```
The Excel file will be located in the directory visible under
the command `tempdir()`.

## Output
### Data output
Each line in the database table `validated_flow_yearly` 
contains information about one flow,
for a unique combination of 
c("reportercode", "partnercode", "productcode", "flow", "year")
For more information on this unique combination,
see the merge part of the `addpartnerflow()` function, 
Each single record can be caracterized by 6 figures:

1. weight, tradevalue and quantity as reported by the _reporter_ 
2. weight, tradevalue and quantity as reported by the _partner_ 

### Reports and visualisation
Reporting templates are placed in inst/templates.
Once the package is installed, reports will be placed in a 
"report" folder by default.

These commands will only work once a database has been installed 
and configured.
```
createcompletenessreport(productcode_ = 440710)
creatediscrepancyreport(productcode_ = 440799, reporter_ = "Germany")
createoverviewreport(reporter_ = "Italy")
```
