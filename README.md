Trade Flows
-----------
Steve Johnson described this work as: 
Procedures to automatically grab data from the 
COMTRADE and COMEXT system, feed it through algorithms to compare 
and amend unit values and mirror export and
import values, and then to deliver to the online user-interface.


Examples and demonstration
------------------------
See example use in 


Workflow
--------
### Input
Data from the comtrade API
### Output



Tools
-----
#### This is a package
Created based on [instructions from Hadley](http://r-pkgs.had.co.nz/).
`devtools::load_all()` or __Cmd + Shift + L__, reloads all code in the package.
Add packages to the list of required packages
`devtools::use_package("dplyr")`
`devtools::use_package("ggplot2", "suggests")`
For data I followed his recommendations in r-pkgs/data.rmd
`devtools::use_data(mtcars)`
`devtools::use_data_raw()` # To create a data-raw/ folder and add it to .Rbuildignore
    
## Version tracking system with git
The .git repository is backed on bitbucket.
Use devtools::install_bitbucket() to install the package.

### Packages used
These should be mentionned in the package description file.


### Shiny
A demonstration with time series plot and bar chart will be made
with shiny and the ggplot2 package, based on the diamond example using.
