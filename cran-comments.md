## Test environments
* macOS-latest, R release
* windows-latest, R 3.6
* ubuntu-16.04, R devel
* ubuntu-16.04, R release
* ubuntu-16.04, R oldrel
* ubuntu-16.04, R 3.5

See more info [here](https://github.com/PhilPalmer/AutoPlate/actions)

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 3 NOTES:
❯ checking package dependencies ... NOTE
  Imports includes 22 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

The package has a lot of dependencies. 
Depending on the fucntions the user needs the R package should continue to work if some of the packages cannot be installed.

❯ checking top-level files ... NOTE
  Non-standard files/directories found at top level:
    ‘dev’ ‘pkgdown’

While the `dev` directory is non-standard for R packages it standard for [Golem](https://github.com/ThinkR-open/golem) R Shiny apps
`pkgdown` directory only contains a `.yml` configuration file to ensure the website for the documentation is built correctly

❯ checking R code for possible problems ... NOTE
app_server: no visible global function definition for ‘setNames’
  app_server : <anonymous>: no visible binding for global variable
    ‘types’
  app_server : <anonymous>: no visible binding for global variable
    ‘exclude’
  app_server : <anonymous>: no visible binding for global variable
    ‘virus’
  app_server: no visible binding for global variable ‘types’
  app_server: no visible binding for global variable ‘exclude’
  app_server: no visible binding for global variable ‘virus’
  exclude_wells: no visible binding for global variable ‘plate_number’
  exclude_wells: no visible binding for global variable ‘exclude’
  exclude_wells: no visible binding for global variable ‘wcol’
  exclude_wells: no visible binding for global variable ‘wrow’
  init_assay_df: no visible binding for global variable ‘filename’
  init_assay_df: no visible binding for global variable ‘WellPosition’
  .....
  Consider adding
    importFrom("graphics", "par")
    importFrom("stats", "predict", "setNames")
  to your NAMESPACE file.
  
  Found the following calls to attach():
  File ‘autoplate/R/fct_3_results.R’:
    attach(update_cols_order(assay_df), warn.conflicts = FALSE)
    attach(update_cols_order(assay_df), warn.conflicts = FALSE)
    attach(update_cols_order(assay_df), warn.conflicts = FALSE)
  See section ‘Good practice’ in ‘?attach’.
  
  Found the following calls to data() loading into the global environment:
  File ‘autoplate/R/app_server.R’:
    data(dilutions_pmn)
    data(dilutions_ella)
    data("example_data_column_descriptions")
  See section ‘Good practice’ in ‘?data’.
  ......

Edited for brevity
I don't believe that any of these variables should be global varialbes. They are mainly the names of columns in a dataframe which is used as an input to serveral funcions
The `autoplate` R library can either be run as a Shiny app or the functions used in the app can be run within R. 
These notes regarding attaching data in the global environment will only happen when running the R shiny app and not when using the functions from the package.