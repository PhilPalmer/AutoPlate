#!/usr/bin/env Rscript

# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp(appTitle="AutoPlate")
# Or use the blue button on top of this file

# Run app
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
autoplate::run_app() # add parameters here (if any)
