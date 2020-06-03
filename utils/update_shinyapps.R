
# BEFORE RUNNING THE SCRIPT:
#
# replace the following chunk in guinterp.R:
#
# shinyOptions(
#   ui_lang = language,
#   demo_mode = demo
# )
#
# with:
#
# shinyOptions(
#   ui_lang = "en",
#   demo_mode = TRUE
# )

remotes::install_github("r-spatial/lwgeom", ref = "467dc314a0bda011e806a8b56c8dcc153f63527c")
devtools::load_all(".") # local branch shinyapps
rsconnect::appDependencies(system.file(package="guinterp"))
rsconnect::deployApp(
  appName = "guinterp",
  appTitle = "GUInterp: a Shiny GUI for Spatial Interpolation",
  appId = "2387850",
  account = "ranghetti"
)
