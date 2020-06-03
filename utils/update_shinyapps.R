
# Before running this, rebase branch "shinyapps" on "develop"

# remotes::install_github("ranghetti/guinterp", ref = "develop")
remotes::install_github("r-spatial/lwgeom", ref = "467dc314a0bda011e806a8b56c8dcc153f63527c")
devtools::load_all(".") # local branch shinyapps
rsconnect::appDependencies(system.file(package="guinterp"))
rsconnect::deployApp(
  # system.file(package="guinterp"),
  appName = "guinterp",
  appTitle = "GUInterp: a Shiny GUI for Spatial Interpolation",
  appId = "2387850",
  account = "ranghetti"
)
