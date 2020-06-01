remotes::install_github("ranghetti/guinterp", ref = "develop")
rsconnect::appDependencies(system.file("apps/guinterp", package="guinterp"))
# devtools::load_all(".")
rsconnect::deployApp(
  # system.file(package="guinterp"),
  appName = "guinterp",
  appTitle = "GUInterp: a Shiny GUI for Spatial Interpolation",
  appId = "2387850",
  account = "ranghetti"
)
