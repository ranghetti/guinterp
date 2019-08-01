ui_guinterp <- shinydashboard::dashboardPage(
  skin = "red",
  shinydashboard::dashboardHeader(
    title = "GUInterp",
    tags$li(class ="dropdown", tags$h3(
      style = "color:white;margin:0;padding-top:12px;padding-bottom:12px;padding-left:50px;padding-right:50px;",
      "Interpolatore dati puntiformi"
    )),
    tags$li(class ="dropdown", tags$a(
      href="https://github.com/ranghetti/guinterp",
      tags$img(src="github_logo.png"),
      style="margin:0;padding-top:2px;padding-bottom:2px;padding-left:10px;padding-right:10px;",
      target="_blank"
    )),
    tags$li(class ="dropdown", tags$a(
      href="http://www.irea.cnr.it",
      tags$img(src="irea_logo.png"),
      style="margin:0;padding-top:2px;padding-bottom:2px;padding-left:10px;padding-right:10px;",
      target="_blank"
    ))
  ),
  # shinydashboard::dashboardHeader(title="Interpolatore rese"),

  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "tabs",

      # Tags
      shinyalert::useShinyalert(),
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(
        text = "shinyjs.closeWindow = function() { window.close(); }",
        functions = c("closeWindow")
      ),
      shinyjs::extendShinyjs(
        text = "shinyjs.resetApp = function() { history.go(0); }",
        functions = c("resetApp")
      ),
      shiny::tags$head(shiny::tags$style(".darkbutton{background-color:#28353b;color:#b8c7ce;width:200px;")), # background color and font color


      # div(
      #   style = "margin-top:30pt;margin-left:12pt;",
      #   tags$h4("Menu")
      # ),

      # shinydashboard::menuItem("Selezione input", tabName = "tab_input", icon = icon("folder-open")),

      hidden(shinydashboard::menuItem("Interpolazione", tabName = "tab_interp", icon = icon("folder-open"))),

      ### Bar with inputs and commands
      source(
        system.file("apps/guinterp/ui/tab_input_ui.R", package="guinterp"),
        local=TRUE
      )$value

    )
  ),

  shinydashboard::dashboardBody(
    # shiny::tags$head(
    #   shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    # ),
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "tab_interp",
        source(
          system.file("apps/guinterp/ui/tab_interp_ui.R", package="guinterp"),
          local=TRUE
        )$value
      )
    )
  ) # end of dashboardBody

) # end of dashboardPage
