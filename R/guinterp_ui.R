#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny div fluidRow column conditionalPanel uiOutput plotOutput
#'  strong span actionLink radioButtons textInput htmlOutput checkboxInput hr
#'  sliderInput
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#'  sidebarMenu menuItem dashboardBody tabItems tabItem box
#' @importFrom shinyFiles shinySaveButton shinyDirButton shinyFilesButton
#' @importFrom shinyWidgets switchInput
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom shinyjs disabled hidden
#' @import shiny.i18n
#' @importFrom leaflet leafletOutput
#' @importFrom DT dataTableOutput
#' @noRd
guinterp_ui <- function(request) {
  tagList(

    shinydashboard::dashboardPage(
      skin = "red",
      shinydashboard::dashboardHeader(
        title = "GUInterp",
        tags$li(class ="dropdown", tags$h3(
          style = "color:white;margin:0;padding-top:12px;padding-bottom:12px;padding-left:50px;padding-right:50px;",
          ht("_gui_title")
        )),
        tags$li(class ="dropdown", tags$a(
          href="http://guinterp.ranghetti.info",
          shiny::icon("book"),
          style="margin:0;padding-top:11px;padding-bottom:11px;padding-left:10px;padding-right:10px;font-size:30px;",
          target="_blank"
        )),
        tags$li(class ="dropdown", tags$a(
          href="https://github.com/ranghetti/guinterp",
          shiny::icon("github"),
          style="margin:0;padding-top:11px;padding-bottom:11px;padding-left:10px;padding-right:10px;font-size:30px;",
          target="_blank"
        )),
        tags$li(class ="dropdown", tags$a(
          href="http://www.irea.cnr.it",
          tags$img(src="img/irea_logo.png"),
          style="margin:0;padding-top:2px;padding-bottom:2px;padding-left:10px;padding-right:10px;",
          target="_blank"
        ))
        # tags$li(class ="dropdown", pickerInput(
        #   "sel_language", NULL,
        #   choices = c("en", "it"),
        #   choicesOpt = list(content = c(
        #     "<img src='https://raw.githubusercontent.com/lipis/flag-icon-css/master/flags/4x3/gb.svg' height='100%'>\u2000EN",
        #     "<img src='https://raw.githubusercontent.com/lipis/flag-icon-css/master/flags/4x3/it.svg' height='100%'>\u2000IT"
        #   ))
        # )
      ),

      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          style = "position: fixed; overflow: visible;",
          id = "tabs",

          # Tags
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

          hidden(shinydashboard::menuItem(ht("_Interpolate"), tabName = "tab_interp", icon = icon("folder-open"))),

          ### Bar with inputs and commands
          source(
            system.file("app/ui/tab_input_ui.R", package="guinterp"),
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
              system.file("app/ui/tab_interp_ui.R", package="guinterp"),
              local=TRUE
            )$value
          )
        )
      ) # end of dashboardBody

    ) # end of dashboardPage

  )
}
