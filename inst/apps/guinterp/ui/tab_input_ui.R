### Content of the sidebar ###
shiny::div(

  ## Input selection (visible when interp_onoff is OFF)
  shiny::conditionalPanel(
    condition = "!output.interp_onoff",
    shiny::div(
      style = "margin-top:20px;margin-bottom:50px;",
      shiny::div(
        shiny::div(
          style = "margin-top:10px;padding-left:15px;",
          shiny::span(
            style="display:inline-block",
            shiny::strong("Vettoriale dei poligoni")
          ),
          shiny::span(
            style="display:inline-block",
            shiny::actionLink("help_load_borders", icon("question-circle"))
          )
        ),
        actionButton(
          "button_load_borders",
          label = "\u2000Carica",
          class = "darkbutton",
          icon=icon("upload")
        )
      ),
      shiny::div(
        shiny::div(
          style = "margin-top:10px;padding-left:15px;",
          shiny::span(
            style="display:inline-block",
            shiny::strong("Punti da interpolare")
          ),
          shiny::span(
            style="display:inline-block",
            shiny::actionLink("help_load_inputpts", icon("question-circle"))
          )
        ),
        shinyjs::disabled(actionButton(
          "button_load_inputpts",
          label = "\u2000Carica",
          class = "darkbutton",
          icon=icon("upload")
        ))
      )
    )
  ), # end of input conditionalPanel

  shiny::conditionalPanel(
    condition = "output.interp_onoff",
    shiny::div(
      shiny::div(
        style = "margin-top:20px;margin-bottom:50px;",
        actionButton(
          "interp_button",
          label = strong("\u2000Interpola"),
          class = "darkbutton",
          icon=icon("cogs")
        )
      ),
      shiny::div(
        style = "margin-top:20px;",
        actionButton(
          "close_interp_button",
          label = "\u2000Chiudi",
          class = "darkbutton",
          icon=icon("window-close")
        )
      )
    )
  ),

  shiny::div(
    style = "margin-top:20px;",
    actionButton(
      "close_app_button",
      label = "\u2000Esci dall'interfaccia",
      class = "darkbutton",
      icon=icon("sign-out-alt")
    )
  )

)
