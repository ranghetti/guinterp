### Content of the sidebar ###
shiny::div(

  ## Input selection (visible when interp_onoff is OFF)
  shiny::conditionalPanel(
    condition = "!output.interp_onoff",
    shiny::div(
      shiny::div(
        shiny::span(
          shiny::strong("Scegli il vettoriale dei poligoni\u2000"),
          shiny::actionLink("help_fields_path", shiny::icon("question-circle"))
        ),
        shiny::div(
          style = "margin-top:10px;",
          actionButton(
            "button_load_borders",
            label = "\u2000Carica",
            class = "darkbutton",
            width = "100%",
            icon=icon("upload")
          )
        )
      ),
      shiny::div(
        shiny::span(
          shiny::strong("Scegli uno o pi\u00F9 files puntiformi\u2000"),
          shiny::actionLink("help_rawdata_paths", shiny::icon("question-circle"))
        ),
        shiny::div(
          style = "margin-top:10px;",
          actionButton(
            "button_load_inputpts",
            label = "\u2000Carica",
            class = "darkbutton",
            width = "100%",
            icon=icon("upload")
          )
        )
      )
    )
  ), # end of input conditionalPanel

  shiny::conditionalPanel(
    condition = "output.interp_onoff",
    shiny::div(
      shiny::div(
        style = "margin-top:10px;",
        actionButton(
          "do_leave_yield_interp",
          label = "\u2000Esci",
          class = "darkbutton",
          width = "100%",
          icon=icon("upload")
        )
      ),
      shiny::div(
        style = "margin-top:10px;",
        actionButton(
          "interp_button",
          label = "\u2000Interpola",
          class = "darkbutton",
          width = "100%",
          icon=icon("upload")
        )
      )
    )
  )

)
