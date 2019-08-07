shiny::observeEvent(input$help_out_proj, {
  shiny::showModal(shiny::modalDialog(
    title = "Sistema di riferimento",
    shiny::p(shiny::HTML(
      "Specificare il sistema di riferimento dei raster in output,",
      "indicando la stringa PROJ4 del CRS, <i>oppure</i>",
      "il codice numerico EPSG (es. 32632), <i>oppure</i>",
      "la zona UTM (es. 32N)."
    )),
    easyClose = TRUE,
    footer = NULL
  ))
})


shiny::observeEvent(input$help_turbo, {
  shiny::showModal(shiny::modalDialog(
    title = "Efficienza di calcolo",
    shiny::p(shiny::HTML(
      "La velocit\u00E0 dell'interpolazione spaziale pu\u00F2 essere notevolmente",
      "incrementata impostando questa opzione su \"Uso server/workstation\";",
      "tuttavia, effettuare questa scelta solo se l'app \u00E8 utilizzata su",
      "una macchina con almeno 8 CPU e 16 GB di RAM, altrimenti vi \u00E8",
      "un elevato rischio di saturare le risorse hardware."
    )),
    shiny::p(shiny::HTML(
      "Lasciando l'opzione su \"Uso desktop\" l'interpolazione \u00E8 pi\u00F9 lenta",
      "(viene utilzizato un solo socket) ma richiede meno risorse."
    )),
    easyClose = TRUE,
    footer = NULL
  ))
})


shiny::observeEvent(input$help_maxptdist, {
  shiny::showModal(shiny::modalDialog(
    title = "Distanza massima dai punti",
    shiny::p(shiny::HTML(
      "\u00E8 possibile indicare una distanza massima dai punti di resa",
      "oltre la quale non effettuare interpolazioni: per farlo,",
      "impostare il primo selettore su \"S\u00EC\", e indicare la distanza voluta",
      "con il secondo selettore."
    )),
    shiny::p(shiny::HTML(
      "Impostando il primo selettore su \"No\" tutta la superficie dei campi",
      "viene interpolata (<strong>attenzione:</strong> l'attendibilit\u00E0",
      "delle stime diminuisce all'aumentare della distanza dal punto pi\u00F9 vicino!)."
    )),
    easyClose = TRUE,
    footer = NULL
  ))
})


shiny::observeEvent(input$help_interp_method, {
  shiny::showModal(shiny::modalDialog(
    title = "Metodo di interpolazione",
    shiny::p(shiny::HTML(
      "<ul><li><strong>Kriging ordinario</strong>:",
      "\u00E8 il metodo predefinito di interpolazione,",
      "e consente di ottenere risultati pi\u00F9 affidabili;",
      "di contro, il tempo di calcolo aumenta in modo quadratico",
      "rispetto alla numerosit\u00E0 dei punti in ogni campo.</li>",
      "<li><strong>Distanza inversa ponderata</strong> (IDW):",
      "\u00E8 un metodo pi\u00F9 veloce per effettuare l'interpolazione spaziale,",
      "ma i risultati sono meno affidabili,",
      "soprattutto nel caso il filtraggio dei punti sia stato saltato",
      "o effettuato in maniera approssimativa."
    )),
    easyClose = TRUE,
    footer = NULL
  ))
})


shiny::observeEvent(input$help_auto_vgm, {
  shiny::showModal(shiny::modalDialog(
    title = "Definizione del variogramma",
    shiny::p(shiny::HTML(
      "<ul><li><strong>Automatica</strong>:",
      "il kriging viene effettuato andando a modellare automaticamente",
      "l'autocorrelazione dei punti (selezionati mediante il filtraggio),",
      "usando un modello esponenziale.",
      "Il calcolo \u00E8 effettuato campo per campo, pertanto campi",
      "con autocorrelazioni diverse verranno interpolati usando modelli diversi.</li>",
      "<li><strong>Semiautomatica</strong>:",
      "\u00E8 simile alla precedente, con la possibilit\u00E0 di scegliere il tipo di modello",
      "(esponenziale, sferoidale, gaussiano o a onda) e di fissare il valore",
      "del <em>range</em> nei semivariogrammi modellati automaticamente",
      "(questo consente di ottenere risultati visivamente simili tra i campi).",
      "L'uso di questa modalit\u00E0 (o di quella automatica) \u00E8 consigliato in caso",
      "di interpolazione di campi eterogenei tra loro (es. colture diverse);",
      "ci\u00f2 nonostante, la scelta di una modalit\u00E0 automatica o semiautomatica",
      "non garantisce che l'interpolazione sia corretta in caso di mancata",
      "convergenza nella modellazione dei punti.</li>",
      "<li><strong>Manuale</strong>:",
      "tutti i parametri del semivariogramma possono essere definiti dall'utente",
      "consentendogli di controllarli usando il grafico interattivo.",
      "\u00E8 la modalit\u00E0 consigliata in caso di interpolazione di un singolo campo",
      "o di campi tra loro omogenei, in quanto fornisce il maggior controllo",
      "possibile all'utente; \u00E8 invece sconsigliata se i campi sono tra loro",
      "eterogenei, dato che il modello definito \u00E8 unico per l'interpolazione",
      "di tutti i campi.</li></ul>"
    )),
    easyClose = TRUE,
    footer = NULL
  ))
})


shiny::observeEvent(input$help_filter_buttons, {
  shiny::showModal(shiny::modalDialog(
    title = "Filtraggio dei punti",
    shiny::p(shiny::HTML(
      "Il filtraggio permette di eliminare i punti che non si vuole utilizzare",
      "nel corso dell'interpolazione."
    )),
    shiny::p(shiny::HTML(
      "Per poter avviare l'interpolazione \u00E8 necessario scegliere una",
      "delle tre opzioni sottostanti:",
      "<ul><li><strong>Manuale</strong>:",
      "il filtraggio viene controllato dall'utente tramite una GUI apposita,",
      "che si apre dopo aver cliccato sul bottone.</li>",
      "<li><strong>Predefinito</strong>:",
      "vengono automaticamente utilizzati solo i punti",
      "che rispecchiano le condizioni dei filtri impostate come predefinite",
      "(tali condizioni possono essere impostate nella modalit\u00E2 manuale);",
      "di base, le condizioni sono di selezionare i punti:",
      "<ol><li>i cui valori di resa ricadano tra il 2° e il 98° percentile",
      "della distribuzione di ogni campo;</li>",
      "<li>che si trovino a pi\u00F9 di 5 m di distanza dal bordo del campo.</li></ol></li>",
      "<li><strong>Solo valori estremi</strong>:",
      "vengono automaticamente utilizzati tutti i punti",
      "ad eccezione dei valori estremi (ovvero i cui valori di resa",
      "ricadano tra il 2° e il 98° percentile della distribuzione di ogni campo);</li>",
      "<li><strong>Nessuno</strong>:",
      "vengono utilizzati tutti i punti."
    )),
    easyClose = TRUE,
    footer = NULL
  ))
})



