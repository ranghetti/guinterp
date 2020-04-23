## File with the content of the modal dialogs with documentation.
# Since text strings are quite longs, text labels are stored here instead than
# within files translation_<l>.csv, so to avoid using very long text records.

shiny::observeEvent(input$help_load_borders, {
  shiny::req(i18n$translation_language)
  switch(
    i18n$translation_language,

    en = shiny::showModal(shiny::modalDialog(
      title = "Select border polygons",
      size = "m",
      shiny::p(
        "It is possible to load a vector file containing the polygons in which",
        "points must be interpolated:",
        "in this case, the user can choose to threat each file record",
        "as a separate polygon (default setting),",
        "so that points related to different polygons will be interpolated separately;",
        "otherwise, it is possible to specify which variable identifies different",
        "polygons (using it as an ID), or to dissolve all records",
        "in a unique polygon."
      ),
      shiny::p(
        "If a polygon vector file is not specified, a single border can be",
        "automatically generated using the bounding box of the points."
      ),
      shiny::p(
        "In both the cases, borders can be perfected after loading them,",
        "setting a maximum distance from points",
        "in the output format definition."
      ),
      easyClose = TRUE,
      footer = NULL
    )),

    it = shiny::showModal(shiny::modalDialog(
      title = "Seleziona i poligonali dei bordi",
      size = "m",
      shiny::p(
        "\u00e8 possibile caricare un file vettoriale contenente i poligoni entro",
        "cui i punti andranno interpolati:",
        "in questo caso, si pu\u00f8 scegliere di considerare",
        "ogni record del poligonale come un poligono separato (opzione predefinita)",
        "in modo che i punti entro poligoni diversi vengano interpolati separatamente,",
        "oppure si pu\u00f8 selezionare la variabile del file contenente gli",
        "identificativi dei poligoni o scegliere di fondere tutte le geometrie",
        "in un unico poligono."
      ),
      shiny::p(
        "In assenza di un vettoriale dei poligoni, un singolo bordo pu\u00f8",
        "essere automaticamente generato usando il",
        "bounding box dei punti caricati."
      ),
      shiny::p(
        "In entrambi i casi, i bordi potranno essere ulteriormente raffinati dopo",
        "il caricamento, impostando una distanza massima dai punti",
        "nella definizione del formato di output."
      ),
      easyClose = TRUE,
      footer = NULL
    ))

  )
})


shiny::observeEvent(input$help_load_inputpts, {
  shiny::req(i18n$translation_language)
  switch(
    i18n$translation_language,

    en = shiny::showModal(shiny::modalDialog(
      title = "Select files with points to be interpolated",
      size = "s",
      shiny::p(
        "Points to be interpolated can be loaded both as",
        "vector spatial files and as tabular (CSV, XLSX or XLS).",
        "The variable containing numeric values",
        "to be interpolated must be specified;",
        "moreover, in the case of tabular files, the two fields containing",
        "geographic coordinates must be also indicated."
      ),
      easyClose = TRUE,
      footer = NULL
    )),

    it = shiny::showModal(shiny::modalDialog(
      title = "Seleziona i file dei punti da interpolare",
      size = "s",
      shiny::p(
        "I punti che devono essere interpolati possono essere caricati sia",
        "in formato vettoriale che tabellare (CSV, XLSX o XLS).",
        "Nel primo caso \u00e8 necessario specificare la variabile",
        "contenente i valori numerici da interpolare",
        "mentre nel secondo, oltre a questa, vanno indicate anche le due variabili",
        "contenenti le coordinate geografiche."
      ),
      easyClose = TRUE,
      footer = NULL
    ))

  )
})


shiny::observeEvent(input$help_out_proj, {
  shiny::req(i18n$translation_language)
  switch(
    i18n$translation_language,

    en = shiny::showModal(shiny::modalDialog(
      title = "Coordinate Reference System",
      size = "s",
      shiny::p(shiny::HTML(
        "Specify the output CRS",
        "providing the PROJ.4 string, <i>or</i>",
        "the EPSG numeric code (e.g. 32632), <i>or</i>",
        "the UTM zone (e.g. 32N)."
      )),
      easyClose = TRUE,
      footer = NULL
    )),

    it = shiny::showModal(shiny::modalDialog(
      title = "Sistema di riferimento",
      size = "s",
      shiny::p(shiny::HTML(
        "Specificare il sistema di riferimento dei raster in output,",
        "indicando la stringa PROJ.4 del CRS, <i>oppure</i>",
        "il codice numerico EPSG (es. 32632), <i>oppure</i>",
        "la zona UTM (es. 32N)."
      )),
      easyClose = TRUE,
      footer = NULL
    ))

  )
})


shiny::observeEvent(input$help_turbo, {
  shiny::req(i18n$translation_language)
  switch(
    i18n$translation_language,

    en = shiny::showModal(shiny::modalDialog(
      title = "Computation efficiency",
      size = "s",
      shiny::p(shiny::HTML(
        "Interpolation speed can be considerably increased",
        "setting this option to \"Server/workstation usage\";",
        "nevertheless, be careful to do it only if the app is used on ",
        "a PC with at least 8 CPU and 16 GB RAM, otherwise it is easy to",
        "saturate harware resources."
      )),
      shiny::p(shiny::HTML(
        "Leaving the default value \"Desktop usage\", interpolation can be slower",
        "(a single socket is used), but it uses less hardware resources."
      )),
      easyClose = TRUE,
      footer = NULL
    )),

    it = shiny::showModal(shiny::modalDialog(
      title = "Efficienza di calcolo",
      size = "s",
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

  )
})


shiny::observeEvent(input$help_maxptdist, {
  shiny::req(i18n$translation_language)
  switch(
    i18n$translation_language,

    en = shiny::showModal(shiny::modalDialog(
      title = "Maximum distance from points",
      size = "s",
      shiny::p(shiny::HTML(
        "It is possible to specify a maximum distance from points,",
        "beyond which interpolation is not performed: to do it,",
        "set the forst selector to \"Yes\", and set the desired distance",
        "in the second selector."
      )),
      shiny::p(shiny::HTML(
        "Setting the first selector to \"No\", the whole polygons surface",
        "is interpolated (<strong>warning:</strong> reliability of output values",
        "decrease with increasing the distance from the nearest point!)."
      )),
      easyClose = TRUE,
      footer = NULL
    )),

    it = shiny::showModal(shiny::modalDialog(
      title = "Distanza massima dai punti",
      size = "s",
      shiny::p(shiny::HTML(
        "\u00C8 possibile indicare una distanza massima dai punti",
        "oltre la quale non effettuare interpolazioni: per farlo,",
        "impostare il primo selettore su \"S\u00EC\", e indicare la distanza voluta",
        "con il secondo selettore."
      )),
      shiny::p(shiny::HTML(
        "Impostando il primo selettore su \"No\" tutta la superficie dei poligoni",
        "viene interpolata (<strong>attenzione:</strong> l'attendibilit\u00E0",
        "delle stime diminuisce all'aumentare della distanza dal punto pi\u00F9 vicino!)."
      )),
      easyClose = TRUE,
      footer = NULL
    ))

  )
})


shiny::observeEvent(input$help_interp_method, {
  shiny::req(i18n$translation_language)
  switch(
    i18n$translation_language,

    en = shiny::showModal(shiny::modalDialog(
      title = "Interpolation method",
      size = "m",
      shiny::p(shiny::HTML(
        "<ul><li><strong>Ordinary Kriging</strong>:",
        "it is the default interpolation method,",
        "which allows obtaining more reliable results;",
        "conversely, computation time increases like the second powers",
        "of the point number overlapping each polygon.</li>",
        "<li><strong>Inverse Distance Weighted</strong> (IDW):",
        "it is a faster interpolation method,",
        "unless results being less accurate,",
        "particularly if points were not adequately filtered."
      )),
      easyClose = TRUE,
      footer = NULL
    )),

    it = shiny::showModal(shiny::modalDialog(
      title = "Metodo di interpolazione",
      size = "m",
      shiny::p(shiny::HTML(
        "<ul><li><strong>Kriging ordinario</strong>:",
        "\u00E8 il metodo predefinito di interpolazione,",
        "e consente di ottenere risultati pi\u00F9 affidabili;",
        "di contro, il tempo di calcolo aumenta in modo quadratico",
        "rispetto alla numerosit\u00E0 dei punti in ogni poligono.</li>",
        "<li><strong>Distanza inversa ponderata</strong> (IDW):",
        "\u00E8 un metodo pi\u00F9 veloce per effettuare l'interpolazione spaziale,",
        "ma i risultati sono meno affidabili,",
        "soprattutto nel caso il filtraggio dei punti sia stato saltato",
        "o effettuato in maniera approssimativa."
      )),
      easyClose = TRUE,
      footer = NULL
    ))

  )
})


shiny::observeEvent(input$help_auto_vgm, {
  shiny::req(i18n$translation_language)
  switch(
    i18n$translation_language,

    en = shiny::showModal(shiny::modalDialog(
      title = "Variogram definition",
      size = "m",
      shiny::p(shiny::HTML(
        "<ul><li><strong>Automatic</strong>:",
        "kriging is performed automatically modelling",
        "the autocorrelation of points (after filtering them),",
        "using an exponential model.",
        "Computation is performed polygon by polygon, so polygons characterised",
        "by different autocorrelation values will be interpolated using different models.</li>",
        "<li><strong>Semiautomatic</strong>:",
        "it is similar to the previous case, with the possibility to choose the model type",
        "(exponential, spheroidal or gaussian) and to set the range value",
        "within the automatically- defined semivariograms",
        "(this feature allows obtaining similar results among polygons).",
        "Using this modality (or the automatic one) is suggested if",
        "polygons to be interpolated are etherogeneous;",
        "nevertheless, choosing an automatic or semiautomatic mode",
        "does not grant the interpolation to be correct if models",
        "do not converge.</li>",
        "<li><strong>Manual</strong>:",
        "all the parameters of the semivariogram can be defined by the user,",
        "allowing him to control them using the interactive plot.",
        "This is the suggested mode if a single polyogn is being interpolated,",
        "or if polygons are omogeneous, because it provides the higher user control",
        "on what is done; it is not suggested if polygons are etherogeneous,",
        "since the defined model is unique, so the same model is applied",
        "to all the polygons.</li></ul>"
      )),
      easyClose = TRUE,
      footer = NULL
    )),

    it = shiny::showModal(shiny::modalDialog(
      title = "Definizione del variogramma",
      size = "m",
      shiny::p(shiny::HTML(
        "<ul><li><strong>Automatica</strong>:",
        "il kriging viene effettuato andando a modellare automaticamente",
        "l'autocorrelazione dei punti (selezionati mediante il filtraggio),",
        "usando un modello esponenziale.",
        "Il calcolo \u00E8 effettuato poligono per poligono, pertanto poligoni",
        "con autocorrelazioni diverse verranno interpolati usando modelli diversi.</li>",
        "<li><strong>Semiautomatica</strong>:",
        "\u00E8 simile alla precedente, con la possibilit\u00E0 di scegliere il tipo di modello",
        "(esponenziale, sferoidale, gaussiano o a onda) e di fissare il valore",
        "del <em>range</em> nei semivariogrammi modellati automaticamente",
        "(questo consente di ottenere risultati visivamente simili tra i campi).",
        "L'uso di questa modalit\u00E0 (o di quella automatica) \u00E8 consigliato in caso",
        "di interpolazione di campi eterogenei tra loro;",
        "ci\u00f2 nonostante, la scelta di una modalit\u00E0 automatica o semiautomatica",
        "non garantisce che l'interpolazione sia corretta in caso di mancata",
        "convergenza nella modellazione dei punti.</li>",
        "<li><strong>Manuale</strong>:",
        "tutti i parametri del semivariogramma possono essere definiti dall'utente",
        "consentendogli di controllarli usando il grafico interattivo.",
        "\u00E8 la modalit\u00E0 consigliata in caso di interpolazione di un singolo poligono",
        "o di campi tra loro omogenei, in quanto fornisce il maggior controllo",
        "possibile all'utente; \u00E8 invece sconsigliata se i campi sono tra loro",
        "eterogenei, dato che il modello definito \u00E8 unico per l'interpolazione",
        "di tutti i campi.</li></ul>"
      )),
      easyClose = TRUE,
      footer = NULL
    ))

  )
})


shiny::observeEvent(input$help_filter_buttons, {
  shiny::req(i18n$translation_language)
  switch(
    i18n$translation_language,

    en = shiny::showModal(shiny::modalDialog(
      title = "Points filtering",
      size = "m",
      shiny::p(shiny::HTML(
        "Filtering allows removing points which is not useful to consider",
        "during the interpolation."
      )),
      shiny::p(shiny::HTML(
        "One of the following filtering modes must be chosen:",
        "<ul><li><strong>Manual</strong>:",
        "filtering rules are defined by the user with a specific GUI,",
        "which becomes visible when \"Manual\" is selected.</li>",
        "<li><strong>Default</strong>:",
        "only points matching the default rules are used",
        "(default rules can be set with the GUI accessible with the \"Manual\" mode);",
        "predefined default conditions are the followings:",
        "<ol><li>values included between the 2° and the 98° percentiles",
        "of the distribution of each polygon;</li>",
        "<li>values further than 5 metres from the polygons' borders.</li></ol></li>",
        "<li><strong>Only extremes</strong>:",
        "all points except extreme values are used",
        "(so points with values included between the 2° and the 98° percentiles",
        "of the distribution of each polygon;</li>",
        "<li><strong>None</strong>:",
        "all points are used.</li></ul>"
      )),
      easyClose = TRUE,
      footer = NULL
    )),

    it = shiny::showModal(shiny::modalDialog(
      title = "Filtraggio dei punti",
      size = "m",
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
        "<ol><li>i cui valori ricadano tra il 2° e il 98° percentile",
        "della distribuzione di ogni campo;</li>",
        "<li>che si trovino a pi\u00F9 di 5 m di distanza dal bordo del campo.</li></ol></li>",
        "<li><strong>Solo valori estremi</strong>:",
        "vengono automaticamente utilizzati tutti i punti",
        "ad eccezione dei valori estremi (ovvero i cui valori",
        "ricadano tra il 2° e il 98° percentile della distribuzione di ogni campo);</li>",
        "<li><strong>Nessuno</strong>:",
        "vengono utilizzati tutti i punti.</li></ul>"
      )),
      easyClose = TRUE,
      footer = NULL
    ))

  )
})


shiny::observeEvent(input$help_focal, {
  shiny::req(i18n$translation_language)
  switch(
    i18n$translation_language,

    en = shiny::showModal(shiny::modalDialog(
      title = "Smoothing rasters",
      size = "s",
      shiny::p(shiny::HTML(
        "Smoothing raster after having interpolated them",
        "is an operation useful to reduce residual anomalies",
        "(particularly after using an IDW interpolation method),",
        "and if the variogram range is comparable to the raster resolution)."
      )),
      shiny::p(
        "If this option is active (default), a smoothing with a weighted mean",
        "is applied to raster, using a Gaussian filter",
        "with a radius equal to the raster resolution."
      ),
      easyClose = TRUE,
      footer = NULL
    )),

    it = shiny::showModal(shiny::modalDialog(
      title = "Livellamento dei raster",
      size = "s",
      shiny::p(shiny::HTML(
        "Applicare una perequazione (<em>smoothing</em>) ai raster interpolati",
        "\u00e8 un'operazione utile per livellare eventuali irregolarit\u00e0",
        "(soprattutto se l'interpolazione \u00e8 stata effettuata con IDW,",
        "oppure se il <em>range</em> del variogramma ha un valore simile",
        "alla risoluzione del raster)."
      )),
      shiny::p(
        "Se questa opzione è attiva (scelta predefinita), ai raster interpolati",
        "viene applicata una perequazione con media mobile utilizzando un filtro",
        "gaussiano con raggio parti alla risoluzione del raster."
      ),
      easyClose = TRUE,
      footer = NULL
    ))

  )
})


shiny::observeEvent(input$help_v_options, {
  shiny::req(i18n$translation_language)
  switch(
    i18n$translation_language,

    en = shiny::showModal(shiny::modalDialog(
      title = "Advanced settings",
      size = "m",
      shiny::p(shiny::HTML(
        "<em>Note:</em> these settings influences the processing speed;",
        "wrong values could affect the reliability of the outputs.",
        "Ensure to edit them only being aware of what is being done."
      )),
      shiny::p(shiny::HTML(
        "<ul><li><strong>Maximum distance points-pixels</strong>:",
        "it corresponds to the argument \"maxdist\" of functions",
        "<tt><a href='https://www.rdocumentation.org/packages/gstat/versions/2.0-2/topics/krige'",
        "target='_blank'>krige</a>()</tt> and",
        "<tt><a href='https://www.rdocumentation.org/packages/gstat/versions/2.0-2/topics/idw'",
        "target='_blank'>idw</a>()</tt>;",
        "value is automatically determied as the 150% of the variogram range.</li>",
        "<li><strong>Numbers of points per pixels</strong>:",
        "it corresponds to the argument \"nmax\" of functions",
        "<tt><a href='https://www.rdocumentation.org/packages/gstat/versions/2.0-2/topics/krige'",
        "target='_blank'>krige</a>()</tt> and",
        "<tt><a href='https://www.rdocumentation.org/packages/gstat/versions/2.0-2/topics/idw'",
        "target='_blank'>idw</a>()</tt>;",
        "default value is 500 (or the total number of points",
        "if this value is lower than 500).</li></ul>"
      )),
      easyClose = TRUE,
      footer = NULL
    )),

    it = shiny::showModal(shiny::modalDialog(
      title = "Opzioni avanzate di interpolazione",
      size = "m",
      shiny::p(shiny::HTML(
        "<em>Nota:</em> queste impostazioni hanno effetto sulla velocità",
        "di processamento dell'interpolazione;",
        "valori scorretti potrebbero inficiare la validità dell'output.",
        "Assicurarsi di modificarli solo se si è consapevoli di quello",
        "che si sta facendo."
      )),
      shiny::p(shiny::HTML(
        "<ul><li><strong>Distanza massima punti-pixel</strong>:",
        "corrisponde all'argomento \"maxdist\" delle funzioni",
        "<tt><a href='https://www.rdocumentation.org/packages/gstat/versions/2.0-2/topics/krige'",
        "target='_blank'>krige</a>()</tt> e",
        "<tt><a href='https://www.rdocumentation.org/packages/gstat/versions/2.0-2/topics/idw'",
        "target='_blank'>idw</a>()</tt>;",
        "il valore determinato automaticamente corrisponde al 150% del",
        "<em>range</em> del semivariogramma.</li>",
        "<li><strong>Numero di punti per pixel</strong>:",
        "corrisponde all'argomento \"nmax\" delle funzioni",
        "<tt><a href='https://www.rdocumentation.org/packages/gstat/versions/2.0-2/topics/krige'",
        "target='_blank'>krige</a>()</tt> e",
        "<tt><a href='https://www.rdocumentation.org/packages/gstat/versions/2.0-2/topics/idw'",
        "target='_blank'>idw</a>()</tt>;",
        "il valore determinato automaticamente è 500 (oppure il numero",
        "totale di punti nel caso questo sia minore di 500).</li></ul>"
      )),
      easyClose = TRUE,
      footer = NULL
    ))

  )
})
