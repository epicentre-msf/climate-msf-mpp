mod_stopover_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    h5("Itinerary"),
    hr(),
    div(
      id = ns("inputs"),
      stopover_input(
        ns,
        index = 1,
        city_lab = tooltip(
          span("Steps of the itinary", bsicons::bs_icon("info-circle")),
          "Input cities travelled to"
        )
      ),
      stopover_input(ns, index = 2, link_input = TRUE),
    ),
    div(
      class = "d-flex mb-3 justify-content-end",
      div(
        class = "pe-2",
        actionButton(
          ns("remove"),
          "-",
          class = "btn-danger btn-sm"
        ) %>% tooltip("Remove a city", placement = "top")
      ),
      actionButton(
        ns("add"),
        "+",
        class = "btn-info btn-sm"
      ) %>% tooltip("Add another city", placement = "top")
    )
  )
}

mod_stopover_input_server <- function(id, cities) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # set this to number of inputs you starts with
    n_inputs <- reactiveVal(2)

    # only enable remove option when number of inputs is > 2
    observe({
      n <- n_inputs()
      shinyjs::toggleState("remove", condition = n > 2)
    })

    observe({
      shinyWidgets::updateVirtualSelect("p1", choices = cities, selected = "PAR")
      shinyWidgets::updateVirtualSelect("p2", choices = cities, selected = "DKR")
    })

    observeEvent(input$add, {
      index <- n_inputs() + 1
      insertUI(
        selector = paste0("#", ns("inputs")),
        where = "beforeEnd",
        ui = stopover_input(
          ns,
          index,
          choices = cities,
          link_input = TRUE
        )
      )
      n_inputs(index)
    })

    observeEvent(input$remove, {
      index <- n_inputs()
      removeUI(selector = paste0("#", "origin", index))
      n_inputs(index - 1)
    })

    df <- reactive({
      index <- n_inputs()
      #req(input[[paste0("n", index)]])
      selected_cities <- purrr::map_chr(1:index, ~ input[[paste0("p", .x)]])
      data.frame(
        start_var = head(selected_cities, -1),
        end_var = tail(selected_cities, - 1)
      )
    })

    # return stop overs df
    reactive(df())
  })
}

stopover_input <- function(
    ns,
    index,
    choices = NULL,
    choices_link = NULL,
    selected = NULL,
    city_lab = NULL,
    link_input = FALSE
) {
  div(
    id = paste0("origin", index),
    #class = "d-flex p-0 justify-content-center",

    if(link_input){
      div(
        class = "d-flex justify-content-center",
        shinyWidgets::virtualSelectInput(
          label = "Type of travel",
          choices = c("Car", "Rail", "Plane"),
          selected = c("Plane"),
          placeholder = "Travel type",
          width = "50%",
          inputId = ns(paste0("link_", index))
        )
      )},
    div(
      class = "p-0",
      shinyWidgets::virtualSelectInput(
        inputId = ns(paste0("p", index)),
        label = city_lab,
        choices = choices,
        selected = selected,
        search = TRUE,
        autoSelectFirstOption = TRUE,
        placeholder = "Select city...",
        position = "bottom",
        # dropboxWrapper = "body",
        showDropboxAsPopup = FALSE,
        showOptionsOnlyOnSearch = FALSE,
        optionsCount = 5
      )
    )
  )
}
