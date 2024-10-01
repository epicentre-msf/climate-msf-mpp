mod_origin_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h5("Origins"),
    hr(),
    div(
      id = ns("inputs"),
      city_input(
        ns,
        index = 1,
        city_lab = tooltip(
          span("City of origin", bsicons::bs_icon("info-circle")),
          "Where are people travelling from?"
        ),
        num_lab = tooltip(
          span("People", bsicons::bs_icon("info-circle")),
          "How many people are travelling from this location?"
        )
      ),
      city_input(ns, index = 2),
    ),
    div(
      class = "d-flex mb-3 justify-content-end",
      div(
        class = "pe-2",
        actionButton(
          ns("remove"),
          "-",
          class = "btn-danger btn-sm"
        ) %>% tooltip("Remove a location", placement = "top")
      ),
      actionButton(
        ns("add"),
        "+",
        class = "btn-info btn-sm"
      ) %>% tooltip("Add another location", placement = "top")
    )
  )
}

mod_origin_input_server <- function(id, cities) {
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
      shinyWidgets::updateVirtualSelect("p2", choices = cities, selected = "LON")
    })
    
    observeEvent(input$add, {
      index <- n_inputs() + 1
      insertUI(
        selector = paste0("#", ns("inputs")),
        where = "beforeEnd",
        ui = city_input(ns, index, choices = cities)
      )
      n_inputs(index)
    })
    
    observeEvent(input$remove, {
      index <- n_inputs()
      removeUI(selector = paste0("#", "origin", index))
      n_inputs(index - 1)
    })
    
    df_origin <- reactive({
      index <- n_inputs()
      req(input[[paste0("n", index)]])
      selected_cities <- purrr::map_chr(1:index, ~ input[[paste0("p", .x)]])
      n_people <- purrr::map_int(1:index, ~ as.integer(input[[paste0("n", .x)]]))
      tibble::tibble(
        origin_id = selected_cities,
        n_participant = n_people
      ) %>% dplyr::filter(origin_id != "", !is.na(n_participant))
    })
    
    # return city df
    reactive(df_origin())
  })
}

city_input <- function(ns,
                       index,
                       choices = NULL,
                       selected = NULL,
                       city_lab = NULL,
                       num_lab = NULL,
                       n_val = 1) {
  div(
    id = paste0("origin", index),
    class = "d-flex p-0 justify-content-center",
    div(
      class = "p-0 flex-grow-1",
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
    ),
    div(
      class = "p-0",
      numericInput(
        inputId = ns(paste0("n", index)),
        label = num_lab,
        value = n_val,
        min = 1,
        step = 1,
        width = "60px"
      )
    )
  )
}