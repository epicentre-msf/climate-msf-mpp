mod_stopover_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    h5("Type of travel"),
    hr(),
    shinyWidgets::radioGroupButtons(
      inputId = ns("pass_frei"),
      label = "Passenger or Freight ?",
      #choices = c("Passenger" = "passenger", "Freight" = "freight"),
      choiceNames = list(shiny::icon("person"), shiny::icon("box") ),
      choiceValues = list("passenger", "freight" ),
      size = "sm",
      selected = "passenger",
      justified = TRUE
    ),

    div(
      class = "p-0",
      numericInput(
        inputId = ns("f_weight"),
        label =  tooltip(
          span("Weight", bsicons::bs_icon("info-circle")),
          "Input freight weight in tons"
        ),
        value = 1,
        min = .1,
        max = 1000,
        step = .1,
        width = "80px"
      )
    ),
    br(),
    h5("Itinerary"),
    hr(),
    div(
      id = ns("inputs"),
      stopover_input(
        ns,
        index = 1,
        city_lab = tooltip(
          span("Steps of the itinary",
               bsicons::bs_icon("info-circle")),
          "Input cities travelled to"
        )
      ),
      stopover_input(ns,
                     index = 2,
                     link_input = TRUE
      ),
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

    observe({
      cond <- input$pass_frei == "freight"
      shinyjs::toggle("f_weight", condition = cond, anim = TRUE)
    })

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

    #observe change in
    observeEvent(
      input$pass_frei,

      {
        if(input$pass_frei == "freight") {

          purrr::walk( 2:n_inputs(), ~ shinyWidgets::updateRadioGroupButtons(
            inputId = paste0("link_", .x),
            choiceValues = list("plane", "truck"),
            choiceNames = list(shiny::icon("plane"), shiny::icon("truck")),
            selected = "plane"
          )
          )
        } else {

          purrr::walk( 2:n_inputs(), ~ shinyWidgets::updateRadioGroupButtons(
            inputId = paste0("link_", .x),
            choiceValues = list("plane", "train"),
            choiceNames = list(shiny::icon("plane"), shiny::icon("train")),
            selected = "plane"
          )
          )
        }
      }
    )

    df <- reactive({
      index <- n_inputs()
      selected_cities <- purrr::map_chr(1:index, ~ input[[paste0("p", .x)]])
      selected_link <- purrr::map_chr(2:index, ~ input[[paste0("link_", .x)]])

      data.frame(
        start_var = head(selected_cities, -1),
        end_var = tail(selected_cities, - 1),
        link = selected_link
      )
    })

    # return stop overs df
    reactive(list(data = df(),
                  weight = input$f_weight,
                  pass_frei = input$pass_frei))
  })
}

stopover_input <- function(
    ns,
    index,
    choices = NULL,
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
        shinyWidgets::radioGroupButtons(
          inputId = ns(paste0("link_", index)),
          label = tooltip(
            span("Travel mode",
                 bsicons::bs_icon("info-circle")),
            "Travel mode between two cities"
          ),
          justified = TRUE,
          choiceValues = list("plane", "train"),
          choiceNames = list(shiny::icon("plane"), shiny::icon("train")),

          #= c(`<i class="fa-solid fa-plane"></i>` = "air", `<i class="fa-solid fa-train"></i>` = "rail"),
          #choices = c(`<i class="fa-solid fa-plane"></i>` = "air", `<i class="fa-solid fa-train"></i>` = "rail"),

          width = "50%"
        )
      )
    },
    div(
      class = "p-0",
      shinyWidgets::virtualSelectInput(
        inputId = ns(paste0("p", index)),
        label = city_lab,
        selected = selected,
        search = TRUE,
        choices = choices,
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
