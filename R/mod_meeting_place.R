mod_meeting_place_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Meeting Place Planner",
    icon = bsicons::bs_icon("people-fill"),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        id = ns("sb"),
        width = 300,
        gap = 0,
        bg = "#eaeaea",
        open = TRUE,
        mod_origin_input_ui(ns("origin")),
        h5("Destinations"),
        hr(),
        bslib::layout_columns(
          col_widths = 12,
          shinyWidgets::radioGroupButtons(
            inputId = ns("msf_all"),
            label = "All destinations or only MSF?",
            choices = c("All" = "all", "MSF" = "msf"),
            size = "sm",
            selected = "all",
            justified = TRUE
          ),
          shinyWidgets::virtualSelectInput(
            inputId = ns("msf_type_select"),
            label = "MSF type",
            choices = msf_type_vec,
            placeholder = "All types",
            multiple = TRUE,
            search = FALSE,
            disableSelectAll = TRUE,
            showDropboxAsPopup = FALSE
          ),
          shinyWidgets::virtualSelectInput(
            inputId = ns("select_dest"),
            label = tooltip(
              span(
                "Filter possible destinations",
                bsicons::bs_icon("info-circle")
              ),
              "Select all possible destinations for a meeting. By default all locations are included in the calculation."
            ),
            choices = NULL,
            multiple = TRUE,
            search = TRUE,
            autoSelectFirstOption = TRUE,
            placeholder = "All cites",
            position = "bottom",
            showDropboxAsPopup = FALSE,
            showOptionsOnlyOnSearch = FALSE,
            optionsCount = 5
          )
        ),
        hr(),
        bslib::input_task_button(
          ns("go"),
          "Get meeting places",
          label_busy = "Calculating...",
          width = "100%",
          class = "btn-primary"
        )
      ),
      bslib::card(
        full_screen = TRUE,
        min_height = 200,
        max_height = 400,
        bslib::card_header(
          class = "d-flex align-items-center",
          bslib::card_title("Optimal Meeting locations")
        ),
        bslib::card_body(
          padding = 0,
          reactableOutput(ns("tbl"))
        )
      ),
      bslib::card(
        full_screen = TRUE,
        min_height = 300,
        bslib::card_header(
          class = "d-flex",
          bslib::card_title("Map of travels", class = "pe-2"),
          uiOutput(ns("dest_text"))
        ),
        bslib::card_body(
          padding = 0,
          leaflet::leafletOutput(ns("map"))
        )
      )
    )
  )
}

mod_meeting_place_server <- function(id,
                                     distance_mat,
                                     emissions_mat,
                                     air_msf,
                                     df_conversion,
                                     network,
                                     is_mobile) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      cond <- input$msf_all == "msf"
      shinyjs::toggle("msf_type_select", condition = cond, anim = TRUE)
    })

    df_origin <- mod_origin_input_server("origin", orig_cities)

    # Filter the destinations to map on, this will also update choices for destination selector
    dest_fil <- reactive({
      df_out <- dest
      if (input$msf_all == "msf") {
        df_out <- df_out |> filter(msf)
      }
      if (input$msf_all == "msf" && length(input$msf_type_select)) {
        msf_type_select <- paste(input$msf_type_select, collapse = "|")
        df_out <- df_out |> filter(str_detect(msf_type, pattern = msf_type_select))
      }
      df_out
    })

    observe({
      choices <- dest_fil() |>
        shinyWidgets::prepare_choices(
          label = city_name,
          value = city_code,
          group_by = country_name
        )
      shinyWidgets::updateVirtualSelect("select_dest", choices = choices)
    })

    df_dists <- reactive({
      req(df_origin())
      on.exit({
        if (is_mobile()) {
          toggle_sidebar(id = "sb", open = FALSE)
        }
      })

      # final selection of destinations using the picker inputs
      dest_cities <- dest_fil()

      # filter to selected cities if not empty
      if (length(input$select_dest)) {
        dest_cities <- dest_cities |> filter(city_code %in% input$select_dest)
      }

      dest_cities <- dest_cities |> pull(city_code)

      # Get best locations from the matrix
      all_dest <- best_locations(
        distance_mat,
        emissions_mat,
        df_origin(),
        destinations = dest_cities
      ) |>
        mutate(rank = row_number()) |>
        relocate(rank, 1) |>
        left_join(
          select(
            dest_fil(),
            city_code,
            city_name,
            city_lon,
            city_lat,
            country_name,
            oc,
            msf_type
          ),
          by = c("name_dest" = "city_code")
        ) |>
        select(
          rank,
          city_code = name_dest,
          city_lon,
          city_lat,
          city_name,
          country_name,
          grand_tot_km,
          grand_tot_emission,
          oc,
          msf_type
        )
    }) |> bindEvent(input$go)

    # Make a reactable
    orange_pal <- function(x) rgb(colorRamp(c("#B8CCAD", "#BF6C67"))(x), maxColorValue = 255)

    output$tbl <- reactable::renderReactable({
      validate(
        need(input$go > 0, "Select origins and destinations (optional) then click 'Get meeting places' to see results.")
      )
      req(df_dists())

      df <- df_dists() |>
        select(-c(city_lon, city_lat)) |>
        head(50)

      reactable(
        df,
        highlight = TRUE,
        searchable = TRUE,
        compact = TRUE,
        pagination = FALSE,
        rowStyle = list(cursor = "pointer"),
        onClick = rt_get_city_code(id = ns("rt_city_code")),
        defaultColDef = colDef(align = "center", format = colFormat(separators = TRUE, locales = "fr-Fr")),
        columns = list(
          rank = colDef("Rank", align = "left", maxWidth = 50),
          city_code = colDef(show = FALSE),
          city_name = colDef("City", align = "left", maxWidth = 150),
          country_name = colDef("Country", align = "left", maxWidth = 150),
          grand_tot_km = colDef(
            "Total Km",
            align = "left",
            format = colFormat(separators = TRUE, locales = "fr-Fr", digits = 0),
            maxWidth = 150
          ),
          grand_tot_emission = colDef(
            "Total Emissions (kg CO2e)",
            align = "left",
            format = colFormat(separators = TRUE, locales = "fr-Fr", digits = 0),
            maxWidth = 150,
            style = if (nrow(df) > 1) {
              function(value) {
                normalized <- (value - min(df$grand_tot_emission)) / (max(df$grand_tot_emission) - min(df$grand_tot_emission) + 1)
                color <- orange_pal(normalized)
                list(background = color)
              }
            } else {
              background <- "white"
            }
          ),
          oc = colDef("Operational Center", align = "left", maxWidth = 200),
          msf_type = colDef("MSF type", align = "left")
        )
      )
    })

    #* Map  =========================================================================

    map_dest <- reactiveVal()
    # when new locations are calculated update map dest to rank 1 city
    observe({
      req(df_dists())
      md <- df_dists() |>
        filter(rank == 1) |>
        pull(city_code)
      map_dest(md)
    })
    # when a row in the table is clicked update map dest with that city code
    observe({
      req(input$rt_city_code)
      map_dest(input$rt_city_code)
    })

    # update choices of input

    # observeEvent(df_dists(), {
    #   choices <- df_dists() |>
    #     shinyWidgets::prepare_choices(
    #       label = city_name,
    #       value = city_code,
    #       group_by = country_name
    #     )
    #   shinyWidgets::updateVirtualSelect("map_dest", choices = choices)
    # })

    output$dest_text <- renderUI({
      req(map_dest())
      city_name <- dest[dest$city_code == map_dest(), "city_name", drop = TRUE]
      tags$p("Destination: ", tags$b(city_name), tags$small("  (click a row in the table above to change the destination)"))
    })

    output$map <- leaflet::renderLeaflet({
      req(map_dest())

      pal <- colorFactor(
        palette = c("darkred", "steelblue", "orange"),
        domain = c("destination", "origin", "shortest stop-over")
      )
      # origins
      map_ori <- df_origin() |> left_join(dest, by = join_by(origin_id == city_code))

      # destination selected
      map_dest <- map_dest()

      # get the shortest path in network for all origin and this destination
      short_paths <- purrr::map(
        map_ori$origin_id,
        ~ sfnetworks::st_network_paths(net, from = .x, to = map_dest)
      )

      short_nodes <- unique(unname(unlist(purrr::map(short_paths, ~ .x |>
                                                       pull(node_paths) |>
                                                       unlist()))))

      stop_nodes <- setdiff(short_nodes, c(map_ori$origin_id, map_dest))

      short_edges <- unname(unlist(purrr::map(short_paths, ~ .x |>
                                                pull(edge_paths) |>
                                                unlist())))

      nodes <- net |>
        activate("nodes") |>
        filter(name %in% short_nodes) |>
        st_as_sf() |>
        mutate(type = case_when(
          name == map_dest ~ "destination",
          name %in% map_ori$origin_id ~ "origin",
          name %in% stop_nodes ~ "shortest stop-over"
        ))

      edges <- net |>
        activate("edges") |>
        slice(short_edges) |>
        st_as_sf()

      leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron", group = "Light") |>
        leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = FALSE)) |>
        leaflet.extras::addFullscreenControl(position = "topleft") |>
        leaflet.extras::addResetMapButton() |>
        leaflet::addPolylines(data = edges) |>
        addLegend(
          position = "topright",
          pal = pal,
          values = unique(nodes$type)
        ) |>
        leaflet::addCircleMarkers(
          data = mutate(nodes,
                        city_lon = unlist(map(nodes$geometry,1)),
                        city_lat = unlist(map(nodes$geometry,2))
          ),
          lng = ~city_lon,
          lat = ~city_lat,
          radius = 10,
          color = ~"white",
          fillOpacity = 1,
          weight = 1,
          fillColor = ~ pal(type),
          label = ~city_name
        )
    }) |> bindEvent(map_dest(), df_dists())
  })
}

best_locations <- function(
    distance_mat,
    emissions_mat,
    df_origin,
    destinations) {
  if (length(setdiff(df_origin$origin_id, colnames(distance_mat))) > 0) {
    stop(paste0("Origins: ", paste(setdiff(df_origin$origin_id, colnames(distance_mat)), collapse = ", "), " are not in the matrix"))
  }
  if (length(setdiff(destinations, colnames(distance_mat))) > 0) {
    stop(paste0("Destinations: ", paste(setdiff(destinations, colnames(distance_mat)), collapse = ", "), " are not in the matrix"))
  }

  # sort the df_origins by alphabetical order
  df_origin <- arrange(df_origin, origin_id)

  # sort the destination by alphabetical order
  destinations <- sort(destinations)

  # Distances -----------------------------------------------
  # sort the distance matrix in alphabetical order
  distance_mat <- distance_mat[sort(rownames(distance_mat)), sort(colnames(distance_mat))]

  # 1. filter the possible destinations & Filter rows of origins
  distance_mat_sub <- distance_mat[df_origin$origin_id, destinations]

  # 3. Multiply rows by value of input
  distance_mat_sub <- sweep(as.matrix(distance_mat_sub), 1, df_origin$n_participant, FUN = "*")

  # 4. Sum all rows together and sort
  distance_mat_sum <- sort(colSums(distance_mat_sub))

  # Emissions -----------------------------------------------
  # sort the emissions matrix in alphabetical order
  emissions_mat <- emissions_mat[sort(rownames(emissions_mat)), sort(colnames(emissions_mat))]

  # 1. filter the possible destinations & Filter rows of origins
  emissions_mat_sub <- emissions_mat[df_origin$origin_id, destinations]

  # 3. Multiply rows by value of input
  emissions_mat_sub <- sweep(as.matrix(emissions_mat_sub), 1, df_origin$n_participant, FUN = "*")

  # 4. Sum all rows together and sort
  emissions_mat_sum <- sort(colSums(emissions_mat_sub))

  # 5. create dataframe and calculate emissions
  df <- data.frame(
    name_dest = destinations,
    grand_tot_km = unname(distance_mat_sum),
    grand_tot_emission = unname(emissions_mat_sum)
  )

  return(df)
}
