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

mod_meeting_place_server <- function(
    id,
    distance_mat,
    emissions_mat,
    air_msf,
    df_conversion,
    f_network,
    is_mobile
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      cond <- input$msf_all == "msf"
      shinyjs::toggle("msf_type_select", condition = cond, anim = TRUE)
    })

    df_origin <- mod_origin_input_server("origin", orig_cities)

    # Filter the destinations to map on, this will also update choices for destination selector
    dest_fil <- reactive({
      df_out <- cities_df
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

      # filter to selected cities if not empty
      if (length(input$select_dest)) {
        dest_cities <- dest_fil() |> filter(city_code %in% input$select_dest) |> pull(city_code)
      } else {
        dest_cities <- dest_fil()$city_code

      }

      # Get best locations from the matrix
      all_dest <- best_locations(
        distance_mat,
        emissions_mat,
        df_origin(),
        cities_df,
        destinations = dest_cities
      ) |>
        arrange(coalesce(grand_tot_emission_train, grand_tot_emission_plane)) |>
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
          grand_tot_km_train,
          grand_tot_emission_train,
          grand_tot_km_plane,
          grand_tot_emission_plane,
          oc,
          msf_type
        )
    }) |> bindEvent(input$go)

    # Make a reactable
    output$tbl <- reactable::renderReactable({
      validate(
        need(input$go > 0, "Select origins and destinations (optional) then click 'Get meeting places' to see results.")
      )

      req(df_dists())

      df <- df_dists() |>
        select(-c(city_lon, city_lat)) |>
        head(50)

      # Define color scale function
      color_scale <- function(value) {

        min_val <- min(df$grand_tot_emission_train, na.rm = TRUE)
        max_val <- max(df$grand_tot_emission_plane, na.rm = TRUE)

        # Normalize the value to a 0-1 scale for color scaling
        normalized <- (value - min_val) / (max_val - min_val)

        # Interpolate color between light green and light red
        palette <- colorRampPalette(c("#B8CCAD", "#eecb84", "#BF6C67"))(100)
        palette[round(normalized * 99) + 1]  # Scale to palette index
      }

      render_reactable_header <- function(name,
                                          tooltip,
                                          icon) {

        if(icon == "plane") {

          tippy::tippy(
            div(
              name,
              div(
                icon("plane", class = "fa-lg"),  # First icon
                style = "margin-top: auto;"  # Forces the icon to the bottom
              ),
              style = "display: flex; flex-direction: column; align-items: center; height: 50px;"  # Adjust height as needed
            ),
            paste0('<span style="font-size:16px;">', tooltip, '</span>'),
            allowHTML = TRUE
          )

        } else if(icon == "plane_train"){

          tippy::tippy(
            div(
              name,
              div(
                icon("plane", class = "fa-lg"),  # First icon
                span("+", style = "margin: 0 5px; font-weight: bold;"),  # "+" sign with spacing
                icon("train", class = "fa-lg"),  # Second icon
                style = "margin-top: auto;"  # Forces the icon to the bottom
              ),
              style = "display: flex; flex-direction: column; align-items: center; height: 50px;"  # Adjust height as needed
            ),
            paste0('<span style="font-size:16px;">', tooltip, '</span>'),
            allowHTML = TRUE
          )
        }
      }

      reactable(
        df,
        highlight = TRUE,
        searchable = TRUE,
        compact = TRUE,
        pagination = FALSE,
        rowStyle = list(cursor = "pointer"),
        onClick = rt_get_city_code(id = ns("rt_city_code")),
        defaultColDef = colDef(align = "center",
                               format = colFormat(separators = TRUE,
                                                  locales = "fr-Fr")),
        columns = list(
          rank = colDef(header = "Rank",
                        align = "left",
                        maxWidth = 50),
          city_code = colDef(show = FALSE),
          city_name = colDef("City",
                             align = "left",
                             maxWidth = 150),
          country_name = colDef("Country",
                                align = "left",
                                maxWidth = 150),
          grand_tot_km_train = colDef(
            header = render_reactable_header(name = "Total Km",
                                             tooltip = "If trips between european cities less than 500km appart are done on trains and all other trips using the plane",
                                             icon = "plane_train"),
            na = "-",
            align = "center",
            format = colFormat(separators = TRUE,
                               locales = "fr-Fr",
                               digits = 0),
            maxWidth = 150
          ),
          grand_tot_emission_train = colDef(
            na = "-",
            header = render_reactable_header(name = "Total Emissions (kg CO2e)",
                                             tooltip = "If trips between european cities less than 500km appart are done on trains and all other trips using the plane",
                                             icon = "plane_train"),
            align = "center",
            format = colFormat(
              separators = TRUE,
              #locales = "fr-Fr",
              digits = 0
            ),
            maxWidth = 150,
            style = if (nrow(df) > 1) {

              function(value){

                list(background  = color_scale(value))
              }
            } else {
              (list(background = "white"))
            }
          ),
          grand_tot_km_plane = colDef(

            header = render_reactable_header(name = "Total Km",
                                             tooltip = "If all trips are travelled using the plane",
                                             icon = "plane"),
            align = "center",
            format = colFormat(separators = TRUE,
                               locales = "fr-Fr",
                               digits = 0),
            maxWidth = 150
          ),
          grand_tot_emission_plane = colDef(
            header = render_reactable_header(name = "Total Emissions (kg CO2e)",
                                             tooltip = "If all trips are travelled using the plane",
                                             icon = "plane"),
            align = "center",
            format = colFormat(separators = TRUE,
                               locales = "fr-Fr",
                               digits = 0),
            maxWidth = 150,
            style = if(nrow(df) > 1) {

              function(value){

                list(background  = color_scale(value))
              }
            } else {
              (list(background = "white"))
            }
          ),
          oc = colDef("Operational Center",
                      align = "left",
                      maxWidth = 200),
          msf_type = colDef("MSF type",
                            align = "left")
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

    output$dest_text <- renderUI({
      req(map_dest())
      city_name <- cities_df[cities_df$city_code == map_dest(), "city_name", drop = TRUE]
      tags$p("Destination: ", tags$b(city_name), tags$small("  (click a row in the table above to change the destination)"))
    })

    output$map <- leaflet::renderLeaflet({
      req(map_dest())

      pal <- colorFactor(
        palette = c("darkred", "steelblue", "orange"),
        domain = c("destination", "origin", "shortest stop-over")
      )
      # origins
      map_ori <- df_origin() |> left_join(cities_df, by = join_by(origin_id == city_code))

      # destination selected
      map_dest <- map_dest()

      # get the shortest path in network for all origin and this destination
      short_paths <- purrr::map(
        map_ori$origin_id,
        ~ sfnetworks::st_network_paths(f_network, from = .x, to = map_dest)
      )

      short_nodes <- unique(unname(unlist(purrr::map(short_paths, ~ .x |>
                                                       pull(node_paths) |>
                                                       unlist()))))

      stop_nodes <- setdiff(short_nodes, c(map_ori$origin_id, map_dest))

      short_edges <- unname(unlist(purrr::map(short_paths, ~ .x |>
                                                pull(edge_paths) |>
                                                unlist())))

      nodes <- f_network |>
        activate("nodes") |>
        filter(name %in% short_nodes) |>
        st_as_sf() |>
        mutate(type = case_when(
          name == map_dest ~ "destination",
          name %in% map_ori$origin_id ~ "origin",
          name %in% stop_nodes ~ "shortest stop-over"
        ))

      edges <- f_network |>
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
          data = mutate(
            nodes,
            city_lon = unlist(map(nodes$geometry, 1)),
            city_lat = unlist(map(nodes$geometry, 2))
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
    }) |>
      bindEvent(map_dest(), df_dists())
  })
}

# Function to retrieve the best locations based on origins input, also retrieve the distance and emissions if trips under 550km are travelled by train
best_locations <- function(
    distance_mat,
    emissions_mat,
    df_origin,
    cities_df,
    destinations
) {

  if (length(setdiff(df_origin$origin_id, colnames(distance_mat))) > 0) {
    stop(paste0("Origins: ", paste(setdiff(df_origin$origin_id, colnames(distance_mat)), collapse = ", "), " are not in the matrix"))
  }
  if (length(setdiff(destinations, colnames(distance_mat))) > 0) {
    stop(paste0("Destinations: ", paste(setdiff(destinations, colnames(distance_mat)), collapse = ", "), " are not in the matrix"))
  }
  # Plane distance and emissions matrix -----------------------------------------------
  # Distances
  # 1. filter the possible destinations & filter rows of origins
  distance_mat_plane <- distance_mat[df_origin$origin_id, destinations, drop = FALSE]

  # Emissions
  # 1. filter the possible dest_select & filter rows of origins
  emissions_mat_plane <- emissions_mat[df_origin$origin_id, destinations, drop = FALSE]

  # Trains distance and Emissions matrix -----------------------------------------------

  # European matrix and train distances and emissions
  dest_europe <- filter(cities_df, city_code %in% destinations, continent == "Europe") |> pull(city_code)
  ori_europe <- filter(cities_df, city_code %in% df_origin$origin_id, continent == "Europe") |> pull(city_code)

  if (length(dest_europe) != 0 & length(ori_europe) != 0) {
    # filter matrix to keep only European cities
    europe_mat <- distance_mat_plane[ori_europe, dest_europe, drop = FALSE]

    # removes the values where distance is more than 550 (not travelled by train)
    distance_mat_train <- ifelse(europe_mat < 550, europe_mat * 1.2, NA)

  } else {
    distance_mat_train <- NULL
  }

  # Trains emission matrix
  emissions_mat_train <- if (is.null(distance_mat_train)) {
    NULL
  } else {
    distance_mat_train * 0.005
  }
  # Need to complete the train matrices with plane data for all segments without a train
  complete_train_mat <- function(train_mat, plane_mat) {
    if (is.null(train_mat)) {
      train_mat <- NULL
    } else {
      row_diff <- setdiff(rownames(plane_mat), rownames(train_mat))
      col_diff <- setdiff(colnames(plane_mat), colnames(train_mat))

      # fill the missing rows/cols in train matrix
      col <- matrix(nrow = nrow(train_mat), ncol = length(col_diff))
      colnames(col) <- col_diff

      train_mat <- cbind(train_mat, col)

      row <- matrix(nrow = length(row_diff), ncol = ncol(train_mat))
      rownames(row) <- row_diff

      train_mat <- rbind(train_mat, row)

      #order matrix in same order
      train_mat <- train_mat[rownames(plane_mat), , drop = FALSE]
      train_mat <- train_mat[,colnames(plane_mat),  drop = FALSE]

      # replace NA values in trains by the corresponding values in plane matrix
      train_mat[is.na(train_mat)] <- plane_mat[is.na(train_mat)]
    }

    return(train_mat)
  }

  distance_mat_train <- complete_train_mat(distance_mat_train, distance_mat_plane)
  emissions_mat_train <- complete_train_mat(emissions_mat_train, emissions_mat_plane)

  # 3. Multiply matrices rows by value of input
  distance_mat_plane_pp <- sweep(as.matrix(distance_mat_plane), 1, df_origin$n_participant, FUN = "*")
  emissions_mat_plane_pp <- sweep(as.matrix(emissions_mat_plane), 1, df_origin$n_participant, FUN = "*")

  if (is.null(distance_mat_train)) {
    distance_mat_train_pp <- NULL
    emissions_mat_train_pp <- NULL
  } else {
    distance_mat_train_pp <- sweep(as.matrix(distance_mat_train), 1, df_origin$n_participant, FUN = "*")
    emissions_mat_train_pp <- sweep(as.matrix(emissions_mat_train), 1, df_origin$n_participant, FUN = "*")
  }

  # 4. Sum and sort all rows together
  distance_sum_plane <- colSums(distance_mat_plane_pp)
  emissions_sum_plane <- colSums(emissions_mat_plane_pp)
  distance_sum_plane <- distance_sum_plane[order(names(distance_sum_plane))]
  emissions_sum_plane <- emissions_sum_plane[order(names(emissions_sum_plane))]

  if (is.null(emissions_mat_train_pp)) {
    distance_sum_train <- NULL
    emissions_sum_train <- NULL
  } else {
    distance_sum_train <- colSums(distance_mat_train_pp)
    emissions_sum_train <- colSums(emissions_mat_train_pp)

    # sort vectors
    distance_sum_train <- distance_sum_train[order(names(distance_sum_train))]
    emissions_sum_train <- emissions_sum_train[order(names(emissions_sum_train))]
  }

  # 5. create data.frame
  df <- data.frame(
    name_dest = sort(destinations),
    grand_tot_km_train = if (is.null(distance_sum_train)) {
      NA
    } else {
      unname(distance_sum_train)
    },
    grand_tot_emission_train = if (is.null(emissions_sum_train)) {
      NA
    } else {
      unname(emissions_sum_train)
    },
    grand_tot_km_plane = unname(distance_sum_plane),
    grand_tot_emission_plane = unname(emissions_sum_plane)
  ) |>
    mutate(
      grand_tot_km_train = ifelse(grand_tot_km_train == grand_tot_km_plane, NA, grand_tot_km_train),
      grand_tot_emission_train = ifelse(grand_tot_emission_train == grand_tot_emission_plane, NA, grand_tot_emission_train)
    )

  return(df)
}

############################################### TEST ZONE  #############################################################################
#
# library(dplyr)
#
# cities_df <- rio::import(here::here("data", "clean", "network", "dest_cities.rds")) |> as_tibble()
# distance_mat <- rio::import(here::here("data", "clean", "matrix", "distance_matrix_fligths.rds"))
# emissions_mat <- rio::import(here::here("data", "clean", "matrix", "emissions_matrix_flights.rds"))
#
# df_origin <- data.frame(
#   origin_id = c("PAR", "LON"),
#   n_participant = c(1, 1)
# )
#
# destinations <- c("LRT")
#
# best_locations(
#   distance_mat,
#   emissions_mat,
#   df_origin,
#   cities_df,
#   destinations = destinations
# )|>
#   arrange(grand_tot_emission_plane) |>
#   mutate(rank = row_number()) |>
#   relocate(rank, 1)

##############################################################################################################
