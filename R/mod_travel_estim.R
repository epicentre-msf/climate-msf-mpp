mod_travel_estim_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Single Travel Estimation",
    icon = bsicons::bs_icon("airplane-fill"),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        id = ns("sb"),
        gap = 0,
        bg = "#ffffff",
        open = TRUE,
        mod_stopover_input_ui(ns("travel_estim")),
        bslib::input_task_button(
          ns("go_estim"),
          "Get travel emissions",
          label_busy = "Calculating...",
          width = "100%",
          class = "btn-primary"
        )
      ),
      bslib::card(
        full_screen = TRUE,
        min_height = 200,
        bslib::card_header(
          class = "d-flex align-items-center",
          bslib::card_title("Travel Emissions")
        ),
        bslib::card_body(
          reactableOutput(ns("tbl"))
        )
      ),
      bslib::card(
        full_screen = TRUE,
        min_height = 300,
        bslib::card_header(
          class = "d-flex align-items-center",
          bslib::card_title("Travel map")
        ),
        bslib::card_body(
          padding = 0,
          leaflet::leafletOutput(ns("map"))
        )
      )
    )
  )
}

mod_travel_estim_server <- function(id,
                                    distance_mat,
                                    emissions_mat,
                                    cities_network,
                                    air_msf,
                                    is_mobile) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    out <- mod_stopover_input_server("travel_estim", orig_cities)

    df_stop <- reactive( out()$data )

    pass_frei <- reactive( out()$pass_frei )

    df <- reactive({

      if (length(setdiff(c(df_stop()$start_var, df_stop()$end_var), colnames(distance_mat))) > 0) {
        stop(paste0("Cities: ", paste(setdiff(c(df_stop()$start_var, df_stop()$end_var), colnames(distance_mat)), collapse = ", "), " are not in the matrix"))
      }

      on.exit({
        if (is_mobile()) {
          toggle_sidebar(id = "sb", open = FALSE)
        }
      })

      #browser()
      if(pass_frei() == "freight"){

        df <- df_stop() |>
          mutate(
            # index the matrices
            distance_km = case_when( link == "plane" ~ distance_mat[cbind(start_var, end_var)],
                                     link == "truck" ~ distance_mat[cbind(start_var, end_var)] * 1.5
                                       ),
            weight = out()$weight,
            fct = case_when(link == "plane" ~ 1250,
                            link == "truck" ~ 136),
            trip_emissions = round(digits = 1, distance_km * weight * fct / 1000 )
          )
      } else {

        df <- df_stop() |>
          mutate(
            # index the matrices
            distance_km = case_when(link == "plane" ~  distance_mat[cbind(start_var, end_var)],
                                    link == "train" ~ distance_mat[cbind(start_var, end_var)] * 1.1
                                    ),
            trip_emissions = case_when(link == "plane" ~ emissions_mat[cbind(start_var, end_var)],
                                       link == "train" ~ round(digits = 1, distance_km * 0.005 )
            )
          )
      }

      # add the cities and countries
      df |>
        left_join(select(cities_df, city_code, start_city = city_name, start_country = country_name), by = join_by(start_var == city_code)) |>
        left_join(select(cities_df, city_code, end_city = city_name, end_country = country_name), by = join_by(end_var == city_code)) |>
        select(start_var, start_city, start_country, end_var, end_city, end_country, link, distance_km, trip_emissions)
    })  |>
      bindEvent(input$go_estim)

    output$tbl <- renderReactable({

      validate(
        need(input$go_estim > 0, "Select your itinerary origin, any stop-overs and destination then click 'Get travel emissions' to see results.")
      )

      req(df())

      # Define color scale function
      color_scale <- function(value) {

        min_val <- min(df_tbl$trip_emissions, na.rm = TRUE)
        max_val <- max(df_tbl$trip_emissions, na.rm = TRUE)

        # Normalize the value to a 0-1 scale for color scaling
        normalized <- (value - min_val) / (max_val - min_val)

        # Interpolate color between light green and light red
        palette <- colorRampPalette(c("#B8CCAD", "#eecb84", "#BF6C67"))(100)
        palette[round(normalized * 99) + 1]  # Scale to palette index
      }


      df_tbl <- df() |>
        mutate(
          start_city = paste0(start_city, " (", start_country, ")"),
          end_city = paste0(end_city, " (", end_country, ")"),
        ) |>
        select(-c(start_var, start_country, end_var, end_country))

      # get a table
      #browser()
      react_tbl <- reactable(
        df_tbl,
        highlight = TRUE,
        searchable = FALSE,
        compact = TRUE,
        defaultColDef = colDef(align = "center", format = colFormat(separators = TRUE, locales = "fr-Fr")),
        columns = list(
          start_city = colDef("Start city", align = "left", footer = htmltools::tags$b("Total")),
          end_city = colDef("End city", align = "left"),
          link = colDef("Transport mode",
                        align = "center",
                        cell =  function(value){
                          shiny::icon(value)
                        }
          ),
          distance_km = colDef("Segment distance (Km)",
                               align = "left",
                               format = colFormat(separators = TRUE, locales = "fr-Fr", digits = 0),
                               footer = function(values) {
                                 htmltools::tags$b(sprintf("%.0f km", sum(values)))
                               }
          ),
          trip_emissions = colDef(
            "Segment Emissions (kg CO2e)",
            align = "left",
            format = colFormat(separators = TRUE, locales = "fr-Fr", digits = 0),
            footer = function(values) {
              htmltools::tags$b(sprintf("%.0f kgCO2e", sum(values)))
            },
            style = if (nrow(df_tbl > 1)) {

              function(value){

                list(background = color_scale(value))
              }

            } else {
              background <- "white"
            }
          )
        )
      )
      return(react_tbl)
    })

    # Map

    output$map <- renderLeaflet({
      city_order <- unique(c(df()$start_var, df()$end_var))
      leaf_dat <- cities_df |>
        filter(city_code %in% city_order) |>
        mutate(city_code = factor(city_code, levels = city_order)) |>
        arrange(city_code)

      leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron", group = "Light") |>
        leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = FALSE)) |>
        leaflet.extras::addFullscreenControl(position = "topleft") |>
        leaflet.extras::addResetMapButton() |>
        leaflet::addCircleMarkers(
          data = leaf_dat,
          lng = ~city_lon,
          lat = ~city_lat,
          radius = 10,
          fillColor = ~"darkred",
          color = ~"white",
          fillOpacity = 1,
          weight = 1,
          label = ~city_name
        ) |>
        leaflet::addPolylines(
          data = leaf_dat,
          lng = ~city_lon,
          lat = ~city_lat
        )
    })
  })
}
