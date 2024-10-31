# generate the data source table

flights <- readRDS(here::here("data", "clean", "flights", "full_amex_wagram_cwt.rds")) |> as_tibble()

tab <- flights |>
  filter(travel_type == "air") |>
  mutate(org = fct_lump_n(org, n = 6)) |>

  summarise(.by = c("org"),
            data_source = paste(unique(data_source), collapse = ", "),
            min_year = year(min(invoice_date)),
            max_year = year(max(invoice_date)),
            n_flights = n()) |>
  mutate(
    data_source = case_when(str_detect(data_source,", ") ~ "Multiple",
                            .default = data_source),

    across(c(org, data_source, min_year, max_year), ~as.character(.x)),
    org = fct(org, levels = c("OCG", "OCB", "OCA", "OCP", "OCBA", "Epicentre", "Other"))) |>
  arrange(org) |>
  janitor::adorn_totals()

tab <- tab |>
  epithemes::epitheme_gt() |>
  gt::cols_label(
    "org" ~ "Section",
    "data_source" ~ " Data source",
    "min_year" ~"Minimum Year",
    "max_year" ~"Maximum Year",
    "n_flights" ~"N flights"
    )

gt::gtsave(
  tab,
  filename = here::here("www", "source-tab.png"),
  vwidth = 1,  # Pixel width
  vheight = 1,  # Pixel height
  zoom = 3       # Increase resolution (scales up by 3x)
)


