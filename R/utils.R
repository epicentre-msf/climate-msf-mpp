
#' Format numbers with scale units when large
#'
#' @param x a number to format
#' @param accuracy accuracy of labels, passed to [`scales::number`]
#'
#' @noRd
frmt_num <- function(x, accuracy = .1) {
  n <- scales::number(x, accuracy = accuracy, scale_cut = scales::cut_short_scale())
  n <- stringr::str_remove(n, "\\.0+(?=[a-zA-Z])")
  n <- stringr::str_remove(n, "\\.0+$")
  n
}


fmt_n <- function(n) {
  
  if (is.na(n)) {
    out <- "(Unknown)"
    return(out)
    
  } else if (n < 1000) {
    out <-  n
    
    return(out)
    
  } else {
    out <- scales::number(
      n,
      accuracy = .1,
      scale_cut = c(0, K = 1e3, M = 1e6))
    
    return( stringr::str_remove(out, "\\.0"))
  }
}
fmt_n <- Vectorize(fmt_n)


force_reactive <- function(x) {
  if (shiny::is.reactive(x)) {
    x()
  } else {
    x
  }
}

calc_radius <- function(n, scale_factor = 30) {
  sqrt(n) / sqrt(max(n)) * scale_factor
}

rt_get_city_code <- function(id) {
  reactable::JS(sprintf("
  function(rowInfo, colInfo) {
    // Send the click event to Shiny, which will be available at input$query_select
    if (window.Shiny) {
      var cc = rowInfo.row['city_code'];
      Shiny.setInputValue('%s', cc);
    }
  }
", id))
}

render_reactable_header <- function(
    name,
    tooltip,
    icon) {
  if (icon == "plane") {
    htmltools::div(
      style = paste("text-decoration: underline;", "text-decoration-style: dotted;", "cursor: help"),
      tippy::tippy(
        div(
          name,
          div(
            icon("plane", class = "fa-lg"), # First icon
            style = "margin-top: auto;" # Forces the icon to the bottom
          ),
          style = "display: flex; flex-direction: column; align-items: center; height: 50px;" # Adjust height as needed
        ),
        paste0('<span style="font-size:16px;">', tooltip, "</span>"),
        allowHTML = TRUE
      )
    )
  } else if (icon == "plane_train") {
    htmltools::div(
      style = paste("text-decoration: underline;", "text-decoration-style: dotted;", "cursor: help"),
      tippy::tippy(
        div(
          name,
          div(
            icon("plane", class = "fa-lg"), # First icon
            span("+", style = "margin: 0 5px; font-weight: bold;"), # "+" sign with spacing
            icon("train", class = "fa-lg"), # Second icon
            style = "margin-top: auto;" # Forces the icon to the bottom
          ),
          style = "display: flex; flex-direction: column; align-items: center; height: 50px;" # Adjust height as needed
        ),
        paste0('<span style="font-size:16px;">', tooltip, "</span>"),
        allowHTML = TRUE
      )
    )
  }
}
