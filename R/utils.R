
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
