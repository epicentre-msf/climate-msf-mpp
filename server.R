# server file for Carbon-Travel-App

server <- function(input, output, session) {

  # send usage logs to /root/logs if running in shinyproxy container
  # add "/var/log/shinylogs:/root/logs" to the container volumes in the
  # shinyproxy app config to save logs to central location on machine
  if (is_sp_env) {
    # user logs
    track_usage(
      storage_mode = store_json(path = "/root/logs"),
      app_name = app_name,
      what = "session",
      exclude_users = c("paul.campbell@epicentre.msf.org", "paul")
    )
  }

  mod_travel_estim_server(
    id = "travel_estim",
    d_mat_cities,
    air_msf,
    df_conversion,
    c_net,
    is_mobile = reactive(input$is_mobile)
  )

  mod_meeting_place_server(
    id = "mp",
    d_mat_flights,
    e_mat_flights,
    air_msf,
    f_network = f_net,
    is_mobile = reactive(input$is_mobile)
  )

}
