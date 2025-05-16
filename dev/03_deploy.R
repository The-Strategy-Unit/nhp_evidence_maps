deploy <- function(server_name, app_id) {
  rsconnect::deployApp(
    appName = "nhp_evidence_maps",
    appTitle = "NHP: Models of Care evidence-map app",
    server = server_name,
    appFiles = c(
      "R/",
      "inst/",
      "NAMESPACE",
      "DESCRIPTION",
      "app.R"
    ),
    appId = app_id,
    lint = FALSE,
    forceUpdate = TRUE
  )
}

deploy("connect.strategyunitwm.nhs.uk", 246)
deploy("connect.su.mlcsu.org", 118)
