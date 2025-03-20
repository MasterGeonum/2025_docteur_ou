server <- function(input, output, session) {

  # Appeler les serveurs des sous-applications
  onglet1_server(input, output, session)
  onglet1_bis_server(input, output, session)
  onglet2_server(input, output, session)
  server_3(input, output, session)

 }