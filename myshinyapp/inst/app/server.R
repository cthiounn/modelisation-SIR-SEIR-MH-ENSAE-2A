server <- function(input, output) {
  
  output$distSIRPlot <- renderPlot(
    myshinyapp::plot_simulation_graph(myshinyapp::simulate_SIR(c("beta"=input$beta,"gamma"=input$gamma,"initI"=1, "N"=763)))
  )
  output$distSEIRPlot <- renderPlot(
    myshinyapp::plot_simulation_graph(myshinyapp::simulate_SEIR(c("beta"=input$beta2,"gamma"=input$gamma2,"sigma"=input$sigma,"initI"=1, "N"=763)))
  )
  
  resultats <- eventReactive(input$run, {
    myshinyapp::simulerMH(input$initial_beta,input$initial_gamma,input$number_iteration)
  })
  output$distMHPlot_beta_sim <- renderPlot(
    myshinyapp::representerMH_beta_distrib(resultats(),input$number_burnin)
  )
  output$distMHPlot_beta_candidats <- renderPlot(
    myshinyapp::representerMH_beta_candidats(resultats(),input$number_burnin)
  )
  output$distMHPlot_gamma_sim <- renderPlot(
    myshinyapp::representerMH_gamma_distrib(resultats(),input$number_burnin)
  )
  output$distMHPlot_gamma_candidats <- renderPlot(
    myshinyapp::representerMH_gamma_candidats(resultats(),input$number_burnin)
  )
  
  output$distMHPlot_sim <- renderPlot(
    myshinyapp::representerMH_simulation_finale(resultats(),input$number_burnin)
  )

  output$distSIRSEIRPlot <- renderPlot(
    myshinyapp::plot_simulation_graph_SIR_SEIR(
      myshinyapp::simulate_SIR(c("beta"=input$beta3,"gamma"=input$gamma3,"initI"=1, "N"=763)),
      myshinyapp::simulate_SEIR(c("beta"=input$beta3,"gamma"=input$gamma3,"sigma"=input$sigma3,"initI"=1, "N"=763))
    )
  )
  
  
  
}