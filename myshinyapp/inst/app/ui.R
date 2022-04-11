ui <- shiny::navbarPage("Simulation SIR/SEIR",
                 shiny::tabPanel("Simulation SIR",
                          shiny::fluidPage(
                            shiny::sidebarLayout(
                              shiny::sidebarPanel(
                                shiny::sliderInput("beta",
                                            "Beta:",
                                            min = 0,
                                            max = 2,
                                            value = 1,
                                            step=0.1),
                                shiny::sliderInput("gamma",
                                            "Gamma:",
                                            min = 0,
                                            max = 1,
                                            value = 0.1)
                              ),
                              # Show a plot of the generated distribution
                              shiny::mainPanel(shiny::plotOutput("distSIRPlot"))
                            )
                          )
                          ),
                 shiny::tabPanel("Simulation Metropolis-Hastings",
                          shiny::fluidPage(
                            shiny::sidebarLayout(
                              shiny::sidebarPanel(
                                shiny::sliderInput("initial_beta",
                                            "Initial Beta:",
                                            min = 0,
                                            max = 2,
                                            value = 1,
                                            step=0.1),
                                shiny::sliderInput("initial_gamma",
                                            "Initial Gamma:",
                                            min = 0,
                                            max = 1,
                                            value = 0.1),
                                shiny::sliderInput("number_iteration",
                                            "Number of iterations:",
                                            min = 1000,
                                            max = 10000,
                                            value = 1000,
                                            step=1000),
                                shiny::sliderInput("number_burnin",
                                            "BurnIn number:",
                                            min = 0,
                                            max = 10000,
                                            value = 0,
                                            step=1000),
                                shiny::actionButton("run", "Run simulation")
                                
                              ),
                              # Show a plot of the generated distribution
                              shiny::mainPanel(shiny::plotOutput("distMHPlot_beta_sim"),
                                        shiny::plotOutput("distMHPlot_beta_candidats"),
                                        shiny::plotOutput("distMHPlot_gamma_sim"),
                                        shiny::plotOutput("distMHPlot_gamma_candidats"),
                                        shiny::plotOutput("distMHPlot_sim")
                                        )
                            )
                          )),
                 shiny::tabPanel("Simulation SEIR",
                          shiny::fluidPage(
                            shiny::sidebarLayout(
                              shiny::sidebarPanel(
                                shiny::sliderInput("beta2",
                                            "Beta:",
                                            min = 0,
                                            max = 2,
                                            value = 1,
                                            step=0.1),
                                shiny::sliderInput("gamma2",
                                            "Gamma:",
                                            min = 0,
                                            max = 1,
                                            value = 0.1)
                                ,
                                shiny::sliderInput("sigma",
                                            "Sigma:",
                                            min = 0,
                                            max = 1,
                                            value = 0.5)
                              ),
                              # Show a plot of the generated distribution
                              shiny::mainPanel(shiny::plotOutput("distSEIRPlot"))
                            )
                            )
                 )
)