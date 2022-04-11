time = c(3,4,5,6,7,8,9,10,11,12,13,14,15)
infected = c(31,82,216,299,269,242,190,125,81,52,25,22,7)
data=cbind(time, infected)

##SIR model
SIR<-function(t,x,parms){
  ##taille de chaque compartiment et de la population
  S = x[1]
  I = x[2]
  R = x[3]
  Z = x[4]
  N = x[1]+x[2]+x[3]
  ##valeurs des parametres
  beta = parms["beta"]
  gamma = parms["gamma"]
  ##variations
  dS=-beta*S*I/N
  dI=beta*S*I/N-gamma*I
  dR=gamma*I
  dZ=beta*S*I/N
  res = c(dS,dI,dR,dZ)
  list(res)
}
simulate_SIR=function(parameters){
  #parameters
  parms = c(parameters["beta"],parameters["gamma"])
  N=parameters["N"]
  #initial conditions
  init <- c(N-parameters["initI"],parameters["initI"],0,0)
  #simulation
  temps <- seq(0,15)
  solveSIR <- deSolve::lsoda(y =init, times=temps, func = SIR,
                    parms = parms)
  solutionSIR=as.data.frame(solveSIR)
  names(solutionSIR)=c("time","S","I","R","Z")
  #merge with data
  sir_data=merge(data,solutionSIR)
  return(sir_data)
}


plot_simulation_graph=function(simulation_dataframe){
  df_observed<- data.frame(x=time,y=infected,type="observed")
  df_simul <- data.frame(x=simulation_dataframe$time,y=simulation_dataframe$I,type="simulated")
  df<-rbind(df_observed,df_simul)
  ggplot2::ggplot(data=df) + ggplot2::geom_line(mapping = ggplot2::aes(x , y ,colour = type)) 
} 
  
calculer_vraisemblance=function(beta,gamma){
  theta_init =c("beta"=beta,"gamma"=gamma,"initI"=1, "N"=763)
  simulation_dataframe<-simulate_SIR(theta_init)
  return(prod(dpois(simulation_dataframe$infected,simulation_dataframe$I)))
}

calculer_posterior<-function(beta,gamma){
  return(calculer_vraisemblance(beta,gamma)*dunif(beta,min=0,max=10)*dunif(gamma,min=0,max=1))
}

# On choisit une loi normal pour choisir un candidat
calculer_selon_distribution_proposee<-function(parametres){
  return(rnorm(2,mean=parametres,c(0.5,0.1)))
}

calculer_metropolis_hastings <- function(valeurs_initiales_choisies, nombre_iterations_voulues){
  resultats_parametres <- array(dim = c(nombre_iterations_voulues+1,2))
  resultats_parametres[1,] <- valeurs_initiales_choisies
  dernier_resultat_posterior <- calculer_posterior(valeurs_initiales_choisies[1],valeurs_initiales_choisies[2])
  
  for (i in 1:nombre_iterations_voulues){
    
    # Calcul du candidat et de son posterior
    valeur_courante <- resultats_parametres[i,]
    candidat <- calculer_selon_distribution_proposee(valeur_courante)
    candidat_posterior <- calculer_posterior(candidat[1],candidat[2])
    
    # Recuperation du posterior courant
    courant_posterior <- dernier_resultat_posterior
    
    # Acceptation du candidat Ratio >1
    if (candidat_posterior >= courant_posterior){
      resultats_parametres[i+1,] <- candidat
      dernier_resultat_posterior <- candidat_posterior
    }
    else{
      # Calcul du ratio
      ratio = candidat_posterior/ courant_posterior
      
      # Règle de décision d'acception-rejet du candidat
      alea_uniforme <- runif(1)
      
      # Acception du candidat
      if (alea_uniforme < ratio){
        resultats_parametres[i+1,] <- candidat
        dernier_resultat_posterior <- candidat_posterior
      }
      # Rejet du candidat = reprise de la valeur courante
      else{
        resultats_parametres[i+1,]  <- resultats_parametres[i,]
      }
    }
  }
  return(resultats_parametres)
}


simulerMH<-function(beta,gamma,nombre_iteration){
  resultats_parametres= calculer_metropolis_hastings(c(beta,gamma),nombre_iteration)
  return(resultats_parametres)
}

representerMH_beta_distrib<-function(resultats_parametres,nombre_iteration_burnIn){
  
  distribution_simulee_beta=resultats_parametres[-(1:nombre_iteration_burnIn),1]
  beta_moyen=mean(distribution_simulee_beta)
  
  hist(distribution_simulee_beta, main="Posterior de Beta", xlab="Valeur de Beta" )
  abline(v=beta_moyen,col='red' )

}

representerMH_beta_candidats<-function(resultats_parametres,nombre_iteration_burnIn){
  
  distribution_simulee_beta=resultats_parametres[-(1:nombre_iteration_burnIn),1]
  beta_moyen=mean(distribution_simulee_beta)
  
  plot(distribution_simulee_beta, type = "l", xlab="Itérations" , main = "Candidat de Beta pour chaque itération" )
  abline(h=beta_moyen,col='red' )
}

representerMH_gamma_distrib<-function(resultats_parametres,nombre_iteration_burnIn){
  
  distributation_simulee_gamma=resultats_parametres[-(1:nombre_iteration_burnIn),2]
  gamma_moyen=mean(distributation_simulee_gamma)
  
  
  
  hist(distributation_simulee_gamma, main="Posterior de Gamma", xlab="Valeur de Gamma")
  abline(v =gamma_moyen,col='red')
}



representerMH_gamma_candidats<-function(resultats_parametres,nombre_iteration_burnIn){
  
  distributation_simulee_gamma=resultats_parametres[-(1:nombre_iteration_burnIn),2]
  gamma_moyen=mean(distributation_simulee_gamma)
  plot(distributation_simulee_gamma, type = "l", xlab="Itérations" , main = "Candidat de Gamma pour chaque itération" )
  abline(h=gamma_moyen,col='red' )
  
}


representerMH_simulation_finale<-function(resultats_parametres,nombre_iteration_burnIn){
  
  distribution_simulee_beta=resultats_parametres[-(1:nombre_iteration_burnIn),1]
  distributation_simulee_gamma=resultats_parametres[-(1:nombre_iteration_burnIn),2]
  beta_moyen=mean(distribution_simulee_beta)
  gamma_moyen=mean(distributation_simulee_gamma)
  
  theta_init =c("beta"=beta_moyen,"gamma"=gamma_moyen,"initI"=1, "N"=763)
  plot_simulation_graph(simulate_SIR(theta_init))
}


time2 = c(0:50)
infected2 = c(c(31,82,216,299,269,242,190,125,81,52,25,22,7), rep(c(0),38))
data2=cbind(time2, infected2)

##SEIR model
SEIR<-function(t,x,parms){
  ##taille de chaque compartiment et de la population
  S = x[1]
  E = x[2]
  I = x[3]
  R = x[4]
  Z = x[5]
  N = x[1]+x[2]+x[3]+x[4]
  ##valeurs des parametres
  beta = parms["beta"]
  gamma = parms["gamma"]
  sigma = parms["sigma"]
  ##variations
  dS=-beta*S*I/N
  dE=beta*S*I/N-sigma*E
  dI=sigma*E-gamma*I
  dR=gamma*I
  dZ=beta*S*I/N
  res = c(dS,dE,dI,dR,dZ)
  list(res)
}
simulate_SEIR=function(parameters){
  #parameters
  parms = c(parameters["beta"],parameters["gamma"],parameters["sigma"])
  N=parameters["N"]
  #initial conditions
  init <- c(N-parameters["initI"],0,parameters["initI"],0,0)
  #simulation
  temps <- seq(0,50)
  solveSEIR <- deSolve::lsoda(y =init, times=temps, func = SEIR,
                     parms = parms)
  solutionSEIR=as.data.frame(solveSEIR)
  names(solutionSEIR)=c("time","S","E","I","R","Z")
  #merge with data
  seir_data=merge(data2,solutionSEIR,all=TRUE)
  return(seir_data)
}