library(dplyr)
library(lubridate)
library (deSolve) 
library(ggplot2)


load("peste_data.RData") # load peste data


# constants ---------------------------------------------------------------


PESTE_PLOTS_DIR <- "peste_plots"
dir.create(PESTE_PLOTS_DIR, showWarnings = FALSE)

SEIR_COMPARTMENTS <-  c("Susceptible", "Exposed", "Infectious", "Recovered")
CATEGORY_MAPPING <-
  c(
    "Enfermos existentes segun parte anterior" = SEIR_COMPARTMENTS[3], # infectious
    "Nuevamente acometidos" = SEIR_COMPARTMENTS[3], # infectious
    "Pasados a convalecientes" = SEIR_COMPARTMENTS[4], # recovered
    "Curados" = SEIR_COMPARTMENTS[4], # recovered
    "Total de enfermos que restan" = SEIR_COMPARTMENTS[2] # exposed
  )


# functions ---------------------------------------------------------------


seir_model <- function(current_timepoint, state_values, parameters) {
  #' SEIR Model Function
  #'
  #' This function implements the SEIR (Susceptible-Exposed-Infectious-Recovered) model 
  #' to compute the derivatives of each compartment with respect to time.
  #'
  #' @param current_timepoint The current time point.
  #' @param state_values A vector containing the current values of the state variables (S, E, I, R).
  #' @param parameters A list containing the parameters of the SEIR model (beta, delta, gamma).
  #' 
  #' @return A list containing the derivatives of each compartment (dS, dE, dI, dR).
  
  # create state variables (local variables)
  S <- state_values[1]        # susceptible
  E <- state_values[2]        # exposed
  I <- state_values[3]        # infectious
  R <- state_values[4]        # recovered
  
  with ( 
    as.list (parameters),     # variable names within parameters can be used 
    {
      # compute derivatives
      dS <- (-beta * S * I)
      dE <- (beta * S * I) - (delta * E)
      dI <- (delta * E) - (gamma * I)
      dR <- (gamma * I)
      
      # combine results
      results <- c (dS, dE, dI, dR)
      list (results)
    }
  )
}


run_seir <- function(df, localidad, npopulation) {
  #' Run SEIR Epidemic Simulation
  #'
  #' This function simulates the SEIR (Susceptible-Exposed-Infectious-Recovered) epidemic model 
  #' for a given sub-population defined by a specific locality.
  #'
  #' @param df A data frame containing the epidemic data with columns for "Municipio" (locality), 
  #' "Categoria" (epidemic category), and "Fecha" (date).
  #' @param localidad The name of the locality (sub-population) for which the simulation will be run.
  #' @param npopulation The total population of the locality.
  #' 
  #' @return NULL. The function generates a plot showing the dynamics of the Susceptible, Exposed, 
  #' Infectious, and Recovered sub-populations over time.

  df_tmp <- subset(df, Municipio == localidad) # choose sub-population by localidad
  df_tmp$Casos <- df_tmp$Casos/npopulation # normalize "Casos" by "npopulation"
  
  # calculate "Total_Casos"
  df_tmp <- df_tmp %>%
    # group by "Municipio", "Categoria" and "Fecha"
    group_by(Municipio, Categoria, Fecha) %>%
    # sum the cases for each group
    summarise(Total_Casos = sum(Casos)) %>%
    # ungroup the df_tmp
    ungroup()
  
  # parameters
  contact_rate <- mean(df_tmp$Total_Casos)     # average number of contacts per day
  transmission_rate <- 0.5                                 # transmission rate
  infectious_period <- 4.3                                 # average infectious period
  latent_period <- 2.5                                     # average latent period
  
  # compute values of beta (transmission rate) and gamma (recovery rate)
  beta_value <- contact_rate * transmission_rate
  gamma_value <- 1 / infectious_period
  delta_value <- 1 / latent_period
  
  # compute Ro - reproductive number
  Ro <- beta_value / gamma_value
  print(paste0("Reproductive number (Ro) for ", localidad, " is ", Ro))
  
  # disease dynamics parameters
  parameter_list <- c(beta = beta_value, gamma = gamma_value, delta = delta_value)
  
  # initial values for sub-population
  X <- nrow(df_tmp[df_tmp$Categoria == SEIR_COMPARTMENTS[3], ]) # infectious
  Y <- nrow(df_tmp[df_tmp$Categoria == SEIR_COMPARTMENTS[4], ]) # recovered
  Z <- nrow(df_tmp[df_tmp$Categoria == SEIR_COMPARTMENTS[2], ]) # exposed
  W <- npopulation - X - Y - Z # susceptible = total population - infectious - recovered - exposed
  N <- npopulation # total population
  
  # initial state values for the differential equations
  initial_values <- c(S = W/N, E = X/N, I = Y/N, R = Z/N) # population ratios
  # initial_values <- c(S = W, E = X, I = Y, R = Z) # without normalization
  timepoints <- seq(0, nrow(df_tmp) - 1, by = 1)
  
  # simulate the SEIR epidemic
  model_seir <- lsoda(initial_values, timepoints, seir_model, parameter_list)

  # plot simulation and historical data
  historical_data <- df_tmp[df_tmp$Categoria == SEIR_COMPARTMENTS[3], ]$Total_Casos
  plot(I ~ time, data = model_seir, type = "l", lwd = 3, ylim = c(0, 0.15), col = "black", xlab = "Time (days)", ylab = "Ratio infections", main = paste0("Simulated plague infections - ", localidad))
  axis(side = 1, at = seq(0, 100, by = 5))
  barplot(historical_data, col = rgb(0.1, 0.1, 0.1, alpha = 0.5), add = TRUE)
  
  # plot dynamics of susceptible, exposed, infectious and recovered sub-population
  # plot(S ~ time, data = model_seir, type = "b", ylim = c(0, 1), col = "blue", xlab = "Time (days)", ylab = "Proportion of individuals", main = paste0("SEIR Epidemic - ", localidad))
  # lines(E ~ time, data = model_seir, type = "b", col = "pink")
  # lines(S ~ time, data = model_seir, type = "b", col = "red")
  # lines(R ~ time, data = model_seir, type = "b", col = "green")
  # legend("right", legend = SEIR_COMPARTMENTS, col = c("blue", "pink", "red", "green"), lty = 1, bty = "n", inset = c(0.01, 0.05))
}


# main --------------------------------------------------------------------


# convert "Categoria" to the corresponding SEIR compartment
df_peste$Categoria <- CATEGORY_MAPPING[df_peste$Categoria]


# SEIR --------------------------------------------------------------------


# simulate the SEIR epidemic by "Municipio"
run_seir(df_peste, LOCALIDADES_STR[1], 3626) # Artà
run_seir(df_peste, LOCALIDADES_STR[2], 1179) # Capdepera
run_seir(df_peste, LOCALIDADES_STR[3], 1338) # Sant Llorenç des Cardassar
run_seir(df_peste, LOCALIDADES_STR[4], 1684) # Son Servera

rm(list = ls())
