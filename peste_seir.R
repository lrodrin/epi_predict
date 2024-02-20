library(dplyr)
library(lubridate)
library (deSolve) 

category_mapping <-
  c(
    "Enfermos existentes segun parte anterior" = "Infectious",
    "Nuevamente acometidos" = "Infectious",
    "Muertos" = "Recovered",
    "Pasados a convalecientes" = "Recovered",
    "Curados" = "Recovered",
    "Total de enfermos que restan" = "Exposed"
  )

# Convert "Categoria" column to the corresponding SEIR compartment
df_peste$Categoria <- category_mapping[df_peste$Categoria]

# Choose "Artà"
df_peste.arta <- subset(df_peste, Municipio == "Artà")

# Calculate "Total_Casos"
df_peste.arta <- df_peste.arta %>%
  # Group by "Municipio", "Categoria" and "Fecha"
  group_by(Municipio, Categoria, Fecha) %>%
  # Sum the cases for each group
  summarise(Total_Casos = sum(Casos)) %>%
  # Ungroup the data
  ungroup()

# Function to compute derivatives of the differential equations
seir_model <- function (current_timepoint, state_values, parameters) {
  
  # create state variables (local variables)
  S = state_values[1]        # susceptible
  E = state_values[2]        # exposed
  I = state_values[3]        # infectious
  R = state_values[4]        # recovered
  
  with ( 
    as.list (parameters),     # variable names within parameters can be used 
    {
      # compute derivatives
      dS = (-beta * S * I)
      dE = (beta * S * I) - (delta * E)
      dI = (delta * E) - (gamma * I)
      dR = (gamma * I)
      
      # combine results
      results = c (dS, dE, dI, dR)
      list (results)
    }
  )
}

# Parameters
contact_rate = 10                     # number of contacts per day
transmission_probability = 0.07       # transmission probability
infectious_period = 5                 # infectious period
latent_period = 2                     # latent period

# Compute values of beta (tranmission rate) and gamma (recovery rate)
beta_value = contact_rate * transmission_probability
gamma_value = 1 / infectious_period
delta_value = 1 / latent_period

# Compute Ro - Reproductive number
Ro = beta_value / gamma_value

# Disease dynamics parameters
parameter_list = c (beta = beta_value, gamma = gamma_value, delta = delta_value)

# Initial values for sub-population
X <- nrow(df_peste.arta[df_peste.arta$Categoria == "Infectious", ])
Y <- nrow(df_peste.arta[df_peste.arta$Categoria == "Recovered", ])
Z <- nrow(df_peste.arta[df_peste.arta$Categoria == "Exposed", ])
W <- 3626 - X - Y - Z # Susceptible = Total population - Infectious - Recovered - Exposed 
N <- 3626 # Total population

# Initial state values for the differential equations
initial_values <- c (S = W/N, E = X/N, I = Y/N, R = Z/N)
timepoints <- seq(0, nrow(df_peste.arta) - 1, by = 1)

# Simulate the SEIR epidemic
output <- lsoda (initial_values, timepoints, seir_model, parameter_list)

# Plot dynamics of Susceptibles, Exposed, Infectious and Recovered sub-populations in the same plot
plot(S ~ time, data = output, type = 'b', ylim = c(0, 1), col = 'blue', ylab = 'Population', main = 'SEIR Epidemic - Artà')
lines(E ~ time, data = output, type = 'b', col = 'pink')
lines(I ~ time, data = output, type = 'b', col = 'red')
lines(R ~ time, data = output, type = 'b', col = 'green')
legend('topright', legend = c("Susceptible", "Exposed", "Infectious", "Recovered"), col = c('blue', 'pink', 'red', 'green'), lty = 1, bty = 'n', inset = c(0.02, 0.02))

output_df <- as.data.frame(output)

# Crear el gráfico SEIR con ggplot2
ggplot(output_df, aes(x = time)) +
  geom_line(aes(y = S, color = "Susceptible")) +
  geom_line(aes(y = E, color = "Exposed")) +
  geom_line(aes(y = I, color = "Infectious")) +
  geom_line(aes(y = R, color = "Recovered")) +
  scale_color_manual(values = c("Susceptible" = "blue", "Exposed" = "pink", "Infectious" = "red", "Recovered" = "green")) +
  labs(title = "SEIR Epidemic - Artà", y = "Population") +
  theme_minimal() +
  theme(legend.position = "top")