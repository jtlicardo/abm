install.packages("ggplot2")
library(ggplot2)

population_size <- 1000 # Total population
initial_infected <- 10 # Initially infected individuals
infection_prob <- 0.05 # Probability of infection per contact
recovery_rate <- 0.1 # Probability of recovery per time step
num_days <- 100 # Number of days to simulate

# Initialization
status <- rep("susceptible", population_size)
status[sample(population_size, initial_infected)] <- "infected"
data <- data.frame(day = 0, susceptible = population_size - initial_infected, infected = initial_infected, recovered = 0)

# Simulation
set.seed(123)
for (day in 1:num_days) {
  
  # Counters for new infections and recoveries each day
  new_infected <- 0
  new_recovered <- 0
  
  # Iterate over each individual in the population
  for (i in 1:population_size) {
    if (status[i] == "infected") {
      if (runif(1) < recovery_rate) {
        # Change status to 'recovered' if the random number is below the recovery rate
        status[i] <- "recovered"
        new_recovered <- new_recovered + 1
      } else {
        # Select random contacts 
        contacts <- sample(population_size, 5, replace = TRUE)
        for (contact in contacts) {
          # Check if contact is susceptible and if infection occurs based on infection probability
          if (status[contact] == "susceptible" && runif(1) < infection_prob) {
            status[contact] <- "infected"
            new_infected <- new_infected + 1
          }
        }
      }
    }
  }
  
  susceptible_count <- sum(status == "susceptible")
  infected_count <- sum(status == "infected")
  recovered_count <- sum(status == "recovered")
  
  data <- rbind(data, data.frame(day = day, susceptible = susceptible_count, infected = infected_count, recovered = recovered_count))
}

ggplot(data, aes(x = day)) +
  geom_line(aes(y = susceptible, colour = "Susceptible")) +
  geom_line(aes(y = infected, colour = "Infected")) +
  geom_line(aes(y = recovered, colour = "Recovered")) +
  labs(title = "Epidemic Spread Simulation", x = "Day", y = "Number of Individuals") +
  scale_colour_manual(values = c("Susceptible" = "blue", "Infected" = "red", "Recovered" = "green"))

ggsave("epidemic_simulation.png", width = 10, height = 6)
