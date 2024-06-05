library(ggplot2)

population_size <- 1000
initial_infected <- 10
infection_prob <- 0.05
recovery_rate <- 0.1
death_rate <- 0.01
num_days <- 100

# Age structure: 0-19, 20-39, 40-59, 60+
age_groups <- sample(c("0-19", "20-39", "40-59", "60+"), population_size, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2))
contact_rate <- ifelse(age_groups == "0-19", 6, ifelse(age_groups == "20-39", 8, ifelse(age_groups == "40-59", 5, 3)))
susceptibility <- ifelse(age_groups == "60+", 1.5, 1)

status <- rep("susceptible", population_size)
status[sample(population_size, initial_infected)] <- "infected"

data <- data.frame(day = 0, susceptible = population_size - initial_infected, infected = initial_infected, recovered = 0, dead = 0)

set.seed(123)
for (day in 1:num_days) {
  new_infected <- 0
  new_recovered <- 0
  new_dead <- 0
  
  for (i in 1:population_size) {
    if (status[i] == "infected") {
      if (runif(1) < death_rate) {
        status[i] <- "dead"
        new_dead <- new_dead + 1
      } else if (runif(1) < recovery_rate) {
        status[i] <- "recovered"
        new_recovered <- new_recovered + 1
      } else {
        contacts <- sample(population_size, contact_rate[i], replace = TRUE)
        for (contact in contacts) {
          if (status[contact] == "susceptible" && runif(1) < infection_prob * susceptibility[contact]) {
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
  dead_count <- sum(status == "dead")
  
  data <- rbind(data, data.frame(day = day, susceptible = susceptible_count, infected = infected_count, recovered = recovered_count, dead = dead_count))
}

# 1. Overview of simulation

ggplot(data, aes(x = day)) +
  geom_line(aes(y = susceptible, colour = "Susceptible")) +
  geom_line(aes(y = infected, colour = "Infected")) +
  geom_line(aes(y = recovered, colour = "Recovered")) +
  geom_line(aes(y = dead, colour = "Dead")) +
  labs(title = "Epidemic Spread Simulation", x = "Day", y = "Number of Individuals") +
  scale_colour_manual(values = c("Susceptible" = "blue", "Infected" = "red", "Recovered" = "green", "Dead" = "black"))

ggsave("epidemic_simulation.png", width = 10, height = 6)

# 2. Daily New Infections, Recoveries, and Deaths

data$new_infected <- c(NA, diff(data$infected) + diff(data$recovered) + diff(data$dead))
data$new_recovered <- c(NA, diff(data$recovered))
data$new_dead <- c(NA, diff(data$dead))

daily_data <- data.frame(day = 1:num_days, 
                         new_infected = data$new_infected[-1], 
                         new_recovered = data$new_recovered[-1], 
                         new_dead = data$new_dead[-1])

ggplot(daily_data, aes(x = day)) +
  geom_line(aes(y = new_infected, colour = "New Infections")) +
  geom_line(aes(y = new_recovered, colour = "New Recoveries")) +
  geom_line(aes(y = new_dead, colour = "New Deaths")) +
  labs(title = "Daily New Infections, Recoveries, and Deaths", x = "Day", y = "Number of Individuals") +
  scale_colour_manual(values = c("New Infections" = "orange", "New Recoveries" = "green", "New Deaths" = "black"))

ggsave("epidemic_simulation_daily.png", width = 10, height = 6)


# 3. Cumulative Cases (Infected + Recovered + Dead) Over Time

data$cumulative_cases <- data$infected + data$recovered + data$dead

ggplot(data, aes(x = day, y = cumulative_cases)) +
  geom_line(colour = "purple") +
  labs(title = "Cumulative Cases Over Time", x = "Day", y = "Cumulative Number of Cases")

ggsave("epidemic_simulation_cumulative.png", width = 10, height = 6)

# 4. Age Distribution of Deaths

age_death_data <- data.frame(age_group = unique(age_groups), deaths = numeric(length(unique(age_groups))))

for (age_group in unique(age_groups)) {
  age_death_data[age_death_data$age_group == age_group, "deaths"] <- sum(status[age_groups == age_group] == "dead")
}

ggplot(age_death_data, aes(x = age_group, y = deaths)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(title = "Age Distribution of Deaths", x = "Age Group", y = "Number of Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("epidemic_simulation_age_dist_death.png", width = 10, height = 6)
