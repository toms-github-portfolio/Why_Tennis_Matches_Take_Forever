# Why_Tennis_Matches_Take_Forever
A mini project using 3 different data sets to provide insight into why tennis matches take so long.

---
title: "R Notebook"
output:
  html_document:
    df_print: paged
  pdf_document: default
---
# Small project using data visualization to provide evidence on why tennis matches take so long and how the surface, each player and tournament are signifcant factors 

# In general, players are only active (point in progress) for 15 minutes out of every hour on the court due to the given time inbetween each point 
```{r}
#install.packages("tidyverse")

#library(tidyverse)

# Install the ggplot2 package if not already installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Load the ggplot2 package
library(ggplot2)
```

```{r}
players_time <- read.csv("players_time.csv")
serve_times <- read.csv("serve_times.csv")
events_time <- read.csv("events_time.csv")

players_time
serve_times
events_time
```
```{r}
# HOW EACH SURFACE CORRELATES TO HOW LONG A TENNIS MATCH LASTS

# Scatter Plot with Smooth Line
ggplot(data = events_time, aes(x = surface, y = seconds_added_per_point)) +
  geom_point(mapping = aes(colour = surface)) +
  geom_smooth()

# Summary Statistic Plot
ggplot(data = events_time) + 
  stat_summary(mapping = aes(x = surface, y = seconds_added_per_point),
               fun.ymin = min,
               fun.ymax = max, 
               fun.y = median)

# Analysis of Variance (ANOVA)
result <- aov(seconds_added_per_point ~ surface, data = events_time)

# Perform the ANOVA test
summary(result)
```
# As we can see above, the court surface is significant at 1% and 5% significance level.

# tennis balls travel slower on clay court surfaces in comparison to grass and this hypothesis is proved correctly in both graphs above. 

```{r}
serve_times

library(ggplot2)

ggplot(data = serve_times, aes(x = factor(game_score), y = seconds_before_next_point)) +
  geom_point(aes(colour = seconds_before_next_point), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Serve Times Analysis",
       x = "Game Score",
       y = "Seconds Before Next Point") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
```
# Further evidence for why tennis matches take so long can be shown above by the score in the match being a significant factor. 

# As we see above, when each game has just started, the average number of seconds before the next point is drastically lower than further down the line because players are affected by fatigue and pressure from the scoreline

# This evidence is subject to each player having different routines and styles however, a link can be made in that when players are in winning positions, they take less time between each point and on pressure points, tend to take longer 

```{r}
events_time

ggplot(data = events_time, aes(x = factor(tournament), y = seconds_added_per_point)) +
  geom_point(aes(colour = seconds_added_per_point), size = 3, position = position_dodge(width = 0.7)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Tournament Times Analysis",
       x = "Tournament",
       y = "Seconds Added per Point") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),  # Adjust text size for better clarity
        legend.position = "top")  # Move legend to the top for
```

LETS NOW ONLY TAKE THE 10 HIGHEST TOURNAMENTS FOR POINTS ADDED SO WE SEE WHICH LOCATIONS THESE WERE

```{r}
library(ggplot2)
library(dplyr)
library(forcats)

# Reorder 'tournament' based on frequency and select top 10
top_tournaments <- events_time %>%
  count(tournament, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(tournament)

# Subset the data for the top 10 tournaments
events_time_subset <- events_time[events_time$tournament %in% top_tournaments, ]

# Plot with the top 10 tournaments
ggplot(data = events_time_subset, 
       aes(x = fct_inorder(tournament), y = seconds_added_per_point)) +
  geom_point(aes(colour = seconds_added_per_point), size = 3, position = position_dodge(width = 0.7)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Tournament Times Analysis",
       x = "Tournament",
       y = "Seconds Added per Point") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        legend.position = "top")
```

# Most of these tournaments above are clay and so we now have further evidence to suggest that the surface does affect the amount of time a tennis match takes
