# Advanced-ish programming: for loops, apply, functions
#
#

# functions:
affirmation <- function(x) {
  paste(x, ", you are awesome", sep = "")
}
affirmation(class_list)

std.error <- function (x) {
  print(sd(x)/sqrt(length(x - 1)))
}
std.error(penguins$bill_length_mm)

# for loop
for (i in 1:10){
  print(paste("hello", i))
}

class_list <- c("henrey", "isabella", "ray", "margaret", "hunter")
for(i in class_list) {
  affirmation(i)
}
sapply(X = class_list, FUN = affirmation)

# Aggregating data by type
library(dplyr)
head(penguins)
species <- unique(penguins$species)
for (i in species) {
  print(i)
  penguins %>% filter(species == i) %>%
    pull(bill_length_mm) %>%
    mean(., na.rm = T) %>% print
}
lapply(X = species, FUN = function(x) mean(penguins$bill_length_mm[penguins$species == x]))
tapply(X = penguins$bill_length_mm, INDEX = penguins$species, FUN = mean, na.rm = T)

aggregate(bill_length_mm ~ species, data = penguins, FUN = mean, na.rm = T)
penguins %>% group_by(species) %>% summarize(sppbill = mean(bill_length_mm))


x <- rnorm(n = 50, mean = 10, sd = 2)
y <- rnorm(n = 50, mean = 11, sd = 2)
t.test(x = x, y = y)$p.value

