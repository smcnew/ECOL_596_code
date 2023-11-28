# Advanced-ish programming: for loops, apply, functions

# How to write functions:
affirmation <- function (name){
  paste(name, "is awesome", sep = " ")
}
affirmation(name = "margaret")

class_list <- c("margaret", "isabella", "maddie")
affirmation(class_list)

standard_error <- function(blue){
  sd(blue)/sqrt(length(blue))
}

standard_error(blue = 1:10)
plotrix::std.error(1:10)


fun = function(x,y){
  x*y
}
fun(y = 1:5, x = (100,200,300,400,500))

compound_word <- function(word1,word2){
  paste(word1,word2, sep = "")
}

compound_word("bean", "soup")
mad_libs_food <- function(noun, adjective = "disgusting") {
  paste(noun, "is", adjective, sep = " ")
}
mad_libs_food("pizza")
nouns <- c("beans", "pizza", "pasta", "oranges")
adjectives <- c("green", "sharp", "gross", "yum")
mad_libs_food(noun = nouns, adjective = adjectives)
library(palmerpenguins)

for (i in unique(penguins$species)){
  print(affirmation(name = i))
}

# simulate p values under a null of no difference between
# two groups
simulated.t.test <- function(meanx, meany) {
x <- rnorm(n = 100, mean = meanx, sd = 1)
y <- rnorm(n = 100, mean = meany, sd = 1)
t.test(x, y)$p.value
}


myps <- rep(NA, 1000)
for (i in 1:1000) {
myps[i] <- simulated.t.test(meanx = 5, meany = 5.3)
}
hist(myps)
myps[myps<0.05] %>% length
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
lapply(X = species, FUN = function(x)
  mean(penguins$bill_length_mm[penguins$species == x]))
tapply(X = penguins$bill_length_mm, INDEX = penguins$species, FUN = mean, na.rm = T)

aggregate(bill_length_mm ~ species, data = penguins, FUN = mean, na.rm = T)
penguins %>% group_by(species) %>% summarize(sppbill = mean(bill_length_mm))


x <- rnorm(n = 50, mean = 10, sd = 2)
y <- rnorm(n = 50, mean = 11, sd = 2)
t.test(x = x, y = y)$p.value




# results
plate1 <- read.csv("datasets/results/plate1.csv")
metadata <- read.csv("datasets/results/metadata.csv")
head(metadata)
head(plate1)

#goal: add sample and treatment from metadata onto plate 1
