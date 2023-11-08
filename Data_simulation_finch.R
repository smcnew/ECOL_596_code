# categorical
n_nests <- 30
d1 <- data.frame(nest = 1:60,
                 site = c(rep("site1", 30), rep("site2", 30)),
                 treatment = c(rep("water", 30),
                                rep("permethrin", 30)))

d1$parasites[d1$treatment == "water"] <- rpois(n_nests, 15) + round(runif(n_nests, min = -6, max = 6))
d1$parasites[d1$treatment == "permethrin"] <- rpois(n_nests, 3)
))
round(1.3)

round(rnorm(n_nests, mean = 0, sd = 7))
write.csv(d1, "~/Downloads/data_philornis.csv")
# continuous
n_nests <- 30
d2 <- data.frame (nest = 1:n_nests,
                  cotton = runif(n_nests, min = 0, max = 8),
                  error = rnorm(n_nests, mean = 0, sd =2))

d2 <- mutate(d2, parasites = 20 -(cotton * 2.1) + error)
plot(parasites ~ cotton, d2)
