# Sabrina McNew
# Scratch script for ECOL 596
# I can add more context notes to the header here

library(ggplot2) # plotting
library(ggthemr) # plot themes
library(tidyr) # data wrangling, automatically includes ggplot and other packages

library(lme4) #tests
library(lmerTest)

library(dslabs)
library(palmerpenguins) # for penguins dataset
library(dplyr) #load last to avoid conflicts between packages

# Set general plot parameters somewhere where they won't get lost
ggthemr(palette = "flat", layout = "clean", text_size = 22)

# Load Data ---------------------------------------------------------------
finch <- read.csv("datasets/finch.csv")
fish <- read.delim("datasets/maine_fish.txt", header = TRUE, sep = "\t")
swallows <- read.csv("datasets/swallows.csv")
nestlings <- read.csv("datasets/swallow_nestlings.csv")
penguins <- penguins

# Data processing section 1 -----------------------------------------------
finch <- mutate(finch, species = as.factor(species))

finch %>% select(date, type_of_site, species, mass,
                 sex, band, pox_IUR) %>% head


plot(Beak_length ~ beak_depth, data = finch)

finch |>
  ggplot(aes(x = beak_depth, y = beak_length, color = species)) +
  geom_point() +
  theme( text = element_text(size=22))


head(finch)

finch |> ggplot(aes(x = type_of_site, fill = Pox_IUR)) +
  geom_bar()


finch |> select(Band, Species, Wing, Mass) |>
         filter(Species %in% c("medium","small")) |>
         mutate(sci_name = case_when(Species == "medium" ~ "G.fortis",
                                     Species == "small" ~ "G.fuliginosa")) |>
        summarize(mean_wing = mean(Wing, na.rm = T))

finch |> summarize(mean_wing = mean(Wing, na.rm = T))
finch |> group_by(Species) |>
  summarize(mean_wing = mean(Wing, na.rm = T))

finch %>% filter(Species == "large") %>% pull (Wing)

aggregate(Wing ~ Species + type_of_site, data = finch, mean)

finch |> ggplot(aes(x = Species, y = Mass))+
  geom_boxplot()
str(finch$Plumage)
str(finch$Mass)
finch <- finch %>% mutate(infected = case_when(Pox_IUR == "I" ~ 1,
                                               TRUE ~ 0)) %>%
                  mutate(infected = as.factor(infected))
str(finch$infected)
str(finch$Species)
as.factor(finch$Species) %>% head

finch <- finch %>% mutate(date = as.Date(date, format = "%d-%b-%y"))
str(finch$date)
head(finch)
finch %>% ggplot(aes(x = infected)) + geom_bar()

# Distributions -----------------------------------------------------------


data.frame(x = rnorm(10000)) %>% ggplot(aes(x =x)) + geom_histogram()
data.frame(ticks = c(rpois(5000, lambda = 7))) %>% ggplot(aes(x =ticks)) + geom_histogram()
rbinom(2, size = 1, prob = 0.2)
rpois(10000, lambda = 4 ) %>% hist

finch %>% ggplot(aes(x = Mass)) +geom_histogram()
hist(finch$Mass)
finch %>% ggplot(aes(x = Species, fill = Pox_IUR)) + geom_bar() +
scale_fill_discrete(labels=c("Infected","Recovered", "Uninfected"))
table(finch$Species, finch$Pox_IUR)

head(finch)
finch1 <- select(finch, site, Species, Wing, Mass)
head(finch1)

locations <- finch %>% select(site, Latitude, Longitude) %>% distinct(site, .keep_all = TRUE)
locations
dim(finch1)
dim(locations)
left_join(finch1, locations ) %>% dim

plants <- data.frame(site = c("a","a","b","b","b","c","c","c"),
                     plant = c(1:8), height = signif(rnorm(8, mean = 4, sd = 1),3))
plants
site_covars <- data.frame(site = c("a","a","b","c"), elev = c(1800, 2000, 40, 1600),
                          precip = c(20, 24, 5, 18))
site_covars
left_join(plants, site_covars)
finch$date <- as.character(finch$date)

finch %>% select(date, Band, Species) %>% separate(date, c("year","month","day")) %>%
  unite("date", c("year", "month", "day"), sep = "-") %>% head



#probability density
mean = 8
sd = 2
x <- seq(mean - 4 * sd, mean + 4 * sd, length.out = 100)
pdf <- dnorm(x, mean, sd)
data2 <- as.data.frame(cbind(x,pdf))
ggplot(data2, aes(x = x, y = pdf)) + geom_line() +
 # geom_vline(xintercept = mean, color = "purple" ) +
  labs(x = "", y = "probability density")+
 geom_area(data = filter(data2, x > 3.5 & x < 12.5),
           aes(x = x, y = pdf), alpha = 0.5)


1- pnorm(72, mean = 64, sd = 3)

coin.flip <- function(n){
  for(i in 1:n) {
  sample(c("H","T"), size = 5, replace = T, prob = c(0.5, 0.5)) %>% print()
  }
}
coin.flip(5)

num_heads <- 0:20
prob <- dbinom(num_heads, size = 20, prob = 0.5)

# Create a data frame
df <- data.frame("N_heads" = as.factor(num_heads), "Probability" = prob)

# Plot the probability density
ggplot(df, aes(x = N_heads, y = Probability)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of heads", y = "Probability") #+
  ggtitle("Probability Density of Righties")

df %>% filter(N_heads %in% c(14:16)) %>% summarize(total = sum(Probability))

head(finch)
finch %>% ggplot(aes(x = type_of_site)) + geom_bar() + labs(x = "Habitat")

pt(0.56, df = 24, lower.tail = F)

# Probability density for df = 24
df <- 24

# Create a data frame with x values
data <- data.frame(x = seq(-5, 5, length.out = 1000))

# Calculate the probability density function (PDF) of the t-distribution for each x
data$y <- dt(data$x, df)

# Plot the density of the t-distribution
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1) +
  labs(x = "x", y = "Density") +
  geom_area(data = filter(data, x > .56 ),
            aes(x = x, y = y), alpha = 0.5) +
  labs(x = "t")


# Regression --------------------------------------------------------------
head(penguins)
penguins %>% filter(species == "Adelie") %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) + geom_point() +
  geom_smooth(method = "lm") + labs (x = "bill length (mm)", y = "bill depth (mm)")


penguins %>% ggplot(aes(x = species, y = bill_length_mm)) +geom_boxplot() +
  labs(y = "bill length (mm)")

penguins %>% ggplot(aes(x = flipper_length_mm, y = bill_length_mm)) +
  geom_point() + labs(x = "flipper length (mm)", y = "bill length (mm)")

mod2 <- lm(bill_length_mm ~ flipper_length_mm, data = penguins)
# predicting values
predict(mod2, newdata = data.frame(flipper_length_mm  = 300))
summary(mod2)


# Tukey pair-wise contrasts
library(emmeans)
mod3 <- lm(bill_length_mm ~ species, data = penguins)
emmeans(mod3, specs = "species") %>% pairs(adjust = "tukey")

head(penguins)

head(fish)

fish %>% ggplot(aes(x = acres, y = fish)) + geom_point() +
  labs(x = "lake size (acres)", y = "fish species")


fish %>% ggplot(aes(x = scale(log(acres)), y = fish)) + geom_point() +
  labs(x = "lake size (log acres, centered and scaled)", y = "fish species")
lm(fish ~ log(acres), data = fish) %>% summary

# Interactions
penguins %>% ggplot(aes(y = flipper_length_mm, x = body_mass_g/1000,
                        color = species)) + geom_point() +
  labs(x = "flipper length (mm)", y = "mass (kg)") +
  geom_smooth(method = "lm", se = FALSE)

penguins$body_mass_kg  <- penguins$body_mass_g/1000
lm(body_mass_kg ~
    flipper_length_mm * species, data = penguins) %>% summary()

# GLMs --------------------------------------------------------------------

fish %>% ggplot(aes(y = fish, x = depth_mu)) + geom_point() +
  labs(x =" mean lake depth", y ="fish spp") +
  xlim(0, 70) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"))
  geom_smooth(method = "lm")

lm(fish ~ depth_mu, data = fish) %>% plot
glm1 <- glm(fish ~ depth_mu, data = fish, family = "poisson")
summary(glm1)
predict(glm1, newdata = data.frame (depth_mu = 40), type = "response")


head(fish)


# overdispersion
fish %>% ggplot(aes(x = fish)) +geom_histogram(color = "black")
mean(fish$fish)
var(fish$fish)

glm(fish ~ log(depth_mu +1), data = fish, family = "quasipoisson") %>% summary()
MASS::glm.nb(fish ~ log(depth_mu + 1), data = fish) %>% summary()

# Logistic regression
swallows <- read.csv("datasets/swallows.csv")
head(swallows)

swallows %>% ggplot(aes(x = treatment, y = as.factor(success))) + geom_bar(stat = "identity")
glm(as.factor(success) ~ treatment, data = swallows, family = "binomial") %>% summary

LaplacesDemon::invlogit(0.5306)
LaplacesDemon::invlogit(0.5306 - 1.6881)

percentage_data <- swallows %>% group_by(treatment) %>% summarize(percent_fledge = mean(success))
ggplot(percentage_data, aes(x = treatment, y = percent_fledge)) + geom_col()

swallows <- swallows %>% mutate(percent_fledge = nestlings_lived / brood_size)
swallows %>% select(success, nestlings_died, nestlings_lived, percent_fledge) %>% head


glm(cbind(nestlings_lived, nestlings_died) ~ treatment, data = swallows, family = "binomial") %>% summary


# LMMs
#
nestlings <- read.csv("/Users/sabrinamcnew/Library/CloudStorage/Dropbox/Projects/mcnew_etal_tres_nestling_telos/compiled_data/comp_data.csv")
nestlings <- nestlings %>% select(band, year, predator, Mass, site_nest, Nestling_Fate, Flat_Wing)
nestlings <- nestlings %>% separate(site_nest, into = c("Unit", "Number", "nest", "year2")) %>%
            mutate(site = paste(Unit, Number, sep = "_")) %>%
            select(-c(Unit, Number, year2)) %>%
            rename(treatment = predator, mass = Mass, nestling_fate = Nestling_Fate, wing = Flat_Wing)
write.csv(nestlings, "datasets/swallow_nestlings.csv", row.names = F)
head(nestlings)

nestlings %>% ggplot(aes(y = mass, x = treatment)) + geom_boxplot()
lm(mass ~ treatment, data = nestlings) %>% summary
lmer(mass ~ treatment + (1|site/nest) + (1|year), data = nestlings) %>% summary
lmer(mass ~ treatment + (1 + nest | site) + (1|year), data = nestlings) %>% summary

summary(nestlings$Mass)

head(nestlings)

head(nestlings)
nestlings %>% ggplot()
head(penguins)
penguins <- filter(penguins, !is.na(sex))
penguins %>% filter() ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = island)) + geom_point()


nestmeans <- nestlings %>% group_by(nest) %>% summarize(mean_mass = mean(mass))
nestmeans$group <- split(nestmeans$mean_mass, 5)
head(nestmeans)
cutlevs <- levels(nestmeans$group)
nestmeans <- nestmeans %>% mutate(site = case_when(group == cutlevs[1] ~ "unit_1",
                                                  group == cutlevs[2] ~ "unit_2",
                                                  group == cutlevs[3] ~ "unit_4",
                                                  group == cutlevs[4] ~ "unit_5",
                                                  group == cutlevs[5] ~ "unit_3")) %>%
  select(nest, site)

nestlings <- nestlings %>% select (-site) %>%  left_join(., nestmeans)

head(nestlings)
nestlings %>% ggplot(aes(x = wing, y = mass, color = site)) + geom_point() +
  geom_smooth(aes(color = site, fill = site), method = "lm")
lmer(mass ~ wing + (1|site), data = nestlings) %>% summary
write.csv(nestlings, "datasets/nestlings.csv", row.names = FALSE)
palette()

# causal inference
#
students <- data.frame(sportiness = rnorm(500), academicness = rnorm(500))
students <- students %>% mutate(UofA = case_when(sportiness + academicness >1.7  ~ 1,
                                                TRUE ~ 0 )) %>%
  mutate(UofA = as.factor(UofA))
students %>% filter(UofA == 1)  %>%
  ggplot(aes(x = academicness, y = sportiness, color = UofA)) + geom_point() +
  geom_smooth(method = "lm", se=F) +
  scale_color_manual(values = "#61D04F")
geom_point(aes(col = UofA)) +
  geom_smooth(method = "lm", se= F)


# backwards and forwards selection
head(fish)

glm(fish ~ acres + depth_mu + depth_max + elev_ft + management + lat,
    data = fish, family = "quasipoisson") %>% summary

# Git integration
usethis::create_github_token()
gitcreds::gitcreds_set()


# PCA
head(penguins)

gentoo_pca <- penguins %>% filter(species == "Gentoo") %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_kg)

head(gentoo_pca)
penguins %>% filter(species == "Gentoo") %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point()+
  geom_smooth(method = "lm", se= F) +
  labs(x="var 1", y = "var 2")


str(pca1)
summary(pca1)
pca1$rotation
pca1$x
plot(pca1)
pca2 <- penguins %>% select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_kg) %>%
  prcomp(center = TRUE, scale. = TRUE)
str(pca2)

devtools::install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(pca2,
         obs.scale = 1,
         var.scale = 1,
         groups = penguins$species,
         ellipse = T)
cor(penguins$flipper_length_mm, penguins$body_mass_kg)


# Chocolate chip cookies --------------------------------------------------
# wrangle the data into wide format, simplify ingredients
cookies <- read.csv("datasets/choc_chip_cookie_ingredients.csv")
head(cookies)
cookies <- filter(cookies, Ingredient %in% c("all purpose flour",
                                            "baking powder",
                                            "baking soda",
                                            "butter",
                                            "egg",
                                            "salt",
                                            "sugar",
                                            "vanilla",
                                            "light brown sugar",
                                            "dark brown sugar",
                                            "bittersweet chocolate chip",
                                            "semisweet chocolate chip",
                                            "milk chocolate chip")) %>%
  mutate(Ingredient = str_replace_all(Ingredient, " ", "_")) %>%
  select(Ingredient, Recipe_Index, Quantity) %>%
  pivot_wider(id_cols = Recipe_Index,
                        names_from = Ingredient,
                        values_from = Quantity,
                        names_sep = "_",
                        values_fn = mean,
                        values_fill = 0)
head(cookies)
write.csv(cookies, "datasets/cookies.csv")
cookies %>% dplyr::group_by(Recipe_Index, Ingredient) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L)
cookies %>% filter(Recipe_Index == "Misc_109")




# dslabs ------------------------------------------------------------------

murders <- murders
str(murders$region)

testnorm <- rpois(n = 100, lambda = 7)
qqnorm(testnorm)
qqline(testnorm)

