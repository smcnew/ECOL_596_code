# An example of using PCs vs spatial data in a model
# Test dataset: hosts_bioclim.csv

library(ggbiplot) # for pca
library(dplyr)

dat <- read.csv("datasets/hosts_bioclim.csv")
dat$community <- 1:18
head(dat)
# total.host = response variable, aka, species richness of hosts at 18 sites
# all other columns are bioclim variables of those sites.


# PCA the bioclim variables
climpca <- dat %>% select(-c(total.host, community)) %>% prcomp(scale = T, center = T)
summary(climpca)

# Inspect how PCA looks
ggbiplot(climpca,
         obs.scale = 1,
         var.scale = 1,
         varname.size = 2,
         labels = dat$community,
         labels.size = 4)

# Grab the PCA values
climpca$x %>% head()

# Create a new predictor dataset
model_dat <- select(dat, total.host) %>%
                   mutate(pc1 = climpca$x[,1],
                          pc2 = climpca$x[,2],
                          pc3 = climpca$x[,3],
                          mean_temp = dat$bio1, # mean temp
                          total_precip = dat$bio12) # Add pcs
model_dat
glm(total.host ~ pc1 + pc2 + pc3, data = model_dat, family = "quasipoisson") %>% summary
glm(total.host ~ mean_temp + total_precip, data = model_dat, family = "quasipoisson") %>% summary
