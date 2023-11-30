# I also had issues with glmmTMB version. Google info here:
# https://github.com/glmmTMB/glmmTMB/issues/886
# I updated my R but it didn't really help. But I don't think the warning/error is fatal .

install.packages("ggeffects")


library('ggplot2')
library('glmmTMB')
library('ggeffects')
d <- data.frame (diameter = c(17,16,15,13,11, 19,17,15,11,11, 19,15,14,11,8),
                 plant_density = c(1000,2000,3000,4000,5000, 1000,2000,3000,4000,5000, 1000,2000,3000,4000,5000),
                 plotx = as.factor( c(1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3)))

glmm.model <- glmmTMB(diameter ~  plant_density + (1|plotx),
                      data = d,
                      family="gaussian")
plot(ggpredict(glmm.model, terms = "plant_density"), show_data = T)

