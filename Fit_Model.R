source("data_clean.R")

#-------------------------------------------------------------------------------
library(rstanarm)
M1.0 <- stan_glm(score ~ Region + GDP + social + life_ex + freedom + 
                 trust + generosity, data = happiness, refresh = 0)
print(M1.0)
pp_check(M1.0)


#-------------------------------------------------------------------------------
M1 <- lm(score ~ Region + GDP + social + life_ex + freedom + 
           trust + generosity, data = happiness)
print(M1)
AIC(M1)
summary(M1)
par(mfrow= c(2,2))
plot(M1)

M1.1 <- lm(score ~ Region:GDP + social + life_ex + freedom + 
             trust + generosity, data = happiness)
print(M1.1)
AIC(M1.1)
summary(M1.1)
par(mfrow= c(2,2))
plot(M1.1)

#-------------------------------------------------------------------------------
library(lme4, arm)
library(lattice)
M2 <- lmer(score ~ (1 | Region) + GDP + social + life_ex + freedom + 
             trust + generosity, data = happiness)
print(M2)
AIC(M2)
plot(M2)
qqmath(M2, main='QQ Plot')

ranef(M2)
fixef(M2)
coef(M2)

M2.1 <- lmer(score ~ (GDP | Region) + GDP + social + life_ex + freedom + 
             trust + generosity, data = happiness)
print(M2.1)
AIC(M2.1)
plot(M2.1)
qqmath(M2.1, main='QQ Plot')

ranef(M2.1)
fixef(M2.1)
coef(M2.1)

library(sjmisc)
library(sjPlot)
library(lme4)
library(glmmTMB)
plot_model(M2.1, sort.est = TRUE, show.values = TRUE, value.offset = .3, type = "re") +
  theme_bw()

M2.2 <- lmer(score ~ (GDP + generosity | Region) + GDP + social + life_ex + freedom + 
               trust, data = happiness)
print(M2.2)
AIC(M2.2)
plot(M2.2)
qqmath(M2.2, main='QQ Plot')

ranef(M2.2)
fixef(M2.2)
coef(M2.2)

# ------------------------------------------------------------------------------
Model <- lmer(score ~ (GDP + social + life_ex + freedom + trust + generosity | Region) + 
                GDP + social + life_ex + freedom + trust + generosity, data = happiness)

print(Model)
AIC(Model)
plot(Model)
qqmath(Model, main='QQ Plot')
summary(Model)
summary(Model)$coefficient
parameters::p_value(Model)

ranef(Model)
fixef(Model)
coef(Model)
plot_model(Model, sort.est = TRUE, show.values = TRUE, value.offset = .3, type = "re") +
  theme_bw()
