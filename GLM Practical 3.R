EIA <- read.csv('EIA.csv')
EIA$impact <- as.factor(EIA$impact)
EIA$MonthOfYear <- as.factor(EIA$MonthOfYear)
EIA$Year <- as.factor(EIA$Year)
attach(EIA)  # take care!

fit.pois.sqrt <- glm(count ~ tidestate + observationhour + DayOfMonth + MonthOfYear + Year + x.pos + y.pos + Year:x.pos + Year:y.pos, data = EIA, family = poisson(link = 'sqrt'))
summary(fit.pois.sqrt)

##2.-
#yit∼Poisson(λit=η2it)

##3.-
fit.pois <- glm(count ~ tidestate + observationhour + DayOfMonth + MonthOfYear + Year + x.pos + y.pos + Year:x.pos + Year:y.pos, data = EIA, family = poisson)
summary(fit.pois)

##4.-
#yit∼Poisson(λit=exp(ηit)

##5.- False: The values in the GVIF^{(1/(2*Df))} are used to quantify collinearity when there is more than one coefficient associated with one or more of the model covariates.
fit.pois.offset <- glm(count ~ tidestate + observationhour + DayOfMonth + MonthOfYear + Year + x.pos + y.pos + Year:x.pos + Year:y.pos, data = EIA, family = poisson, offset = log(area))
vif(fit.pois.offset)
fit.pois.offset.no_x.pos <- update(fit.pois.offset, . ~ . - x.pos)
vif(fit.pois.offset.no_x.pos)

##6.- False: An equivalent alternative to including an offset term in a model is to include the effort covariate in the model, to account for uneven survey effort
AIC(fit.pois, fit.pois.sqrt, fit.pois.offset)

##7.-The model chosen using the AIC criteria assumes the relationship between the y-coordinate and the response is nonlinear in nature.
library(MASS)
step_aic_offset <- step(fit.pois.offset, direction = "both")
step_aic_offset
AIC(step_aic_offset)
step_bic_offset <- step(fit.pois.offset, direction = "both", k = log(length(EIA)))
step_bic_offset
BIC(step_bic_offset)

##8.-While the (default) anova function returns the same results as the (default) Anova function, we are using the latter here because it automatically returns p-values.
Anova(step_bic_offset)

##9.- This function investigates the fit of all possible models while the stepwise selection function does not necessarily consider all candidate models.?????
options(na.action = 'na.fail')
dredge(step_bic_offset)

##10.-
summary(step_bic_offset)

 ##11.- ?????
predict(step_bic_offset, newdata = data.frame('tidestate' = 'EBB', 'observationhour' = 10, 'MonthOfYear' = 1, 'Year' = 11, 'x.pos' = -2061, 'y.pos' = -1158, 'area' = mean(area)), type = 'response')
str(step_bic_offset)

##12.-???
summary(step_bic_offset)

##13.-15.1102
stepBIC.pois.offset.OD <- glm(count ~ tidestate + observationhour + MonthOfYear + Year + x.pos + y.pos + Year:x.pos + Year:y.pos, family = quasipoisson, data = EIA, offset = log(area))
summary(stepBIC.pois.offset.OD)

##14.-???
#Based on the p-values in the overdispersed model, both interaction terms would be dropped from the stepBIC.pois.offset.OD model (at 5% significance).
summary(step_bic_offset)
summary(stepBIC.pois.offset.OD)

##15.- True

