library(ggplot2)
library(car)
library(effects)
library(nlme)
library(MuMIn)
library(stats)

setwd("/Users/alvarofernandezdelacigonabarreiro/Documents/R St. Andrews/GLM Practical")

###ANALYSIS OF EIA DATA
EIA_data <- read.csv("EIA.csv")
density <- EIA_data$count/EIA_data$area
EIA_data <- cbind(EIA_data, density = density)
EIA_data$tidestate <- as.factor(EIA_data$tidestate)

###DATA EXPLORATION
# first group the data by gridcodes and find the mean density for each cell
library(dplyr)
newdata <- group_by(EIA_data, GridCode) %>%
  summarise(x.pos = first(x.pos), y.pos = first(y.pos), area = first(area), density = mean(density))

# pick a nice colour scheme
col <- colorRampPalette(rev(rgb(c(231,117,27), c(41,112,158), c(138,179,119), max = 255)))(100)

# plot the data
p <- ggplot(newdata)
p <- p + geom_tile(aes(x = x.pos, y = y.pos, fill = density, height = 1000, width = 1000)) + 
  scale_fill_gradientn(colours = col, space = "Lab", na.value = "grey50", guide = "colourbar")
p + theme_bw() + coord_equal()

##1.- C?all true


###FITTING MULTIPLE COVARIATE LINEAR MODELS

##2.-difference is 10 factors

# month as continuous
fit.full <- lm(density ~ tidestate + observationhour + DayOfMonth + MonthOfYear + impact + Year + x.pos + y.pos, data = EIA_data)
coefficients_mc <- coef(fit.full)
aic_value_mc <- AIC(fit.full)
aic_value_mc
coefficients_mc
# month as a factor
fit.full.fac <- lm(density ~ tidestate + observationhour + DayOfMonth + as.factor(MonthOfYear) + impact + Year + x.pos + y.pos, data = EIA_data)
coefficients_mf <- coef(fit.full.fac)
aic_value_mf
coefficients_mf
#Using AIC comparing both models it can be seen that month as a factor(discrete), which value is 170702.2
#is better than month as continuous, which AIC value is 170706.9. Factor<continuous

##3.-False A
vif(fit.full.fac)
aic_value_mf <- AIC(fit.full.fac)
aic_value_mf



###ADDING INTERACTION TERMS
fit.fullfac.noyr <- update(fit.full.fac, .~. - Year)
fit.fullfac.noimp <- update(fit.full.fac, .~. - impact)

AIC(fit.full, fit.full.fac, fit.fullfac.noimp, fit.fullfac.noyr)
##one of the two:
fit.interac <- update(fit.fullfac.noimp, .~. + Year:x.pos+Year:y.pos) #as a function of year??


###MODEL SELECTION
Anova(fit.interac)

#4.-True

new_fit.interac <- step(fit.interact, direction = "both", trace = 0)
AIC(new_fit.interac)
options(na.action = 'na.fail')
model_set <- dredge(new_fit.interac)
model_set

#5.-

new_fit.interac2 <- step(fit.interact, direction = "both", trace = 0)
BIC(new_fit.interac2)
AIC(new_fit.interac2)
new_fit.interac2

#6.-False                                    ####ADD BIC OR AIC TO STEP FUCNTION; STEPBIC/STEPAIC

Anova(fit.interac)
fit.interac
Anova(fit.full.fac)

#7.- D???

summary(new_fit.interac)
summary(fit.interac)
Anova(new_fit.interac)
anova(fit.interac)
dredge(fit.interac)

#8.-adjusted R-squared

summary(new_fit.interac)

#9.-5.21 (SQUARE IT CUNT)

#10.- B

#11 A???

###GENERALISED LEAST SQUARES MODELLING. 
library(nlme)
EIA_data$density <- as.numeric(EIA_data$density)
EIA_data$sqrtdensity <- sqrt(EIA_data$density)
fit.gls <- gls(sqrtdensity ~ tidestate + observationhour + impact + x.pos + y.pos + MonthOfYear + impact:x.pos + impact:y.pos, data = EIA_data, method = 'ML')
fit.gls.exp <- gls(sqrtdensity ~ tidestate + observationhour + impact + x.pos + y.pos + MonthOfYear + impact:x.pos + impact:y.pos, data = EIA_data, weights = varExp(), method = 'ML')
fit.gls.power <- gls(sqrtdensity ~ tidestate + observationhour + impact + x.pos + y.pos + MonthOfYear + impact:x.pos + impact:y.pos, data = EIA_data, weights = power(), method = 'ML')

#12.- True

plot_mean_variance <- plot(fitted(fit.gls.exp), residuals(fit.gls.exp, type = 'response'))

cut.fit <- cut(fitted(fit.gls.exp), breaks = quantile(fitted(fit.gls.exp), probs = seq(0, 1, length = 20)))
means1 <- tapply(fitted(fit.gls.exp), cut.fit, mean)
vars1<- tapply(residuals(fit.gls.exp),cut.fit,var)
plot_mean_variance

fitted1<- (summary(fit.gls.exp)$sigma^2)*exp(2*coef(fit.gls.exp$model)*means1)
df1<-data.frame(means1,vars1,fitted1)
colnames(df1)=c("Means","Vars","Fitted")

ggplot(df1, aes(x=Means, y=Vars))+
  geom_point()+geom_line(aes(Means,Fitted),color="red") + 
  xlab("Fitted Values") + ylab("Variance of the residuals")

#13.-Upload to Moodle the graph

#14.-C


###DEALING WITH CORRELATED ERRORS

par(mfrow = c(1,2))
acf(residuals(fit.gls.exp, type = 'response'))
acf(residuals(fit.gls.exp, type = 'normalized'))
par(mfrow = c(1,1))


EIA_data$block <- paste(EIA_data$Year, EIA_data$MonthOfYear, EIA_data$DayOfMonth, EIA_data$GridCode, sep = '')
library(dplyr)
EIA2 <- arrange(EIA_data, block, Year, MonthOfYear, DayOfMonth, GridCode)

# AR(1)
corAR1_struct <- corAR1(value = 0, form = ~ 1 | GridCode/DayOfMonth, fixed = FALSE)
AR1_model <- gls(sqrtdensity ~ Year + MonthOfYear + DayOfMonth + GridCode, data = EIA2,
                           correlation=corAR1_struct, method="ML")
AR1_model

# AR(2)
corAR2_struct <- corARMA(p = 2, q = 0, form = ~ 1 | GridCode/DayOfMonth)
AR2_model <-gls(sqrtdensity ~ Year + MonthOfYear + DayOfMonth + GridCode, data = EIA2,
                correlation = corAR2_struct, method = "ML")

AR2_model

AIC(AR1_model)
AIC(AR2_model)

par(mfrow = c(1,2))
acf(residuals(AR1_model, type = 'response'))
acf(residuals(AR1_model, type = 'normalized'))
par(mfrow = c(1,1))

par(mfrow = c(1,2))
acf(residuals(AR2_model, type = 'response'))
acf(residuals(AR2_model, type = 'normalized'))
par(mfrow = c(1,1))

#15.-B

F_test_AR1 <- anova(AR1_model, type = "marginal")
F_test_AR1
F_test_AR2 <- anova(AR2_model, type = "marginal")
F_test_AR2
anova(fit.gls.exp)


#20.-B