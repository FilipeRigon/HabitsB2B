library(plm)
library(stargazer)
library(readr)

# Carregar dados dos paineis

data <- read.csv2(file= "C:/base.csv")

E <- pdata.frame(data, index=c("clientes","semiannual"), drop.index=TRUE, row.names=TRUE)

View(data)

View(E)

punbalancedness(E) # Teste para checagem de quão próximo é de um painél balanced

# Tabela Descritiva dos dados

install.packages("sjPlot")

library(sjPlot)

sjt.itemanalysis(E)

sjt.itemanalysis(E, factor.groups = NULL,
                 factor.groups.titles = "auto", scale = FALSE,
                 min.valid.rowmean = 2, alternate.rows = TRUE, sort.column = NULL,
                 show.shapiro = TRUE, show.kurtosis = TRUE,
                 show.corr.matrix = TRUE, CSS = NULL, encoding = NULL,
                 file = NULL, use.viewer = TRUE, remove.spaces = TRUE)

library(ggplot2)

stargazer(E, type = "html", out="C:/descriptive.html")

# Matriz de Correlação

correlation.matrix1 <- cor(E[,c("PurchaseHabit","PromotionHabit","CashFlowLevel","CashFlowVolatility")])

correlation.matrix2 <- cor(E[,c("PurchaseHabit","PromotionHabit","CashFlowLevel","CashFlowVolatility","SalesForce","InternetSearch","Housing")])

library(corrplot)

corrplot(E$PurchaseHabit, method="number")


stargazer(correlation.matrix1, title="Correlation Matrix",type = "html", out="C:/Users/Filipe/Dropbox/correlation1.html")

stargazer(correlation.matrix2, title="Correlation Matrix",type = "html", out="C:/Users/Filipe/Dropbox/correlation2.html")


# Estimadores para Log Cash Flow Level E Volatility

estimador.fe.level <- plm(log(CashFlowLevel)~PurchaseHabit+PromotionHabit+SalesForce+Seasonality+Housing+InternetSearch, data = E, model = "within")

estimador.re.level <- plm(log(CashFlowLevel)~PurchaseHabit+PromotionHabit+SalesForce+Seasonality+Housing+InternetSearch, data = E, model = "random")

estimador.fe.volatility <- plm(CashFlowVolatility~PurchaseHabit+PromotionHabit+SalesForce+Seasonality+Housing+InternetSearch, data = E, model = "within")
summary(estimador.fe.volatility)

estimador.re.volatility <- plm(CashFlowVolatility~PurchaseHabit+PromotionHabit+SalesForce+Seasonality+Housing+InternetSearch, data = E, model = "random")
summary(estimador.re.volatility)

## Resultados dos estimadores

summary(estimador.fe.level)

summary(estimador.re.level)

summary(estimador.fe.volatility)

summary(estimador.re.volatility)


# Tabelas de Resultados no Stargazer 

install.packages("stargazer")

library("stargazer")

library(sandwich)


## Tabela dos Estimadores

stargazer(estimador.fe.level, estimador.re.level, type="html",
          column.labels = c("Fixed-Effect, Random-Effect"),
          column.separate = c(2, 2),
          dep.var.labels = "Log of cash flow level",
          covariate.labels = c("Purchase Habit", "Promotion Habit",
                               "Sales Force", "Seasonality",
                               "Housing","Internet Search"),
          title = "Cash Flow Level",
          label = "tab:cashflowlevel",
          no.space = TRUE,
          out="C:/tabela1.html"
)

stargazer(estimador.fe.volatility, estimador.re.volatility, type="html",
          column.labels = c("Fixed-Effect, Random-Effect"),
          column.separate = c(2, 2),
          dep.var.labels = "Cash Flow Volatility",
          covariate.labels = c("Purchase Habit", "Promotion Habit",
                               "Sales Force", "Seasonality",
                               "Housing","Internet Search"),
          title = "Cash Flow Volatility",
          label = "tab:cashflowVolatility",
          no.space = TRUE,
          out="C:/tabela2.html"
)

dor.fe.level,cluster = c("cluster_id")))[, 2]), type = "text")

# Testes

# Tests on Individual and/or Time Effects

# Test F
# A F test for the presence of individual effects is implemented in the function pFtest, which
# compares the nestedmodels ols and within.

estimador.pooling.level <- plm(log(CashFlowLevel)~PurchaseHabit+PromotionHabit+SalesForce+Seasonality+Housing+InternetSearch, data = E, model = "pooling", index = "clientes")

estimador.pooling.volatility <- plm(CashFlowVolatility~PurchaseHabit+PromotionHabit+SalesForce+Seasonality+Housing+InternetSearch, data = E, model = "pooling", index = "clientes")


pFtest(estimador.fe.level, estimador.pooling.level)

pFtest(estimador.fe.volatility, estimador.pooling.volatility)

# The Breusch and Pagan (1980) test, versao corrigida para a questão da distribuição
# normal - Honda (1985)
# Lagrange Multiplier Test - (Honda) for unbalanced panels

plmtest(estimador.pooling.level)

plmtest(estimador.pooling.volatility)


# Tests for Correlated Effects

# . E(X*Ci) = 0: the individual effects are not correlated with the explanatory variables; in this
# case, both estimators are consistent, but the random effects estimator is more efficient than
# the fixed effects.
# . E(X*Ci)??? 0: the individual effects are correlated with the explanatory variables; in this case,
# the fixed effects estimator, which estimates out the individual effects, is consistent. On the
# contrary, the random effects estimator is inconsistent because one component of the composite
# error, the individual effect, is correlated with the explanatory variables.

# Hausman Test

phtest(estimador.fe.level,estimador.re.level)

phtest(estimador.fe.volatility,estimador.re.volatility)


# Versões robustas do teste de Hausman

#### The formula method, if method="chisq" (default), computes the original version of the test based 
### on a quadratic form; if method="aux" then the auxiliary-regression-based version in Wooldridge (2010, Sec. 10.7.3.) is computed instead.

###  https://rdrr.io/rforge/plm/man/phtest.html

form <- E$log(CashFlowLevel)~E$PurchaseHabit+E$PromotionHabit+E$SalesForce+E$Seasonality+E$Housing+E$InternetSearch

phtest(estimador.fe.level,estimador.re.level)

phtest(estimador.fe.volatility,estimador.re.volatility)

phtest(form, data = E)
phtest(estimador.pooling, data = E, method = "aux")

# robust Hausman test (regression-based)
phtest(estimador.fe.level,estimador.re.level, method = "aux", vcov = vcovHC)

# robust Hausman test with vcov supplied as a
# function and additional parameters
phtest(estimador.fe.level,estimador.re.level, method = "aux",
       vcov = function(x) vcovHC(x, method="white2", type="HC3"))

##### Testes para serial correlation

## Teste do Wooldridge, 2010 pg 299

## The unobserved effects test à la Wooldridge (seeWooldridge, 2010, 10.4.4), is a semi-parametric
# test for the null hypothesis that sigma^2 C = 0, i.e., that there are no unobserved effects in the residuals.

# The AR(1) test is valid because the errors vit are 
# serially uncorrelated under the null H0 : sigma^2 c (and we are assuming that Xit is
# strictly exogenous).Under the null hypothesis that the vit are serially
# uncorrelated, this statistic is distributed asymptotically as standard normal.


fm1 <- log(CashFlowLevel) ~ PurchaseHabit + PromotionHabit + SalesForce + Seasonality + Housing + InternetSearch
pwtest(fm1, E)

fm2 <- CashFlowVolatility ~ PurchaseHabit + PromotionHabit + SalesForce + Seasonality + Housing + InternetSearch
pwtest(fm2, E)

## The null hypothesis of no unobserved effects is rejected.


## Teste para autocorrelação adaptado para o modelo de Efeitos Fixos com T pequeno

## Testes serial correlation tests for fixed effects model 
### Wooldridge's within-based serial correlation pág. 102

pwartest(estimador.fe.level)
pwartest(estimador.fe.volatility)

pwartest(log(CashFlowLevel)~PurchaseHabit+PromotionHabit+SalesForce+Seasonality+Housing+InternetSearch, data = E, type = "HC3")
pwartest(CashFlowVolatility~PurchaseHabit+PromotionHabit+SalesForce+Seasonality+Housing+InternetSearch, data = E, type = "HC3")


## Teste para time effect

plmtest(estimador.pl.level, effect = "twoways", type = "kw")
plmtest(estimador.pl.volatility, effect = "twoways", type = "kw")

plmtest(estimador.pl.level, effect = "time")
plmtest(estimador.pl.volatility, effect = "time")

plmtest(estimador.pl.level, effect = "twoways", type = "ghm")
plmtest(estimador.pl.volatility, effect = "twoways", type = "ghm")


### Residuos

plot(E$CashFlowLevel)

library(ggplot2)

ggplot(E, aes(x=CashFlowLevel)) + geom_density()

ggplot(E, aes(x=CashFlowLevel)) + geom_histogram(binwidth=.5)


# Exploring Panel Data - Gráficos

devtools::install_github("joachim-gassen/ExPanDaR")
library(ExPanDaR)

ExPanD()

# Carregar dados dos grupos de hábitos

groups_habitos <- read_csv("C:/groupshabits.csv")

View(groups_habitos)

head(groups_habitos)

head(E)

head(data)

head(attr(E, "index"))

summary(E)

qqnorm(E$CashFlowLevel)
qqnorm(E$CashFlowVolatility)
qqnorm(E$PurchaseHabit)
qqnorm(E$PromotionHabit)
qqplot(E$CashFlowLevel, E$CashFlowVolatility)
qqplot(log(E$CashFlowLevel), E$CashFlowVolatility)

H$InternetSearch

library(ExPanDaR)
graph <- prepare_trend_graph(basefinal[c("Semiannual", "PurchaseHabit", "PromotionHabit")], "Semiannual")


x <- E$PurchaseHabit 
h<-hist(x, breaks=125, col="red", xlab="Purchase Habit Distribution", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)



x <- E$PromotionHabit 
h<-hist(x, breaks=125, col="red", xlab="Purchase Habit Distribution", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)


hist(E$PurchaseHabit, breaks=12, col="red", main = paste("Histogram of" , "Purchase Habit"), xlab = "Purchase Habit")
hist(E$PromotionHabit, breaks=12, col="red", main = paste("Histogram of" , "Promotion Habit"), xlab = "Promotion Habit")



## Gráficos

library(readxl)
groups_habitos <- read_excel("C:/groupshabits.xlsx")
View(groups_habitos)


library(ggplot2)

# Purchase vs Cash Flow Level

bp <- ggplot(groups_habitos, aes(x=meanpurchase, y=log(CashFlowLevel), color=GroupPurchase)) + geom_point(shape=1) + xlab("Mean Purchase Habit") +
  ylab("Log of Cash Flow Level")

bp + scale_color_discrete(name="Purchase Habit",
                          breaks=c("LowPurchase","AvgPurchase","HighPurchase"),
                          labels=c("Low","Med", "High"))

# Purchase vs Cash Flow Volatility

bp1 <- ggplot(groups_habitos, aes(x=meanpurchase, y=CashFlowVolatility, color=GroupPurchase)) + geom_point(shape=1) + xlab("Mean Purchase Habit") +
  ylab("Cash Flow Volatility")

bp1 + scale_color_discrete(name="Purchase Habit",
                           breaks=c("LowPurchase","AvgPurchase","HighPurchase"),
                           labels=c("Low","Med", "High"))


# Promotion vs Cash Flow Volatility

bp1 <- ggplot(groups_habitos,aes(x=meanpromo, y=CashFlowVolatility, color=GroupPromotion)) + geom_point(shape=1) + xlab("Mean Promotion Habit") +
  ylab("Cash Flow Volatility")

bp1 + scale_color_discrete(name="Promotion Habit",
                           breaks=c("NoPromotion","LowPromotion","AvgPromotion","HighPromotion"),
                           labels=c("Ordinary Order","Low","Med", "High"))


# Matriz de Correlação CLV E HABITOS

library(readxl)

clvgrupos <- read_excel("groupshabits.xlsx")

View(clvgrupos)

correlation.matrix3 <- cor(clvgrupos[,c("CLV","meanpromotionhabit","meanpurchasehabit")])

library(corrplot)

stargazer(correlation.matrix3, title="Correlation Matrix",type = "html", out="C:/Users/Filipe/Dropbox/correlation3.html")