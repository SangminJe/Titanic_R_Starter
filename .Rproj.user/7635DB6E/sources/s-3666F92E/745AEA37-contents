data <- iris

summary(iris)
str(iris)

# Generate Missing Values
install.packages('missForest') 
library(missForest)

iris.mis <- prodNA(iris, noNA=0.1)

# Check NAs
summary(iris.mis)

# iris.mis delete categorical varable(Species)
iris.mis <- subset(iris.mis, select = -c(Species))

# install MICE
install.packages('mice')
library(mice)
library(VIM)

# missing data matrix
md.pattern(iris.mis)


# VIM missig data matrix - better version of md.pattern
aggr(iris.mis, col=c('navyblue','yellow'),
     prop=F, numbers = T, sortVars = T,
     labels=names(iris.mis), cex.axis=.6,
     gap = 3, ylab=c('Missin Data', 'Pattern')
     )

imputed_data <- mice(iris.mis, m=5, maxit=50, method='pmm', seed = 500)
summary(imputed_data)

# Check imputed data
imputed_data$imp$Sepal.Width

# Get complete data
completeData <- complete(imputed_data,5)

# Build Predictive model
fit <- with(data = iris.mis, exp = lm(Sepal.Width ~ Sepal.Length + Petal.Width))

# combine results of 5 models
combine <- pool(fit)
summary(combine)


