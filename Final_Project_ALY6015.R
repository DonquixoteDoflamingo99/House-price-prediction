#Data preparation for predicting Sale Price
library(ggplot2)
library(dplyr)
library(Hmisc)
library(corrplot)
library(leaps)
library(RColorBrewer)
library(car)
library(MASS)
library(Hmisc)
library(data.table)
library(gridExtra)
library(glmnet)
install.packages("Metrics")
library(Metrics)
library(ISLR)

df<-read.csv(file=file.choose(), header = TRUE)
df
head(df)
summary(df)
str(df)
ls(df)

#Percentage of missing values
nrow(df)
df2 <- as.data.frame(sapply(df, function(x) (sum(is.na(x))*100)/2930)) 
colnames(df2)

df2 = rename(df2, "percentage" = "sapply(df, function(x) (sum(is.na(x)) * 100)/2930)")
df2 <- tibble::rownames_to_column(df2, "variables")
df2 <- subset(df2, percentage!=0)
ggplot(data=df2, aes(x = reorder(variables, -percentage), y = percentage)) +
  geom_bar(stat="identity", color="blue", fill="white")+
  theme(axis.text.x=element_text(angle=45, hjust=1)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Variables") + ylab("Percentage") + ggtitle("Percentage of missing values in the dataset")

#Number of NA in each row.
x <- apply(df, MARGIN = 1, FUN = function(x) length(x[is.na(x)]) )
#apply(df, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]) )
ncol(df)
unique(x)
mean(x)
data_frame(nulls = x) %>%
  ggplot(.,aes(nulls)) +
  geom_histogram(fill="blue", position="dodge")+
  theme_minimal() +
  geom_vline(data=df, aes(xintercept=4.762799),
             linetype="dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) #+
#scale_y_continuous(labels = paste0(c(250,500,750,1000)))



#Data Imputaton

#Electrical

df[df$Electrical== "",]
table(df3$Electrical, useNA = 'always')
df3 <- df

df3 <- df3 %>% mutate(Electrical = ifelse(Electrical == "", "SBrkr", Electrical))

#For non-numerical data imputing mode is a common choice.

#
#Garage Area and Garage Cars
df3[is.na(df3$Garage.Area),]
df3 <- df3 %>% slice(-c(2237))

nrow(df3)

df3[is.na(df3$Garage.Cars),]
table(df3$Garage.Cars)
#Garage area and garage cars had same missing values row.

#For remaining categorical variables in garage, the NA indicates none and the blank values indicates "NA"

garage <- c('Garage.Type', 'Garage.Finish', 'Garage.Qual', 'Garage.Cond')
#df3[ , (garage) := lapply(.SD, nafill, fill=0), .SDcols = garage]


table(df3$Garage.Type)
df3[is.na(df3$Garage.Type),]
table(df3$Sale.Type)

#Garage.Type
df3$Garage.Type[is.na(df3$Garage.Type)] <- 'None'
table(df3$Garage.Type, useNA = 'always')
df3[df3 == ""] <- NA 
write.csv(df3, "check.csv")

#Garage.Finish
df3$Garage.Finish[is.na(df3$Garage.Finish)] <- 'None'
table(df3$Garage.Finish, useNA = 'always')

#Garage.Qual
df3$Garage.Qual[is.na(df3$Garage.Qual)] <- 'None'
table(df3$Garage.Qual, useNA = 'always')

#Garage.Cond
df3$Garage.Cond[is.na(df3$Garage.Cond)] <- 'None'
table(df3$Garage.Cond, useNA = 'always')

#Total.Bsmt.SF, Bsmt.Half.Bath, and Bsmt.Full.Bath
df3[is.na(df3$Total.Bsmt.SF),]
df3[is.na(df3$Bsmt.Full.Bath),]
df3[is.na(df3$Bsmt.Half.Bath),]


df3$Total.Bsmt.SF[is.na(df3$Total.Bsmt.SF)] = 0
df3$Bsmt.Full.Bath[is.na(df3$Bsmt.Full.Bath)] = 0
df3$Bsmt.Half.Bath[is.na(df3$Bsmt.Half.Bath)] = 0


# BsmtFin SF 1, BsmtFin SF 2, and Bsmt Unf SF
df3[is.na(df3$BsmtFin.SF.1),]
df3[is.na(df3$BsmtFin.SF.2),]
df3[is.na(df3$Bsmt.Unf.SF),]

df3$BsmtFin.SF.1[is.na(df3$BsmtFin.SF.1)] = 0
df3$BsmtFin.SF.2[is.na(df3$BsmtFin.SF.2)] = 0
df3$Bsmt.Unf.SF[is.na(df3$Bsmt.Unf.SF)] = 0

#For remaining categorical variables in Basement, the NA indicates none and the blank values indicates "NA"

#Bsmt Qual, 
df3$Bsmt.Qual[is.na(df3$Bsmt.Qual)] <- 'None'
table(df3$Bsmt.Qual, useNA = 'always')



#Bsmt Cond
df3$Bsmt.Cond[is.na(df3$Bsmt.Cond)] <- 'None'
table(df3$Bsmt.Cond, useNA = 'always')



#Bsmt Exposure
df3$Bsmt.Exposure[is.na(df3$Bsmt.Exposure)] <- 'None'
table(df3$Bsmt.Exposure, useNA = 'always')



#BsmtFin Type 1
df3$BsmtFin.Type.1[is.na(df3$BsmtFin.Type.1)] <- 'None'
table(df3$BsmtFin.Type.1, useNA = 'always')

#BsmtFin Type 2
df3$BsmtFin.Type.2[is.na(df3$BsmtFin.Type.2)] <- 'None'
table(df3$BsmtFin.Type.2, useNA = 'always')

#===================================================================================
#Updated missing values
nrow(df3)
df4 <- as.data.frame(sapply(df3, function(x) (sum(is.na(x))*100)/2929)) 
colnames(df4)

df4 = rename(df4, "percentage" = "sapply(df3, function(x) (sum(is.na(x)) * 100)/2929)")
df4 <- tibble::rownames_to_column(df4, "variables")

row_sub = apply(df4, 1, function(row) all(row > 0))
df4 <- df4[row_sub,]
ggplot(data=df4, aes(x = reorder(variables, -percentage), y = percentage)) +
  geom_bar(stat="identity", color="blue", fill="white")+
  theme(axis.text.x=element_text(angle=45, hjust=1)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Variables") + ylab("Percentage") + ggtitle("Percentage of missing values in the dataset")

#========================================================================================

#In Fireplace Qu, The NA values ,mean that there is none

df3$Fireplace.Qu[is.na(df3$Fireplace.Qu)] <- 'None'
table(df3$Fireplace.Qu, useNA = 'always')

#write.csv(df3, "fpp.csv")

#Lot Frontage
table(df3$Lot.Frontage, useNA = 'always')

#Lot Frontage can be related to the Neighbourhood.

#is.numeric(df3$Lot.Frontage)
#df3 <- as.numeric(df3$Lot.Frontage)

#This part is remaining, we will do it in next phase
#Figuring out the relation to impute NAs of lot.frontage

#Mas.Vnr.Type, and Mas.Vnr.Area

df3[is.na(df3$Mas.Vnr.Area),]
df3[df3$Mas.Vnr.Type== "",]
unique(df3$Mas.Vnr.Area)

table(df3$Mas.Vnr.Type, useNA = 'always')



#The rows with missing values are same for both variables

df3$Mas.Vnr.Area[is.na(df3$Mas.Vnr.Area)] = 0


df3[df3$Mas.Vnr.Type== "",] <- 'None'



#===========================================================================
#Answering Questions

#Converting sale price to numeric
summary(df)
nrow(df)
nrow(df3)
vec <- df$SalePrice
typeof(vec)
vec <- vec[-1578]
length(vec)
df3$SalePrice <- vec


#Highest Sale Price
max(df3$SalePrice)


#Highest number of houses were sold in which year
table(df3$Yr.Sold)
year_sold <- table(df3$Yr.Sold) %>% sort
year_sold
display.brewer.all() 
col1 <- scale_color_brewer(palette = "Set2")
barplot(year_sold, 
        col = brewer.pal(5, name = "Purples"),
        xlab = "Year",
        ylab= "Number of houses")

#House style
table(df3$House.Style)
house_style <- table(df3$House.Style) %>% sort
house_style
barplot(house_style, 
        col = brewer.pal(8, name = "OrRd"),
        xlab = "House Style",
        ylab= "Number of houses")


#Overall Quality Rating 
table(df3$Overall.Qual)

#Roofstyle
table(df3$Roof.Style)
roof_style <- table(df3$Roof.Style)
roof_style
roof_styleddf <- data.frame(roof_style)
library(plotly)
names(roof_styleddf) <- c("House_style","Number_of_houses")
fig <- plot_ly(roof_styleddf, x = ~House_style, y = ~Number_of_houses, type = 'scatter', mode = 'lines')

fig

summary(df3)




write.csv(df3,"check2.csv")




#===============================================================================

cor <- cor(df3[sapply(df3, is.numeric)])
cor <- round(cor,3)
corrplot(cor)
upper<-cor
upper[upper.tri(cor)]<-""
upper<-as.data.frame(upper)
upper



#APA Correlation Matrix
#Now we can define a function to return the matrix with the stars appended to significant correlations. The row names are kept with the index appended. The column names are replaced with the index.

#' @param mat an rcorr object or a double matrix
#' @param corrtype is either pearson or spearman. Will be passed into Hmsic::rcorr if mat is not already an rcorr object
#' @return `mat` with stars appended for each level of significants (p < 0.05, p < 0.01, p < 0.001)
apaCorr <- function(mat, corrtype = "pearson") {
  matCorr <- mat
  if (class(matCorr) != "rcorr") {
    matCorr <- rcorr(mat, type = corrtype)
  }
  
  # Add one star for each p < 0.05, 0.01, 0.001
  stars <- apply_if(round(matCorr$r, 2), matCorr$P < 0.05, function(x) paste0(x, "*"))
  stars <- apply_if(stars, matCorr$P < 0.01, function(x) paste0(x, "*"))
  stars <- apply_if(stars, matCorr$P < 0.001, function(x) paste0(x, "*"))
  # Put - on diagonal and blank on upper diagonal
  stars[upper.tri(stars, diag = T)] <- "-"
  stars[upper.tri(stars, diag = F)] <- ""
  n <- length(stars[1,])
  colnames(stars) <- 1:n
  # Remove _ and convert to title case
  row.names(stars) <- tools::toTitleCase(sapply(row.names(stars), gsub, pattern="_", replacement = " "))
  # Add index number to row names
  row.names(stars) <- paste(paste0(1:n,"."), row.names(stars))
  stars
}


dfStars <- apaCorr(as.matrix(df3[sapply(df3, is.numeric)]), corrtype = "pearson")
#kable(dfStars, format = "markdown")
print(dfStars)
write.csv(dfStars, "stars.csv")
#https://stefaneng.github.io/apa_correlation_table/




#============================================================================

#Changes in mean sale price by overall quality

mean.df <- aggregate(SalePrice ~ Overall.Qual, df3, mean)
names(mean.df)[2] <- "Saleprice_Mean"


colnames(df3)

ggplot(mean.df, aes(x=Overall.Qual, y=Saleprice_Mean)) + 
  geom_point()+
  geom_smooth(method=lm)  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Mean changes in Sale Price with Overall Quality") + scale_y_continuous(labels = paste0(c(0,100,200,300,400,500), "K"))
df3$Overall.Cond



#================================================================
df3$Gr.Liv.Area

ggplot(df3, aes(x=Gr.Liv.Area, y=SalePrice)) + 
  geom_point()+
  geom_smooth(method=lm)  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Mean changes in Sale Price with Overall Quality") + scale_y_continuous(labels = paste0(c(0,200,400,600,800), "K"))


#==============================================================================

#Distribution of sale price
ggplot(df3, aes(x=SalePrice)) + 
  geom_density()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Density plot for sale price")+ geom_vline(aes(xintercept=mean(SalePrice)),
                                                     color="blue", linetype="dashed", size=1)+
  scale_x_continuous(labels = paste0(c(0,200,400,600,800), "K"))



#==================================================================
#Correlation table for Sale Price
corelation <- cor(df3$SalePrice)
cor1 <- cor(df3[-1], df3$SalePrice) 
i1 <- sapply(df3, is.numeric)
y1 <- "SalePrice" #change it to actual column name
x1 <- setdiff(names(df3)[i1], y1)
cor2 <- cor(df3[x1], df3[[y1]])
corrplot(cor2)


#===================================================================
ggplot(df3, aes(x=SalePrice, fill=Overall.Qual)) +
  geom_density()+ theme(legend.position="top")
df3$Overall.Qual
df3$Lot.Frontage



ggplot(df3, aes(x = SalePrice)) +
  geom_density(color = 4,
               fill = 4,
               alpha = 0.25)
ggplot(df3, aes(x = SalePrice, colour = Overall.Qual, fill = Overall.Qual)) +
  geom_density()
ggplot(df3, aes(x = SalePrice, colour = Overall.Qual, fill = Overall.Qual)) +
  geom_density() +
  facet_grid(~Overall.Qual) +  scale_x_continuous(labels = paste0(c(0,2,4,6,8))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Density plot for sale price (in 100K)")+ geom_vline(aes(xintercept=mean(SalePrice)),
                                                               color="blue", linetype="dashed", size=1)


#===================================================================================

#SUBSET Regression

#Before removing the outliers
qqPlot(df3$SalePrice, main = "Q-Q Plot for Sale Price before removing the outliers")

out <- boxplot(df3$SalePrice, plot=FALSE)$out
as.vector(out)
length(out)

final <- df3

final = filter(final, !(SalePrice %in% as.vector(out)))
nrow(final)
qqPlot(final$SalePrice, main = "Q-Q Plot for Sale Price after removing the outliers")

str(df3)



rownames(final)<-final$PID
final <- final %>%
  dplyr::select(-PID, -?..Order)
final


str(final)
summary(final)

sapply(lapply(final, unique), length) #count unique values
lapply(df[as.vector(colnames(final))], unique)

final <- final[, !sapply(final, is.character)]

leaps <- regsubsets(SalePrice ~. , nbest = 82, data = final, really.big = T )

reg_summary <- summary(leaps)
names(reg_summary)
reg_summary$outmat
write.csv(reg_summary$outmat, "final.csv")


par(mar = c(1, 1, 1, 1))
# Set up a 2x2 grid so we can look at 4 plots at once
par(mfrow = c(2,2))
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

# We will now plot a red dot to indicate the model with the largest adjusted R^2 statistic.
# The which.max() function can be used to identify the location of the maximum point of a vector
adj_r2_max = which.max(reg_summary$adjr2) # 11

# The points() command works like the plot() command, except that it puts points 
# on a plot that has already been created instead of creating a new plot
points(adj_r2_max, reg_summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)

# We'll do the same for C_p and BIC, this time looking for the models with the SMALLEST statistic
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min = which.min(reg_summary$cp) # 10
points(cp_min, reg_summary$cp[cp_min], col = "red", cex = 2, pch = 20)

plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min = which.min(reg_summary$bic) # 6
points(bic_min, reg_summary$bic[bic_min], col = "red", cex = 2, pch = 20)


reg611 <- lm(SalePrice ~ MS.SubClass + Overall.Qual + Year.Built + Year.Remod.Add + Gr.Liv.Area + Bsmt.Half.Bath + Fireplaces + Garage.Cars, data = final)
summary(reg611)

set.seed(123)
library(caret)
train <- createDataPartition(final$SalePrice, p=0.5, list = FALSE)

trainingData <- final[train,]

testData <- final[-train,]

dim(trainingData)
dim(testData)


perfect.mod <- lm(SalePrice ~ MS.SubClass + Overall.Qual + Year.Built + Year.Remod.Add + Gr.Liv.Area + Bsmt.Half.Bath + Fireplaces + Garage.Cars, data = trainingData)

summary(perfect.mod)

perfect.mod$fitted.values
perfect.mod.R2 <- cor(perfect.mod$fitted.values, trainingData$SalePrice)^2

perfect.mod.R2




perfect.mod.test.pred <- predict(perfect.mod, newdata=testData)

perfect.mod.test.R2 <- cor(perfect.mod.test.pred, testData$SalePrice)^2

perfect.mod.test.R2


Overfitting.perfect.mod <- perfect.mod.R2 - perfect.mod.test.R2

Overfitting.perfect.mod


vif(reg611)
plot(reg611)


plot(leaps, scale = "adjr2", main = "Selecting the best model")
options(max.print=999999)
leaps$lopt




#Lasso Regression

set.seed(123)
trainIndex <- sample(x= nrow(final), size = nrow(final)*0.7)
train <- final[trainIndex,]
test <- final[-trainIndex,]
dim(train)
dim(test)

dim(final)

new_final <- subset (final, select = c (SalePrice, MS.SubClass, Overall.Qual, Year.Built, Year.Remod.Add, Gr.Liv.Area, Bsmt.Half.Bath ,Fireplaces, Garage.Cars))

train_x <- model.matrix(SalePrice~MS.SubClass + Overall.Qual + Year.Built + Year.Remod.Add + Gr.Liv.Area + Bsmt.Half.Bath + Fireplaces + Garage.Cars, train)[,-1] 
test_x <- model.matrix(SalePrice~MS.SubClass + Overall.Qual + Year.Built + Year.Remod.Add + Gr.Liv.Area + Bsmt.Half.Bath + Fireplaces + Garage.Cars, test)[,-1]

train_y <- train$SalePrice
test_y <- test$SalePrice


#Find the best values of lambda

#Find the best lambda value using the cross validation
set.seed(123)
cv.lasso <- cv.glmnet(train_x, train_y, alpha = 1, nfolds = 10)
plot(cv.lasso)

log(cv.lasso$lambda.min)
log(cv.lasso$lambda.1se)

#Fit the regression model for training data using lambda min

model.min <- glmnet(train_x, train_y, alpha = 1, lambda = cv.lasso$lambda.min)
model.min

#Regression coefficients
coef(model.min)


#Fit the regression model for training data using lambda 1se
model.1se <- glmnet(train_x, train_y, alpha = 1, lambda = cv.lasso$lambda.1se)
model.1se

#Regression coefficients
coef(model.1se)
coef(model.min)


#Train set predictions using 1se model 

preds.train.1se <- predict(model.1se, newx = train_x)

train.1se.rmse <- rmse(train_y, preds.train.1se)

# Test set predictions using 1se model 


preds.test.1se <- predict(model.1se, newx = test_x)

test.1se.rmse <- rmse(test_y, preds.test.1se)

#Train rmse > test rmse = Overfitting is there.



#Train set predictions using min model 

preds.train.min <- predict(model.min, newx = train_x)

train.min.rmse <- rmse(train_y, preds.train.min)

# Test set predictions using min model 


preds.test.min <- predict(model.min, newx = test_x)

test.min.rmse <- rmse(test_y, preds.test.min)

eval_results <- function(true, predicted, new_final) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(new_final))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

eval_results(train_y,preds.train.min, new_final)


set.seed(123)

#glmnet function 
cv.ridge = cv.glmnet(train_x,train_y,nfolds = 10)
# cv.ridge is an object of class glmnet that contains all the relevant information of the fitted model for further use.

# lambda.min
cv.ridge$lambda.min

# lambda.1se
cv.ridge$lambda.1se

# extracting all of the fitted models 
summary(cv.ridge)


#Plot the results from the cv.glmnet function provide an interpretation.

# We can visualize the coefficients by executing the plot method
plot(cv.ridge)


# Fit a Ridge regression model against the training set and report on the coefficients.
# Fit the model on training data using lambda with family as Gaussian as for the ridge we will use Gaussian distribution
lambdas <- 10^seq(2, -3, by = -.1)
model.ridge = glmnet(train_x, train_y, alpha = 0, family = 'gaussian', lambda = lambdas)  
model.ridge

summary(model.ridge)

# After the model has tried with different lambdas, we will see the optimal lambda by using below formula
cv_ridge = cv.glmnet(train_x, train_y, alpha = 0, lambda = lambdas)
optimal_lambda = cv_ridge$lambda.min
optimal_lambda

# Display regression coefficients
coef(model.ridge)

# Display coefficients of ols model with no regularization
ols = lm(SalePrice ~.,data = new_final)
coef(ols)

# Determine the performance of the fit model against the training set by calculating the root mean square error.

# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, new_final) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(new_final))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}
eval_results(train_y,preds.train.min, new_final)

# Prediction and evaluation on train data
predictions_train <- predict(model.ridge, s = optimal_lambda, newx = train_x)
eval_results(train_y, predictions_train, train)

# Determine the performance of the fit model against the test set by calculating the root mean square error (RMSE). Is your model overfit?

# Prediction and evaluation on test data
predictions_test <- predict(model.ridge, s = optimal_lambda, newx = test_x)
eval_results(test_y, predictions_test, test)

# CompARISON
## Perform step wise selection and then fit a model.

fit1 = lm(SalePrice ~., data = new_final)

stepAIC(fit1, direction = "both")

summary(fit1)
