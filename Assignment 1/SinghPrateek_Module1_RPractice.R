# ALY6015 Module 1 R Practice: Singh Prateek ------------------------------------------------
#----------------- Author: Prateek Singh
#----------------- Submission Date: 18th Jan, 2022
#----------------- Tutor: Jiyoung Yun


# Step: Installing New Libraries ------------------------------------------------

install.packages("RColorBrewer")
install.packages("leaps")


# Step: Importing Libraries ------------------------------------------------

library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(car)
library(psych)
library(ggpubr)
library(leaps)
library(MASS)


# Step: Load the Dataset and display summary ------------------------------------------------

Ames <- read.csv('AmesHousing.csv', header = TRUE)

Ames <- Ames %>% rename(Order = ï..Order, '1st.Flr.SF' = X1st.Flr.SF, '2nd.Flr.SF' = X2nd.Flr.SF)

str(Ames)

summary(Ames)

describe(Ames)


# Step: Checking normality of the variables and check if there are any outliers ------------------------------------------------

hist(Ames$SalePrice, 
     xlab = "Property Sales Price ($)",                                         # Changing X-Axis Label
     main = "Plot 1: Sales Price of AMES Housing Property",                     # Adding Title to the Scatter Plot
     col.main = "Brown",                                                        # Changing Color of the Title
     col.lab = "dark blue",                                                     # Color of the X and Y Axis labels
     col= "cadetblue")                                                          # Color of the Histogram/Frequency Plot
     

axis(1,
     col = "blue",                                                              # Axis line color
     col.ticks = "green",                                                       # Ticks color
     col.axis = "red")                                                          # Labels color

axis(2,
     col = "blue",                                                              # Axis line color
     col.ticks = "green",                                                       # Ticks color
     col.axis = "red")                                                          # Labels color


describe(Ames$SalePrice)


ggboxplot(Ames$Gr.Liv.Area, 
          ylab = "Ground Living Area (Sq. Feet)", 
          xlab = FALSE, 
          color = "cyan4", 
          title = "Plot 2: Ground Living Area Boxplot",
          bxp.errorbar = TRUE,
          notch = TRUE,
          fill = "#ff355e",
          ggtheme = theme_minimal())


describe(Ames$Gr.Liv.Area)


# Step: Imputing missing values with the variable's mean value ------------------------------------------------

colSums(is.na(Ames))                                                            # To find NA values in dataset

Ames$Lot.Frontage[is.na(Ames$Lot.Frontage)] <- mean(Ames$Lot.Frontage,na.rm=TRUE)
Ames$BsmtFin.SF.1[is.na(Ames$BsmtFin.SF.1)] <- mean(Ames$BsmtFin.SF.1,na.rm=TRUE)
Ames$BsmtFin.SF.2[is.na(Ames$BsmtFin.SF.2)] <- mean(Ames$BsmtFin.SF.2,na.rm=TRUE)
Ames$Bsmt.Unf.SF[is.na(Ames$Bsmt.Unf.SF)] <- mean(Ames$Bsmt.Unf.SF,na.rm=TRUE)
Ames$Total.Bsmt.SF[is.na(Ames$Total.Bsmt.SF)] <- mean(Ames$Total.Bsmt.SF,na.rm=TRUE)
Ames$Bsmt.Full.Bath[is.na(Ames$Bsmt.Full.Bath)] <- mean(Ames$Bsmt.Full.Bath,na.rm=TRUE)
Ames$Bsmt.Half.Bath[is.na(Ames$Bsmt.Half.Bath)] <- mean(Ames$Bsmt.Half.Bath,na.rm=TRUE)
Ames$Garage.Yr.Blt[is.na(Ames$Garage.Yr.Blt)] <- mean(Ames$Garage.Yr.Blt,na.rm=TRUE)
Ames$Garage.Cars[is.na(Ames$Garage.Cars)] <- mean(Ames$Garage.Cars,na.rm=TRUE)
Ames$Garage.Area[is.na(Ames$Garage.Area)] <- mean(Ames$Garage.Area,na.rm=TRUE)
Ames$Mas.Vnr.Area[is.na(Ames$Mas.Vnr.Area)] <- mean(Ames$Mas.Vnr.Area,na.rm=TRUE)


# Step: Producing a correlation matrix of the numeric values ------------------------------------------------

options(max.print=1000000)

cor(Ames[sapply(Ames,is.numeric)])

cor(Ames[, unlist(lapply(Ames, is.numeric))])


# Creating a DF to have only required and effective numerical variables

Ames_Cor <- data.frame(Ames$SalePrice, 
                   Ames$Lot.Frontage, 
                   Ames$Lot.Area,
                   Ames$Overall.Qual,
                   Ames$Overall.Cond,
                   Ames$Mas.Vnr.Area,
                   Ames$Total.Bsmt.SF,
                   Ames$Gr.Liv.Area, 
                   Ames$Garage.Area)

Ames_Cor <- cor(Ames_Cor)                                                       # Converting DF to Correlation Matrix


# Step: Producing a Correlation Matrix Plot ------------------------------------------------

corrplot(Ames_Cor, 
         order = "hclust", 
         hclust.method = "single", 
         method =  "shade", 
         addCoef.col = "Dark orange",
         title = "Plot 3: Correlation Matrix for AMES Housing with respect to Sales Price",
         mar = c(1,1,2,1))


# Step: Plotting scatter plot for the continuous variable with the highest, lowest and close to 0.50 correlation with SalePrice ------------------------------------------------


# Highest Correlation with Sales Price

scatterplot(SalePrice ~ Gr.Liv.Area, 
            data = Ames, 
            axes = FALSE,
            main = "Plot 4: Scatterplot for the Highest Correlation with Sales Price",
            xlab = "Ground Floor Living Area (Sq. Feet)", 
            ylab = "Sales Price ($)", 
            col.lab = "dark blue",
            regLine = list(method = lm, lty = 4, lwd = 2, col = "Red"), 
            grid = FALSE, 
            smooth = TRUE,
            cex = 0.75,
            cex.main = 1.25,
            cex.lab = 1,
            col.main = "Brown")

axis(1,
     col = "blue",                                                              
     col.ticks = "green",                                                       
     col.axis = "red",
     cex.axis = 0.75)                                                          

axis(2,
     col = "blue",                                                              
     col.ticks = "green",                                                       
     col.axis = "red",
     cex.axis = 0.75)   

# Lowest Correlation with Sales Price

scatterplot(SalePrice ~ Lot.Area, 
            data = Ames, 
            axes = FALSE,
            main = "Plot 5: Scatterplot for the Lowest Correlation with Sales Price",
            xlab = "Lot Area Size (Sq. Feet)", 
            ylab = "Sales Price ($)", 
            col.lab = "dark blue",
            regLine = list(method = lm, lty = 4, lwd = 2, col = "Red"), 
            grid = FALSE, 
            smooth = TRUE,
            cex = 0.75,
            cex.main = 1.25,
            cex.lab = 1,
            col.main = "Brown")

axis(1,
     col = "blue",                                                              
     col.ticks = "green",                                                       
     col.axis = "red",
     cex.axis = 0.75)                                                          

axis(2,
     col = "blue",                                                              
     col.ticks = "green",                                                       
     col.axis = "red",
     cex.axis = 0.75) 

# ~ 50% Correlation with Sales Price

scatterplot(SalePrice ~ Mas.Vnr.Area, 
            data = Ames, 
            axes = FALSE,
            main = "Plot 6: Scatterplot for around 50% Correlation with Sales Price",
            xlab = "Masonry Veneer Area (Sq. Feet)", 
            ylab = "Sales Price ($)", 
            col.lab = "dark blue",
            regLine = list(method = lm, lty = 4, lwd = 2, col = "Red"), 
            grid = FALSE, 
            smooth = TRUE,
            cex = 0.75,
            cex.main = 1.25,
            cex.lab = 1,
            col.main = "Brown")

axis(1,
     col = "blue",                                                              
     col.ticks = "green",                                                       
     col.axis = "red",
     cex.axis = 0.75)                                                          

axis(2,
     col = "blue",                                                              
     col.ticks = "green",                                                       
     col.axis = "red",
     cex.axis = 0.75) 


# Step: Fitting a regression model using continuous variable ------------------------------------------------

Ames_Reg_Model <- lm(formula = SalePrice ~ Overall.Cond +
          Mas.Vnr.Area +
          Total.Bsmt.SF +
          Gr.Liv.Area +
          Garage.Area, data = Ames)

summary(Ames_Reg_Model)

AIC(Ames_Reg_Model)
BIC(Ames_Reg_Model)


# Step: Plotting the regression model and interpreting the graphs produced ------------------------------------------------

par(mfrow = c(2,2))
plot(Ames_Reg_Model)

mtext("Plot 7: AMES Housing Regression Model", side=3, line = -2, outer=TRUE, col = "Brown", cex = 1.5)

dev.off()


# Step: Checking model for multi-collinearity ------------------------------------------------

vif(Ames_Reg_Model)


# Step: Checking model for outliers ------------------------------------------------

outlierTest(model = Ames_Reg_Model)

# High leverage observation

hat.plot <- function(Ames_Reg_Model) {
        p <- length(coefficients(Ames_Reg_Model))
        n <- length(fitted(Ames_Reg_Model))
        plot(hatvalues(Ames_Reg_Model), main = "Hat Values Indexing")
        abline(h = c(2,3)*p/n, col = "Dark Orange", lty = 4)
        identify(1:n, hatvalues(Ames_Reg_Model), names(hatvalues(Ames_Reg_Model)))
}
hat.plot(Ames_Reg_Model)

# Influential Observations

cutoff <- 4/(nrow(Ames) - length(Ames_Reg_Model$coefficients) - 2)
plot(Ames_Reg_Model, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "Dark Orange")


# Step: Removing outliers ------------------------------------------------

Ames_Corrected <- Ames[-c(1499, 2181, 2182),]
Ames_Corrected

# Running Regression Model post removing outliers

Ames_Corrected_Reg_Model <- lm(formula = SalePrice ~ Overall.Cond +
                             Mas.Vnr.Area +
                             Total.Bsmt.SF +
                             Gr.Liv.Area +
                             Garage.Area, data = Ames_Corrected)

summary(Ames_Corrected_Reg_Model)


# Step: Using the all subsets regression method to identify the “best” model. ------------------------------------------------

Ames_Corrected <- data.frame(Ames_Corrected$SalePrice, 
                             Ames_Corrected$Overall.Cond, 
                             Ames_Corrected$Mas.Vnr.Area, 
                             Ames_Corrected$Total.Bsmt.SF, 
                             Ames_Corrected$Gr.Liv.Area, 
                             Ames_Corrected$Garage.Area)

colnames(Ames_Corrected)[1] <- "SalePrice"
colnames(Ames_Corrected)[2] <- "Overall.Cond"
colnames(Ames_Corrected)[3] <- "Mas.Vnr.Area"
colnames(Ames_Corrected)[4] <- "Total.Bsmt.SF"
colnames(Ames_Corrected)[5] <- "Gr.Liv.Area"
colnames(Ames_Corrected)[6] <- "Garage.Area"

Best_Model <- regsubsets(SalePrice ~ ., data = Ames_Corrected, nbest = 5)

summary(Best_Model)

Best_Model_Summary <- summary(Best_Model)

data.frame(
        Adj.R2 = which.max(Best_Model_Summary$adjr2),
        CP = which.min(Best_Model_Summary$cp),
        BIC = which.min(Best_Model_Summary$bic)
)



Ames_BestFit_Reg_Model <- lm(formula = SalePrice ~ Total.Bsmt.SF +
                                       Gr.Liv.Area +
                                       Garage.Area, data = Ames_Corrected)

Ames_BestFit_Reg_Model <- regsubsets(SalePrice ~ Total.Bsmt.SF +
                                             Gr.Liv.Area +
                                             Garage.Area, data = Ames_Corrected, nbest = 5)

summary(Ames_BestFit_Reg_Model)

Ames_BestFit_Reg_Model_Summary <- summary(Ames_BestFit_Reg_Model)

data.frame(
        Adj.R2 = which.max(Ames_BestFit_Reg_Model_Summary$adjr2),
        CP = which.min(Ames_BestFit_Reg_Model_Summary$cp),
        BIC = which.min(Ames_BestFit_Reg_Model_Summary$bic)
)

