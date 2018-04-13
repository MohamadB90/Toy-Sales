library(readxl)
library(ggplot2)
library(reshape2)
library(scales)
library(corrplot)
library(minpack.lm)



# 1) Import the sales data into R
tsd <- read_excel("C:/Users/gborghol/Documents/Training/R practice/toy_sales_data.xlsx")
planned_spend <- read_excel("C:/Users/gborghol/Documents/Training/R practice/toy_sales_data.xlsx","planned_spend")


# 2) Create a plot of sales, TV investment and Digital investment in the y axis with time in
# the x axis

ts <- reshape2::melt(tsd, id.vars="month", 
                      measure.vars=c("sales", "tv_spend", "digital_spend"))

ggplot(ts, aes(x=month, y=value, color=variable)) + geom_line()+ ggtitle("Time Series Data") +labs(x="Month",y="$ Value")+ scale_y_continuous(label=comma)



# 3) Report the correlations among sales, TV and Digital investment

cor.data <- data.frame(tsd$sales,tsd$tv_spend,tsd$digital_spend)

cor(cor.data,use="all.obs",method="pearson")

#sales is positively correlated with both tv and digital spends
#sales seems to be more strongly correlated with digital spend than tv spend (0.66 and 0.44 repectively)

# 4) Fit a regression model to data, using all data points you have available

#To fit the regression,first, a negative exponential transformation needs to be applied to TV and Digital spend variables
#Second, an Adstock needs to be applied to the transformed variables
#Third, a regression would be fitted to the ad-stocked variables in addition to the remaining independent variables (trend + xmas)


#Step 1: Transformation of TV and Digital spends.

#Step 2: Ad Stock TV and Digital


adstock<-function(x,rate=0){
  return(as.numeric(filter(x=x,filter=rate,method="recursive")))
}

AdstockRate<-function(tsd,Sales,tv_spend){
  modFit<-nls(data=Data,Impact~a+b*adstock(Ads,rate),
              start=c(a=1,b=1,rate=0))
  if(summary(modFit)$coefficients[3,1]>0){
    AdstockRate=summary(modfit)$coefficients[3,1]
  }
  else{
    nls.out<-nlsLM(Impact~a+b*adstock(Ads,rate),data=Data,start=list(a=1,b=1,rate=0),
                   lower=c(a=-Inf,b=-Inf,rate=0),upper=c(a=Inf,b=Inf,rate=1)) 
    AdstockRate=summary(nls.out)$coefficients[3,1]
  }
  return(AdstockRate)
}


# Step 3: Fitting the Regression

# Note, a regression was fitted to the original variables in the code below so as to maintain the continuity of
#the remaining tasks considering Step 1 limitations.

model.tsd <- tsd[,c("sales","tv_spend","digital_spend","trend","xmas")]

plot(model.tsd) 


tsd.reg <- lm(sales~digital_spend+tv_spend+trend+xmas, data = model.tsd)

print(tsd.reg)

summary(tsd.reg)

plot(tsd.reg, which = c(1,2))

a <- coef(tsd.reg)[1]
print(a)

Xdigital_spend <- coef(tsd.reg)[2]
Xtv_spend <- coef(tsd.reg)[3]
Xtrend <- coef(tsd.reg)[4]
Xxmas <- coef(tsd.reg)[5]


# residuals versus fits seems evenly distributied.


#Equation : Y = a+Xtv_spend.x1+Xdigital_spen.x2+Xtrend.x3+Xxmas.x4




# a) Report on the adjusted R-squared
# Adjusted R-squared = 0.8984 => 89% of the variation in the dependent variable can be explained by the 
# independent variables that have an effect on the dependent variable.


# b) Report the p-value and significance of each regressor

# p-value of each regressor:
# digital_spend p-value = 1.07e-05 < 0.05 => significant
# tv_spend p-value = 2.58e-06 < 0.05 => significant
# digital_spend p-value = 1.44e-06 < 0.05 => significant
# digital_spend p-value = 0.00711 < 0.05 => significant

#significance of each regressor:

#digital_spend: highest in significance and thus is the independent variable that has the most effect on sales.
#tv_spend: second highest significant variable in affecting sales.
#trend: is significant in its impact on sales but its impact is much lighter than the previous 2.
#xmas: is the least significant variable and differs vastly in impact when compared to the other independent variables.

# 5) Calculate the contribution from TV Spend to sales in % and absolute dollar value

#Tv spend contribution Percentage

tv_cont_perc <- sum(Xtv_spend*tsd$tv_spend)/sum(tsd$sales)
print(tv_cont_perc)


#TV Spend Dollar Value
tv_cont_dollar <- sum(Xtv_spend*tsd$tv_spend)
print(tv_cont_dollar)


# 6) Calculate TV (ROI)

tv_ROI <- sum(Xtv_spend*tsd$tv_spend)/sum(tsd$tv_spend)
print(tv_ROI)

# 7) Calculate expected sales for the first 3 months of 2018.


Jan18_sales <- (Xdigital_spend*planned_spend$digital_spend + Xtv_spend*planned_spend$tv_spend) [1]
print(Jan18_sales)

Feb18_sales <- (Xdigital_spend*planned_spend$digital_spend + Xtv_spend*planned_spend$tv_spend) [2]
print(Feb18_sales)

Mar18_sales <- (Xdigital_spend*planned_spend$digital_spend + Xtv_spend*planned_spend$tv_spend) [3]
print(Mar18_sales)

total_expected_sales <- Jan18_sales+Feb18_sales+Mar18_sales
print(total_expected_sales)


#8) In your opinion what additional data would improve your model and why?

#Competitor Media Spend, Number of Stores and Offers data.
#These variables would likely prove to be significant predictors of sales.This data is to be 
#included to measure the effect of all different marketing inputs combined together on our sales.
