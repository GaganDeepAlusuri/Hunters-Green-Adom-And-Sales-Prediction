rm(list=ls())
library(rio)
library(dplyr)
hg.df  = import("HuntersGreenHomeSales.xlsx", sheet = 'Data')
colnames(hg.df) = tolower(make.names(colnames(hg.df)))

str(hg.df)

#do not need
#slnoskm, status,bathsfull,bathshalf,spa,subdivn,pendingdate,lppersqft, splsale,cdom_cumuldaysmls,

reduced.hg.df = hg.df %>% select(-c('slnoskm','status', 
                                    'bathsfull', 'bathshalf', 'spa', 'subdivn', 'lppersqft', 'sppersqft'))

str(reduced.hg.df)
reduced.hg.df$address = gsub("^[^a-zA-Z]*", "", reduced.hg.df$address) #Removing the numbers before drive names

#treating NAs
sum(is.na(reduced.hg.df)) #4 NA belonging to garages
sum(is.na(reduced.hg.df$garages))
#table(reduced.hg.df$bathstotal, reduced.hg.df$garages)
#reduced.hg.df[which(is.na(reduced.hg.df$garages)) ,]
#removing those 4 rows
reduced.hg.df = na.omit(reduced.hg.df)

#Converting relevant variables to factors.
reduced.hg.df$splsale = as.factor(reduced.hg.df$splsale)
reduced.hg.df$splsale = relevel(reduced.hg.df$splsale, "None")
#looking at roofs
#We need Shingle and Tile
unique(reduced.hg.df$roof)
reduced.hg.df$roof = gsub("Slate, Tile", "Tile", reduced.hg.df$roof)
reduced.hg.df$roof = gsub("Shake, Shingle", "Shingle", reduced.hg.df$roof)
reduced.hg.df$roof = gsub("Slate", "Tile", reduced.hg.df$roof)
reduced.hg.df$roof = gsub("Shingle, Tile", "Shingle", reduced.hg.df$roof)
reduced.hg.df$roof = gsub("Concrete, Tile", "Tile", reduced.hg.df$roof)

reduced.hg.df = reduced.hg.df %>% filter(!(roof %in% c( "Built-Up", "Other" )))
reduced.hg.df$garages = as.factor(reduced.hg.df$garages)

#Creating a price category
# reduced.hg.df$price_category = cut(reduced.hg.df$listprice, breaks = c(0, 330000, 500000, Inf),labels = c("affordable", "mid-range", "costly"), include.lowest = TRUE)
#Creating Age variable
reduced.hg.df$age = as.integer(substr(reduced.hg.df$pendingdate, 1, 4)) - reduced.hg.df$yrblt 
#encoding shingle - 1, tile - 2
reduced.hg.df$roof = ifelse(reduced.hg.df$roof == "Shingle", 1, 2)
reduced.hg.df$roof = as.factor(reduced.hg.df$roof)
#encoding pool:  Private-1, Community, Private,Community - 0
reduced.hg.df$pool_binary = ifelse(reduced.hg.df$pool == "Private", 1, 0)
reduced.hg.df$pool_binary = as.factor(reduced.hg.df$pool_binary)


#-----------------------------------------------------------------------------------------------------------------------#

#adom regression

#slicing

#reduced.hg.df = slice(reduced.hg.df, c(1:470, 483:500))

#

reduced.hg.df = reduced.hg.df %>% rename(adom = adom_agentdaysonmarket) #renaming adom_agentsdaysonmarket


#Ashton park way is the base.
#backward.test=step(lm(adom~.,data=reduced.hg.df,direction=c("forward")))

adom1.out = lm(adom ~ listprice + age + sqft + beds + bathstotal + pool_binary + splsale, data = reduced.hg.df)
summary(adom1.out)

# High correlation between beds-bathstotal, sqft-beds, listprice-sqft
adom2.out = lm(adom ~ listprice  + age + pool_binary + splsale, data = reduced.hg.df)
summary(adom2.out)



#removing rows having adom 0
#reduced.hg.df.filtered <- reduced.hg.df %>%filter(adom != 0)
adom3.out = lm(adom ~ listprice + age+ splsale, data = reduced.hg.df)
summary(adom3.out)

#stargazer
library(stargazer)
stargazer(adom1.out, adom2.out, adom3.out, title="Models for ADOM", align=TRUE, out = "adomz.html")

#----------------------LINE----------------------------------------------------------------#
par(mfrow = c(2, 2))
#L
plot(adom3.out, 1)

#N
hist(adom3.out$residuals, col = 'red')
qqnorm(adom3.out$residuals)
qqline(adom3.out$residuals, col = 'red', lwd=2)

#Equality of Variances
plot(reduced.hg.df$adom,rstandard(adom3.out),pch=19,main="Adom Residual Plot", ylim=c(-4,4))
abline(0,0,col="red",lwd=3)

#Checking Muliticollinearity 
df_numeric <- reduced.hg.df %>% select_if(is.numeric)
library(psych)
corPlot(df_numeric, upper = FALSE)

#vif
car::vif(adom3.out)




plot(reduced.hg.df$adom, reduced.hg.df$lotsqft)

#modelling pricesold
reduced.hg.df = reduced.hg.df %>% rename(cdom = cdom_cumuldaysmls)

price_df = reduced.hg.df[, c("pricesold", "lotsqft", "cdom", "age", "garages","beds","bathstotal", "roof", "pool_binary", "address")]


df_numeric <- price_df %>% select_if(is.numeric)
corPlot(df_numeric, upper = FALSE)

price1.out = lm(pricesold ~ lotsqft + cdom + age + beds+ garages + roof +  bathstotal + splsale + pool_binary, data = reduced.hg.df)
summary(price1.out)

#high correlation between- beds-bathstotal
price2.out = lm(pricesold ~ lotsqft + cdom + age + beds + garages + roof + splsale + pool_binary , data = reduced.hg.df)
summary(price2.out)

price3.out = lm(log(pricesold) ~ lotsqft + cdom + age + beds + garages + roof + splsale + pool_binary , data = reduced.hg.df)
summary(price3.out)

#----------------------LINE----------------------------------------------------------------#
par(mfrow = c(2, 2))
#L
plot(price3.out, 1)

#N
hist(price3.out$residuals, col = 'red')
qqnorm(price3.out$residuals)
qqline(price3.out$residuals, col = 'red', lwd=2)

#Equality of Variances
plot(reduced.hg.df$pricesold,rstandard(price3.out),pch=19,main="Price Residual Plot", ylim=c(-4,4))
abline(0,0,col="red",lwd=3)

#Checking Muliticollinearity 
df_numeric <- reduced.hg.df %>% select_if(is.numeric)
library(psych)
corPlot(df_numeric, upper = FALSE)

#vif
car::vif(adom3.out)















#LINE
library(PerformanceAnalytics)
df_numeric <- reduced.hg.df %>% select_if(is.numeric)
chart.Correlation(df_numeric, histogram = TRUE, method = "pearson")
