#Read df
setwd("~/DataScience/DS_710/Homework/ds710fall2017finalproject")
btc <- read.csv('btc.csv')
ltc <- read.csv('ltc.csv')
eth <- read.csv('eth.csv')
#Summary Stats
summary(btc)
summary(ltc)
summary(eth)
#Create a class variable for each type of cryptocurrency
btc$class <- 'btc'
ltc$class <- 'ltc'
eth$class <- 'eth'
#Combine three sets into one for analysis
full <- rbind(btc, ltc, eth)
#Boxplot of original full data
boxplot(polarity~class, data = full)
boxplot(retweets~class, data = full)
#Create filtered dataset for only tweets with polarity score not equal to zero.
#This is done to limit the scope of the analysis to only tweets textblob found emotion  in.
full_pol <- full[which(full$polarity != 0),]
#Boxplot of polarity scores forr emotion tweets.
boxplot(polarity~class, data = full_pol, main = "Boxplots of Polarity for Crypto Tweets", ylab = "Polarity")

#Create variables for each polarity for histograms
btc_pol <- full_pol[which(full_pol$class == 'btc'),]
ltc_pol <- full_pol[which(full_pol$class == 'ltc'),]
eth_pol <- full_pol[which(full_pol$class == 'eth'),]
#Histograms for each currency. Histograms show relative normality in shape.
par(mfrow = c(1, 3))
hist(btc_pol$polarity, main = "Bitcoin", xlab = "Polarity")
hist(ltc_pol$polarity, main = "Litecoin", xlab = "Polarity")
hist(eth_pol$polarity, main = "Ethereum", xlab = "Polarity")
#Code for multiple comparison test. Games Howell test is used due to different 
#Sample sizes and variances. 
source('./onewayComp.R')
onewayComp(polarity~class, data = full_pol, var.equal=F)
