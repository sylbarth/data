
# World Bank data
# https://databank.worldbank.org/source/world-development-indicators

indicators = c("NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CD", "NE.GDI.FTOT.ZS", "SP.URB.TOTL.IN.ZS", "PA.NUS.FCRF", "FP.CPI.TOTL.ZG", "NE.CON.TOTL.KD.ZG")

# --- auto download
#install.packages("wbstats")
library(wbstats)

data = data.frame(wb_data(indicator=indicators) )
head(data)


# --- create row names
nams = paste0(data$iso3c, data$date)
row.names(data) = nams


# --- create a second data set with numerical data
numdata = data[,-(1:4)]
head(numdata)


# --- plot
plot(data[,5:6], pch=19)
plot(data[,5:8], pch=19)

plot(data[data$iso3c=="CHN",5:ncol(data)], pch=19)
plot(data[data$iso3c=="FRA",5:ncol(data)], pch=19)
plot(data[data$iso3c=="FRA",5:ncol(data)], pch=19)

colour = ifelse(data[data$iso3c=="CHN","NY.GDP.MKTP.KD.ZG"]<10, "red", "green")
plot(data[data$iso3c=="CHN",5:ncol(data)], pch=19, col=colour)

colour = ifelse(data[,"NY.GDP.MKTP.KD.ZG"]<10, "red", "green")
plot(data[,5:ncol(data)], pch=19, col=colour)

colour = ifelse(data[,"FP.CPI.TOTL.ZG"]>10, "red", "green")
plot(data[,5:ncol(data)], pch=19, col=colour)



# --- PCA
#install.packages("FactoMineR")
library(FactoMineR)
pca = PCA(numdata, scale.unit=TRUE)
plot(pca)



# --- rpart
library(rpart)
library(rpart.plot)

training        = numdata[,c("NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CD", "NE.GDI.FTOT.ZS", "SP.URB.TOTL.IN.ZS", "NE.CON.TOTL.KD.ZG")]
training$TARGET = ifelse(numdata[,"FP.CPI.TOTL.ZG"]>5, 1, 0)
training        = na.omit(training)

m1 <- rpart(factor(TARGET)~., data=training)
p1 <- predict(m1, training, type="class")
table(p1, training$TARGET)

prp(m1, extra=2)
prp(m1, extra=8, type=5)

