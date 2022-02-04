
# World Bank data
# https://databank.worldbank.org/source/world-development-indicators

indicators = c("EN.ATM.CO2E.PC",    # CO2 emission
               "EG.USE.ELEC.KH.PC", # electric power consumption
               "EN.ATM.NOXE.KT.CE") # dioxyde

# --- auto download
#install.packages("wbstats")
library(wbstats)

data = data.frame(wb_data(indicator=indicators) )
head(data)

data = data[data$date==2014,]
data = na.omit(data)


# --- create row names
nams = data$iso3c
row.names(data) = nams


# --- create a second data set with numerical data
numdata = data[,-(1:4)]
head(numdata)



# --- ranking
numdatas = scale(numdata)
round(apply(numdatas, 2, mean), 2)
round(apply(numdatas, 2, sd),   2)

# --- adjust some indicators
#numdatas[,"DEBT"]  = -numdatas[,"DEBT"]
#numdatas[,"START"] = -numdatas[,"START"]
#numdatas[,"COMMO"] = -numdatas[,"COMMO"]

# --- compute scores
zscores = apply(numdatas, 1, mean)
zscores
scores = (zscores-min(zscores))/(max(zscores)-min(zscores))*100
scores

round(zscores[order(zscores)], 2)
barplot(round(zscores[order(zscores)], 2), col="#cc3300")
barplot(round(zscores[order(zscores)], 2), col="#cc3300", las=3)
barplot(round(zscores[order(zscores)], 2), col="#cc3300", las=1, horiz=T)

barplot(round(scores[order(scores)], 2), col="#cc3300", las=3)

e)
row.names(data) = nams


# --- create a second data set with numerical data
numdata = data[,-(1:4)]
head(numdata)


