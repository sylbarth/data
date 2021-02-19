
# --- read crises
data = read.csv("https://raw.githubusercontent.com/sylbarth/data/main/ews.csv")
colnames(data)[1] = "iso3c"
data = na.omit(data)



# --- add World Bank indicators ?
# World Bank data
# https://databank.worldbank.org/source/world-development-indicators

#install.packages("wbstats")
library(wbstats)

new_data = data.frame( wb_data(indicator = c("NY.GDP.PCAP.KD", "SP.DYN.LE00.IN", "SP.DYN.IMRT.IN")) )
new_data = new_data[,-c(1,3)]

data = merge(data, new_data, by = c("iso3c","date"), all.x = TRUE, all.y = FALSE)


# --- prepare the dataset
dbx = data[,3:ncol(data)]
rownames(dbx) = paste0(data$iso3c,data$date)


# --- rpart
library(rpart)
library(rpart.plot)

m1 <- rpart(factor(PCRISIS1)~., data=dbx)
p1 <- predict(m1, dbx, type="class")
table(p1, dbx$PCRISIS1)

prp(m1,extra=2)







