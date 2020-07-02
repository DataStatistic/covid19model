# Random intercept beta model

library(readxl)
require(betareg)

# Dataset
base1 <- data.frame(read.csv2("CovidData.csv"))
head(base1)

base = subset(base1, select = c(Let1,Let2,Let3,Let4,Let5,Let6,Let7,Let8,Let9,
  p60years,Literacy,Beds,Health,Obesity,Test,Diabetes,Respiratory.diseases,HIV.AIDS,Cardiovascular,
  Poverty,Sex,Doctors,Passengers,Quarantine,Respiratory.infectious,GDP,Tobacco,Urbanpop,Height,Density,Nurse,cod))

base = na.omit(base[base$Let1!=0 & base$Let2!=0 & base$Let3!=0 &
                    base$Let4!=0 & base$Let5!=0 & base$Let6!=0 &
                    base$Let7!=0 & base$Let8!=0 & base$Let9!=0,])
nrow(base)
summary(base)

base1 = cbind(base[,-(1:9)],Let = base$Let1)
base2 = cbind(base[,-(1:9)],Let = base$Let2)
base3 = cbind(base[,-(1:9)],Let = base$Let3)
base4 = cbind(base[,-(1:9)],Let = base$Let4)
base5 = cbind(base[,-(1:9)],Let = base$Let5)
base6 = cbind(base[,-(1:9)],Let = base$Let6)
base7 = cbind(base[,-(1:9)],Let = base$Let7)
base8 = cbind(base[,-(1:9)],Let = base$Let8)
base9 = cbind(base[,-(1:9)],Let = base$Let9)

basemix = rbind(base1,base2,base3,base4,base5,base6,base7,base8,base9)
basemix

# Model fit
modmix = betareg(Let~p60years+Beds+Respiratory.diseases+Diabetes+Test+
   Literacy+Health+Obesity+HIV.AIDS+Cardiovascular+
   Poverty+Sex+Doctors+Passengers+Quarantine+Respiratory.infectious+GDP+
   Tobacco+Urbanpop+Height+Density+Nurse | cod, 
   data = basemix)

res = summary(modmix) ; res
