library(readr)
library(ggplot2)
bigdata <- read_csv("C:/Users/jburr/Downloads/Accidental_Drug_Related_Deaths__2012-June_2017 (1).csv")
attach(bigdata)
bigdata$Combo = rep('',nrow(bigdata))
attach(bigdata)

bigdata[is.na(bigdata)] <- 0

bigdata$Heroin[bigdata$Heroin=='Y']<-'Heroin'
bigdata$Cocaine[bigdata$Cocaine=='Y']<-'Cocaine'
bigdata$Fentanyl[bigdata$Fentanyl=='Y']<-'Fentanyl'
bigdata$Oxycodone[bigdata$Oxycodone=='Y']<-'Oxycodone'
bigdata$Oxymorphone[bigdata$Oxymorphone=='Y']<-'Oxymorphone'
bigdata$EtOH[bigdata$EtOH=='Y']<-'EtOH'
bigdata$Hydrocodone[bigdata$Hydrocodone=='Y']<-'Hydrocodone'
bigdata$Benzodiazepine[bigdata$Benzodiazepine=='Y']<-'Benzodiazepine'
bigdata$Methadone[bigdata$Methadone=='Y']<-'Methadone'
bigdata$Amphet[bigdata$Amphet=='Y']<-'Amphet'
bigdata$Tramad[bigdata$Tramad=='Y']<-'Tramad'
bigdata$`Morphine (not heroin)`[bigdata$`Morphine (not heroin)`=='Y']<-'Morphine'


bigdata$Combo = with(bigdata, paste(Heroin,Cocaine,Fentanyl,Oxycodone,Oxymorphone,EtOH,Hydrocodone,Benzodiazepine,Methadone,Amphet,Tramad,`Morphine (not heroin)`,Other,sep=" "))

agg1 = aggregate(CaseNumber~Combo,FUN=length,bigdata)
agg1$per = agg1$CaseNumber/sum(agg1$CaseNumber) * 100
ggplot(data=agg1, aes(x=Combo, y=CaseNumber)) + geom_bar(stat="identity")

agg2 = aggregate(CaseNumber~Combo+Location,FUN=length,bigdata)

agg3 = aggregate(CaseNumber~Location,FUN=length,bigdata)

aggRace = aggregate(CaseNumber~Race,FUN=length,bigdata)
ggplot(data=aggRace, aes(x=Race, y=CaseNumber)) + geom_bar(stat="identity")

aggSex = aggregate(CaseNumber~Sex,FUN=length,bigdata)
ggplot(data=aggSex, aes(x=Sex, y=CaseNumber)) + geom_bar(stat="identity")

aggAge = aggregate(CaseNumber~Age,FUN=length,bigdata)
colnames(aggAge)[2] = 'Deaths'
ggplot(data=aggAge, aes(x=Age, y=Deaths)) + geom_bar(stat="identity")

# modeling
bigdata$Op = rep(0,nrow(bigdata))
attach(bigdata)
df <- within(bigdata, Op[Heroin == 'Heroin' |  Fentanyl == 'Fentanyl' | Oxycodone == 'Oxycodone' | Oxymorphone == 'Oxymorphone' | Hydrocodone == 'Hydrocodone' | Methadone == 'Methadone' | `Morphine (not heroin)` != 0 | `Any Opioid` == 'Y'  ] <- 1)
library(randomForest)
rf = randomForest(as.factor(Op)~Age+as.factor(Sex)+as.factor(Race),df)
rf$confusion


