'''#test <- read.table(rdata.csv, header=TRUE, sep=",")

fConvert <- function(i, j){
for (i in 1:22) {
  for (j in 2:169) {
    if (!is.null(rdata[i,j])){rdata[23,j] = rdata$ID[i]} 
    else {rdata[i,j] = "null"}
}
}
'''
library("reshape")
### sender data reshape function:
reshapeData <- function(senderData, fileName){
  meltData <- melt(senderData, id =c("BU", "L2","CountryCode","HomeCountryDesc","JobFunction", "JobLevel"))
  newData <- meltData[!(meltData$value =="" | is.na(meltData$value)),]
  write.csv(newData, file=fileName)
  return (newData)
}

reshapeData(PPS.SENDER0, "new.PPS.SENDER0.csv") ### with PPS0 as Sender--as final submission
reshapeData(CMP.SENDER, "new.CMP.SENDER.csv") ### with CMP as Sender
# reshapeData(PPS.SENDER3, "new.PPS.SENDER3.csv") ### with PPS3 as Sender
reshapeData(FIN.SENDER, "new.FIN.SENDER.csv") ### with FIN as Sender
reshapeData(OST.SENDER, "new.OST.SENDER.csv") ### with OST as Sender
reshapeData(TNO.SENDER, "new.TNO.SENDER.csv") ### with TNO as Sender
# reshapeData(PPS.SENDER2, "new.PPS.SENDER2.csv") ### with PPS2 as Sender
# reshapeData(PPS.SENDER1, "new.PPS.SENDER1.csv") ### with PPS1 as Sender
reshapeData(EG.SENDER, "new.EG.SENDER.csv") ### with EG as Sender
reshapeData(ES.SENDER, "new.ES.SENDER.csv") ### with ES as Sender
reshapeData(SW.SENDER, "new.SW.SENDER.csv") ### with SW as Sender

### receiver data reshape function:
reshapeReData <- function(receiverData, fileName){
  meltData <- melt(receiverData, id =c("BU", "L2","CountryCode","HomeCountryDesc","JobFunction", "JobLevel"))
  newData <- meltData[!(meltData$value =="" | is.na(meltData$value)),]
  write.csv(newData, file=fileName)
  return (newData)
}

reshapeReData(PPS.RECEIVER, "new.PPS.RECEIVER.csv") ### with PPSG as receiver -- final version
reshapeReData(FIN.RECEIVER, "new.FIN.RECEIVER.csv") ### with FIN as receiver
reshapeReData(GCO.RECEIVER, "new.GCO.RECEIVER.csv") ### with GCO as receiver
reshapeReData(SGR.RECEIVER, "new.SGR.RECEIVER.csv") ### with SGR as receiver
reshapeReData(CMP.RECEIVER1, "new.CMP.RECEIVER1.csv") ### with CMP1 as receiver
reshapeReData(CMP.RECEIVER2, "new.CMP.RECEIVER2.csv") ### with CMP2 as receiver
reshapeReData(CMP.RECEIVER3, "new.CMP.RECEIVER3.csv") ### with CMP3 as receiver
reshapeReData(CMP.RECEIVER4, "new.CMP.RECEIVER4.csv") ### with CMP4 as receiver
reshapeReData(CMP.RECEIVER5, "new.CMP.RECEIVER5.csv") ### with CMP5 as receiver
reshapeReData(CMP.RECEIVER6, "new.CMP.RECEIVER6.csv") ### with CMP6 as receiver from PPS updated by Ailun
reshapeReData(CMP.RECEIVER7, "new.CMP.RECEIVER7.csv") ### with CMP7 as receiver from ESHP updated by Ailun
# reshapeReData(CMP.RECEIVER, "new.CMP.RECEIVER.csv") ### with CMP as receiver 


### concatenate the receiver files:
new.CMP.RECEIVER1 <- read.csv("new.CMP.RECEIVER1.csv", sep = ",")
new.CMP.RECEIVER2 <- read.csv("new.CMP.RECEIVER2.csv", sep = ",")
new.CMP.RECEIVER3 <- read.csv("new.CMP.RECEIVER3.csv", sep = ",")
new.CMP.RECEIVER4 <- read.csv("new.CMP.RECEIVER4.csv", sep = ",")
new.CMP.RECEIVER5 <- read.csv("new.CMP.RECEIVER5.csv", sep = ",")
new.FIN.RECEIVER <- read.csv("new.FIN.RECEIVER.csv", sep = ",")
new.SGR.RECEIVER <- read.csv("new.SGR.RECEIVER.csv", sep = ",")
new.GCO.RECEIVER <- read.csv("new.GCO.RECEIVER.csv", sep = ",")
conReceiver <- rbind(new.CMP.RECEIVER1, new.CMP.RECEIVER2, new.CMP.RECEIVER3,new.CMP.RECEIVER4,new.CMP.RECEIVER5,new.FIN.RECEIVER, new.SGR.RECEIVER,new.GCO.RECEIVER)
write.csv(conReceiver, file = "conReceiver.csv")

### concatenate the sender files:
new.PPS.SENDER1 <- read.csv("new.PPS.SENDER1.csv", sep = ",")
new.PPS.SENDER2 <- read.csv("new.PPS.SENDER2.csv", sep = ",")
new.PPS.SENDER3 <- read.csv("new.PPS.SENDER3.csv", sep = ",")
new.CMP.SENDER <- read.csv("new.CMP.SENDER.csv", sep = ",")
new.SW.SENDER <- read.csv("new.SW.SENDER.csv", sep = ",")
new.EG.SENDER <- read.csv("new.EG.SENDER.csv", sep = ",")
new.ES.SENDER <- read.csv("new.ES.SENDER.csv", sep = ",")
new.FIN.SENDER <- read.csv("new.FIN.SENDER.csv", sep = ",")
#new.SGR.SENDER <- read.csv("new.SGR.SENDER.csv", sep = ",")
new.OST.SENDER <- read.csv("new.OST.SENDER.csv", sep = ",")
new.TNO.SENDER <- read.csv("new.TNO.SENDER.csv", sep = ",")
conSENDER <- rbind(new.PPS.SENDER1, new.PPS.SENDER2, new.PPS.SENDER3,new.ES.SENDER,new.CMP.SENDER,new.FIN.SENDER, new.OST.SENDER, new.TNO.SENDER, new.EG.SENDER, new.SW.SENDER)
write.csv(conSENDER, file = "conSENDER.csv")


#write.csv(newData, file="new.ES.SENDER.csv")

'''
senderData <- SW.SENDER[, c(1:15)]
new.SW.SENDER <- melt(senderData, id =c("BU", "L2","CountryCode","HomeCountryDesc","JobFunction", "JobLevel"))
new.SW.SENDER <- new.SW.SENDER[!(new.SW.SENDER$value =="" | is.na(new.SW.SENDER$value)),]
new.SW.SENDER <- (data.frame(strsplit(variable, ".")))
'''
