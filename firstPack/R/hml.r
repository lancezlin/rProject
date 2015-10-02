# load function/job level/SAP mixed data from excel
# when first run these functions, install these packages
'''install.packages("gdata")
install.packages("sqldf")'''  
library(gdata)
library(sqldf)
# setting working directory to the desktop/HML/R_HML
setwd("C:/Users/linla/Desktop/HML/R_HML")

####################################################################################################

hrbi.mix <- read.csv("hmlhrbi.csv", header=TRUE, stringsAsFactors=FALSE)
# exclude the Executives 
sub.data <- function(indata){
  levelList <- c("BAS", "PRI","COR", "SEN", "ADV", "ENT", "INT", "SPE", "EXP","MAS","SU1","SU2","MG1","MG2")
  outdata <- subset(indata, ManagementLevel %in% levelList)
  return(outdata)
}
hrbi_mix <- as.data.frame(sub.data(hrbi.mix), stringsAsFactors=FALSE)
# hrbi_mix
# 1st: job function mix function-input any data file
job.function.mix <- function(indata, fileName){
  outdata <- sqldf("select JobFunctionGroup, count(*) as countID from indata group by JobFunctionGroup")
  write.csv(outdata, file=fileName)
  return (outdata)
}
# 2nd: job level mix function - input any data file
job.level.mix <- function(indata, fileName){
  outdata <- sqldf("select ManagementLevel, count(*) as countID from indata group by ManagementLevel")
  write.csv(outdata, file=fileName)
  return(outdata)
}
# 3rd: Salary Admin Plan mix function - input any data file
country.sap.mix <- function(indata, fileName){
  outdata <- sqldf("select PayGroupCountryCode, SalaryAdministrationPlanCode, count(*) as countID from indata group by SalaryAdministrationPlanCode")
  write.csv(outdata, file=fileName)
  return(outdata)
}

sapMix <- as.data.frame(country.sap.mix(hrbi_mix, "sapMix.csv"), stringsAsFactors=FALSE)
functionMix <- as.data.frame(job.function.mix(hrbi_mix, "functionMix.csv"), stringsAsFactors=FALSE)
levelMix <- as.data.frame(job.level.mix(hrbi_mix, "levelMix.csv"), stringsAsFactors=FALSE)


countryHC <- as.data.frame(aggregate(sapMix$countID, by=list(sapMix$PayGroupCountryCode), FUN=sum ), stringsAsFactors=FALSE)
countryHC$PayGroupCountryCode <- countryHC$Group.1
sapMix_temp1 <- merge(x = sapMix, y = countryHC, by = "PayGroupCountryCode", all.x = TRUE)
#### sapMix_temp1$HCpercent <- as.numeric(sapMix_temp1$countryHC/sapMix_temp1$x)



#############################################################################################################
# loading the job architecture data--AMS + APJ + EMEA
amsMid <- read.csv("amsMidpoint.csv", header=TRUE, stringsAsFactors=FALSE)
apjMid <- read.csv("apjMidpoint.csv", header=TRUE, stringsAsFactors=FALSE)
emeaMid <- read.csv("emeaMidpoint.csv", header=TRUE, stringsAsFactors=FALSE)


Midpoint.prep <- function(ams, apj, emea){
  # Ams conversion & conbination
  Region <- as.character(ams$Region)
  SubRegion <- as.character(ams$SubRegion)
  CountryName <- as.character(ams$CountryName)
  CountryCode <- as.character(ams$CountryCode)
  ManagementLevel <- as.character(ams$ManagementLevel)
  CurrencyCode <- as.character(ams$CurrencyCode)
  JobFunctionGroup <- as.character(ams$JobFunctionGroup)
  FY15GradeProfile <- as.character(ams$FY15GradeProfile)
  MidPoint <- as.numeric(as.character(ams$SEGMENT4MID))
  amsMidPoint <- as.data.frame(cbind(Region, SubRegion, CountryName, CountryCode, ManagementLevel, JobFunctionGroup, CurrencyCode, FY15GradeProfile, MidPoint), stringsAsFactors=FALSE)
  # APJ conversion & conbination
  Region <- as.character(apj$Region)
  SubRegion <- as.character(apj$SubRegion)
  CountryName <- as.character(apj$CountryName)
  CountryCode <- as.character(apj$CountryCode)
  ManagementLevel <- as.character(apj$ManagementLevel)
  CurrencyCode <- as.character(apj$CurrencyCode)
  JobFunctionGroup <- as.character(apj$JobFunctionGroup)
  FY15GradeProfile <- as.character(apj$FY15GradeProfile)
  MidPoint <- as.numeric(as.character(apj$SEGMENT4MID))
  apjMidPoint <- as.data.frame(cbind(Region, SubRegion, CountryName, CountryCode, ManagementLevel, JobFunctionGroup, CurrencyCode, FY15GradeProfile, MidPoint),stringsAsFactors=FALSE)
  # EMEA conversion & conbination
  Region <- as.character(emea$Region)
  SubRegion <- as.character(emea$SubRegion)
  CountryName <- as.character(emea$CountryName)
  CountryCode <- as.character(emea$CountryCode)
  ManagementLevel <- as.character(emea$ManagementLevel)
  CurrencyCode <- as.character(emea$CurrencyCode)
  JobFunctionGroup <- as.character(emea$JobFunctionGroup)
  FY15GradeProfile <- as.character(emea$FY15GradeProfile)
  MidPoint <- as.numeric(as.character(emea$SEGMENT4MID))
  emeaMidPoint <- as.data.frame(cbind(Region, SubRegion, CountryName, CountryCode, ManagementLevel, JobFunctionGroup, CurrencyCode, FY15GradeProfile, MidPoint),stringsAsFactors=FALSE)
  hpMidPoint <- as.data.frame(rbind(amsMidPoint, apjMidPoint, emeaMidPoint), stringsAsFactors=FALSE)
  return(hpMidPoint)
}
hpMidPoint <- as.data.frame(Midpoint.prep(amsMid, apjMid, emeaMid), stringsAsFactors=FALSE)
hpMidPoint$MidPoint <- as.numeric(hpMidPoint$MidPoint)

### exclude executives and lower than BAS levels
exclude.exe <- function(indata){
	MgtLevel <- c("BAS", "PRI", "COR", "SEN", "ADV", "ENT", "INT", "SPE", "EXP", "MAS", "SU1", "SU2", "MG1", "MG2")
	outdata <- subset(indata, ManagementLevel %in% MgtLevel)
	return(outdata)
}
hpMid_temp1 <- as.data.frame(exclude.exe(hpMidPoint), stringAsFactors=FALSE)
hpMid_temp2 <- aggregate(hpMid_temp1$MidPoint, by=list(hpMid_temp1$Region, hpMid_temp1$CountryCode, hpMid_temp1$ManagementLevel, hpMid_temp1$JobFunctionGroup, hpMid_temp1$FY15GradeProfile), FUN=mean)

write.csv(hpMidPoint, "hpMid.csv")
write.csv(hpMid_temp1, "hpMid_temp1.csv")
write.csv(hpMid_temp2, "hpMid_temp2.csv")
'''
outputdata <- sqldf("select Region, SubRegion, CountryName, CountryCode, JobFunctionGroup, ManagementLevel, FY15GradeProfile, SEGMENT4MID from inputdata")
  return(outputdata)
'''

'''
conv <- paste(amsMid$SEGMENT4MID)
convertFactor <- as.numeric(paste(amsMid$SEGMENT4MID))

amsMid_Char <- as.data.frame(lapply(amsMid, as.character), StringsAsFactors=FALSE)
a <- as.character(amsMid$Region)
class(a)
b <- as.character(amsMid$ManagementLevel)
class(b)
c <- cbind(a,b)
c
'''
'''
for (i in 1:35067) {
  mgtLevel = rep(NA, 35067)
if (as.character(amsMidpoint$ManagementLevel[i])=="23 Base"){
mgtLevel[i] == "BAS"} else {mgtLevel[i] == "No"}
}
a <- amsMid
amsMid_r <- cbind(as.character(a$Region), as.character(a$SubRegion), as.character(a$CountryName), as.character(a$CountryCode), as.character(a$JobFunctionGroup), as.character(a$ManagementLevel),  as.character(a$FY15GradeProfile), as.numeric(a$SEGMENT4MID))
'''
