library(fgui)
files <- list.files(pattern = "\\.inp$")
file = sub('\\.inp$', '', files) 
dir.create(file[1])
OutFile  = choose.dir()
Input = paste(OutFile,".inp",sep ="")
Report =paste(OutFile,".rpt",sep ="")
Output =paste(OutFile,".out",sep ="")
swmm = choose.dir()
swmm = paste(swmm, "\\swmm5.exe",sep ="")
SS4 = function(SWMMOptFile){
  SWMMOptFile = SWMMOptFile
}
SWMMOptFile <- guiv(SS4,argFilename = list(SWMMOptFile = NULL))

SS5 = function(ParametersFile){
  ParametersFile = ParametersFile
}
ParametersFile <- guiv(SS5,argFilename = list(ParametersFile = NULL))

SS = function(iType){
  iType = iType
}
iType <- guiv(SS,argList = list(iType = c("0","1","2","3")))
          

SS1 = function(vIndex){
  vIndex = vIndex
}
vIndex <- guiv(SS1,argList = list(vIndex = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13")))
SWMMExe(swmm,Input,Report,Output)
headObj = GetObjectsSWMM (Output)
headObj = getSWMMTimes (headObj)
Nodes = {}
Subcatchments = {}
Pollutants = {}
Links = {}
  if (iType== 0) {
  Subcatchments  = headObj$SubCatchName
}
if (iType ==1){
  Nodes = headObj$NodesName
}
  if (iType ==2){
    Links  = headObj$linksName
  }

SS2 = function(ObjectName){
  ObjectName = ObjectName
}
ObjectName <- guiv(SS2,argList = list(ObjectName = c("Nodes","Subcatchments","Links")))
#ObjectName = noquote(ObjectName)
ObjectName1 = ObjectName

SS3 = function(nameInOutputFile){
  nameInOutputFile = nameInOutputFile
}
nameInOutputFile <- guiv(SS3,argList = list(nameInOutputFile = ObjectName))
Q2 = getSWMMTimeSeriesData(headObj,iType,nameInOutputFile,vIndex)
Q2= mean(Q2)
# Date = headObj$SWMMTimes
# SimulatedData<-{}
# SimulatedData$Times= data.frame(Date)
# SimulatedData$Flow = data.frame(Q)
# 
# SimulatedDat = data.frame(SimulatedData)
# Simulated = {}
# Simulated$Q = SimulatedDat$Q
# Aggregate = {}
# Aggregate$Sim = Simulated$Q
# 
# sum1={}
# i=1
# j=1
# k=1
# sum1$Sim[k] = 0
# for (i in 1:(length(Aggregate$Sim)-1)){
#   sum1$Sim[k] = sum1$Sim[k] + Aggregate$Sim[i]
# }
n=50

vals <- list(list(var="$1$",
                  dist="unif",
                  params=c(0,100)),
             list(var="$2$",
                  dist="unif",
                  params=c(0,100)),
             list(var="$3$",
                  dist="unif",
                  params=c(20,100)),
             list(var="$4$",
                  dist="unif",
                  params=c(50,200)),
             list(var="$5$",
                  dist="unif",
                  params=c(10,30)),
             list(var="$6$",
                  dist="unif",
                  params=c(0.1,1)),
             list(var="$7$",
                  dist="unif",
                  params=c(0.01,0.5)),
             list(var="$8$",
                  dist="unif",
                  params=c(0.1,1)),
             list(var="$9$",
                  dist="unif",
                  params=c(2,8)))
samp = generateMCSample(n, vals)

samp = samp[2:10]

names(samp) <- c(" ", "  ", " "," "," "," "," "," "," ")
samp = t(samp)
samp = data.frame (samp)

samp$Code = c("$1$","$2$","$3$","$4$","$5$","$6$","$7$","$8$","$9$")
SWMMOpt= ReadSWMMOptFile(SWMMOptFile)
# Bounds  = ParametersBound(ParametersFile)
# x1 = Bounds$Negative.5
# x2 =  Bounds$Negative.10
# x3 =  Bounds$Negative.15
# x4 = Bounds$Negative.20
# x5 = Bounds$Positive.5
# x6 = Bounds$Positive.10
# x7 = Bounds$Positive.15
# x8 = Bounds$Positive.20
# 
# SS6 = function(iType){
#   iType = iType
# }
# iType <- guiv(SS,argList = list(iType = c("0","1","2","3")))
# 
# SWMMOpt= ReadSWMMOptFile(SWMMOptFile)
Q1 = {}
for (i in 1:n){
  Input=paste(OutFile,i,'.inp',sep="")
  ReplaceCodes<<- replaceCodesInTemplateFile(SWMMOpt,samp[,i],as.matrix(samp["Code"]),Input)
  Report=paste(OutFile,i,'.rpt',sep="")
  Output=paste(OutFile,i,'.out',sep="")
  SWMMExe(swmm,Input,Report,Output)
  headObj = GetObjectsSWMM (Output)
  headObj = getSWMMTimes (headObj)
  Q = getSWMMTimeSeriesData(headObj,iType,nameInOutputFile ,vIndex)
  Q1[i] = mean(Q)
}


par(mfrow=c(1,2))
hist(Q1, breaks=20, main="Histogram of Q (CFS)", xlab="Q (CFS)",col = "blue")
boxplot(Q1, ylab="Q (CFS)", main="Boxplot of Q (CFS)",col = "green")

#Tornado Plot
par(mfrow=c(1,1))
SS1 = function(Impreviousness1){
  Impreviousness1 = Impreviousness1
}
Impreviousness1 = guiv(SS1)
SS1 = function(Impreviousness2){
  Impreviousness2 = Impreviousness2
}
Impreviousness2 = guiv(SS1)
SS1 = function(hydraulicwidth1){
  hydraulicwidth1 = hydraulicwidth1
}
hydraulicwidth1 = guiv(SS1)
SS1 = function(hydraulicwidth2){
  hydraulicwidth2 = hydraulicwidth2
}
hydraulicwidth2 = guiv(SS1)

SS1 = function(hydraulicwidth2){
  hydraulicwidth2 = hydraulicwidth2
}
hydraulicwidth2 = guiv(SS1)
SS1 = function(bermheight){
  bermheight = bermheight
}
bermheight = guiv(SS1)
SS1 = function(vegetationVolume){
  vegetationVolume = vegetationVolume
}
vegetationVolume = guiv(SS1)
SS1 = function(surfaceroufness){
  surfaceroufness = surfaceroufness
}
surfaceroufness = guiv(SS1)

SS1 = function(surfaceslope){
  surfaceslope = surfaceslope
}
surfaceslope = guiv(SS1)
SS1 = function(swalesideslope){
  swalesideslope = swalesideslope
}
swalesideslope = guiv(SS1)

#  Impreviousness1 = 1
#  Impreviousness2 = 87
#  hydraulicwidth1 = 27
#  hydraulicwidth2 = 127.1
# bermheight = 20.4
# vegetationVolume = 0.2125
#  surfaceroufness = 0.085
#  surfaceslope = 0.425
# swalesideslope = 4.25
 samp1 = ((samp[1,][1:n] - Impreviousness1)/Impreviousness1)
 minTop = min(samp1)
 MaxTop = max(samp1)
 Imperv1  =rbind(minTop,MaxTop)
 samp2 = ((samp[2,][1:n] - Impreviousness2)/Impreviousness2)
 minTop = min(samp2)
 MaxTop = max(samp2)
 Imperv2  =rbind(minTop,MaxTop)
 samp3 = ((samp[3,][1:n]- hydraulicwidth1)/hydraulicwidth1)*100
 minTop = min(samp3)
 MaxTop = max(samp3)
 Hydwidth1  =rbind(minTop,MaxTop)
 samp4 = ((samp[4,][1:n] - hydraulicwidth2)/hydraulicwidth2)*100
 minTop = min(samp4)
 MaxTop = max(samp4)
 Hydwidth2  =rbind(minTop,MaxTop)
samp5 = ((samp[5,][1:n] - bermheight)/bermheight)*100
minTop = min(samp5)
MaxTop = max(samp5)
bermH  =rbind(minTop,MaxTop)
 samp6 = ((samp[6,][1:n] - vegetationVolume)/vegetationVolume)*100
 minTop = min(samp6)
 MaxTop = max(samp6)
 VegVolume  =rbind(minTop,MaxTop)
 samp7 = ((samp[7,][1:n] - surfaceroufness)/surfaceroufness)*100
 minTop = min(samp7)
 MaxTop = max(samp7)
 Roughness  =rbind(minTop,MaxTop)
 samp8 = ((samp[8,][1:n] - surfaceslope)/surfaceslope)*100
 minTop = min(samp8)
 MaxTop = max(samp8)
Slope  =rbind(minTop,MaxTop)
 samp9 = ((samp[9,][1:n] - swalesideslope)/swalesideslope)*100
 minTop = min(samp9)
 MaxTop = max(samp9)
 SideSlope  =rbind(minTop,MaxTop)
 
 
 
#Q3 = (Q1-Q2)/Q2
Data1 = cbind(Imperv1,Imperv2,Hydwidth1,Hydwidth2,bermH,VegVolume,Roughness,Slope,SideSlope)
Data1 = data.matrix(Data1)

barplot(Data1[1,], horiz = T, las=1, xlim = c(-500,500), xaxt='n', ylab = '', beside=T, col=c('springgreen'))
barplot(Data1[2,], horiz = T, las=1, xlim = c(-500,500), xaxt='n', ylab = '', beside=T, col=c('indianred2'), add = TRUE)
x <- seq(-500,500, length=10) 
axis(1, at=pretty(x),  lab=paste0(pretty(x) ," %"), las=TRUE)



