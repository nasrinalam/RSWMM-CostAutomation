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
SWMMExe(swmm,Input,Report,Output)
headObj = GetObjectsSWMM(Output)
headObj = getSWMMTimes(headObj)
iType = 1
vIndex = 4
nameInOutputFile = "J11510.66" 
Q = getSWMMTimeSeriesData(headObj,iType,nameInOutputFile,vIndex)
SimulatedData = SimulatedData(Q,headObj)

SS = function(ParametersFile){
 
return(ParametersFile)
}
StatParameters = c("NashsutcliffeTimesMinus1_2","PercentBias2","linearCorrelationTimesMinus1_2") 
ParametersFile <- guiv(SS,argFilename = list(ParametersFile = NULL))
iteration = 1
Bounds = ParametersBound(ParametersFile) 
initial=c(as.vector(Bounds["Initial"]))$Initial
lower= c(as.vector(Bounds["Minimum"]))$Minimum
upper = c(as.vector(Bounds["Maximum"]))$Maximum
 
fit <- guiv(OptimizationFunction,argFilename = list(SWMMOptFile = NULL),argList = list(initial = c("initial"), lower = c("lower"),upper = c("upper"),Timeseries = c("Timeseries"),OutFile = c("OutFile"), swmm = c("swmm"), StatParameters = c("StatParameters")))

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
filelist <- list.files(pattern = "^Combinations.*\\.xlsx$")

files <- lapply(filelist, read.xlsx, sheetName = "Sheet1", header=TRUE) 
files <- lapply(files, function(x) x[-1])
Combination1 = Reduce(function(...) merge(..., by =1:3, all=T), files)

Rsquared = Combination1$linearCorrelationTimesMinus1_2
NSE = Combination1$NashsutcliffeTimesMinus1_2
PBIAS = Combination1$PercentBias2