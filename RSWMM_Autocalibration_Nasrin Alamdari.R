SWMMExe <-function(swmm,Input,Report,Output,iType,vIndex){
  swmm=paste('"',swmm,'"',sep="")
  Input=paste('"', Input,'"',sep="")
  Report=paste('"', Report,'"',sep="")
  Output=paste('"', Output,'"',sep="")
  
  # Running executable file
  
  system(paste(swmm,Input,Report,Output,sep=" "),show.output.on.console=T)
  return(Output)
}
GetObjectsSWMM<-function(Output){
  BinaryFile = file(Output,"rb")
  Status ={}
  seek(BinaryFile,1*4,"start")
  #the version number of the engine (currently 51000)
  Status$VersionNum = readBin(BinaryFile, integer(), n = 1, size = 4)
  #a code number for the flow units that are in effect where
  
  #0 = CFS
  #1 = GPM
  #2 = MGD
  #3 = CMS
  #4 = LPS
  #5 = LPD
  Status$UnitCode = readBin(BinaryFile, integer(), n = 1, size = 4)
  #the number of subcatchments in the project reported on
  Status$SubCatchNum = readBin(BinaryFile, integer(), n = 1, size = 4)
  #the number of nodes in the project reported on
  Status$NodesNum = readBin(BinaryFile, integer(), n = 1, size = 4)
  #the number of links in the project reported on
  Status$LinksNum = readBin(BinaryFile, integer(), n = 1, size = 4)
  #the number of pollutants in the project
  Status$PollutantsNum = readBin(BinaryFile, integer(), n = 1, size = 4)
  seek(BinaryFile,-2*4,"end") 
  #Check the error status (0 or 1)
  Status$Err= readBin(BinaryFile, integer(), n = 1, size = 4)
  if (Status$Err == 0){
    print ("There is no error in the file")
  }else{
    print("find the error in the file")
  }
  
  seek(BinaryFile,-6*4,"end")
  Status$ObjectID = readBin(BinaryFile, integer(), n = 1, size = 4)
  #Getting Object ID
  seek(BinaryFile,Status$ObjectID,"start")
  #subcatchment ID names
  Status$SubCatchName = {}
  if (Status$SubCatchNum>0){
    for (i in 1:Status$SubCatchNum){
      Status$SubCatchName[i]= readChar(BinaryFile,readBin(BinaryFile, integer(), n = 1, size = 4),useBytes = FALSE) 
      
    }
    
  }
  else {
    print ("No Subcatchment")
  }
  #node ID names
  Status$NodesName = {}
  if (Status$NodesNum>0){
    for (i in 1:Status$NodesNum){
      Status$NodesName[i]= readChar(BinaryFile,readBin(BinaryFile, integer(), n = 1, size = 4),useBytes = FALSE) 
      
    }
    
  }
  else {
    print ("No Node")
  }
  
  #link ID names
  Status$LinksName = {}
  if (Status$LinksNum>0){
    for (i in 1:Status$LinksNum){
      Status$LinksName[i]= readChar(BinaryFile,readBin(BinaryFile, integer(), n = 1, size = 4),useBytes = FALSE) 
      
    }
    
  }
  else {
    print ("No Link")
  }
  #Pollutant names
  Status$PollutantsName = {}
  if (Status$PollutantsNum>0){
    for (i in 1:Status$PollutantsNum){
      Status$PollutantsName[i]= readChar(BinaryFile,readBin(BinaryFile, integer(), n = 1, size = 4),useBytes = FALSE) 
    }
    
  }
  else {
    print ("No Pollutant")
  }
  #pollutant concentration units codes
  #0 for mg/L
  #1 for ug/L
  #2 for counts/L.
  Status$PollutantsUnitCode = {}
  for (i in 1:Status$PollutantsNum){
    Status$PollutantsUnitCode[i]= readBin(BinaryFile, integer(), n = 1, size = 4) 
  }
  #Getting Object Properties
  seek(BinaryFile,-5*4,"end")
  Status$ObjectProperties = readBin(BinaryFile, integer(), n = 1, size = 4)
  seek(BinaryFile,Status$ObjectProperties,"start")
  #Number of subcatchment properties saved (Currently equal to 1)
  Status$NumofSubCatchSaved = readBin(BinaryFile, integer(), n = 1, size = 4)
  #Code number of each subcatchment property saved (Currently equal to 1 for subcatchment area)
  Status$CodeNumSubCatchSaved = readBin(BinaryFile, integer(), n = 1, size = 4)
  #Value of each property for each subcatchment (Subcatchment area (ac or ha) for each subcatchment)
  Status$SubCatchArea=readBin(BinaryFile,what="double",n=Status$SubCatchNum,size=4);
  #Number of node properties saved (Currently equal to 3)
  Status$NumofNodesSaved = readBin(BinaryFile, integer(), n = 1, size = 4)
  
  #Code number of each node property saved
  #0 (node type code)
  #2 (node invert elevation)
  #3 (node max. depth)
  
  
  Status$CodeNumNodesSaved = readBin(BinaryFile, integer(), n = Status$NumofNodesSaved, size = 4)
  
  Status$TypeofNodes = {}
  if(Status$NodesNum>0){
    NodeType=readBin(BinaryFile,what="double",n=Status$NumofNodesSaved*Status$NodesNum,size=4)
    
    Status$CodeNumNodesSaved=NodeType[seq(from=1,by=3,to=length(NodeType))]
    #0 = Junction
    
    #1 = Outfall
    
    #2 = Storage
    
    #3 = Divider
    for (i in 1 :length(Status$CodeNumNodesSaved)){
      if (Status$CodeNumNodesSaved[i]==0){
        Status$TypeofNodes[i]="Junction"}
      else if (Status$CodeNumNodesSaved[i]==1){
        Status$TypeofNodes[i]="Outfall"}
      else if (Status$CodeNumNodesSaved[i]==2){
        Status$TypeofNodes[i]="Storage"}
      else if (Status$CodeNumNodesSaved[i]==3){
        Status$TypeofNodes[i]="Divider"}
    }
    
    
    Status$InvertElevation=NodeType[seq(from=2,by=3,to=length(NodeType))] 
    Status$MaximumDepth=NodeType[seq(from=3,by=3,to=length(NodeType))] 
    
  }
  
  #Number of node properties saved (Currently equal to 5)
  Status$NumofLinksSaved = readBin(BinaryFile, integer(), n = 1, size = 4)
  
  #Code number of each link property saved
  #0 (link type code)
  #4 (upstream invert offset)
  #4 (downstream invert offset)
  #3 (link max. depth) 
  #5 (link length)
  Status$CodeNumLinkSaved = readBin(BinaryFile, integer(), n = Status$NumofLinksSaved, size = 4)
  Status$TypeofLinks = {}
  if(Status$LinksNum>0){
    LinkType=readBin(BinaryFile,what="double",n=Status$NumofLinksSaved*Status$LinksNum,size=4)
    Status$CodeNumLinksSaved=LinkType[seq(from=1,by=5,to=length(LinkType))]
    #0 = Conduit 
    #1 = Pump 
    #2 = Orifice 
    #3 = Weir 
    #4 = Outlet
    for (i in 1 :length(Status$CodeNumLinksSaved)){
      if (Status$CodeNumLinksSaved[i]==0){
        Status$TypeofLinks[i]="Conduit"}
      else if (Status$CodeNumLinksSaved[i]==1){
        Status$TypeofLinks[i]="Pump"}
      else if (Status$CodeNumLinksSaved[i]==2){
        Status$TypeofLinks[i]="Orifice"}
      else if (Status$CodeNumLinksSaved[i]==3){
        Status$TypeofLinks[i]="Weir"}
      else if (Status$CodeNumLinksSaved[i]==4){
        Status$TypeofLinks[i]="Outlet"}
      #4 (upstream invert offset)
      Status$UpstreamInvertOffset=LinkType[seq(from=2,by=5,to=length(LinkType))]
      #4 (downstream invert offset)
      Status$DownstreamInvertOffset=LinkType[seq(from=3,by=5,to=length(LinkType))]
      #3 (link max. depth) 
      Status$MaximumDepth=LinkType[seq(from=4,by=5,to=length(LinkType))]
      #5 (link length)
      Status$LinkLength=LinkType[seq(from=5,by=5,to=length(LinkType))]
      
      
      
    }
  }
  
  #Getting Reporting Variables
  #Number of subcatchment variables (currently 8 + number of pollutants).
  Status$NumofSubCatchVariables = readBin(BinaryFile, integer(), n = 1, size = 4)
  #Code number of each subcatchment variable
  #0 for rainfall (in/hr or mm/hr),
  #1 for snow depth (in or mm),
  #2 for evaporation loss (in/day or mm/day),
  #3 for infiltration losses (in/hr or mm/hr),
  #4 for runoff rate (flow units),
  #5 for groundwater outflow rate (flow units),
  #6 for groundwater water table elevation (ft or m),
  #7 for unsaturated zone moisture content (fraction)
  #8 for runoff concentration of first pollutant,
  #...
  #7 + N for runoff concentration of N-th pollutant.
  Status$CodeNumSubCatch = readBin(BinaryFile, integer(), n = Status$NumofSubCatchVariables, size = 4)
  #Number of node variables (currently 6 + number of pollutants)
  Status$NumNodesVariables = readBin(BinaryFile, integer(), n = 1, size = 4)
  #Code number of each node variable
  #0 for depth of water above invert (ft or m),
  #1 for hydraulic head (ft or m),
  #2 for volume of stored + ponded water (ft3 or m3),
  #3 for lateral inflow (flow units),
  #4 for total inflow (lateral + upstream) (flow units),
  #5 for flow lost to flooding (flow units),
  #6 for concentration of first pollutant,
  #...
  #5 + N for concentration of N-th pollutant.
  Status$CodeNumNode = readBin(BinaryFile, integer(), n = Status$NumNodesVariables, size = 4)
  #Number of link variables (currently 5 + number of pollutants)
  Status$NumLinksVariables = readBin(BinaryFile, integer(), n = 1, size = 4)
  #Code number of each link variable:
  #0 for flow rate (flow units),
  #1 for flow depth (ft or m),
  #2 for flow velocity (ft/s or m/s),
  #3 for flow volume (ft3 or m3)
  #4 for fraction of conduit's area filled or setting for non-conduits
  #5 for concentration of first pollutant,
  #...
  #4 + N for concentration of N-th pollutant.
  Status$CodeNumLink = readBin(BinaryFile, integer(), n = Status$NumLinksVariables, size = 4)
  #Number of system-wide variables (currently 14)
  Status$NumSystemVariables = readBin(BinaryFile, integer(), n = 1, size = 4)
  #Code number of each system-wide variable:
  #0 for air temperature (deg. F or deg. C),
  #1 for rainfall (in/hr or mm/hr),
  #2 for snow depth (in or mm),
  #3 for evaporation + infiltration loss rate (in/hr or mm/hr),
  #4 for runoff flow (flow units),
  #5 for dry weather inflow (flow units),
  #6 for groundwater inflow (flow units),
  #7 for RDII inflow (flow units),
  #8 for user supplied direct inflow (flow units),
  #9 for total lateral inflow (sum of variables 4 to 8) (flow units),
  #10 for flow lost to flooding (flow units),
  #11 for flow leaving through outfalls (flow units),
  #12 for volume of stored water (ft3 or m3),
  #13 for evaporation rate (in/day or mm/day)
  Status$CodeNumSystems = readBin(BinaryFile, integer(), n = Status$NumSystemVariables, size = 4)
  #Getting Reporting Interval
  Status$BytesPerPeriod= 2*4 +(Status$SubCatchNum*(Status$NumofSubCatchVariables) + 
                                 Status$NodesNum*(Status$NumNodesVariables) +
                                 Status$LinksNum*(Status$NumLinksVariables) + Status$NumSystemVariables)*4;
  seek(BinaryFile,-3*4,"end")
  Status$ReportingPeriods= readBin(BinaryFile, integer(), n = 1, size = 4)
  Status$BinaryFile = BinaryFile
  seek(BinaryFile,-4*4,"end")
  Status$ComputedResults= readBin(BinaryFile, integer(), n = 1, size = 4) 
  #Writing function to get table of computed results
  #ObjectType is the type of the Object that you want to see the result (i.e., Subcatchment, node, conduit, or system)
  #Object ID is the name of object to see the result (i.e., S1 or J2)
  #Index is the position of one object among other objects
  #Codenum is the code number of each variable from abover list
  #Time period
  
  
  return(Status)
}
getSWMMResult<-function(headObj,iType,iIndex,vIndex,period){  
  SUBCATCH=0
  NODE     = 1;
  LINK     = 2;
  SYS      = 3;
  
  
  f=headObj$BinaryFile
  StartPos=headObj$ComputedResults
  
  off = StartPos + period*(headObj$BytesPerPeriod) + 2*4;
  if ( iType == SUBCATCH )
  {
    off = off+ 4*(iIndex*(headObj$NumofSubCatchVariables) + vIndex);
  }
  else if (iType == NODE)
  {
    off = off+ 4*(headObj$SubCatchNum*(headObj$NumofSubCatchVariables) +
                    iIndex*(headObj$NumNodesVariables) + vIndex);
  }
  else if (iType == LINK)
  {
    off = off+ 4*(headObj$SubCatchNum*(headObj$NumofSubCatchVariables) +
                    headObj$NodesNum*(headObj$NumNodesVariables) +
                    iIndex*(headObj$NumLinksVariables) + vIndex);
  }
  else if (iType == SYS)
  {
    off = off+ 4*(headObj$SubCatchNum*(headObj$NumofSubCatchVariables) +
                    headObj$NodesNum*(headObj$NumNodesVariables) +
                    headObj$LinksNum*(headObj$NumLinksVariables) + vIndex);
    
  }
  
  seek(f,off,"start")
  Status=readBin(f,what="double",size=4,n=1)
  return(Status)
} 
getSWMMTimes<-function(headObj){
  #gets the time stamps of the SWMM results in binary file
  f=headObj$BinaryFile
  seek(f,headObj$ComputedResults,"start")
  
  headObj$SWMMTimes<-array(NaN,headObj$ReportingPeriods)
  if(headObj$ReportingPeriods>0){
    for(i in 1:headObj$ReportingPeriods){
      
      headObj$SWMMTimes[i]<-readBin(f,what="double",size=8,n=1)
      #      if(i<100){
      #       print(headObj$SWMMTimes[i])
      #    }
      seek(f,headObj$BytesPerPeriod-8,"current")
    }
  }else{
    stop("No time steps listed in SWMM output file.")
    
  }
  #Convert SWMM times to R POSIXlt datetimes
  headObj$SWMMTimes<-headObj$SWMMTimes*86400.0+as.POSIXct(strptime("12/30/1899", format="%m/%d/%Y",tz="GMT"))#edit 2/10/2012 to force GMT time zone rather than locale specific
  return(headObj)
}
getSWMMTimeSeriesData<-function(headObj,iType,nameInOutputFile,vIndex){
  
  if(iType==0){
    iIndex=(0:(headObj$SubCatchNum-1))[headObj$SubCatchName==nameInOutputFile]
  }else if(iType==1){
    iIndex=(0:(headObj$NodesNum-1))[headObj$NodesName==nameInOutputFile]  
  }else if(iType==2){
    iIndex=(0:(headObj$LinksNum-1))[headObj$LinksName==nameInOutputFile]
  }else if(iType==3){
    
    iIndex=0
  }
  Status=array(NaN,headObj$ReportingPeriods)
  for(period in 0:(-1+headObj$ReportingPeriods)){
    #browser()
    Status[period+1]=getSWMMResult(headObj=headObj,iType=iType,iIndex=iIndex,vIndex=vIndex,period=period)
  }
  return(Status)
}
TimeSeries<-function(Q, headObj){
f= data.frame(Q)
f$hours = c(1:headObj$ReportingPeriods)
require (ggplot2)
p <- ggplot(f, aes(x = hours, y = Q)) + geom_line(color="red", size=1.5)+ scale_x_log10()+ scale_y_log10()
p2<-p + ggtitle("Timeseries")+labs(x="Time",y="Flow(cfs)")
print(p)
return(p)
}
Exceedence1<-function(Q,headObj){
  y=data.frame(Q)
  row_sub = apply(y, 1, function(row) all(row !=0 ))
  h = y[row_sub,]
  rank  <- rank(h, ties.method="min")
  exceedtime <- 100 * (rank / (length(h)+1))
  m=data.frame(exceedtime)
  m$Probability= sort(exceedtime, decreasing = T)
  m$Flow= sort(h)
  require (ggplot2)
  p2 <- ggplot(m, aes(x = Probability, y = Flow)) + geom_line(color="blue", size=1.5)+ scale_x_log10()+ scale_y_log10()
  p3<-p2 + ggtitle("Duration Curve for Simulated")+labs(x="Probability(%)",y="Flow(cfs)")
  print(p3)
  return(p3)
} 
Exceedence2<-function(CalibrationData,headObj){
  #wkObs = loadWorkbook("C:\\Users\\Alamdari\\Research\\R-SWMM Test Files\\Observed.xlsx") 
  CalibrationData = readWorksheet(wkObs, sheet="Observed")
  x=data.frame(CalibrationData)
  rank  <- rank(x$Observed, ties.method="min")
  exceedtime <- 100 * (rank / (nrow(x)+1))
  m=data.frame(exceedtime)
  m$Probability= sort(exceedtime, decreasing = T)
  m$Flow= sort(x$Observed)
  require (ggplot2)
  p2 <- ggplot(m, aes(x = Probability, y = Flow)) + geom_line(color="red", size=1.5)+ scale_x_log10()+ scale_y_log10()
  p3<-p2 + ggtitle("Duration Curve for Observed")+labs(x="Probability(%)",y="Flow(cfs)")
  print(p3)
  return(p3)
} 

# SimulatedData<-function(Q,headObj){
#   Date = headObj$SWMMTimes
#   SimulatedData<-{}
#   SimulatedData$Times= data.frame(Date)
#   SimulatedData$Flow = data.frame(Q)
#   #write.xlsx(SimulatedData,"C:\\Users\\Alamdari\\Research\\R-SWMM Test Files\\Simulated.xlsx",sheet="TestSheet")
#   return(SimulatedData)
# }
# Calculate Runoff Volume
# RunoffVolume<-function(Average){
#   library(readxl)
#   Rainfall = read_excel("C:\\Users\\david\\Research\\DifficultRun_18Subs\\Washington_Hourly_2013.xlsx", sheet="Rainfall_Raw")# load XLConnect package 
#   SimObsDate = data.frame(Average) 
#   A = as.character.Date(Rainfall[,1])
#   m = str_sub(A,12,19)
#   RainfallDate = as.POSIXct(paste(Rainfall[,1], m), format="%Y-%m-%d %H:%M:%S",tz="GMT")
#   Num1 = as.integer(RainfallDate)
#   left = str_sub(Num1,9,10)
#   left = as.numeric(left)
#   for (i in 1:length(left)){
#     if(left[i]==99){
#       Num1[i] = Num1[i]+ 1}
#   }
#   Num2 = as.integer(Average$Date)
#   Events = {}
#   Events$Date = SimObsDate$Date
#   k=1
#   for (i in 1:nrow(SimObsDate)){
#     Events$Sim[k]= 0
#     Events$Obs[k]=0
#     Events$Rain[k] = 0 
#     
#     k=k+1
#   }
#   
#   k=1
#   j=1
#   
#   for (i in 1:nrow(SimObsDate)){
#     for (j in 1:nrow(Rainfall)){
#       if (Num2[i]-Num1[j]==0){
#         Events$Rain[i]= Rainfall$Rainfall[j]
#         Events$Sim[i]= SimObsDate$Sim[i]
#         Events$Obs[i]= SimObsDate$Obs[i]
#         Events$Date[i] = SimObsDate$Date[i]
#       }
#       
#     }
#   }
#   
#   J=Events$Rain
#   H = Events$Sim
#   I = Events$Obs
#   X= Events$Date
#   ReadEventsFile = data.frame(Events)
#   
#   max=0
#   count=0
#   set=0
#   for (i in 1:nrow(ReadEventsFile)){
#     if(J[i]>=0.1){
#       count=count+1
#       if (count>max){
#         max=count
#       }
#       if (J[i+1]<0.1){
#         set=set+1
#       }
#     }
#     else{
#       count=0
#     }
#   }
#   mm <- matrix(0, max, set)
#   nn <-matrix(0, max, set)
#   rr <-matrix(0, max+8, set)
#   xx<-matrix(0,max,set)
#   k=1
#   m=1
#   j=1
#   
#   while (m!=(set+1)){
#     if (J[k]>=0.1){
#       mm[j,m]=J[k]
#       nn[j,m]=H[k]
#       rr[j,m]=I[k]
#       xx[j,m]=X[k]
#       j=j+1
#       if (J[k+1]<0.1){
#         m=m+1
#         j=1
#       }
#       k=k+1 
#     }
#     
#     else{
#       k=k+1
#     }
#   }
#   Sum = {}
#   Sum$Sim = colSums(nn)
#   Sum$Obs = colSums(rr)
#   Format= as.POSIXct(xx, origin = "1970-01-01", tz= "GMT")
#   Date = as.numeric(Format)
#   a = {}
#   b= {}
#   k=1
#   m=1
#   for (i in 1:set){
#     a[k] = xx[1,i]
#     k=k+1
#   }
#   m=2
#   k=1
#   for (i in 1:set){
#     for (m in 2:(max)){
#       if ((xx[m,i] == 0) & (xx[m-1,i]!=0)) {
#         b[k]=xx[m-1,i]
#         
#       }
#       if ((xx[m,i]!=0)){
#         b[k]=xx[m,i]
#       }
#       
#     }
#     k=k+1
#   }
# 
#   RunoffVolume = {}
#   RunoffVolume$Sim = Sum$Sim
#   RunoffVolume$Obs = Sum$Obs
#   return (RunoffVolume)
# }
#Hourly Calibration
Aggregate1<-function(Q){
  #define wkObs
  Sim <- aggregate(Q, 
                   list(hour=cut(as.POSIXct(headObj$SWMMTimes, format="%Y-%m-%d %H:%M:%S",tz="GMT"), "hour")),
                   mean)
  #wkObs = loadWorkbook("2016_obs_watershed_outflow.xlsx") 
  library(readxl)
  CalibrationData = read_excel("Observed.xlsx")
  #Creat Observed Data in the same format of Simulated
  Obs <- aggregate(CalibrationData$Flow, 
                   list(hour=cut(as.POSIXct(CalibrationData$Date, format="%Y-%m-%d %H:%M:%S",tz="GMT"), "hour")),
                   mean)
  Average = {}
  Average$Date = Obs$hour
  Average$Sim = Sim$x
  Average$Obs = Obs$x

  return(Average)
}
#Create your rainfall data date same as writesimulatedObservedData File
#Calibration for the events
CalculateEvent<-function(Average){
  library(stringi)
 library(stringr)
  library(lubridate)
  Rainfall = read_excel("Rainfall_Raw.xlsx")
  #wkSimObsDate = loadWorkbook("C:\\Users\\Alamdari\\Research\\R-SWMM Test Files\\WriteSimulatedObserved.xlsx")
  SimObsDate = data.frame(Average) 
  A = as.character.Date(Rainfall[,1])
  RainfallDate = as.POSIXct(A, format="%Y-%m-%d %H:%M:%S",tz="GMT")
  t <- strftime(RainfallDate, format="%H:%M:%S")
  a <- hms(as.character(t))
  minute = minute(a)
  
  for (i in 1: length(RainfallDate)){
    if(minute[i]==59){
      RainfallDate[i]= RainfallDate[i]+1
    }
  }
  
  Average$Date = as.POSIXct(Average$Date, format="%Y-%m-%d %H:%M:%S",tz="GMT")
  Num1 = as.integer(RainfallDate)
  Num2 = as.integer(Average$Date)
  Events = {}
  Events$Date = SimObsDate$Date
  k=1
  for (i in 1:nrow(SimObsDate)){
    Events$Sim[k]= 0
    Events$Obs[k]=0
    Events$Rain[k] = 0 
    
    k=k+1
  }
  
  k=1
  j=1
  for (j in 1:nrow(Rainfall)){
    for (i in 1:nrow(SimObsDate)){
      if (Num2[i]-Num1[j]==0){
        Events$Rain[i]= Rainfall$Rainfall[j]
        Events$Sim[i]= SimObsDate$Sim[i]
        Events$Obs[i]= SimObsDate$Obs[i]
        k=k+1
      }
      
    }
  }
  
  J=Events$Rain
  H = Events$Sim
  I = Events$Obs
  X= Events$Date
  ReadEventsFile = data.frame(Events)
  
  max=0
  count=0
  set=0
  for (i in 1:nrow(ReadEventsFile)){
    if(J[i]>=0.1){
      count=count+1
      if (count>max){
        max=count
      }
      if (J[i+1]<0.1){
        set=set+1
      }
    }
    else{
      count=0
    }
  }
  mm <- matrix(0, max, set)
  nn <-matrix(0, max, set)
  rr <-matrix(0, max, set)
  xx<-matrix(0,max,set)
  k=1
  m=1
  j=1
  while (m!=(set+1)){
    if (J[k]>=0.1){
      mm[j,m]=J[k]
      nn[j,m]=H[k]
      rr[j,m]=I[k]

      xx[j,m]=X[k]
      j=j+1
      if (J[k+1]<0.1){
        m=m+1
        j=1
      }
      k=k+1 
    }
    
    else{
      k=k+1
    }
  }
  Maxmm <- matrix(0, 1,set)
  Maxnn <-matrix(0, 1,set)
  Maxrr <-matrix(0, 1,set)
  Maxxx <-matrix(0, 1,set)
  k=1
  for (i in 1:set){
    Maxmm[k,i]= max(apply(mm[,(i),drop=FALSE] ,2,max))
    Maxnn[k,i]= max(apply(nn[,(i),drop=FALSE] ,2,max))
    Maxrr[k,i]= max(apply(rr[,(i),drop=FALSE] ,2,max))
    Maxxx[k,i]= xx[1,i]
    i=i+1
  }
  
  FormatDate= as.POSIXct(Maxxx, origin = "1970-01-01", tz= "GMT")
  
#   
#   Char = as.character.Date(FormatDate)
#   #Months = str_sub(Char,1,7)
#   Days = str_sub(Char,1,10)
#   #NumDays = as.integer(Days)
#   Hours = str_sub(Char,12,13)
#   NumHours = as.integer(Hours)
#   
#   set2= 1
#   for (j in 1:set){
#     if (identical(Days[j+1],Days[j], num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
#                   ignore.bytecode = TRUE, ignore.environment = FALSE)==TRUE & (NumHours[j+1]- NumHours[j])<=(6+max)){ # use str_sub
#       set2=set2
#     }
#     else{
#       set2=set2+1  
#     }
#   }
#   Maxr = matrix(0, set+set,set2+set2)
#   Maxn = matrix(0, set+set,set2+set2)
#   Maxm = matrix(0, set+set,set2+set2)
#   k=1
#   m=1
#   j=1
#   for (j in 1:1){
#     if (identical(Days[j+1],Days[j], num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
#                   ignore.bytecode = TRUE, ignore.environment = FALSE)==TRUE & (NumHours[j+1]- NumHours[j])<=(6+max)){ # use str_sub
#       Maxm[k,m] = max(Maxmm[(j):(j+1)])
#       Maxn[k,m] = max(Maxnn[(j):(j+1)])
#       Maxr[k,m] = max(Maxrr[(j):(j+1)])
#       k=k+1
#     }
#     else{
#       Maxm[k,m]= Maxmm[j]
#       Maxn[k,m]= Maxnn[j]
#       Maxr[k,m]= Maxrr[j]
#       m=m+1
#       k=k+1
#     }
#   }
#   
#   for (j in 2:(set)){
#     if (identical(Days[j+1],Days[j], num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
#                   ignore.bytecode = TRUE, ignore.environment = FALSE)==TRUE & (NumHours[j+1]- NumHours[j])<=(6+max)){ # use str_sub
#       
#       
#       Maxm[k,m] = max(Maxmm[(j):(j+1)])
#       Maxn[k,m] = max(Maxnn[(j):(j+1)])
#       Maxr[k,m] = max(Maxrr[(j):(j+1)])
#       k=k+1
#       
#       
#     }
#     
#     else{
#       if (j==2){
#         m=m+1
#         #Maxm[k]= max(Maxmm[i])
#         # Maxn[k]= max(Maxnn[i])
#         #Maxr[k]= max(Maxrr[i])
#         #Max[k]= Maxxx[i]
#         Maxm[k,m]= Maxmm[j]
#         Maxn[k,m]= Maxnn[j]
#         Maxr[k,m]= Maxrr[j]
#         k=k+1
#         m=m+1
#       }
#       else{
#         m=m+1
#         #Maxm[k]= max(Maxmm[i])
#         # Maxn[k]= max(Maxnn[i])
#         #Maxr[k]= max(Maxrr[i])
#         #Max[k]= Maxxx[i]
#         Maxm[k,m]= Maxmm[j+1]
#         Maxn[k,m]= Maxnn[j+1]
#         Maxr[k,m]= Maxrr[j+1]
#         k=k+1
#         m=m+1
#       }
#       
#     }
#     
#     
#     #mydf <- data.frame(mm[i+1],mm[i])
#     #sdf <- stack(mydf)
#     #uni <- unique(sdf[, "values"])
#   }
#   Maxm[is.na(Maxm)] <- 0
#   Maxn[is.na(Maxn)] <- 0
#   Maxr[is.na(Maxr)] <- 0
#   
#   
#   MaxmNew = matrix(0, 1,(set2+set2))
#   MaxnNew = matrix(0, 1,(set2+set2))
#   MaxrNew = matrix(0, 1,(set2+set2))
#   
#   k=1
#   for (i in 1:(set2+set2)){
#     MaxmNew[k,i]= max(apply(Maxm[,(i),drop=FALSE] ,2,max))
#     MaxnNew[k,i]= max(apply(Maxn[,(i),drop=FALSE] ,2,max))
#     MaxrNew[k,i]= max(apply(Maxr[,(i),drop=FALSE] ,2,max))
#     i=i+1
#   }
  MaxmNew = t(Maxmm)  
  MaxnNew = t(Maxnn) 
  MaxrNew = t(Maxrr)
  MaxmNew = data.frame(MaxmNew)
  MaxnNew = data.frame(MaxnNew)
  MaxrNew = data.frame(MaxrNew)
  EventsNew = {}
  EventsNew$Rain =  MaxmNew$MaxmNew
  EventsNew$Sim =  MaxnNew$MaxnNew
  EventsNew$Obs =  MaxrNew$MaxrNew
  Dataframe = data.frame(EventsNew)
  EventsNew = Dataframe[apply(Dataframe[,-1], 1, function(x) !all(x==0)),]
 
  return(EventsNew)
 
}



#Calculating summary statistics

PerfomrmStatistic2<-function(EventsNew){
  require(hydroGOF)
  sim2 = EventsNew$Sim
  obs2 = EventsNew$Obs
  MeanError_2 = me(sim2,obs2)
  MeanSquaredError_2 = mse(sim2,obs2)
  IndexAgreement = d(sim2,obs2)
  IndexAgreementTimesMinus1_2 = -1*IndexAgreement
  Nashsutcliffe2 = NSE(sim2, obs2)
  NashsutcliffeTimesMinus1_2 = -1*Nashsutcliffe2
  
  PercentBias2 = pbias(sim2,obs2)
  
  linearCorrelation2 =cor(sim2,obs2)
  linearCorrelationTimesMinus1_2 = -1*linearCorrelation2
  output2=data.frame(MeanError_2,MeanSquaredError_2,IndexAgreementTimesMinus1_2,NashsutcliffeTimesMinus1_2,PercentBias2,linearCorrelationTimesMinus1_2)
  return(output2)
}

# Creating input files with uncertain parameters and name it as $1$ $2$ $3$
ReadSWMMOptFile<-function(SWMMOptFile){
  SWMMOpt=readLines(con = SWMMOptFile, n = -1L, ok = TRUE, warn = TRUE,
                    encoding = "unknown")
  return(SWMMOpt)
}

#Replace Optimization parameters to the Input File

OptimizationFile<-function(Optimization){
  Optimization = read.csv(file=Optimization, header = TRUE, sep = ",", quote="\"", dec=".",
                          fill = TRUE, comment.char="",stringsAsFactors = FALSE)
  parameters = Optimization[,2]
  replacementCodes = Optimization[,1]
  return (Optimization)
}

replaceCodesInTemplateFile<-function(SWMMOpt,parameters, replacementCodes,File){
  #Optimization = read.csv(file=Optimization, header = TRUE, sep = ",", quote="\"", dec=".",
  #fill = TRUE, comment.char="",stringsAsFactors = FALSE)
  #parameters = Optimization[,2]
  #replacementCodes = Optimization[,1]
  
  for(i in 1:length(parameters)){
    
    SWMMOpt=sub(replacementCodes[i], parameters[i], SWMMOpt,fixed=TRUE)
  }
  writeLines(SWMMOpt, con = File, sep = "\n", useBytes = FALSE)
  
  return(SWMMOpt)
}

#Defining Minim, initial, and Maximum Vallues for uncertain parameters
ParametersBound<-function(ParametersFile){
  Bounds = read.csv(file=ParametersFile, header = TRUE, sep = ",", quote="\"", dec=".",
                    fill = TRUE, comment.char="",stringsAsFactors = FALSE)
  #initial = Bounds$Initial
  #Minimum = Bounds$Minimum
  #Maximum = Bounds$Maximum
  
  return(Bounds)
}

Objectivefunction<-function(SWMMOptFile,x,OutFile,swmm,Timeseries,StatParameters){
  SWMMOpt= ReadSWMMOptFile(SWMMOptFile)
  
  Input=paste(OutFile,iteration,'.inp',sep="")
  ReplaceCodes<<- replaceCodesInTemplateFile(SWMMOpt,x,as.matrix(Bounds["Code"]),Input)
  Report=paste(OutFile,iteration,'.rpt',sep="")
  Output=paste(OutFile,iteration,'.out',sep="")
  swmm= "C:\\Program Files (x86)\\EPA SWMM 5.1\\swmm5.exe"
  SWMMExe(swmm,Input,Report,Output)
  headObj = GetObjectsSWMM (Output)
  headObj = getSWMMTimes (headObj)
  Q = getSWMMTimeSeriesData(headObj,iType = 1,nameInOutputFile = "J11510.66",vIndex = 4)
  #TimeSeries = TimeSeries(Q, headObj)
  #Exceedence = Exceedence(Q, headObj)
  # SimulatedData = SimulatedData(Q, headObj)
  Average = Aggregate1(Q)
  # Average = read.zoo(data.frame(Average))
  EventsNew = CalculateEvent(Average)
  require(hydroGOF)
  sim2 = EventsNew$Sim
  obs2 = EventsNew$Obs
  MeanError_2 = me(sim2,obs2)
  MeanSquaredError_2 = mse(sim2,obs2)
  IndexAgreement = d(sim2,obs2)
  IndexAgreementTimesMinus1_2 = -1*IndexAgreement
  Nashsutcliffe2 = NSE(sim2, obs2)
  NashsutcliffeTimesMinus1_2 = -1*Nashsutcliffe2
  
  PercentBias2 = pbias(sim2,obs2)
  
  linearCorrelation2 =cor(sim2,obs2)
  linearCorrelationTimesMinus1_2 = -1*linearCorrelation2
  library(hydroGOF)
  ggof(EventsNew$Sim, EventsNew$Obs, na.rm = TRUE, pt.style = "ts", ftype = "o", stype="default", 
       gof.leg = TRUE,  digits=2, 
       gofs=c("me","RMSE","PBIAS","NSE","d","R2"),xlab = "Time", ylab=c("Q[cfs]"))
  
  
  output1=data.frame(NashsutcliffeTimesMinus1_2,PercentBias2,linearCorrelationTimesMinus1_2)
  
  library(xlsx)
  
  for (i in iteration){
    write.xlsx(output1,paste("Combinations",i,".xlsx",sep=""))}
  
  SummaryStatistics1 = PerfomrmStatistic2(EventsNew)
  perfStatsToUse=as.numeric(SummaryStatistics1[StatParameters])
  summaryRow=unlist(c(iteration,x,perfStatsToUse))
  #write.xlsx(summaryRow[3:5],"summaryRow.xlsx",sheetName = paste("iteration",iteration),append = TRUE)
  names(summaryRow)= c("iteration",paste("Parameter",t(Bounds["Code"]),sep=""),StatParameters)
  iteration<<-iteration+1
  print(summaryRow)
  return (summaryRow)
}

OptimizationFunction<-function(SWMMOptFile,OutFile,swmm,Timeseries,StatParameters,initial,lower,upper){
  optimOpt={}
  optimOpt$OutFile = OutFile
  optimOpt$SWMMOptFile = SWMMOptFile
  optimOpt$swmm = swmm
  optimOpt$Timeseries = 'getSWMMTimeSeriesData(headObj=headObj,iType = 1,nameInOutputFile = "J11510.66",vIndex = 4)'
  optimOpt$StatParameters = StatParameters
  optimOpt$lower = lower
  optimOpt$upper = upper
  library(mco)
  out= nsga2(Objectivefunction,
             idim=length(optimOpt$lower),
             odim=length(optimOpt$StatParameters),
             OutFile= optimOpt$OutFile,
             SWMMOptFile = optimOpt$SWMMOptFile,
             swmm = optimOpt$swmm,
             Timeseries = optimOpt$Timeseries,
             StatParameters = optimOpt$StatParameters,
             generations=200,
             lower.bounds=as.double(optimOpt$lower),
             upper.bounds=as.double(optimOpt$upper),
             constraints=NULL,popsize = 1000)
}

