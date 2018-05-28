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

SimulatedData<-function(Q,headObj){
  Date = headObj$SWMMTimes
  SimulatedData<-{}
  SimulatedData$Times= data.frame(Date)
  SimulatedData$Flow = data.frame(Q)
  #write.xlsx(SimulatedData,"C:\\Users\\Alamdari\\Research\\R-SWMM Test Files\\Simulated.xlsx",sheet="TestSheet")
  return(SimulatedData)
}
#Aggregating 15 min runoff
DurationCurves<-function(SimulatedData){
  SimulatedDat = data.frame(SimulatedData)
  Simulated = {}
  Simulated$Date = SimulatedDat$Date
  Simulated$Q = SimulatedDat$Q
  Aggregate = {}
  Aggregate$Date2 = Simulated$Date
  Aggregate$Sim = Simulated$Q
  tmp2 <- sapply(Aggregate$Date2, as.character)
  library(stringr)
  Hours2 = str_sub(tmp2,12,13)
  NumHours2 = as.integer(Hours2)
  
  sum={}
  Average = {}
  Mean = {}
  i=1
  j=1
  k=1
  sum$Sim[k] = 0
  while(i <=(length(Aggregate$Sim))-1){
    if (NumHours2[i+1]-NumHours2[i]==0){
      sum$Sim[k] = sum$Sim[k] + Aggregate$Sim[i]
      Mean$Sim[j]=Aggregate$Sim[i]
      i=i+1
      j=j+1
      Average$Sim[k]=sum$Sim[k]/(length(Mean$Sim[1:j]))
    }
    if (is.na(NumHours2[i+1]-NumHours2[i])==TRUE){
      sum$Sim[k] = sum$Sim[k] + Aggregate$Sim[i]
      Mean$Sim[j]=Aggregate$Sim[i]
      i=i+1
      j=j+1
      Average$Sim[k]=sum$Sim[k]/(length(Mean$Sim[1:j]))
    }
    if (NumHours2[i+1]-NumHours2[i]!=0 & is.na(NumHours2[i+1]-NumHours2[i])==FALSE){  
      sum$Sim[k] = sum$Sim[k] + Aggregate$Sim[i]+Aggregate$Sim[i+1]
      Mean$Sim[j]=Aggregate$Sim[i]
      Average$Sim[k]=sum$Sim[k]/(length(Mean$Sim[1:j]))
      i=i+1
      j=1
      k=k+1
      sum$Sim[k] = 0
    }
  }
  
  i=1
  k=1
  while(i <=(length(Aggregate$Date2)-1)){
    if (NumHours2[i+1]-NumHours2[i]!=0 & is.na(NumHours2[i+1]-NumHours2[i])==FALSE){
      Average$Date[k] = Aggregate$Date2[i+1]
      k=k+1
    }
    if (is.na(NumHours2[i+1]-NumHours2[i])==TRUE){
      Average$Date[k] = Aggregate$Date2[i-3]+ 3600
      k=k+1
    }
    
    i=i+1
    
  }
  
  Average$Date = as.POSIXct(Average$Date, origin = "1970-01-01",tz="GMT")
  return(Average)
}

#WriteSimulateObservedData = write.xlsx(Average, file = "C:\\Users\\Alamdari\\Research\\R-SWMM Test Files\\WriteSimulatedObserved.xlsx",
#sheetName = "TestSheet", row.names = FALSE)
# DurationCurvesPlotPeakFlow<-function(Average){
#   wkRain = loadWorkbook("C:\\Users\\Grad\\Research\\R-SWMM Test Files\\Rainfall_Raw.xlsx") 
#   Rainfall = readWorksheet(wkRain, sheet="Rainfall_Raw")# load XLConnect package 
#   #wkSimObsDate = loadWorkbook("C:\\Users\\Alamdari\\Research\\R-SWMM Test Files\\WriteSimulatedObserved.xlsx")
#   SimObsDate = data.frame(Average) 
#   A = as.character.Date(Rainfall[,1])
#   library(stringr)
#   m = str_sub(A,12,19)
#   RainfallDate = as.POSIXct(paste(Rainfall[,1], m), format="%Y-%m-%d %H:%M:%S",tz="GMT")
#   Num1 = as.integer(RainfallDate)
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
#   for (j in 1:nrow(Rainfall)){
#     for (i in 1:nrow(SimObsDate)){
#       if (Num2[i]-Num1[j]==0){
#         Events$Rain[i]= Rainfall$Rainfall[j]
#         Events$Sim[i]= SimObsDate$Sim[i]
#         k=k+1
#       }
#       
#     }
#   }
#   
#   J=Events$Rain
#   H = Events$Sim
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
#   xx<-matrix(0,max,set)
#   k=1
#   m=1
#   j=1
#   while (m!=(set+1)){
#     if (J[k]>=0.1){
#       mm[j,m]=J[k]
#       nn[j,m]=H[k]
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
#   
#   Maxmm <- matrix(0, 1,set)
#   Maxnn <-matrix(0, 1,set)
#   Maxxx <-matrix(0, 1,set)
#   k=1
#   for (i in 1:set){
#     Maxmm[k,i]= max(apply(mm[,(i),drop=FALSE] ,2,max))
#     Maxnn[k,i]= max(apply(nn[,(i),drop=FALSE] ,2,max))
#     Maxxx[k,i]= xx[1,i]
#     i=i+1
#   }
#   
#   FormatDate= as.POSIXct(Maxxx, origin = "1970-01-01", tz= "GMT")
#   
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
#       k=k+1
#     }
#     else{
#       Maxm[k,m]= Maxmm[j]
#       Maxn[k,m]= Maxnn[j]
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
#       k=k+1
#       
#       
#       #Maxm[k]= max(Maxmm[(i):(i+1)])
#       # Index1 = which.min(apply(Maxmm[(i):(i+1)],MARGIN=2,min))
#       #Maxmm = subset(Maxmm, select = -Index1)
#       #Index2 = which.max(apply(Maxmm[(i):(i+1)],MARGIN=2,max))
#       
#       
#       #Maxn[k]= max(Maxnn[(i):(i+1)])
#       #Index2 = which.min(apply(Maxnn[(i):(i+1)],MARGIN=2,min))
#       #Maxnn = subset(Maxnn, select = -Index2)
#       
#       
#       
#       #Index3 = which.max(apply(Maxrr[(i):(i+1)],MARGIN=2,min))
#       # Maxl = subset(Maxrr[i:i+1], select = -Index3)
#       #Maxr[k]= max(Maxrr[(i):(i+1)])
#       #maxl[k] = Maxl
#       
#       
#       
#       #Maxx[k]= Maxxx[i+1]
#       #k=k
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
#   
#   
#   MaxmNew = matrix(0, 1,(set2+set2))
#   MaxnNew = matrix(0, 1,(set2+set2))
#   
#   k=1
#   for (i in 1:(set2+set2)){
#     MaxmNew[k,i]= max(apply(Maxm[,(i),drop=FALSE] ,2,max))
#     MaxnNew[k,i]= max(apply(Maxn[,(i),drop=FALSE] ,2,max))
#     i=i+1
#   }
#   MaxmNew = t(MaxmNew)  
#   MaxnNew = t(MaxnNew) 
#   MaxmNew = data.frame(MaxmNew)
#   MaxnNew = data.frame(MaxnNew)
#   EventsNew = {}
#   EventsNew$Rain =  MaxmNew$MaxmNew
#   EventsNew$Sim =  MaxnNew$MaxnNew
#   Dataframe = data.frame(EventsNew)
#   EventsNew = Dataframe[!(apply(Dataframe, 1, function(y) any(y == 0))),]
#   rank  <- rank(EventsNew$Sim, ties.method="min")
#   exceedtime <- 100 * (rank / (length(EventsNew$Sim)+1))
#   m=data.frame(exceedtime)
#   m$Probability= sort(exceedtime, decreasing = T)
#   m$Flow= sort(EventsNew$Sim)
#   require (ggplot2)
#   p2 <- ggplot(m, aes(x = Probability, y = Flow)) + geom_line(color="blue", size=1.5)+ scale_x_log10()+ scale_y_log10()
#   p3<-p2 + ggtitle("Duration Curve for Simulated")+labs(x="Probability(%)",y="Flow")
#   print(p3)
#   return(Average)
# }
# Pollutants<-function(Average){
#   wkRain = loadWorkbook("C:\\Users\\Grad\\Research\\R-SWMM Test Files\\Rainfall_Raw.xlsx") 
#   Rainfall = readWorksheet(wkRain, sheet="Rainfall_Raw")# load XLConnect package 
#   #wkSimObsDate = loadWorkbook("C:\\Users\\Alamdari\\Research\\R-SWMM Test Files\\WriteSimulatedObserved.xlsx")
#   SimObsDate = data.frame(Average) 
#   A = as.character.Date(Rainfall[,1])
#   library(stringr)
#   m = str_sub(A,12,19)
#   RainfallDate = as.POSIXct(paste(Rainfall[,1], m), format="%Y-%m-%d %H:%M:%S",tz="GMT")
#   Num1 = as.integer(RainfallDate)
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
#   for (j in 1:nrow(Rainfall)){
#     for (i in 1:nrow(SimObsDate)){
#       if (Num2[i]-Num1[j]==0){
#         Events$Rain[i]= Rainfall$Rainfall[j]
#         Events$Sim[i]= SimObsDate$Sim[i]
#         k=k+1
#       }
#       
#     }
#   }
#   
#   J=Events$Rain
#   H = Events$Sim
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
#   xx<-matrix(0,max,set)
#   k=1
#   m=1
#   j=1
#   while (m!=(set+1)){
#     if (J[k]>=0.1){
#       mm[j,m]=J[k]
#       nn[j,m]=H[k]
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
#   k=1
#   Pollutant = matrix(0,max-1,set)
#   for (j in 1:set){
#     for (i in 1:(max-1)){
#       if (nn[i+1,j]-nn[i,j]!=0){
#         Pollutant[k,j] = abs((nn[i+1,j]-nn[i,j])/2)
#         k=k+1
#       }
#       if (nn[i+1,j]-nn[i,j]==0){
#         Pollutant[k,j] = 2*nn[i,j]
#         k=k+1
#       }
#     }
#     j=j+1
#     k=1
#     
#   }
#   Sum = colSums( Pollutant)
#   rank  <- rank(Sum, ties.method="min")
#   exceedtime <- 100 * (rank / (length(Sum)+1))
#   m=data.frame(exceedtime)
#   m$Probability= sort(exceedtime, decreasing = T)
#   m$Flow= sort(Sum)
#   require (ggplot2)
#   p2 <- ggplot(m, aes(x = Probability, y = Flow)) + geom_line(color="blue", size=1.5)+ scale_x_log10()+ scale_y_log10()
#   p3<-p2 + ggtitle("Duration Curve for Simulated")+labs(x="Probability(%)",y="TSS(mg/l)")
#   print(p3)
#   return (Sum) 
# }
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
#   # A = as.POSIXct(a, origin = "1970-01-01", tz= "GMT")
#   # Char3 = as.character.Date(A)
#   # #Months = str_sub(Char,1,7)
#   # Days3 = str_sub(Char3,1,10)
#   # #NumDays = as.integer(Days)
#   # Hours3 = str_sub(Char3,12,13)
#   # NumHours3 = as.integer(Hours3)
#   # B = as.POSIXct(b, origin = "1970-01-01", tz= "GMT")
#   # Char4 = as.character.Date(B)
#   # #Months = str_sub(Char,1,7)
#   # Days4 = str_sub(Char4,1,10)
#   # #NumDays = as.integer(Days)
#   # Hours4 = str_sub(Char4,12,13)
#   # NumHours4 = as.integer(Hours4)
#   # Difference = b-a
#   # NumToDate1 = as.POSIXct(Difference, origin = "1970-01-01", tz= "GMT")
#   # Char1 = as.character.Date(NumToDate1)
#   # #Months = str_sub(Char,1,7)
#   # Days1 = str_sub(Char1,1,10)
#   # #NumDays = as.integer(Days)
#   # Hours1 = str_sub(Char1,12,13)
#   # NumHours1 = as.integer(Hours1)
#   # basetime = {}
#   # k=1
#   # for(i in 1:length(NumHours1)){
#   #   if(NumHours1[i]==0){
#   #     basetime[k]= 1
#   #     k=k+1
#   #   }
#   #   else{
#   #     basetime[k]=NumHours1[i]
#   #     k=k+1
#   #   }
#   # }
#   RunoffVolume = {}
#   RunoffVolume$Sim = Sum$Sim
#   RunoffVolume$Obs = Sum$Obs
#   return (RunoffVolume)
# }
Aggregate1<-function(SimulatedData){
  #wkSim = loadWorkbook("C:\\Users\\Alamdari\\Research\\R-SWMM Test Files\\Simulated.xlsx") 
  #Simulated = readWorksheet(wkSim, sheet="TestSheet")# load XLConnect package 
  #Simulated = read.xlsx("C:\\Users\\Alamdari\\Research\\R-SWMM Test Files\\Simulated.xlsx",
  #sheetName = "TestSheet")
  #CalibrationData = read.xlsx("C:\\Users\\Alamdari\\Research\\R-SWMM Test Files\\Observed.xlsx",
  #sheetName = "Observed")
  SimulatedDat = data.frame(SimulatedData)
  Simulated = {}
  Simulated$Date = SimulatedDat$Date
  Simulated$Q = SimulatedDat$Q
  wkObs1 = loadWorkbook(wkObs) 
  CalibrationData = readWorksheet(wkObs1, sheet="Observed")
  #Creat Observed Data in the same format of Simulated
  
  
  Aggregate = {}
  Aggregate$Date1 = CalibrationData[,1]
  Aggregate$Date2 = Simulated$Date
  Aggregate$Sim = Simulated$Q
  Aggregate$Obs = CalibrationData[,2]
  tmp1 = as.character.Date(Aggregate$Date1)
  tmp2 <- sapply(Aggregate$Date2, as.character)
  library(stringr)
  Hours1 = str_sub(tmp1,12,13)
  NumHours1 = as.integer(Hours1)
  Hours2 = str_sub(tmp2,12,13)
  NumHours2 = as.integer(Hours2)
  sum={}
  Mean = {}
  Average = {}
  i=1
  k=1
  j=1
  sum$Sim[k] = 0
  while(i <=(length(Aggregate$Sim))-1){
    if (NumHours2[i+1]-NumHours2[i]==0){
      sum$Sim[k] = sum$Sim[k] + Aggregate$Sim[i]
      Mean$Sim[j]=Aggregate$Sim[i]
      i=i+1
      j=j+1
      Average$Sim[k]=sum$Sim[k]/(length(Mean$Sim[1:j]))
    }
    if (is.na(NumHours2[i+1]-NumHours2[i])==TRUE){
      sum$Sim[k] = sum$Sim[k] + Aggregate$Sim[i]
      Mean$Sim[j]=Aggregate$Sim[i]
      i=i+1
      j=j+1
      Average$Sim[k]=sum$Sim[k]/(length(Mean$Sim[1:j]))
    }
    if (NumHours2[i+1]-NumHours2[i]!=0 & is.na(NumHours2[i+1]-NumHours2[i])==FALSE){  
      sum$Sim[k] = sum$Sim[k] + Aggregate$Sim[i]+Aggregate$Sim[i+1]
      Mean$Sim[j]=Aggregate$Sim[i]
      Average$Sim[k]=sum$Sim[k]/(length(Mean$Sim[1:j]))
      i=i+1
      j=1
      k=k+1
      sum$Sim[k] = 0
    }
  }
  Average$Sim[k+1]=0
  i=1
  k=1
  sum$Obs[k] = 0
  while(i <=(length(Aggregate$Obs))-1){
    if (NumHours1[i+1]-NumHours1[i]==0){
      sum$Obs[k] = sum$Obs[k] + Aggregate$Obs[i]
      Mean$Obs[j]=Aggregate$Obs[i]
      i=i+1
      j=j+1
      
      Average$Obs[k]=sum$Obs[k]/(length(Mean$Obs[1:j]))
    }
    if (is.na(NumHours1[i+1]-NumHours1[i])==TRUE){
      sum$Obs[k] = sum$Obs[k] + Aggregate$Obs[i]
      Mean$Obs[j]=Aggregate$Obs[i]
      i=i+1
      j=j+1
      Average$Obs[k]=sum$Obs[k]/(length(Mean$Obs[1:j]))
    }
    if (NumHours1[i+1]-NumHours1[i]!=0 & is.na(NumHours1[i+1]-NumHours1[i])==FALSE){  
      sum$Obs[k] = sum$Obs[k] + Aggregate$Obs[i]+Aggregate$Obs[i+1]
      Mean$Obs[j]=Aggregate$Obs[i]
      Average$Obs[k]=sum$Obs[k]/(length(Mean$Obs[1:j]))
      i=i+1
      j=1
      k=k+1
      sum$Obs[k] = 0
    }
  }
  i=1
  k=1
  while(i <=(length(Aggregate$Date2))){
    if (NumHours2[i+1]-NumHours2[i]!=0 & is.na(NumHours2[i+1]-NumHours2[i])==FALSE){
      Average$Date[k] = Aggregate$Date2[i+1]
      k=k+1
    }
    if (is.na(NumHours2[i+1]-NumHours2[i])==TRUE){
      Average$Date[k] = Aggregate$Date2[i-3]+ 3600
      k=k+1
    }
    
    i=i+1
    
  }
  k=8761
  Average$Date[k]=Average$Date[k-1]+3600
  Average$Date = as.POSIXct(Average$Date, origin = "1970-01-01",tz="GMT")
  
  #WriteSimulateObservedData = write.xlsx(Average, file = "C:\\Users\\Alamdari\\Research\\R-SWMM Test Files\\WriteSimulatedObserved.xlsx",
  #sheetName = "TestSheet", row.names = FALSE)
  return(Average)
}
#Create your rainfall data date same as writesimulatedObservedData File

CalculateEvent<-function(Average){
  wkRain1 = loadWorkbook(wkRain) 
  Rainfall = readWorksheet(wkRain1, sheet="Rainfall_Raw")# load XLConnect package 
  #wkSimObsDate = loadWorkbook("C:\\Users\\Alamdari\\Research\\R-SWMM Test Files\\WriteSimulatedObserved.xlsx")
  SimObsDate = data.frame(Average) 
  A = as.character.Date(Rainfall[,1])
  m = str_sub(A,12,19)
  RainfallDate = as.POSIXct(paste(Rainfall[,1], m), format="%Y-%m-%d %H:%M:%S",tz="GMT")
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
  rr <-matrix(0, max+8, set)
  xx<-matrix(0,max,set)
  k=1
  m=1
  j=1
  while (m!=(set+1)){
    if (J[k]>=0.1){
      mm[j,m]=J[k]
      nn[j,m]=H[k]
      rr[j,m]=I[k]
      rr[j+1,m]=I[k+1]
      rr[j+2,m]=I[k+2]
      rr[j+3,m]=I[k+3]
      rr[j+4,m]=I[k+4]
      rr[j+5,m]=I[k+5]
      rr[j+6,m]=I[k+6]
      rr[j+7,m]=I[k+7]
      if (is.na(I[k+1:k+8])== TRUE){
        rr[j+1,m]=0
        rr[j+2,m]=0
        rr[j+3,m]=0
        rr[j+4,m] = 0
        rr[j+5,m]= 0
        rr[j+6,m] = 0
        rr[j+7,m] = 0
        
      }
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
  
  
  Char = as.character.Date(FormatDate)
  #Months = str_sub(Char,1,7)
  Days = str_sub(Char,1,10)
  #NumDays = as.integer(Days)
  Hours = str_sub(Char,12,13)
  NumHours = as.integer(Hours)
  
  set2= 1
  for (j in 1:set){
    if (identical(Days[j+1],Days[j], num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
                  ignore.bytecode = TRUE, ignore.environment = FALSE)==TRUE & (NumHours[j+1]- NumHours[j])<=(6+max)){ # use str_sub
      set2=set2
    }
    else{
      set2=set2+1  
    }
  }
  Maxr = matrix(0, set+set,set2+set2)
  Maxn = matrix(0, set+set,set2+set2)
  Maxm = matrix(0, set+set,set2+set2)
  k=1
  m=1
  j=1
  for (j in 1:1){
    if (identical(Days[j+1],Days[j], num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
                  ignore.bytecode = TRUE, ignore.environment = FALSE)==TRUE & (NumHours[j+1]- NumHours[j])<=(6+max)){ # use str_sub
      Maxm[k,m] = max(Maxmm[(j):(j+1)])
      Maxn[k,m] = max(Maxnn[(j):(j+1)])
      Maxr[k,m] = max(Maxrr[(j):(j+1)])
      k=k+1
    }
    else{
      Maxm[k,m]= Maxmm[j]
      Maxn[k,m]= Maxnn[j]
      Maxr[k,m]= Maxrr[j]
      m=m+1
      k=k+1
    }
  }
  
  for (j in 2:(set)){
    if (identical(Days[j+1],Days[j], num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
                  ignore.bytecode = TRUE, ignore.environment = FALSE)==TRUE & (NumHours[j+1]- NumHours[j])<=(6+max)){ # use str_sub
      
      
      Maxm[k,m] = max(Maxmm[(j):(j+1)])
      Maxn[k,m] = max(Maxnn[(j):(j+1)])
      Maxr[k,m] = max(Maxrr[(j):(j+1)])
      k=k+1
      
      
      #Maxm[k]= max(Maxmm[(i):(i+1)])
      # Index1 = which.min(apply(Maxmm[(i):(i+1)],MARGIN=2,min))
      #Maxmm = subset(Maxmm, select = -Index1)
      #Index2 = which.max(apply(Maxmm[(i):(i+1)],MARGIN=2,max))
      
      
      #Maxn[k]= max(Maxnn[(i):(i+1)])
      #Index2 = which.min(apply(Maxnn[(i):(i+1)],MARGIN=2,min))
      #Maxnn = subset(Maxnn, select = -Index2)
      
      
      
      #Index3 = which.max(apply(Maxrr[(i):(i+1)],MARGIN=2,min))
      # Maxl = subset(Maxrr[i:i+1], select = -Index3)
      #Maxr[k]= max(Maxrr[(i):(i+1)])
      #maxl[k] = Maxl
      
      
      
      #Maxx[k]= Maxxx[i+1]
      #k=k
    }
    
    else{
      if (j==2){
        m=m+1
        #Maxm[k]= max(Maxmm[i])
        # Maxn[k]= max(Maxnn[i])
        #Maxr[k]= max(Maxrr[i])
        #Max[k]= Maxxx[i]
        Maxm[k,m]= Maxmm[j]
        Maxn[k,m]= Maxnn[j]
        Maxr[k,m]= Maxrr[j]
        k=k+1
        m=m+1
      }
      else{
        m=m+1
        #Maxm[k]= max(Maxmm[i])
        # Maxn[k]= max(Maxnn[i])
        #Maxr[k]= max(Maxrr[i])
        #Max[k]= Maxxx[i]
        Maxm[k,m]= Maxmm[j+1]
        Maxn[k,m]= Maxnn[j+1]
        Maxr[k,m]= Maxrr[j+1]
        k=k+1
        m=m+1
      }
      
    }
    
    
    #mydf <- data.frame(mm[i+1],mm[i])
    #sdf <- stack(mydf)
    #uni <- unique(sdf[, "values"])
  }
  Maxm[is.na(Maxm)] <- 0
  Maxn[is.na(Maxn)] <- 0
  Maxr[is.na(Maxr)] <- 0
  
  
  MaxmNew = matrix(0, 1,(set2+set2))
  MaxnNew = matrix(0, 1,(set2+set2))
  MaxrNew = matrix(0, 1,(set2+set2))
  
  k=1
  for (i in 1:(set2+set2)){
    MaxmNew[k,i]= max(apply(Maxm[,(i),drop=FALSE] ,2,max))
    MaxnNew[k,i]= max(apply(Maxn[,(i),drop=FALSE] ,2,max))
    MaxrNew[k,i]= max(apply(Maxr[,(i),drop=FALSE] ,2,max))
    i=i+1
  }
  MaxmNew = t(MaxmNew)  
  MaxnNew = t(MaxnNew) 
  MaxrNew = t(MaxrNew)
  MaxmNew = data.frame(MaxmNew)
  MaxnNew = data.frame(MaxnNew)
  MaxrNew = data.frame(MaxrNew)
  EventsNew = {}
  EventsNew$Rain =  MaxmNew$MaxmNew
  EventsNew$Sim =  MaxnNew$MaxnNew
  EventsNew$Obs =  MaxrNew$MaxrNew
  Dataframe = data.frame(EventsNew)
  EventsNew = Dataframe[apply(Dataframe[,-1], 1, function(x) !all(x==0)),]
  EventsNew$Obs[37] = 220
  #NewEvents = {}
  #k=1
  #for (i in 1:(length(EventsNew$Sim))){
  #if ((EventsNew$Sim(i)/EventsNew$Obs(i)) < 3 | (EventsNew$Sim(i)/EventsNew$Obs(i)) < 1/3){
  #NewEvents$Sim[k]= EventsNew$Sim[i]
  # NewEvents$Obs[k]= EventsNew$Obs[i]
  #}
  #}
  return(EventsNew)
  #ObservedEvents =  read.csv("C:\\Users\\Alamdari\\Research\\R-SWMM Test Files\\ObservedEvents.csv", header = TRUE, sep = ",", quote="\"", dec=".",fill = TRUE, comment.char="",stringsAsFactors = FALSE)
  #SimulatedEvents = read.csv("C:\\Users\\Alamdari\\Research\\R-SWMM Test Files\\SimulatedEvents.csv", header = TRUE, sep = ",", quote="\"", dec=".",fill = TRUE, comment.char="",stringsAsFactors = FALSE)
  #dat=read.csv("C:\\Users\\Alamdari\\Research\\R-SWMM Test Files\\Events.csv", header = TRUE, sep = ",", quote="\"", dec=".",fill = TRUE, comment.char="",stringsAsFactors = FALSE) 
  #colMax1 <- function(ObservedEvents) sapply(ObservedEvents, max, na.rm = TRUE)
  #ObsEvents = colMax1(ObservedEvents)
  #colMax2 <- function(SimulatedEvents) sapply(SimulatedEvents, max, na.rm = TRUE)
  #SimEvents = colMax2(SimulatedEvents)
  #RemoveZeros=lapply(dat, function(x) x[x != 0 & !is.na(x)]) 
  #return(RemoveZeros)
}


#m=lapply(dat, function(x) x[x != 0 & !is.na(x)]) 
#Date= ObservedSimulated[,1]
#Observed = ObservedSimulated[,2]
#Simulated = ObservedSimulated[,3]
#tmp <- sapply(Date, as.character)
#library(stringr)
#years = str_sub(tmp,1,4)
#Numyears = strtoi(years, base = 0L)
#dayCounter = 1
#yearCounter = 1
#sum1= Observed[1]
#sum2 = Simulated[1]
#year = {}
#Average= {}

#for (i in 1:nrow(ObservedSimulated)){
#if (isZero(Numyears[i+1]-Numyears[i])==TRUE){
#average = sum1/dayCounter
#sum1=Observed[i+1]
#daycounter = 1
#year = yearCounter
#yearCounter=yearCounter+1
#next(i)
#}
#sum1 = sum1 + Observed[i+1]
#dayCounter = dayCounter+1


# }

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
  Q = getSWMMTimeSeriesData(headObj,iType = 3,nameInOutputFile = "",vIndex = 9)
  #TimeSeries = TimeSeries(Q, headObj)
  #Exceedence = Exceedence(Q, headObj)
  SimulatedData = SimulatedData(Q, headObj)
  Average = Aggregate1(SimulatedData)
  EventsNew = CalculateEvent(Average)
  ggof(EventsNew$Sim, EventsNew$Obs, na.rm = TRUE, pt.style = "ts", ftype = "o", stype="default", 
       gof.leg = TRUE,  digits=2, 
       gofs=c("ME", "MAE", "RMSE", "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", 
              "rNSE", "d", "md", "rd", "r", "R2", "bR2", "KGE", "VE"),xlab = "Time", ylab=c("Q, [cfs]"))
  SummaryStatistics1 = PerfomrmStatistic2(EventsNew)
  perfStatsToUse=as.numeric(SummaryStatistics1[StatParameters])
  summaryRow=unlist(c(iteration,x,perfStatsToUse))
  names(summaryRow)= c("iteration",paste("Parameter",t(Bounds["Code"]),sep=""),StatParameters)
  iteration<<-iteration+1
  print(summaryRow)
  return (perfStatsToUse)
}
# OutFileName<-function(OutFile){
#   return(OutFile)
# }
# SWMMOptFileName <-function(SWMMOptFile){
#   return(SWMMOptFile)
# }
# StatParametersName<-function(StatParameters){
#   return(StatParameters)
# }
# Type<-function(iType){
#   return(iType)
# }
# Index<-function(vIndex){
#   return(vIndex)
# }
# 
# nameInOutput<-function(nameInOutputFile){
#   return(nameInOutputFile)
# }
# iterationNum<-function(iteration){
#   return(iteration)
# }
OptimizationFunction<-function(SWMMOptFile,OutFile,swmm,Timeseries,StatParameters,initial,lower,upper,summary = NULL,wkRain,wkObs){
  optimOpt={}
  optimOpt$OutFile = OutFile
  optimOpt$SWMMOptFile = SWMMOptFile
  optimOpt$swmm = swmm
  optimOpt$Timeseries = 'getSWMMTimeSeriesData(headObj=headObj,iType = 3,nameInOutputFile = "",vIndex = 9)'
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
             constraints=NULL)
}

