Objectivefunction1<-function(SWMMOptFile,m,OutFile,swmm,Timeseries,StatParameters){
  SWMMOpt= ReadSWMMOptFile(SWMMOptFile)

  Input=paste(OutFile,iteration,'.inp',sep="")
  ReplaceCodes<<- replaceCodesInTemplateFile(SWMMOpt,m,as.matrix(Bounds["Code"]),Input)
  listofLID = c("BIORE","PPAVE","SWALE")
  listofLID1 = c("BIORE" ,"SWALE")
  
  for (i in 1: length(ReplaceCodes)){
    if (ReplaceCodes[i] == "[LID_USAGE]" ){
      str_sub(ReplaceCodes[i+3],18,22) = sample(listofLID1, 1,replace = TRUE)
      str_sub(ReplaceCodes[i+4],18,22) = sample(listofLID1, 1,replace = TRUE)
      str_sub(ReplaceCodes[i+5],18,22) = sample(listofLID1, 1,replace = TRUE)
      str_sub(ReplaceCodes[i+6],18,22) = sample(listofLID, 1,replace = TRUE)
      str_sub(ReplaceCodes[i+7],18,22) = sample(listofLID, 1,replace = TRUE)
      str_sub(ReplaceCodes[i+8],18,22) = sample(listofLID, 1,replace = TRUE)
      str_sub(ReplaceCodes[i+9],18,22) = sample(listofLID, 1,replace = TRUE)
      str_sub(ReplaceCodes[i+10],18,22) = sample(listofLID, 1,replace = TRUE)
      str_sub(ReplaceCodes[i+11],18,22) = sample(listofLID1, 1,replace = TRUE)
      str_sub(ReplaceCodes[i+12],18,22) = sample(listofLID1, 1,replace = TRUE)
      str_sub(ReplaceCodes[i+13],18,22) = sample(listofLID1, 1,replace = TRUE)
    }
  }
  
  for (i in 1: length(ReplaceCodes)){
    if (ReplaceCodes[i] == "[LID_USAGE]" ){
      Name1 = str_sub(ReplaceCodes[i+3],18,22)
      Name2 = str_sub(ReplaceCodes[i+4],18,22)
      Name3 = str_sub(ReplaceCodes[i+5],18,22)
      Name4 = str_sub(ReplaceCodes[i+6],18,22)
      Name5 = str_sub(ReplaceCodes[i+7],18,22)
      Name6 = str_sub(ReplaceCodes[i+8],18,22)
      Name7 = str_sub(ReplaceCodes[i+9],18,22)
      Name8 = str_sub(ReplaceCodes[i+10],18,22)
      Name9 = str_sub(ReplaceCodes[i+11],18,22)
      Name10 = str_sub(ReplaceCodes[i+12],18,22)
      Name11 = str_sub(ReplaceCodes[i+13],18,22)
      Name12 = str_sub(ReplaceCodes[i+14],18,22)
      Name13 = str_sub(ReplaceCodes[i+15],18,22)
      Name14 = str_sub(ReplaceCodes[i+16],18,22)
      Name15 = str_sub(ReplaceCodes[i+17],18,22)
      Name16 = str_sub(ReplaceCodes[i+18],18,22)
      Name17 = str_sub(ReplaceCodes[i+19],18,22)
      Name18 = str_sub(ReplaceCodes[i+20],18,22)
      Name19 = str_sub(ReplaceCodes[i+21],18,22)
      Name20 = str_sub(ReplaceCodes[i+22],18,22)
    }
  }
  
  Name21 = "Pond"
  for (i in 1: length(ReplaceCodes)){
    if (ReplaceCodes[i] == "[SUBCATCHMENTS]" ){
      h1 = str_sub(ReplaceCodes[i+4],52,70) 
      h11 =str_sub(ReplaceCodes[i+5],52,70)
      h12 =str_sub(ReplaceCodes[i+6],52,70)
      h13 =str_sub(ReplaceCodes[i+7],52,70)
      h14 =str_sub(ReplaceCodes[i+8],52,70)
      h15 =str_sub(ReplaceCodes[i+9],52,70)
      h16 =str_sub(ReplaceCodes[i+10],52,70)
      h17 =str_sub(ReplaceCodes[i+11],52,70)
      h18 =str_sub(ReplaceCodes[i+12],52,70)
      h19 =str_sub(ReplaceCodes[i+13],52,70)
      h199 =str_sub(ReplaceCodes[i+14],52,70)
    }
  }

  for (i in 1: length(ReplaceCodes)){
    if (ReplaceCodes[i] == "[LID_USAGE]" ){
      h2 = str_sub(ReplaceCodes[i+3],35,53) 
      h3 = round(as.numeric(h2),0)
      str_sub(ReplaceCodes[i+3],35,53) = "                   "
      str_sub(ReplaceCodes[i+3],43,50) = as.numeric(h1)*43560/as.numeric(h3)
      str_sub(ReplaceCodes[i+3],35,35) =  as.numeric(h3)
      
      
      h4 = str_sub(ReplaceCodes[i+4],35,53) 
      h5 = round(as.numeric(h4),0)
      str_sub(ReplaceCodes[i+4],35,53) = "                   "
      str_sub(ReplaceCodes[i+4],43,50) = as.numeric(h11)*43560/as.numeric(h5)
      str_sub(ReplaceCodes[i+4],35,35) =  as.numeric(h5)
      
      h6 = str_sub(ReplaceCodes[i+5],35,53) 
      h7 = round(as.numeric(h6),0)
      str_sub(ReplaceCodes[i+5],35,53) = "                   "
      str_sub(ReplaceCodes[i+5],43,50) = as.numeric(h12)*43560/as.numeric(h7)
      str_sub(ReplaceCodes[i+5],35,35) =  as.numeric(h7)
      
      
      h8 = str_sub(ReplaceCodes[i+6],35,53) 
      h9 = round(as.numeric(h8),0)
      str_sub(ReplaceCodes[i+6],35,53) = "                   "
      str_sub(ReplaceCodes[i+6],43,50) = as.numeric(h13)*43560/as.numeric(h9)
      str_sub(ReplaceCodes[i+6],35,35) =  as.numeric(h9)
      
      
      h10 = str_sub(ReplaceCodes[i+7],35,53) 
      h18 = round(as.numeric(h10),0)
      str_sub(ReplaceCodes[i+7],35,53) = "                   "
      str_sub(ReplaceCodes[i+7],43,50) = as.numeric(h14)*43560/as.numeric(h18)
      str_sub(ReplaceCodes[i+7],35,35) =  as.numeric(h18)
      
      
      
      h19 = str_sub(ReplaceCodes[i+8],35,53) 
      h20 = round(as.numeric(h19),0)
      str_sub(ReplaceCodes[i+8],35,53) = "                   "
      str_sub(ReplaceCodes[i+8],43,50) = as.numeric(h15)*43560/as.numeric(h20)
      str_sub(ReplaceCodes[i+8],35,35) =  as.numeric(h20)
      
      
      
      h21 = str_sub(ReplaceCodes[i+9],35,53) 
      h22 = round(as.numeric(h21),0)
      str_sub(ReplaceCodes[i+9],35,53) = "                   "
      str_sub(ReplaceCodes[i+9],43,50) = as.numeric(h16)*43560/as.numeric(h22)
      str_sub(ReplaceCodes[i+9],35,35) =  as.numeric(h22)
      
      
      
      h23 = str_sub(ReplaceCodes[i+10],35,53) 
      h24 = round(as.numeric(h23),0)
      str_sub(ReplaceCodes[i+10],35,53) = "                   "
      str_sub(ReplaceCodes[i+10],43,50) = as.numeric(h17)*43560/as.numeric(h24)
      str_sub(ReplaceCodes[i+10],35,35) =  as.numeric(h24)
      
      h233 = str_sub(ReplaceCodes[i+11],35,53) 
      h244 = round(as.numeric(h233),0)
      str_sub(ReplaceCodes[i+11],35,53) = "                   "
      str_sub(ReplaceCodes[i+11],43,50) = as.numeric(h18)*43560/as.numeric(h244)
      str_sub(ReplaceCodes[i+11],35,35) =  as.numeric(h244)
      
      h235 = str_sub(ReplaceCodes[i+12],35,53) 
      h245 = round(as.numeric(h235),0)
      str_sub(ReplaceCodes[i+12],35,53) = "                   "
      str_sub(ReplaceCodes[i+12],43,50) = as.numeric(h19)*43560/as.numeric(h245)
      str_sub(ReplaceCodes[i+12],35,35) =  as.numeric(h245)
      
      h236 = str_sub(ReplaceCodes[i+13],35,53) 
      h246 = round(as.numeric(h236),0)
      str_sub(ReplaceCodes[i+13],35,53) = "                   "
      str_sub(ReplaceCodes[i+13],43,50) = as.numeric(h199)*43560/as.numeric(h246)
      str_sub(ReplaceCodes[i+13],35,35) =  as.numeric(h246)
      
      
      h25 = str_sub(ReplaceCodes[i+11],43,50)
      h26 = str_sub(ReplaceCodes[i+12],43,50)
      h27 = str_sub(ReplaceCodes[i+13],43,50)
      h28 = str_sub(ReplaceCodes[i+14],43,50)
      h29 = str_sub(ReplaceCodes[i+15],43,50)
      h30 = str_sub(ReplaceCodes[i+16],43,50)
      
    }
  }
  
      
  for (i in 1: length(ReplaceCodes)){
    if (ReplaceCodes[i] == "[CURVES]"){
      h31 = str_sub(ReplaceCodes[i+3],40,50)
      
    }
  }
      
  UnitSize1 = as.numeric(h1)*43560/as.numeric(h3)
  UnitSizeArea1 = UnitSize1/43560 #bioretention unit size
  UnitSize2 = as.numeric(h11)*43560/as.numeric(h5)
  UnitSizeArea2 = UnitSize2/43560 #bioretention unit size
  UnitSize3 = as.numeric(h12)*43560/as.numeric(h7)
  UnitSizeArea3 = UnitSize3/43560 #bioretention unit size
  UnitSize4 = as.numeric(h13)*43560/as.numeric(h9)
  UnitSizeArea4 = UnitSize4/43560 #bioretention unit size
  UnitSize5 = as.numeric(h14)*43560/as.numeric(h18)
  UnitSizeArea5 = UnitSize5/43560 #bioretention unit size
  UnitSize6 = as.numeric(h15)*43560/as.numeric(h20)
  UnitSizeArea6 = UnitSize6/43560 #bioretention unit size
  UnitSize7 = as.numeric(h16)*43560/as.numeric(h22)
  UnitSizeArea7 = UnitSize7/43560 #bioretention unit size
  UnitSize8 = as.numeric(h17)*43560/as.numeric(h24)
  UnitSizeArea8 = UnitSize8/43560 #bioretention unit size
  
  UnitSize9 = as.numeric(h25)
  UnitSizeArea9 = UnitSize9/43560 #green roof unit size
  UnitSize10 = as.numeric(h26)
  UnitSizeArea10 = UnitSize10/43560 #green roof unit size
  UnitSize11 = as.numeric(h27)
  UnitSizeArea11 = UnitSize11/43560 #green roof unit size
  UnitSize12 = as.numeric(h28)
  UnitSizeArea12 = UnitSize12/43560 #green roof unit size
  UnitSize13 = as.numeric(h29)
  UnitSizeArea13 = UnitSize13/43560 #green roof unit size
  UnitSize14 = as.numeric(h30)
  UnitSizeArea14 = UnitSize14/43560 #green roof unit size
  
  UnitSize15 = as.numeric(h31)
  UnitSizeArea15 = UnitSize15/43560 #Pond
  
  
  #if (UnitSizeArea1>0.025 & UnitSizeArea2>0.025 &  UnitSizeArea3>0.025 & UnitSizeArea4>0.025 & UnitSizeArea5>0.025 &  UnitSizeArea6>0.025 & UnitSizeArea7>0.025 & UnitSizeArea8>0.025 ){
    
    fileConn<-file(Input)
    writeLines(ReplaceCodes, fileConn)
    close(fileConn)
    Report=paste(OutFile,iteration,'.rpt',sep="")
    Output=paste(OutFile,iteration,'.out',sep="")
    swmm= "C:\\Program Files (x86)\\EPA SWMM 5.1\\swmm5.exe"
    SWMMExe(swmm,Input,Report,Output)
    headObj = GetObjectsSWMM (Output)
    headObj = getSWMMTimes (headObj)
    conn <- file(Report)
    text <- readLines(conn)
    for (i in 1: length(text)){
      if (text[i] == "  Outfall Loading Summary"){
        Q00 =  str_sub(text[i+10],55,63)
        
      }
    }
    Q1 = str_sub(text[(length(text)-5):(length(text)-5)],81,97) 
    Q2 = str_sub(text[(length(text)-5):(length(text)-5)],56,72) 
    Q3 = str_sub(text[(length(text)-5):(length(text)-5)],73,80)
    Q00 = as.numeric(Q00)
    Q1 = as.numeric(Q1)
    Q2 = as.numeric(Q2)
    Q3 = as.numeric(Q3)

    #   Q1 = getSWMMTimeSeriesData(headObj,iType = 1,nameInOutputFile = "Outfall",vIndex = 6)
    #   Q2 = getSWMMTimeSeriesData(headObj,iType = 1,nameInOutputFile = "Outfall",vIndex = 7)
    #   Q3 = getSWMMTimeSeriesData(headObj,iType = 1,nameInOutputFile = "Outfall",vIndex = 8)
    #   Q1 = mean(Q1)
    #   Q2 = mean(Q2)
    #   Q3 = mean(Q3)
    Reduction0  =(Q01-Q00)/Q01
    Reduction1  =(Q4-Q1)/Q4
    Reduction2  =(Q5-Q2)/Q5
    Reduction3  =(Q6-Q3)/Q6
    #if (Reduction1 <0.8 & Reduction1 >0 ){
    ReductionRunoff = -Reduction0
    ReductionTSS = -Reduction1
    ReductionTN = -Reduction2
    ReductionTP = -Reduction3
    conn<-file(Input,open="rt")
    y =  readLines(conn)
    
    
    k=1
    area1 = {}
    CostBMP  ={}
    CostBMP$BMP1[k]=0
    #---------------------------------------------------------------------------
    
    
    Rv = 0.05+(0.009*I)
    
    CostBMPNew = read.xlsx("Cost_Spreedsheet.xlsx", sheetName = "Sheet1")
    for (i in 1:nrow(CostBMPNew)){
      if(Name1 == CostBMPNew$LID[i]){
      B0 = CostBMPNew$B0[i]
      B1 = CostBMPNew$B1[i] 
    }
    }
    
    for (i in 1:nrow(CostBMPNew)){
      if(Name2 == CostBMPNew$LID[i]){
        B2 = CostBMPNew$B0[i]
        B3 = CostBMPNew$B1[i] 
      }
    }
    
    for (i in 1:nrow(CostBMPNew)){
      if(Name3 == CostBMPNew$LID[i]){
        B4 = CostBMPNew$B0[i]
        B5 = CostBMPNew$B1[i] 
      }
    }
    
    for (i in 1:nrow(CostBMPNew)){
      if(Name4 == CostBMPNew$LID[i]){
        B6 = CostBMPNew$B0[i]
        B7 = CostBMPNew$B1[i] 
      }
    }
    
    for (i in 1:nrow(CostBMPNew)){
      if(Name5 == CostBMPNew$LID[i]){
        B8 = CostBMPNew$B0[i]
        B9 = CostBMPNew$B1[i] 
      }
    }
    
    
    for (i in 1:nrow(CostBMPNew)){
      if(Name6 == CostBMPNew$LID[i]){
        B10 = CostBMPNew$B0[i]
        B11 = CostBMPNew$B1[i] 
      }
    }
    
    for (i in 1:nrow(CostBMPNew)){
      if(Name7 == CostBMPNew$LID[i]){
        B12 = CostBMPNew$B0[i]
        B13 = CostBMPNew$B1[i] 
      }
    }
    
    
    
    for (i in 1:nrow(CostBMPNew)){
      if(Name8 == CostBMPNew$LID[i]){
        B14 = CostBMPNew$B0[i]
        B15 = CostBMPNew$B1[i] 
      }
    }
    
    B16 = CostBMPNew$B0[3]
    B17 = CostBMPNew$B1[3] 
    
    
    Number1 = as.numeric(h3)
    Number2 = as.numeric(h5)
    Number3 = as.numeric(h7)
    Number4 = as.numeric(h9)
    Number5 = as.numeric(h18)
    Number6 = as.numeric(h20)
    Number7 = as.numeric(h22)
    Number8 = as.numeric(h24)
    
    
    Parking1 = UnitSizeArea1
    Parking2 = UnitSizeArea2
    Parking3 = UnitSizeArea3
    Parking4 = UnitSizeArea4
    Parking5 = UnitSizeArea5
    Parking6 = UnitSizeArea6
    Parking7 = UnitSizeArea7
    Parking8 = UnitSizeArea8
    
    Roof1 = UnitSize9
    Roof2 = UnitSize10
    Roof3 = UnitSize11
    Roof4 = UnitSize12
    Roof5 = UnitSize13
    Roof6 = UnitSize14
    
    if(Parking1<0.025){
      Parking1=0
      Number1 = 0
      Name1 = "NON"
    }
    else {
    Parking1=Parking1
    Number1 = Number1
    Name1 = Name1
    }

    if(Parking2<0.025){
      Parking2=0
      Number2 = 0
      Name2 = "NON"
    }
    else {

      Parking2=Parking2
      Number2 = Number2
      Name2 = Name2}

    if(Parking3<0.025){
      Parking3=0
      Number3 = 0
      Name3 = "NON"
    }
    else {
      Parking3=Parking3
      Number3 = Number3
      Name3 = Name3}

    if(Parking4<0.025){
      Parking4=0
      Number4 = 0
      Name4 = "NON"
    }
    else {
      Parking4=Parking4
      Number4 = Number4
      Name4 = Name4}
    if(Parking5<0.025){
      Parking5=0
      Number5 = 0
      Name5 = "NON"
    }
    else {
      Parking5=Parking5
      Number5 = Number5
      Name5 = Name5}

    if(Parking6<0.025){
      Parking6=0
      Number6 = 0
      Name6 = "NON"
    }
    else {Parking6=Parking6
    Number6 = Number6
    Name6 = Name6}

    if(Parking7<0.025){
      Parking7=0
      Number7 = 0
      Name7 = "NON"
    }
    else {Parking7=Parking7
    Number7 = Number7
    Name7 = Name7}

    if(Parking8<0.025){
      Parking8=0
      Number8 = 0
      Name8 = "NON"
    }
    else {Parking8=Parking8
    Number8 = Number8
    Name8 = Name8}

    if(Roof1<1000){
      Roof1=0
      Name9 = "NON"
    }
    else {Roof1=Roof1
    Name9 = Name9}

    if(Roof2<1000){
      Roof2=0
      Name10 = "NON"
    }
    else {Roof2=Roof2
    Name10 = Name10}

    if(Roof3<1000){
      Roof3=0
      Name11 = "NON"
    }
    else {Roof3=Roof3
    Name11 = Name11}

    if(Roof4<1000){
      Roof4=0
      Name12 = "NON"
    }
    else {Roof4=Roof4
    Name12 = Name12}

    if(Roof5<1000){
      Roof5=0
      Name13 = "NON"
    }
    else {Roof5=Roof5
    Name13 = Name13}

    if(Roof6<1000){
      Roof6=0
      Name14 = "NON"
    }
    else {Roof6=Roof6
    Name14 = Name14}
    
    PondLocation = UnitSizeArea15
    
    CostBMP$BMP1  = ((as.numeric(B0)*(((1.5*Parking1*1233.48)*Rv)/12)^as.numeric(B1))*Number1)+((as.numeric(B2)*(((1.5*Parking2*1233.48)*Rv)/12)^as.numeric(B3))*Number2)+((as.numeric(B4)*(((1.5*Parking3*1233.48)*Rv)/12)^as.numeric(B5))*Number3)+((as.numeric(B6)*(((1.5*Parking4*1233.48)*Rv)/12)^as.numeric(B7))*Number4)+((as.numeric(B8)*(((1.5*Parking5*1233.48)*Rv)/12)^as.numeric(B9))*Number5)+((as.numeric(B10)*(((1.5*Parking6*1233.48)*Rv)/12)^as.numeric(B11))*Number6)+((as.numeric(B12)*(((1.5*Parking7*1233.48)*Rv)/12)^as.numeric(B13))*Number7)+((as.numeric(B14)*(((1.5*Parking8*1233.48)*Rv)/12)^as.numeric(B15))*Number8)+((35*Roof1))+((35*Roof2))+((35*Roof3))+((35*Roof4))+((35*Roof5))+((35*Roof6))+((as.numeric(B16)*(((1.5*PondLocation*1233.48)*Rv)/12)^as.numeric(B17)))
    
    output1=data.frame(Name1, Parking1, Number1,
                       Name2, Parking2, Number2,
                       Name3, Parking3, Number3,
                       Name4, Parking4, Number4,
                       Name5, Parking5, Number5,
                       Name6, Parking6, Number6,
                       Name7, Parking7, Number7,
                       Name8, Parking8, Number8,
                       Name9, Roof1,
                       Name10, Roof2,
                       Name11, Roof3,
                       Name12, Roof4,
                       Name13, Roof5,
                       Name14, Roof6,
                       Name15, PondLocation,ReductionTSS,ReductionTN, ReductionTP,ReductionRunoff, CostBMP$BMP1)
    library(xlsx)
    
    for (i in iteration){
      write.xlsx(output1,paste("Combinations",i,".xlsx",sep=""))}
    #    filedirect$runoff[k] = output1[i,1]
    #    filedirect$cost[k] = output1[i,2]
    #  }
    #   k=k+1
    
    
    #TimeSeries = TimeSeries(Q, headObj)
    #Exceedence = Exceedence(Q, headObj)
    
    #   SummaryStatistics1 = output1
    perfStatsToUse=as.numeric(output1)
    summaryRow=unlist(c(iteration,m,perfStatsToUse))
    names(summaryRow)= c("iteration",paste("Parameter",t(Bounds["Code"]),sep=""),StatParameters)
    print(summaryRow)
    #}
    iteration<<-iteration+1
  }



OptimizationFunction1<-function(SWMMOptFile,OutFile,swmm,Timeseries,StatParameters,initial,lower,upper){
  optimOpt={}
  optimOpt$OutFile = OutFile
  optimOpt$SWMMOptFile = SWMMOptFile
  optimOpt$swmm = swmm
  optimOpt$Timeseries = 'str_sub(text[(length(text)-5):(length(text)-5)],81,97)  '
  optimOpt$Timeseries2 = 'str_sub(text[(length(text)-5):(length(text)-5)],56,72) '
  #   
  optimOpt$Timeseries3 = 'str_sub(text[(length(text)-5):(length(text)-5)],73,80)'
  
  optimOpt$StatParameters = StatParameters
  optimOpt$lower = lower
  optimOpt$upper = upper
  library(mco)
  out= nsga2(Objectivefunction1,
             idim=length(optimOpt$lower),
             odim=length(optimOpt$StatParameters),
             OutFile= optimOpt$OutFile,
             SWMMOptFile = optimOpt$SWMMOptFile,
             swmm = optimOpt$swmm,
             Timeseries = c(optimOpt$Timeseries,optimOpt$Timeseries2,optimOpt$Timeseries3),
             StatParameters = optimOpt$StatParameters,
             generations=500,
             lower.bounds=as.double(optimOpt$lower),
             upper.bounds=as.double(optimOpt$upper),
             constraints=NULL,popsize = 500)
}


