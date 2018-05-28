library(fgui)
library(stringr)
library(stringi)
library(xlsx)
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
conn <- file(Report)
text <- readLines(conn)

for (i in 1: length(text)){
  if (text[i] == "  Outfall Loading Summary"){
  Q01 =  str_sub(text[i+10],56,63)
  Q4 = str_sub(text[i+10],123,131) 
  Q5 = str_sub(text[i+10],96,105) 
  Q6 =  str_sub(text[i+10],111,120)
    
  }
}

Q01 = as.numeric(Q01)
Q4 = as.numeric(Q4)
Q5 = as.numeric(Q5)
Q6 = as.numeric(Q6)


  SS1 = function(I){
    I = I
  }
  I = guiv(SS1)
  

  conn <- file("DR_HEADWATER_BMPs1.inp")
  text <- readLines(conn)

  fileConn<-file("Seatac_Opt1.inp")
  writeLines(text, fileConn)
  close(fileConn)
  
  conn <- file("Seatac_Opt1.inp")
  text <- readLines(conn)
  for (i in 1: length(text)){
    if (text[i] == "[SUBCATCHMENTS]" ){
      str_sub(text[i+4],52,54) = "$1$"
      str_sub(text[i+5],52,54) = "$2$"
      str_sub(text[i+6],52,54) = "$3$"
      str_sub(text[i+7],52,54) = "$4$"
      str_sub(text[i+8],52,54) = "$5$"
      str_sub(text[i+9],52,54) = "$6$"
      str_sub(text[i+10],52,54) = "$7$"
      str_sub(text[i+11],52,54) = "$8$"
      str_sub(text[i+12],52,54) = "$9$"
      str_sub(text[i+13],52,54) = "$10$"
      str_sub(text[i+14],52,54) = "$11$"
     
    }
  }
  for (i in 1: length(text)){
    if (text[i] == "[LID_USAGE]" ){
      str_sub(text[i+3],35,37) = "$12$"
      str_sub(text[i+4],35,37) = "$13$"
      str_sub(text[i+5],35,37) = "$14$"
      str_sub(text[i+6],35,37) = "$15$"
      str_sub(text[i+7],35,37) = "$16$"
      str_sub(text[i+8],35,37) = "$17$"
      str_sub(text[i+9],35,37) = "$18$"
      str_sub(text[i+10],35,37) = "$19$"
      str_sub(text[i+11],35,37) = "$20$"
      str_sub(text[i+12],35,37) = "$21$"
      str_sub(text[i+13],35,37) = "$22$"
      str_sub(text[i+14],43,45) = "$23$"
      str_sub(text[i+15],43,45) = "$24$"
      str_sub(text[i+16],43,45) = "$25$"
      str_sub(text[i+17],43,45) = "$26$"
      str_sub(text[i+18],43,45) = "$27$"
      str_sub(text[i+19],43,45) = "$28$"
      str_sub(text[i+20],43,45) = "$29$"
      str_sub(text[i+21],43,45) = "$30$"
      str_sub(text[i+22],43,45) = "$31$"

    }
  }
  
  for (i in 1: length(text)){
    if (text[i] == "[CURVES]"){
      str_sub(text[i+3],40,50) = "$32$"
      
    }
  }
Code = c("$1$","$2$","$3$","$4$","$5$","$6$","$7$","$8$","$9$","$10$","$11$","$12$","$13$","$14$","$15$","$16$","$17$","$18$"
         ,"$19$","$20$","$21$","$22$","$23$","$24$","$25$","$26$","$27$","$28$","$29$",
         "$30$","$31$","$32$")
Initial = c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,21780)
Minimum = c(0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,1,1,1,1,1,1,1,1,1,1,1,0.025,0.025,0.025,0.025,0.025,0.025,0.025,0.025,0.025,43560)
Maximum = c(0.038,0.05,0.05,0.6,0.08,0.09,0.05,0.04,0.07,0.09,0.03,3,3,3,5,3,3,3,3,3,3,3,1000,1000,1000,1000,1000,1000,1000,1000,1000,43560)
 df = data.frame(Code,Minimum,Maximum,Initial)
 write.csv(df,"ParametersBound1.csv")
#   for (i in 1: length(text)){
#     if (text[i] == "[SUBCATCHMENTS]"){
#       
#       h=str_sub(text[i+5],52,54)
#     }
#     
#   }
  fileConn<-file("Seatac_Opt2.inp")
  writeLines(text, fileConn)
  close(fileConn)
  
  ParametersFile = "ParametersBound1.csv"
  SWMMOptFile = "Seatac_Opt2.inp"
  iteration = 1
  Bounds = ParametersBound(ParametersFile) 
  initial=c(as.vector(Bounds["Initial"]))$Initial
  lower= c(as.vector(Bounds["Minimum"]))$Minimum
  upper = c(as.vector(Bounds["Maximum"]))$Maximum
  StatParameters = c("Name1", "P1", "Number1",
                     "Name2", "P10", "Number2",
                     "Name3", "P12", "Number3",
                     "Name4", "P2", "Number4",
                     "Name5", "P3", "Number5",
                     "Name6", "P4", "Number6",
                     "Name7", "P5", "Number7",
                     "Name8", "P6", "Number8",
                     "Name9", "P7", "Number9",
                     "Name10", "P8", "Number10",
                     "Name11", "P9", "Number11",
                     "Name12", "R1",
                     "Name13", "R10",
                     "Name14", "R11",
                     "Name15", "R4",
                     "Name16", "R5",
                     "Name17", "R6",
                     "Name18", "R7",
                     "Name19", "R8",
                     "Name20", "R9",
                     "Name21", "LocationforPond","ReductionTSS","ReductionTN", "ReductionTP","ReductionRunoff", "CostBMP$BMP1")
  
  OptimizationFunction1(SWMMOptFile,OutFile,swmm,Timeseries,StatParameters,initial,lower,upper)
  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #setwd("C:/Users/david/Research/SWMM Codes_Environemntal Modeling and Software/C:\Users\david\Research\SWMM Codes_Environemntal Modeling and Software\Cost_Optimzation_Combination")
  
  
  filelist <- list.files(pattern = "^Combinations.*\\.xlsx$")
  files <- lapply(filelist, read.xlsx, sheetName = "Sheet1", header=TRUE) 
  files <- lapply(files, function(x) x[-1])
  Combination1 = Reduce(function(...) merge(..., by =1:58, all=T), files)
  
  Combination1$ReductionTSS = -1*Combination1$ReductionTSS
  Combination1$ReductionTN = -1*Combination1$ReductionTN
  Combination1$ReductionTP = -1*Combination1$ReductionTP
  Combination1$ReductionRunoff = -1*Combination1$ReductionRunoff
  Combination1$TSSreduction = Combination1$ReductionTSS*100
  Combination1$TNreduction = Combination1$ReductionTN*100
  Combination1$TPreduction = Combination1$ReductionTP*100
  Combination1$RunoffReduction = Combination1$ReductionRunoff*100
  
  write.xlsx(Combination1,"CombinationsLID.xlsx")
  #Combination1 = read.xlsx("Cominations_results.xlsx",sheetName = "Sheet1")
  
  Combination1 = read.xlsx("Combinations_LIDNew.xlsx",sheetName = "Sheet1")
  x1 = Combination1$CostBMP.BMP1/1000
  y1 = Combination1$TSSreduction....
  y2 = Combination1$Tnreduction....
  y3 = Combination1$Tpreduction....
  y4 = Combination1$RunoffReduction....

  
  
  
  options(scipen = 5)
  par(pty="m", plt=c(0.1, 1, 0.1, 1), omd=c(0.1,0.9,0.1,0.9))
  plot(x1,y1,pch="*",type = "n", xlab="",labels= TRUE, ylab="",xlim = c(0,1700),ylim = c(0,60), col="blue",axes = F)
  # mtext(side=3, text="Cost Optimization", line=1.2, cex=1.5)
  mtext(side=1, text="Cost (1000$)", line=2.5)
  mtext(side=2, text="Reduction (%)", line=2.5)
  grid()
  lines(x1, y1, col="red",type = 'p', lty=1, lwd=2,pch = 19)
  par(new=TRUE) 
  lines(x1, y2, col="blue",type = 'p', lty=1, lwd=1,pch = 16)
  par(new=TRUE)
  lines(x1, y3, col="purple",type = 'p', lty=1, lwd=1,pch = 16)
  par(new=TRUE) 
  lines(x1, y4, col="green",type = 'p', lty=1, lwd=2,pch = 19)
  par(new=TRUE)
  axis(1, pos=0)
  axis(2, pos=0)
  legend("bottomright", legend = c("TSS","TN","TP","Runoff"), col = c('red','blue','purple','green'), lty = c(1,1,1,1), cex = 1.1  ,pch = 19, lwd = 2,x.intersp = 0.5,y.intersp = 0.9,inset=0.05)
  
 # legend("bottomright", legend = c("TSS Reduction","TN Reduction","TP Reduction","Runoff Reduction"), col = c('red',"blue","purple","green"), lty = c(1), cex = 1.1  ,pch = 16, lwd = 2,x.intersp = 0.5,y.intersp = 0.9,inset=0.05)
  
  
  
  