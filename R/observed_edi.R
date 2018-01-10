##### OBSERVED: Create input file and run daily EDI and copy output file
#' @export
Create.Daily.EDI.Input.Observed <- function(ObsDailyCSV, outdir) {

  #ObsDailyCSV = "D:/AIMS_Test/Projects/64705e7c-b261-45e8-88c7-822c23e52133/Observed/User/PERRYIL.csv"

  library(gdata)

  data = read.csv(ObsDailyCSV, header = T)
  data = data[,1:4]
  colnames(data) = c("year", "mon", "day", "prcp")
  syear = min(data$year); eyear = max(data$year)

  # because data$prcp has been converted to strings because of NAs
  data$prcp = as.numeric(data$prcp)
  data[(which(data$mon == 2 & data$day == 29)-1), "prcp"] = data[(which(data$mon == 2 & data$day == 29)-1), "prcp"] + data[which(data$mon == 2 & data$day == 29), "prcp"]

  out = data[-which(data$mon == 2 & data$day == 29),]

  ##### Temp: Nee to improve using QC Algorithm
  out[out <= 0] = 0

  OutDFile = file.path(outdir, "input.txt")
  gdata::write.fwf(out, OutDFile, width = c(4,4,4,10), sep = "", colnames = FALSE)

  outList = list("syear"= syear, "eyear"=eyear)
  return(outList)

}


##################################################################
#' @export
# StnDir = EnvList$StnDir
# StnFile = EnvList$StnFile
# ObsDayDir = EnvList$ObsDayDir
# EdiObsDir = EnvList$EdiObsDir
# Syear_Cal = EnvList$Syear_Cal
# Eyear_Cal = EnvList$Eyear_Cal
#
Run.Daily.EDI.Observed <- function(StnDir, StnFile, ObsDayDir, EdiObsDir, Syear_Cal, Eyear_Cal,...) {


  stnnms = read.csv(file.path(StnDir, StnFile), header = T)[, c("ID")]

  outdir = file.path(EdiObsDir, "Output")
  SetWorkingDir(outdir)

  for(i in 1:length(stnnms)){
    stnnm = stnnms[i]
    ObsDailyCSV = list.files(ObsDayDir, pattern = glob2rx(paste("*", stnnm, "*.csv", sep="")), full.names = T)
    years = Create.Daily.EDI.Input.Observed (ObsDailyCSV, outdir)
    syear = years$syear; eyear = years$eyear

    ## Update NAMELIST.TXT file
    if(syear <= Syear_Cal) Syear_Cal = Syear_Cal + 1
    Update.NAMELIST (outdir, Syear_Cal, Eyear_Cal)

    EdiDailyExe <- system.file("extdata", "EDI-Daily.exe", package = "rdrought")
    if(!file.exists(file.path(outdir, "EDI-Daily.exe"))) { file.copy(EdiDailyExe, outdir) }


    ## Run Daily EDI
    setwd(outdir)
    system("cmd.exe /c EDI-Daily.exe")

    ## Rename the output.txt file
    FromDFile = paste(outdir, "/output.txt", sep="")
    ToDFile = paste(outdir, "/", stnnms[i], "-EDI_Obs.txt", sep="")
    file.rename(FromDFile, ToDFile)
  }


}


##### OBSERVED: Create input file and run monthly EDI and copy output file
#' @export
Create.Monthly.EDI.Input.Observed <- function(ObsMonthlyCSV, outdir) {

  library(gdata)

  #ObsMonthlyCSV = "F:/Cli-WARA/Database/var-predictand/Thai-NE-22_Stns_Adj-CRU_ERA.csv"
  #outdir = "F:/Cli-WARA/Drought/Observed/Output"

  data = read.csv(ObsMonthlyCSV, header = T)

  data = prcp.mmday2mmmon (data)

  data$year = as.numeric(substr(data$yearmon, 1, 4))
  data$mon = as.numeric(substr(data$yearmon, 6, 7))
  # because data$prcp has been converted to strings because of NAs
  data$prec = round(as.numeric(data$prec), digits = 1)

  out = data[, c("year", "mon", "prec")]
  colnames(out) = c("year", "mon", "prcp")

  OutDFile = file.path(outdir, "input.txt")
  gdata::write.fwf(out, OutDFile, width = c(4,4,10), sep = "", colnames = FALSE)

}


#' @export
Run.Monthly.EDI.Observed <- function(EnvList) {

  StnDir = EnvList$StnDir
  StnFile = EnvList$StnFile
  ObsDayDir = EnvList$ObsDayDir
  ObsMonthDir = EnvList$ObsDayDir
  EdiObsDir = EnvList$EdiObsDir
  Syear_Cal = EnvList$Syear_Cal
  Eyear_Cal = EnvList$Eyear_Cal

  stnnms = read.csv(file.path(StnDir, StnFile), header = T)[, c("ID")]

  outdir = file.path(EdiObsDir, "Output")
  SetWorkingDir(outdir)

  for(i in 1:length(stnnms)){
    stnnm = stnnms[i]
    ObsDailyCSV = list.files(ObsDayDir, pattern = glob2rx(paste("*", stnnm, "*.csv", sep="")), full.names = T)
    Create.Daily.EDI.Input.Observed (ObsDailyCSV, outdir)

    ## Update NAMELIST.TXT file
    Update.NAMELIST (outdir, Syear_Cal, Eyear_Cal)

    ## Run Daily EDI
    setwd(outdir)
    system("cmd.exe /c EDI-Daily.exe")

    ## Rename the output.txt file
    FromDFile = paste(outdir, "/output.txt", sep="")
    ToDFile = paste(outdir, "/", stnnms[i], "-EDI_Obs.txt", sep="")
    file.rename(FromDFile, ToDFile)
  }


}

