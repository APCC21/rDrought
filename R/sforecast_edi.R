#' @export
Create.Monthly.EDI.Input.SForecast <- function(ObsMonthlyCSV, outdir) {

  library(gdata)

  data = read.csv(ObsMonthlyCSV, header=T)
  data = data[c("yearmon", "prec")]
  data[which(data$prec < 0), "prec"] = NA

  # Convert unit from mm/day to mm/month
  data = prcp.mmday2mmmon(data)
  data$year = as.numeric(substr(data$yearmon,1,4))
  data$month = as.numeric(substr(data$yearmon,6, 7))
  data = data[c("year", "month", "prec")]

  # If missing add monthly climatology
  mondata = aggregate(prec~month, data=data, FUN=mean)
  data[which(is.na(data$prec)), "prec"] = mondata[data[which(is.na(data$prec)), "month"], "prec"]

  data$prec = as.numeric(sprintf("%10.1f", data$prec))

  OutDFile = file.path(outdir, "input.txt")
  write.fwf(data, OutDFile, width = c(5, 5, 10), sep = "", colnames = FALSE)

}


#' @export
Run.Monthly.EDI.sforecast <- function(EnvList, fiyearmon) {

  options(stringsAsFactors = FALSE)

  ##### Seasonal Forecast: Create input file and run daily EDI and copy output file
  StnDir = EnvList$StnDir
  StnFile = EnvList$StnFile
  ObsMonDir = EnvList$ObsMonDir
  ObsMonFile = EnvList$ObsMonFile
  FcstDataDir = EnvList$FcstDataDir
  EdiFcstDir = EnvList$EdiFcstDir
  Syear_Fcst = EnvList$Syear_Fcst
  Eyear_Fcst = EnvList$Eyear_Fcst

  # Create Output folder
  outdir = file.path(EdiFcstDir, "Output", fiyearmon)
  SetWorkingDir(outdir)

  #########################################################################
  ####################### Observation-based Simulation ####################
  ObsMonthlyCSV = file.path(ObsMonDir, ObsMonFile)

  Create.Monthly.EDI.Input.SForecast (ObsMonthlyCSV, outdir)

  ## Update NAMELIST.TXT file
  Update.NAMELIST (outdir, Syear_Fcst, Eyear_Fcst)

  EdiDailyExe <- system.file("extdata", "EDI-Monthly.exe", package = "rdrought")
  if(!file.exists(file.path(outdir, "EDI-Monthly.exe"))) { file.copy(EdiDailyExe, outdir) }

  ## Run Monthly EDI
  setwd(outdir)
  system("cmd.exe /c EDI-Monthly.exe")

  ## Rename the output.txt file
  FromDFile = paste(outdir, "/output.txt", sep="")
  ToDFile = paste(outdir, "/EDI_Monthly_Observed.txt", sep="")
  file.rename(FromDFile, ToDFile)

  #########################################################################
  ####################### Forecast-based Simulation ####################
  ### Combine Observed and Forecaste data into one file
  # Read Observed monthly data
  ObsMonDir = EnvList$ObsMonDir
  ObsMonFile = EnvList$ObsMonFile
  ObsMonthlyCSV = file.path(ObsMonDir, ObsMonFile)

  obsdata = read.csv(ObsMonthlyCSV, header=T)
  obsdata = obsdata[c("yearmon", "prec")]

  # Calculate climatology
  climdata = obsdata
  climdata$mon = substr(climdata$yearmon, 6, 7)
  climdata = climdata[which(as.numeric(substr(climdata$yearmon, 1, 4))>= Syear_Fcst & as.numeric(substr(climdata$yearmon, 1, 4)) <= Eyear_Fcst), c("mon", "prec")]
  climdata = aggregate(prec~mon, climdata, FUN=mean, na.rm=TRUE)

  # Read forecast monthly data
  curdir = file.path(FcstDataDir, fiyearmon)
  mdlnms = list.dirs(curdir, recursive = F)

  if(length(mdlnms) > 0){

    # get model names
    imsi = matrix(unlist(strsplit(mdlnms, "/")), nrow=length(mdlnms), byrow=T)
    mdlnms = imsi[, ncol(imsi)]
    mdlcnt = length(mdlnms)

    for(i in 1:mdlcnt){

      mdlnm = mdlnms[i]
      MdlDFile = paste(curdir, "/BestMon-", mdlnm, ".csv", sep="")
      mdldata = read.csv(MdlDFile, header=T)
      mdldata = mdldata[, c("yearmon", "prec")]

      ####### Combine Obs and Fcst Here ############
      ObsFirstDate = as.Date(sprintf("%s-01", obsdata[1, "yearmon"]))
      ObsLastDate = as.Date(sprintf("%s-01", obsdata[nrow(obsdata), "yearmon"]))
      FcstFirstDate = as.Date(sprintf("%s-01", mdldata[1, "yearmon"]))
      FcstLastDate = as.Date(sprintf("%s-01", mdldata[nrow(mdldata), "yearmon"]))

      # 예측자료 기간이 관측자료 기간에 완전히 포함되는 경우
      if(ObsLastDate >= FcstLastDate) {
        data = obsdata
        for(j in 1:nrow(mdldata)){
          data[which(data$yearmon == mdldata$yearmon[j]),] = mdldata[j,]
        }
      }

      # 예측자료 기간이 관측자료 기간에 일부 포함되는 경우
      if(ObsLastDate >= FcstFirstDate & ObsLastDate <= FcstLastDate) {
        rno = which(mdldata$yearmon == obsdata$yearmon[nrow(obsdata)])
        data = obsdata

        for(j in 1:rno){
          data[which(data$yearmon == mdldata$yearmon[j]),] = mdldata[j,]
        }

        data = rbind(data, mdldata[rno:nrow(mdldata),])
      }

      # 예측자료 기간이 관측자료 기간을 완전히 벗어날 때
      if(ObsLastDate <= FcstFirstDate) {
        yearmon = as.data.frame(substr(seq(ObsFirstDate, FcstLastDate, by = "mon"),1, 7))
        colnames(yearmon) = "yearmon"

        data = merge(yearmon, obsdata, all=T)

        for(j in 1:nrow(mdldata)){
          data[which(data$yearmon == mdldata$yearmon[j]),] = mdldata[j,]
        }

        # fill missing with climatology
        rnos = which(is.na(data$prec))
        mons = as.numeric(substr(data[rnos, "yearmon"], 6, 7))
        data[rnos, "prec"] = climdata[mons, "prec"]

      }

      # Convert unit from mm/day to mm/month
      data = prcp.mmday2mmmon(data)
      data$year = as.numeric(substr(data$yearmon,1,4))
      data$month = as.numeric(substr(data$yearmon,6, 7))
      #data$prec = signif(data$prec, 4)
      data$prec = as.numeric(sprintf("%10.1f", data$prec))
      data = data[c("year", "month", "prec")]

      outdir = file.path(EdiFcstDir, "Output", fiyearmon)
      OutDFile = file.path(outdir, "input.txt")
      write.fwf(data, OutDFile, width = c(5, 5, 10), sep = "", colnames = FALSE)

      #Create.Daily.EDI.Input.SForecast (FcstMonthlyCSV, outdir)
      #Create.Monthly.EDI.Input.SForecast (FcstMonthlyCSV, outdir)

      ## Update NAMELIST.TXT file
      Update.NAMELIST (outdir, Syear_Fcst, Eyear_Fcst)

      ## Run Monthly EDI
      setwd(outdir)
      system("cmd.exe /c EDI-Monthly.exe")

      ## Rename the output.txt file
      FromDFile = sprintf("%s/output.txt", outdir)
      ToDFile = sprintf("%s/I%s-%s.txt", outdir, fiyearmon, mdlnm)
      file.rename(FromDFile, ToDFile)



    }

  }


}
