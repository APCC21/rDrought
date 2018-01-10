EDI.Duration.Intensity.Graph <- function(EdiCcDir,...) {

  #EdiCcDir = "D:/AIMS_Test/Projects/64705e7c-b261-45e8-88c7-822c23e52133/DroughtIndex/CChange"

  library(ggplot2)

  options(stringsAsFactors = FALSE)

  SmryDFile = file.path(EdiCcDir, "Analysis/Drought_Summary.csv")

  smry <- read.csv(SmryDFile, header = T)
  stnnms <- unique(smry$stnnm)
  varnms <- c("Duration","Intensity")
  imsi = matrix(unlist(strsplit(smry[!(smry$rcpnm=="Observed" | smry$rcpnm=="historical"), "rcpnm"], "-")), ncol = 2, byrow=T)
  periods = unique(imsi[,2])

  smry <- smry[which(smry$Duration >= 10),  ]

  for(i in 1:length(stnnms)){
    stnnm <- stnnms[i]
    for(j in 1:length(varnms)){
      varnm <- varnms[j]

      for(k in 1:length(periods)){
        period = periods[k]

        #smry_stn <- smry[-which(smry$rcpnm == "Observed"),]

        smry_stn <- smry[which(smry$stnnm == stnnm & (smry$rcpnm == "historical" | grepl(period, smry$rcpnm))), c(varnm, "mdlnm", "rcpnm")]
        #smry_stn <- smry[which(smry$stnnm == stnnm & (smry$rcpnm == "Observed" | smry$rcpnm == "historical" | grepl(period, smry$rcpnm))), c(varnm, "mdlnm", "rcpnm")]
        smry_melt <- reshape2::melt(data = smry_stn, id.vars = c("rcpnm"), measure.vars = c(varnm))
        if(varnm == "Duration"){
          smry_melt$value <- log10(smry_melt$value)
          gg <- ggplot(smry_melt,aes(x=value, colour=rcpnm)) +
            geom_density() +
            theme(legend.title=element_blank()) + scale_x_continuous(name = "log10(Duration)") +
            scale_y_continuous(name = "Density")
          ggpath <- sprintf("%s-%s-S%s.png", stnnm, varnm, period)
          ggsave(ggpath,gg)
        }
        if(varnm == "Intensity"){
          #smry_melt$value <- log(-smry_melt$value)
          gg <- ggplot(smry_melt,aes(x=value, colour=rcpnm)) +
            geom_density() +
            theme(legend.title=element_blank()) + scale_x_continuous(name = "Intensity") +
            scale_y_continuous(name = "Density")
          ggpath <- sprintf("%s-%s-s%s.png", stnnm, varnm, period)
          ggsave(ggpath,gg)
        }

      }

    }
  }


}

#' @export
Create.Daily.EDI.Input.CChange <- function(CcDataDir, dsnm, mdlnm, rcpnm, stnnm, outdir) {

  library(gdata)

  CcDailyCSV = paste(CcDataDir, "/", dsnm, "/", mdlnm, "/", stnnm, "_", dsnm, "_", mdlnm, "_",  rcpnm, ".csv", sep="")

  if(rcpnm == "rcp45" | rcpnm == "rcp85"){
    HistDailyCSV = paste(CcDataDir, "/", dsnm, "/", mdlnm, "/", stnnm, "_", dsnm, "_", mdlnm, "_historical.csv", sep="")
    hist = read.csv(HistDailyCSV, header = T)
    rcp = read.csv(CcDailyCSV, header = T)
    data = rbind(hist, rcp)
  } else {
    data = read.csv(CcDailyCSV, header = T)
  }

  colnm = colnames(data)

  #### Fill climatology for HadGEM, because December is missing.
  ##### Temp: Need to improve using QC algorithm ?
  date = as.data.frame(seq(as.Date(sprintf("%d-01-01",min(data$year))), as.Date(sprintf("%d-12-31",max(data$year))), by="day"))
  colnames(date) = c("date")
  if(nrow(date) != nrow(data)){
    data$date = as.Date(sprintf("%d-%02d-%02d", data$year, data$mon, data$day))
    data = merge(date, data, all=T)
    data$year = substr(data$date,1,4); data$mon = as.numeric(substr(data$date,6,7)); data$day = as.numeric(substr(data$date,9,10))
    data$monday = substr(data$date, 6,10)
    temp <-data[c("prcp", "monday")]
	  #temp <-data[c("prcp", "tmax", "tmin", "wspd", "rhum", "rsds", "monday")]
    clim <- aggregate(.~monday, data = temp, FUN = mean)
    for(i in which(is.na(data$prcp))){data[i, "prcp"] = clim[which(clim$monday == data[i, "monday"]), "prcp"]}
    #for(i in which(is.na(data$tmax))){data[i, "tmax"] = clim[which(clim$monday == data[i, "monday"]), "tmax"]}
    #for(i in which(is.na(data$tmin))){data[i, "tmin"] = clim[which(clim$monday == data[i, "monday"]), "tmin"]}
    #for(i in which(is.na(data$wspd))){data[i, "wspd"] = clim[which(clim$monday == data[i, "monday"]), "wspd"]}
    #for(i in which(is.na(data$rhum))){data[i, "rhum"] = clim[which(clim$monday == data[i, "monday"]), "rhum"]}
    #for(i in which(is.na(data$rsds))){data[i, "rsds"] = clim[which(clim$monday == data[i, "monday"]), "rsds"]}
    #data <- data[c("year", "mon", "day", "prcp", "tmax", "tmin", "wspd", "rhum", "rsds")]
	  data <- data[c(colnm)]
  }

  data = data[, c("year", "mon", "day", "prcp")]
  colnames(data) = c("year", "mon", "day", "prcp")
  syear = min(data$year); eyear = max(data$year)

  data$mon = as.numeric(data$mon)
  data$day = as.numeric(data$day)
  data$prcp = as.numeric(sprintf("%10.2f", data$prcp))

  data[which(data$mon == 2 & data$day == 29)-1, "prcp"] = data[which(data$mon == 2 & data$day == 29)-1, "prcp"] + data[which(data$mon == 2 & data$day == 29), "prcp"]

  out = data[-which(data$mon == 2 & data$day == 29),]


  OutDFile = file.path(outdir, "input.txt")
  write.fwf(out, OutDFile, width = c(4,4,4,10), sep = "", colnames = FALSE)

  outList = list("syear"= syear, "eyear"=eyear)
  return(outList)

}

#############################################################################
#' @export
# StnDir = EnvList$StnDir
# StnFile = EnvList$StnFile
# CcDataDir = EnvList$CcDataDir
# EdiCcDir = EnvList$EdiCcDir
# Syear_His = EnvList$Syear_His
# Eyear_His = EnvList$Eyear_His
# MdlNms = EnvList$MdlNms
# RcpNms = EnvList$RcpNms
# DsNms = EnvList$DsNms
#
Run.Daily.EDI.cchange <- function(StnDir, StnFile, CcDataDir, EdiCcDir, MdlNms, RcpNms, DsNms, Syear_His, Eyear_His,...) {


  RcpNms <- c("historical", RcpNms)

  stnnms = read.csv(file.path(StnDir, StnFile), header = T)[, "ID"]

  for(ii in 1:length(DsNms)){
    dsnm = DsNms[ii]

    for(i in 1:length(MdlNms)){
      mdlnm = MdlNms[i]

      FromDFile = paste(EdiCcDir, "/Output/EDI-Daily.exe", sep="")
      mdldir = paste(EdiCcDir, "/Output/", mdlnm, sep="")
      SetWorkingDir(mdldir)
      ToDFile = paste(mdldir, "/EDI-Daily.exe", sep="")
      file.copy(FromDFile, ToDFile)

      for(j in 1:length(RcpNms)){
        rcpnm = RcpNms[j]

        for(k in 1:length(stnnms)){
          stnnm = stnnms[k]

          # Check file does exist
          CcDailyCSV = paste(CcDataDir, "/", dsnm, "/", mdlnm, "/", stnnm, "_", dsnm, "_", mdlnm, "_",  rcpnm, ".csv", sep="")
          if(file.exists(CcDailyCSV)){

            outdir = file.path(EdiCcDir, "Output", mdlnm)
            SetWorkingDir(outdir)

            years = Create.Daily.EDI.Input.CChange (CcDataDir, dsnm, mdlnm, rcpnm, stnnm, outdir)
            syear = years$syear; eyear = years$eyear

            ## Update NAMELIST.TXT file
            if(syear <= Syear_His) Syear_Cal = Syear_His + 1
            Update.NAMELIST (outdir, Syear_Cal, Eyear_His)

            EdiDailyExe <- system.file("extdata", "EDI-Daily.exe", package = "rdrought")
            if(!file.exists(file.path(outdir, "EDI-Daily.exe"))) { file.copy(EdiDailyExe, outdir) }

            ## Run Daily EDI
            setwd(outdir)
            system("cmd.exe /c EDI-Daily.exe")

            ## Rename the output.txt file
            FromDFile = paste(outdir, "/output.txt", sep="")
            ToDFile = paste(outdir, "/", stnnm, "_", dsnm, "_", mdlnm, "_",  rcpnm, ".txt", sep="")
            file.rename(FromDFile, ToDFile)

          } else {
            message(sprintf("File does not exist: %s\n", CcDailyCSV))
          } # End IF

        } # Station Loop
      } # RCP Loop
    } # Model Loop

  } # Downscaling Loop



}


###############################################################################
#' @export
# MdlNms <- EnvList$MdlNms
# RcpNms <- EnvList$RcpNms
# PrjDir <- EnvList$PrjDir
# EdiObsDir <- EnvList$EdiObsDir
# EdiCcDir <- EnvList$EdiCcDir
# StnDir <- EnvList$StnDir
# StnFile <- EnvList$StnFile
# DsNms <- EnvList$DsNms
# Syear_Obs <- EnvList$Syear_Obs
# Eyear_Obs <- EnvList$Eyear_Obs
# Syear_His <- EnvList$Syear_His
# Eyear_His <- EnvList$Eyear_His
# Syear_Scn <- EnvList$Syear_Scn
# Eyear_Scn <- EnvList$Eyear_Scn
# threshold = -1.0
#
Extract.EDI.Duration.Intensity.CChange <- function(MdlNms, RcpNms, PrjDir, EdiObsDir, EdiCcDir, StnDir, StnFile, DsNms, Syear_Obs, Eyear_Obs, Syear_His, Eyear_His, Syear_Scn, Eyear_Scn, threshold,...) {

  #StnNms = as.list(read.csv(file.path(StnDir, StnFile), header=T)["ID"])
  StnNms = read.csv(file.path(StnDir, StnFile), header=T)
  StnNms = StnNms[,c("ID")]

  NewRcpNms <- c("historical", RcpNms)

  cnt = 1
  #### Observed
  for(k in 1:length(StnNms)){
    stnnm = StnNms[k]

    filename = sprintf("%s-EDI_Obs.txt", stnnm)
    EdiDataFile <- file.path(EdiObsDir, "Output", filename)

    syear = Syear_Obs; eyear = Eyear_Obs
    out = Extract.EDI_IDF (EdiDataFile, threshold, syear, eyear)
    if(nrow(out) > 0){
      out$mdlnm = "OBS"; out$rcpnm = "Observed"; out$stnnm = stnnm
      if(cnt == 1){
        smry = out
      } else {
        temp = out
        smry = rbind(smry, temp)
      }
      cnt = cnt + 1
    }

    message(sprintf("Observed,  %s  has been finished", stnnm))

  }

  #### Climate Chnage Scenarios
  for(i in 1:length(MdlNms)){
    mdlnm = MdlNms[i]
    for(j in 1:length(NewRcpNms)){
      rcpnm = NewRcpNms[j]
      for(k in 1:length(StnNms)){
        stnnm = StnNms[k]

        filename = sprintf("%s_%s_%s_%s.txt", stnnm, DsNms, mdlnm, rcpnm)
        EdiDataFile <- file.path(EdiCcDir, "Output", mdlnm, filename)

        if(rcpnm == "historical"){
          syear = Syear_His; eyear = Eyear_His
          out = Extract.EDI_IDF (EdiDataFile, threshold, syear, eyear)


          if(nrow(out) > 0){
            out$mdlnm = mdlnm; out$rcpnm = rcpnm; out$stnnm = stnnm
            if(cnt == 1){
              smry = out
            } else {
              temp = out
              smry = rbind(smry, temp)
            }
            cnt = cnt + 1
          }

        } else {
          for(m in 1:length(Syear_Scn)){
            syear = Syear_Scn[m]; eyear = Eyear_Scn[m]
            out = Extract.EDI_IDF (EdiDataFile, threshold, syear, eyear)

            if(nrow(out) > 0){
              out$mdlnm = mdlnm; out$rcpnm = sprintf("%s-%02d", rcpnm, m); out$stnnm = stnnm

              if(cnt == 1){
                smry = out
              } else {
                temp = out
                smry = rbind(smry, temp)
              }
              cnt = cnt + 1
            }

          } # Future period
        } # Historical or RCPs

        message(sprintf("%s,   %s,  %s  has been finished", mdlnm, rcpnm, stnnm))

      } # Stations

    } # RCPs

  } # GCMs

  # Error: https://stackoverflow.com/questions/17291283/outputting-a-dataframe-in-r-to-a-csv
  smry <- data.frame(lapply(smry, as.character), stringsAsFactors=FALSE)
  outdir <- file.path(EdiCcDir, "Analysis")
  SetWorkingDir(outdir)
  write.csv(smry, "Drought_Summary.csv", row.names = F)

}



EDI.CChange.Analysis  <- function(MdlNms, RcpNms, PrjDir, ObsDayDir, EdiObsDir, CcDataDir, EdiCcDir, StnDir, StnFile, DsNms, Syear_Cal, Eyear_Cal, Syear_Obs, Eyear_Obs, Syear_His, Eyear_His, Syear_Scn, Eyear_Scn, threshold,...) {

  Run.Daily.EDI.Observed (StnDir, StnFile, ObsDayDir, EdiObsDir, Syear_Cal, Eyear_Cal)
  Run.Daily.EDI.cchange (StnDir, StnFile, CcDataDir, EdiCcDir, MdlNms, RcpNms, DsNms, Syear_His, Eyear_His)
  Extract.EDI.Duration.Intensity.CChange (MdlNms, RcpNms, PrjDir, EdiObsDir, EdiCcDir, StnDir, StnFile, DsNms, Syear_Obs, Eyear_Obs, Syear_His, Eyear_His, Syear_Scn, Eyear_Scn, threshold)
  EDI.Duration.Intensity.Graph (EdiCcDir)
}
