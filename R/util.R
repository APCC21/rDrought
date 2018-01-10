#' @export
Extract.EDI_IDF <- function(EdiDataFile, threshold, syear, eyear) {

  #EdiDataFile = "G:/SCService-RM/Bicol_Drought/CChange/Output/bcc-csm1-1-m/CSSAC_SQM_bcc-csm1-1-m_historical.txt"
  #threshold = 0
  #syear = 1976
  #eyear = 2005

  library(dplyr)

  options(stringsAsFactors = FALSE)

  data = read.table(EdiDataFile, header=T, colClasses  = c("character", "double"))
  data = data[which(as.numeric(substr(data$Date, 1, 4)) >= syear & as.numeric(substr(data$Date, 1, 4)) <= eyear), c("Date", "EDI")]

  cnt = 0
  daycnt = 0
  maxedi = 99

  for(i in 1:nrow(data)){

    if(i == 1){
      pval = cval = data[i, "EDI"]
      pdate = cdate = data[i, "Date"]
    } else {
      pval = data[i-1, "EDI"]
      pdate = data[i-1, "Date"]
      cval = data[i, "EDI"]
      cdate = data[i, "Date"]
    }

    if(daycnt == 0 & cval <= threshold){
      sdate = cdate
      daycnt = daycnt + 1
      if(cval <= maxedi) {maxedi = cval}
    } else if(daycnt > 0 & cval <= threshold) {
      daycnt = daycnt + 1
      if(cval <= maxedi) {maxedi = cval}
    } else if(daycnt > 0 & cval > threshold) {
      edate = pdate
      if(cnt == 0){
        out = c(sdate, edate, daycnt, maxedi)
        cnt = cnt + 1
      } else {
        tmp = c(sdate, edate, daycnt, maxedi)
        out = rbind(out, tmp)
        cnt = cnt + 1
      }
      daycnt = 0
      maxedi = 99

    }

  }


  if(cnt == 0) {
    out = as.data.frame(matrix(ncol=7, nrow=0))
    #colnames(out) = c("SDate", "EDate", "Duration", "Intensity", "mdlnm", "rcpnm", "stnnm")
    colnames(out) = c("SDate", "EDate", "Duration", "Intensity")
  } else {
    if(cnt == 1){
      out = as.data.frame(t(out))
    } else {
      out = as.data.frame(out)
    }

    colnames(out) = c("SDate", "EDate", "Duration", "Intensity")
    row.names(out) = NULL
    out$Duration = as.numeric(out$Duration)
    out$Intensity = as.numeric(out$Intensity)
  }

  return(out)

}


#' @export
Get.Station.Data.9var <- function(stnid, wdir) {

  #wdir = "F:/SForecast-TestRun/Database/obs_samples/asos-daily"
  #stnid = "ID090"

  setwd(wdir)

  srchstr = paste("*", stnid, "*.csv", sep="")
  obsfile <- list.files(wdir, pattern = glob2rx(srchstr), full.names=F)

  obs = read.csv(obsfile, header=T, na.strings = c("-99", "-99.00", "NA"))
  obs = obs[which(!is.na(obs$Year)),]
  colcnt = ncol(obs)
  datestr <- paste(obs[[1]],obs[[2]],obs[[3]],sep="-")
  date <- as.Date(datestr,"%Y-%m-%d")

  if(colcnt == 9){
    obs = cbind(date, obs[,c(4:9)])
    obs[,8:10] = NA
  }

  if(colcnt == 12){
    obs = cbind(date, obs[,c(4:12)])
  }
  year=as.numeric (format(obs[,1],"%Y"))
  month=as.numeric (format(obs[,1],"%m"))
  yearmon=as.character (format(obs[,1],"%Y-%m"))
  data <- cbind(year, month, yearmon, obs)

  return(data)
}

#' @export
Cal.Monthly.Station.Mean <- function(EnvList, varnms){

  library(reshape)
  library(doBy)

  StnDir = EnvList$StnDir
  StnFile = EnvList$StnFile
  ObsMonDir = EnvList$ObsMonDir
  ObsMonFile = EnvList$ObsMonFile
  ObsDayDir = EnvList$ObsDayDir

  VarDFile = file.path(ObsMonDir, ObsMonFile)

  ###### Get Station ID, lat, and Lon information
  setwd(StnDir)
  stninfo = read.csv(StnFile, header=T)
  colnames(stninfo) = c("Lon","Lat", "Elev", "ID", "Ename", "SYear")
  SYear = max(as.numeric(stninfo$SYear)) + 1
  stninfo = stninfo[which(stninfo$SYear <= SYear),c("ID", "Lon", "Lat", "SYear")]
  stnnms = matrix(stninfo$ID)

  for(j in 1:length(stnnms)){
    stnid = stnnms[j]

    obsdaydata = Get.Station.Data.9var(stnid, ObsDayDir)
    colnames(obsdaydata) = c("year", "month", "yearmon", "date", "prec", "tmax", "tmin", "wspd", "rhum", "rsds", "sshine", "cloud", "tavg")
    obsdaydata$t2m = (obsdaydata$tmax + obsdaydata$tmin)/2
    obsdaydata$stnid = stnid

    # select common period
    obsdaydata = obsdaydata[which(obsdaydata$year >= SYear),]

    if(j == 1){
      data = obsdaydata
    } else {
      temp = obsdaydata
      data = rbind(data, temp)
    }
  }

  mdata = reshape::melt(data, id=c("stnid", "yearmon", "year", "month", "date"))

  outdata = doBy::summaryBy(value~variable+yearmon, data=mdata, FUN = mean, na.rm=T)
  meandata = reshape(outdata, idvar = c("yearmon"), timevar = "variable", direction = "wide")
  colnames(meandata) = c("yearmon", "prec", "tmax", "tmin", "wspd", "rhum", "rsds", "sshine", "cloud", "tavg", "t2m")

  ##### just 2 varialbes #########################
  #meandata = meandata[c("yearmon", "prec", "t2m")]
  meandata = meandata[c("yearmon", varnms)]
  setwd(ObsMonDir)
  write.csv(meandata, ObsMonFile, row.names=F)

}

#' @export
SetWorkingDir <- function(wdir) {

  dir.create(wdir, showWarnings=F,recursive=T)
  setwd(wdir)

}

#' @export
prcp.mmday2mmmon <- function(data) {

  moncnt = nrow(data)
  for(i in 1:moncnt){
    daycnt  = numberOfDays(as.Date(paste(data$yearmon[i], "-1", sep="")))
    data[i,2:ncol(data)] = data[i,2:ncol(data)] * daycnt
  }
  return(data)

}

#' @export
numberOfDays <- function(date) {
  m <- format(date, format="%m")

  while (format(date, format="%m") == m) {
    date <- date + 1
  }

  return(as.integer(format(date - 1, format="%d")))
}

#' @export
Update.NAMELIST <- function(wdir, Msyr, Meyr) {
  #wdir = "F:/Drought/Korea/Observed"
  #Msyr = 1976
  #Meyr = 2005

  setwd(wdir)
  sink("NAMELIST.TXT")
  cat("&IOFILE\n")
  cat(" IFILE = 'input.txt',\n")
  cat(" OFILE = 'output.txt' /\n")
  cat("# Set input file (precipitation data) & output file (AWRI & EDI data)\n")
  cat("\n")
  cat("&CalibPeriod\n")
  cat(sprintf(" Msyr = %s,\n", Msyr))
  cat(sprintf(" Meyr = %s /\n", Meyr))
  cat("# Set calibration period (It is recommended to set 30 years, climatological period)\n")
  sink()

}

#' @export
Create.EDI.Map.Data <- function(StnDFile, EdiOutList, MapDate) {

  #StnDFile = file.path(EnvList$StnDir, EnvList$StnFile)
  #EdiOutList = list.files(file.path(EnvList$EdiObsDir, "Output"), pattern = glob2rx("*EDI*.txt"), full.names = T)
  #MapDate = "1983-07-21"

  stndf = read.csv(StnDFile, header = T)
  stnnms = stndf$ID
  cnt = 0
  for(i in 1:length(stnnms)){
    stnnm = stnnms[i]
    X = stndf[which(stndf$ID == stnnm), "Lon"]
    Y = stndf[which(stndf$ID == stnnm), "Lat"]

    edifile = grep(stnnm, EdiOutList, value=TRUE)
    if(length(edifile) == 1){
      edidf = read.table(edifile, header = T, sep="")
      Z = edidf[which(edidf$Date == MapDate), "EDI"]
      out = cbind(X, Y, Z)
      cnt = cnt + 1
    }


    if(cnt == 1){
      mapout = out
    } else {
      tmp = out
      mapout = rbind(mapout, tmp)
    }
  }

  return(as.data.frame(mapout))

}

#' @export
Download.GIS.Data <- function(EnvList, CntryCode3) {

  BndDir = EnvList$BndDir

  # Boundary vector file
  setwd(BndDir)
  bnd.v <-raster::getData('GADM', country=CntryCode3, level=1)

  # Mask Raster file which will be used for interpolation
  fname = paste(CntryCode3, "_msk_alt.zip", sep="")
  MskDFile = file.path(BndDir, fname)
  baseurl = "http://biogeo.ucdavis.edu/data/diva/msk_alt"
  theurl = paste(baseurl, "/", CntryCode3, "_msk_alt.zip", sep="")
  download.file(theurl, MskDFile, mode = "wb")
  unzip(MskDFile, exdir = BndDir)
  mask.r <- raster(file.path(BndDir, paste(CntryCode3, "_msk_alt.grd", sep="")))

  outList = list("bnd.v"=bnd.v, "mask.r"=mask.r)

  return(outList)
}

#' @export
Create.Interpolated.EDI.Map <- function(EnvList, MapDate) {

  #MapDate = "1981-03-23"

  library(gstat)
  library(raster)
  library(maptools)

  EdiObsDir = EnvList$EdiObsDir
  StnDir = EnvList$StnDir
  StnFile = EnvList$StnFile
  BndDir = EnvList$BndDir
  bndry_shp = EnvList$bndry_shp
  mask_grd = EnvList$mask_grd

  stnnms = read.csv(file.path(StnDir, StnFile), header = T)[c("ID")]

  outdir = file.path(EdiObsDir, "Output")
  analdir = file.path(EdiObsDir, "Analysis")
  SetWorkingDir(analdir)

  # Download country bounday
  # Clipping raster using boundary shape file in QGIS: https://i.stack.imgur.com/24dvp.png
  mask.r = raster(file.path(BndDir, mask_grd))
  bnd.shp = maptools::readShapeSpatial(file.path(BndDir, bndry_shp))

  ## Extract EDI values
  StnDFile = file.path(EnvList$StnDir, EnvList$StnFile)
  EdiOutList = list.files(file.path(EnvList$EdiObsDir, "Output"), pattern = glob2rx("*EDI*.txt"), full.names = T)
  edi = Create.EDI.Map.Data (StnDFile, EdiOutList, MapDate)
  colnames(edi) = c("x", "y", "z")

  # inverse distance weighted interpolation with inverse distance power of 0.5
  idw = gstat::gstat(id = "z", formula = z~1, locations = ~x+y, data=edi, nmax=7, set=list(idp = 0.5))
  # now use the interpolation model to interpolate values to the pre-defined and Raster
  idw.r <- raster::interpolate(mask.r, idw)
  idw.r <- raster::mask(idw.r, mask.r)

  #create color ramp starting from red to blue
  png(file.path(analdir, paste("Daily-EDI_", MapDate, ".png", sep="")))
    red2blue <- colorRampPalette(c('red', 'gray96', 'blue'))
    breakpoints = seq(-3.0, 3.0, 0.5)
    plot(idw.r, col=red2blue(n=length(breakpoints)), breaks=breakpoints,
         legend.width=1, legend.shrink=0.75,
         legend.args=list(text='EDI', side=3, font=2, line=0.8, cex=1.2),
         main = sprintf("Drought monitoring on %s", MapDate))
    plot(bnd.shp, add=T)
  dev.off()

}

