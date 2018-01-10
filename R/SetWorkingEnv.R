#' @export
Set.Working.Environment  <- function(envfile, override=list()) {

  data <- yaml::yaml.load_file(envfile)

  data <- lapply(data, function(x) if (is.character(x)) gsubfn::gsubfn("\\$\\((.*?)\\)", data, x) else x)

  if(data$StnDir == "User") data$StnDir = data$ObsDayDir
  if(data$StnDir == "GHCN") data$StnDir = data$ObsGhcnDir

  if(!file.exists(data$EdiObsDir)) dir.create(data$EdiObsDir, showWarnings=F,recursive=T)
  if(!file.exists(data$EdiCcDir)) dir.create(data$EdiCcDir, showWarnings=F,recursive=T)
  if(!file.exists(data$EdiFcstDir)) dir.create(data$EdiFcstDir, showWarnings=F,recursive=T)
  if(!file.exists(data$BndDir)) dir.create(data$BndDir, showWarnings=F,recursive=T)
  if(!file.exists(data$ObsGhcnDir)) dir.create(data$ObsGhcnDir, showWarnings=F,recursive=T)
  if(!file.exists(data$ObsDayDir)) dir.create(data$ObsDayDir, showWarnings=F,recursive=T)
  if(!file.exists(data$ObsMonDir)) dir.create(data$ObsMonDir, showWarnings=F,recursive=T)
  if(!file.exists(data$SmplDir)) dir.create(data$SmplDir, showWarnings=F,recursive=T)

  outList = list("PrjDir"=data$PrjDir,
                 "DbDir"=data$DbDir,
                 "ObsDayDir"=data$ObsDayDir,
                 "ObsMonDir"=data$ObsMonDir,
                 "BndDir"=data$BndDir,
                 "StnDir"=data$StnDir,
                 "SmplDir"=data$SmplDir,
                 "EdiObsDir"=data$EdiObsDir,
                 "EdiCcDir"=data$EdiCcDir,
                 "Syear_Cal"=data$Syear_Cal,
                 "Eyear_Cal"=data$Eyear_Cal,
                 "StnFile"=data$StnFile,
                 "CntryCode3"=data$CntryCode3,
                 "bndry_shp"=data$bndry_shp,
                 "mask_grd"=data$mask_grd,
                 "CChangeOpt"=data$CChangeOpt,
                 "CcDataDir"=data$CcDataDir,
                 "MdlNms"=data$MdlNms,
                 "RcpNms"=data$RcpNms,
                 "DsNms"=data$DsNms,
                 "Syear_Obs"=data$Syear_Obs,
                 "Eyear_Obs"=data$Eyear_Obs,
                 "Syear_His"=data$Syear_His,
                 "Eyear_His"=data$Eyear_His,
                 "Syear_Scn"=data$Syear_Scn,
                 "Eyear_Scn"=data$Eyear_Scn,
                 "threshold"=data$threshold,
                 "EdiRefOpt"=data$EdiRefOpt,
                 "SForecastOpt"=data$SForecastOpt,
                 "FcstDataDir"=data$FcstDataDir,
                 "ObsMonFile"=data$ObsMonFile,
                 "EdiFcstDir"=data$EdiFcstDir,
                 "Syear_Fcst"=data$Syear_Fcst,
                 "Eyear_Fcst"=data$Eyear_Fcst)

  # override
  for (varname in names(override)) {
    outList[[varname]] = override[[varname]]
  }

  return(outList)
}
