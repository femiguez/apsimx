## Testing downloading soil data from ISRIC
## In repeated testing it seems that speed slows down when multiple queries are requested

require(apsimx)

run.test.isric.workflow <- get(".run.local.tests", envir = apsimx.options)

if(run.test.isric.workflow){
  
  trrry <- try(sp1 <- get_isric_soil_profile(lonlat = c(-93, 42)), silent = TRUE)
  
  if(!inherits(trrry, "try-error")){
    # This takes a few seconds
    system.time(sp1 <- get_isric_soil_profile(lonlat = c(-93, 42), fix = TRUE)) ## 1.508 seconds
    system.time(sp2 <- get_ssurgo_soil_profile(lonlat = c(-93, 42), fix = TRUE))  ## 1.858 seconds
    
    plot(sp1)
    
    plot(sp2[[1]])
    
    ## The code below now should add LL15, KL and XF
    sp3 <- get_isric_soil_profile(lonlat = c(-93, 42), fix = TRUE, xargs = list(crops = "Oilpalm"))
    ## Only get SSURGO tables
    ## stbls <- get_ssurgo_tables(lonlat = c(-93, 42))    
  }
}



if(run.test.isric.workflow){
  
  ### Testing the effectiveness of the 'xargs' option
  
  ### Testing the number of layers
  sp4 <- get_isric_soil_profile(lonlat = c(-93, 42), fix = TRUE, xargs = list(nlayers = 5))
  
  if(length(sp4$soil$Depth) != 5){
    stop("Length of sp4$soil$Depth should be equal to 5")
  }
  
  ### Testing the soil.bottom argument
  sp5 <- get_isric_soil_profile(lonlat = c(-93, 42), fix = TRUE, xargs = list(soil.bottom = 210))
  
  if(sum(sp5$soil$Thickness) * 1e-1 != 210){
    stop("Bottom of soil profile shuod be equal to 210")
  }
  
  ### Testing soil bottom and number of layers
  sp6 <- get_isric_soil_profile(lonlat = c(-93, 42), fix = TRUE, xargs = list(nlayers = 7, soil.bottom = 210))
  
  if(sum(sp6$soil$Thickness) * 1e-1 != 210){
    stop("Bottom of soil profile shuod be equal to 210")
  }
  if(length(sp6$soil$Depth) != 7){
    stop("Length of sp6$soil$Depth should be equal to 7")
  }
  
  ### This still works
  sp7 <- get_isric_soil_profile(lonlat = c(-93, 42), fix = TRUE, xargs = list(crops = "Canola"))
  
  if(sp7$crops != "Canola"){
    stop("sp7$crops should be 'Canola'")
  }
  
}
