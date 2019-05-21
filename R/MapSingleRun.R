#' Generate summary maps from a single run of AdpatR
#'
#' @param output.folder.path The directory where the output files are stored
#' @param run.name The name for the AdaptR simulation run
#' @return A set of output files written in the specified folder, each containing the run.name
#' @importFrom raster raster
#' @export
map.single.run <- 
  function(output.folder.path = getwd(), 
           run.name){
  ##_____________________________________________________________________________________##
  ## First, let's run some sanity checks on the parameters
  if(missing(run.name))
    stop("Need to specify a name for this run of Adaptor.")  
  if(missing(output.folder.path))
    warning("No output folder path specified: using working directory.")
  if(!file.exists(output.folder.path))
    stop("Output folder path invalid.")  # check the files are there  
  final.occurrence.fname <- file.path(output.folder.path,paste0(run.name,"_final_occurrence.asc")) 
  if(!file.exists(final.occurrence.fname))
    stop("Output files with this run name were not found in the specified folder.")
  ## find adaptive env grids for this run...
  Z.files<-list.files(paste0(output.folder.path), pattern=glob2rx(paste0(run.name,"*_final_Z.asc")),full=T)
  Vp.files<-list.files(paste0(output.folder.path), pattern=glob2rx(paste0(run.name,"*_final_Vp.asc")),full=T)
  if(length(Z.files) != length(Vp.files))
    stop("Different number of output grids for env tolerance mean (z) and stdev (vp).")
  
  if(length(Z.files) > 0){
    for(i.env in 1:length(Z.files)){
      env.z.fname <- Z.files[i.env]
      env.vp.fname <- Vp.files[i.env]
      if(!file.exists(env.z.fname))
        stop("Output files (adaptive environments) with this run name were not found in the specified folder.")
      if(!file.exists(env.vp.fname))
        stop("Output files (adaptive environments) with this run name were not found in the specified folder.")
      } # end for i.env
    } # end if n.env.vars.adapt > 0   
  ##_____________________________________________________________________________________##
  
  final.occurrence.ras <- raster::raster(final.occurrence.fname)
  plot(final.occurrence.ras, main="Occupancy",col=c("grey","black"), legend=F)

  if(length(Z.files) > 0){
    for(i.env in 1:length(Z.files)){
      env.z.fname <- Z.files[i.env]
      env.vp.fname <- Vp.files[i.env]
      env.z.ras <- raster::raster(env.z.fname)
      env.vp.ras <- raster::raster(env.vp.fname)
      env.ID <- substr(Z.files[i.env], nchar(Z.files[i.env])-13+1, nchar(Z.files[i.env])-12)
      plot(final.occurrence.ras, main=paste0("Mean tolerance for environment ",env.ID), col=c("grey","black"),legend=F)
      plot(env.z.ras ,add=T)
      plot(final.occurrence.ras, main=paste0("Variance in tolerance for environment ",env.ID), col=c("grey","black"),legend=F)
      plot(env.vp.ras,add=T)
      } # end for i.env
    } # end if n.env.vars.adapt > 0
  
  ##_____________________________________________________________________________________##

  } # End map.single.run function
