#' Create compacted environmental grids for AdaptR
#'
#' @param compactor.parameter.file.name A file path to a text file (.txt) to be created by this function, where the grid compactor parameter file will be saved
#' @param ncols The number of columns on the spatial grid
#' @param nrows The number of rows on the spatial grid
#' @param n.env.vars The number of environment variables being considered
#' @param n.time.points The number of time points for which compacted environment grids are to be created
#' @param raw.env.grids.name.files A list of length n.env.vars of filepaths to text files holding the filepaths of individual environment grids to be compacted.
#' @param output.env.name.file A filepath to a text file (.txt) holding the filepaths where the compacted environment grids will be saved. 
#' @return A set of compacted environment files in the specified location

#' @importFrom Rcpp evalCpp
#' @useDynLib AdaptR
#' @export
CompactGrids <- 
function(compactor.parameter.file.name,
                       ncols,
                       nrows,
                       n.env.vars,
                       n.time.points,
                       raw.env.grids.name.files,
                       output.env.name.file){
  ##_____________________________________________________________________________________##
  ## First, let's run some sanity checks on the parameters
  if(missing(compactor.parameter.file.name))
    stop("Need to specify a file path for the environment grid compactor parameter file (compactor.parameter.file.name).")  
  if(missing(ncols))
    stop("Need to specify the number of columns in the grid (ncols).")
  if(missing(nrows))
    stop("Need to specify the number of rows in the grid (nrows).")  
  if(missing(n.time.points))
    stop("Need to specify the number of time points for grids to be compacted (n.time.points).")  
  if(missing(n.env.vars))
    stop("Need to specify the number of environmental variables (n.env.vars) for which compacted grids are to be created.")  
  if(!(length(raw.env.grids.name.files) == n.env.vars))
    stop("You have specified a different number of raw environment variable file names to n.env.vars.")
  for(i.env in 1:n.env.vars){
    if(!file.exists(raw.env.grids.name.files[i.env]))
      stop("Need to specify valid file providing names for the environment grids for each variable (raw.env.grids.name.files).")
  } # end for i.env  
  if(!file.exists(output.env.name.file))
    stop("Need to specify valid file providing names for the compacted environment grids (output.env.name.file).")  
  
  ##_____________________________________________________________________________________##
  ## Now, write the compactor parameter file...
  parameter.file = compactor.parameter.file.name                    ##CHANGED TO TAKE A SPECIFIED PARAMETER FILE NAME
  jobscript = file(parameter.file,'w')   # 'wb' might create a "unix-friendly" binary mode
  writeLines(paste0("NCOLS ",ncols), con = jobscript)
  writeLines(paste0("NROWS ",nrows), con = jobscript)
  writeLines(paste0("NUM_ENVIRONMENT_VARIABLES ", n.env.vars), con = jobscript)  
  writeLines(paste0("NUM_TIME_POINTS ",n.time.points), con = jobscript)
  writeLines("ENV_NAME_FILES", con = jobscript)
  for(i.env in 1:n.env.vars){ 
    writeLines(raw.env.grids.name.files[i.env], con = jobscript)    
  }# end for i.env
  writeLines("OUTPUT_NAME_FILE", con = jobscript)
  writeLines(output.env.name.file, con = jobscript)
  writeLines("PERCENTAGE_SUCCESS 100",  con = jobscript)
  close(jobscript)
  
  ## Now test that the parameter file has been written out properly
  if(!file.exists(parameter.file))
    stop("Creation of the compactor parameter file has failed. Ensure a correct file path and suitable name have been provided for the 'compactor.parameter.file.name' argument, so a parameter file can be created.")  
  
  ##_____________________________________________________________________________________##  
  # load the dll
#  package.path<-system.file(package="AdaptR")
#  r_arch <- .Platform$r_arch
#  file.path.source<-file.path(package.path, "libs", r_arch, "main.dll")
  # load the dll
#  dyn.load(file.path.source)  # call MuruCompactor from dll
#  Compactor.out <- .C("MuruCompactor",  argv = as.character(c(parameter.file)), arg_i_catch = as.integer(c(0,0)))
#  dyn.unload(file.path.source)
  ##_____________________________________________________________________________________##   
  Compactor.out <- rcpp_MuruCompactor(parameter.file)  
  
  ##_____________________________________________________________________________________##  
  
  if(Compactor.out == -2 )
    stop("CompactGrids has not run because the parameter file is formatted incorrectly.")  
  if(Compactor.out == -1 )
    stop("CompactGrids has not run because the parameter file does not exist.")   

  message(paste0("Grid compaction completed. The compacted grids are in the specified output folder."))
    
} # end CompactGrids function
