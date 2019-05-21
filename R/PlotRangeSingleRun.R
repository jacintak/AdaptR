#' Generate summary plot of range dynamics from a single run of AdpatR
#'
#' @param output.folder.path The directory where the output files are stored
#' @param run.name The name for the AdaptR simulation run
#' @return summary plots of the entire distribution over time

#' @export
plot.range.single.run <- 
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
    temporal.occupancy.fname <- file.path(output.folder.path,paste0(run.name,"_occupancy_summary.txt")) 
    if(!file.exists(temporal.occupancy.fname))
      stop("Output files with this run name were not found in the specified folder.")
    ##_____________________________________________________________________________________##
    # read in the data
    R1 = read.table(temporal.occupancy.fname, header=TRUE)
    # work out how many adaptive environment variables we have
    if(ncol(R1)>4)
      {
      n.env <- (ncol(R1)-4)/2
      # Now loop through the adaptive env variables and plot them
      for(i.env in 1:n.env)
        {
        par(mfrow=c(3,1), mar=c(0,4,0.5,1), mgp =c(2.5,0.5,0),las=1)
        # Range RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
        plot(Cells_Occupied/1000 ~ Time, data=R1, type="l", ylim=c(0,((max(R1$Cells_Occupied)*1.1))/1000), ylab="Range Grid Cells (x1000)", axes=F)
        polygon(x=c(R1$Time,rev(R1$Time)), y=c(R1$Cells_Occupied/1000,rev(R1$Cells_Occupied/1000)), col="lightgrey",border="lightgrey" )
        lines(Cells_Occupied/1000 ~ Time, data=R1)
        box()
        axis(side=2, at=round(seq(0,(max(R1$Cells_Occupied)*1.1)/1000,len=7),1), las=1 )
        # Adaptive tolerance trait value AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA   
        Z.min<-min(R1[,(3+(i.env*2))])
        Z.max<-max(R1[,(3+(i.env*2))])
        Z.range<-Z.max-Z.min
        Z.buffer<-0.1*Z.range
        plot(R1[,(3+(i.env*2))] ~ Time, data=R1, type="l", ylim=c((Z.min-Z.buffer),(Z.max+Z.buffer)), ylab="Mean Env tolerance", axes=F)
        polygon(x=c(R1$Time,rev(R1$Time)), y=c(R1[,(3+(i.env*2))],rev(R1[,(3+(i.env*2))])), col="lightgrey",border="lightgrey" )
        lines(R1[,(3+(i.env*2))] ~ Time, data=R1)
        box()
        axis(side=2, at=round(seq((Z.min-Z.buffer),(Z.max+Z.buffer),len=7),1), las=1 )
        # Adaptive tolerance trait variance VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV 
        vp.min<-min(R1[,(4+(i.env*2))])
        vp.max<-max(R1[,(4+(i.env*2))])
        vp.range<-vp.max-vp.min
        vp.buffer<-0.1*vp.range
        par(mar=c(3.5,4,0.5,1))
        plot(R1[,(4+(i.env*2))] ~ Time, data=R1, type="l", ylim=c((vp.min-vp.buffer),(vp.max+vp.buffer)), ylab="Variance Env. Tol.", axes=F)
        polygon(x=c(R1$Time,rev(R1$Time)), y=c(R1[,(4+(i.env*2))],rev(R1[,(4+(i.env*2))])), col="lightgrey",border="lightgrey" )
        lines(R1[,(4+(i.env*2))] ~ Time, data=R1)
        box()
        axis(side=2, at=round(seq((vp.min-vp.buffer),(vp.max+vp.buffer),len=7),1), las=1 )
        axis(side=1, at=round(seq(0,nrow(R1),len=10),0), labels=as.integer(seq(min(R1$Time),max(R1$Time),len=10)), las=1, cex=0.5 )
        }#end for i.env
      } # end if ncol(R1)>4
    else # if no adaptive env grids, just plot the range
      {
      # Range RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
      plot(Cells_Occupied/1000 ~ Time, data=R1, type="l", ylim=c(0,((max(R1$Cells_Occupied)*1.1))/1000), ylab="Range Grid Cells (x1000)", axes=F)
      polygon(x=c(R1$Time,rev(R1$Time)), y=c(R1$Cells_Occupied/1000,rev(R1$Cells_Occupied/1000)), col="lightgrey",border="lightgrey" )
      lines(Cells_Occupied/1000 ~ Time, data=R1)
      box()
      axis(side=2, at=round(seq(0,(max(R1$Cells_Occupied)*1.1)/1000,len=7),1), las=1 )
      axis(side=1, at=round(seq(0,nrow(R1),len=10),0), labels=as.integer(seq(min(R1$Time),max(R1$Time),len=10)), las=1, cex=0.5 )
      }# end else ncol(R1)>4
  } # End plot.range.single.run function

#############################################################################################
