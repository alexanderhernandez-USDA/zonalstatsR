#' @title zonal_stats
#'
#' @description Perform index or volume calculations, and extract data by
#'      polygon or points.
#'
#' @param img_dir File path to folder with rasters
#' @param bands List or vector containing band names in order for rasters
#' @param to_run List or vector containing the index names to run
#' @param gpkg File path to geopackage or sf object
#' @param out_gpkg Optional, path to an output geopackage
#' @param save_dir Optional, path to a folder to save calculation rasters in
#' @param points Set to FALSE by default, set to TRUE if geopackage uses points
#' @param buffer Set to 0 be default, set to a nonzero number to
#'      generate buffers around points. When using buffer, please have your
#'      geopackage and raster in a CRS which uses meters
#'
#' @return An sf dataframe with geopackage data and extracted index information
#'
#'
#' @import terra
#' @import sf
#' @name zonal_stats
#' @export
zonal_stats <- function(img_dir,bands,to_run,gpkg,out_gpkg="",
                        save_dir="",points=FALSE,buffer=0){
  startT <- Sys.time()
  proc_dir <- paste(".",Sys.getpid(),"_proc_dir",sep="")
  if(!dir.exists(proc_dir)){
    dir.create(proc_dir)
  }
  if(class(gpkg)[1]=="character"){
    gpkg <- st_read(gpkg)
  }
  if(buffer != 0){
    gpkg <- st_buffer(gpkg,buffer)
  }
  indices <- read_indices(system.file("extdata","indices.conf",package="zonalstatsR"))
  res <- run_indices(to_run,indices,bands,img_dir,proc_dir,
                     gpkg,save_dir=save_dir,points=points)
  unlink(proc_dir,recursive=TRUE)
  if(out_gpkg!=""){
    st_write(res,out_gpkg,append=FALSE)
  }
  endT <- Sys.time()
  print(endT - startT)
  return(res)
}
