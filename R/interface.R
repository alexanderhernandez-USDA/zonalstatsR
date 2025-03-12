library(terra)
library(sf)
library(exactextractr)

zonal_stats <- function(img_dir,bands,to_run,gpkg,out_gpkg="",save_dir="",points=FALSE,buffer=0){
  startT <- Sys.time()
  proc_dir <- paste(Sys.getpid(),"_proc_dir",sep="")
  if(!dir.exists(proc_dir)){
    dir.create(proc_dir)
  }
  if(class(gpkg)[1]=="character"){
    gpkg <- st_read(gpkg)
  }
  indices <- read_indices("inst/indices.conf")
  res <- run_indices(to_run,indices,bands,img_dir,proc_dir,gpkg,save_dir=save_dir,points=points)
  unlink(proc_dir,recursive=TRUE)
  endT <- Sys.time()
  print(endT - startT)
  return(res)
}
