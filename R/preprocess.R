#' @import terra
#' @import sf
#' @import doParallel
#' @import foreach
#' @import parallel
#' @import doParallel
#' @noRd
img_clip <- function(img_dir,proc_dir,gpkg){
  imgs <- list.files(path=img_dir,pattern="(.tif|.tiff|.TIF|.TIFF)$",
                     all.files=FALSE,full.names=FALSE)
  base_name <- basename(img_dir)
  if(!dir.exists(file.path(proc_dir,paste(base_name,"_clips",sep="")))){
    dir.create(file.path(proc_dir,paste(base_name,"_clips",sep="")))
  }

  n_cores <- detectCores()
  if(n_cores %/% 2 > length(imgs)){
    cluster <- makeCluster(length(imgs))
  }else{
    cluster <- makeCluster(n_cores %/% 2)
  }
  registerDoParallel(cluster)
  foreach(i=imgs,.packages=c("terra","sf")) %dopar% {
    data <- rast(file.path(img_dir,i))
    inter <- relate(vect(gpkg),ext(data),"intersects")
    data_bound <- vect(ext(data),crs=crs(data))
    joined <- st_join(gpkg,st_as_sf(data_bound,crs=crs(data)),sparse=FALSE)
    st_write(joined,
             file.path(proc_dir,paste(tools::file_path_sans_ext(i),".gpkg",sep="")),append=FALSE)
    img_crop <- crop(data,st_bbox(joined))
    writeRaster(img_crop,file.path(proc_dir,paste(base_name,"_clips",sep=""),i),
                overwrite=TRUE)
  }
  stopCluster(cluster)
  return(file.path(proc_dir,paste(base_name,"_clips",sep="")))
}

#' @noRd
read_indices <- function(conf_path){
  content <- readLines(conf_path)
  indices <- list()
  detail <- list()
  name <- ""
  desc <- ""
  calc <- ""
  for (l in content){
    if(substr(l,1,1)!="#" && l!=""){
      if(substr(l,1,1)=="["){
        name <- substr(l,2,nchar(l)-1)
      }else if(grepl("desc",l)){
        desc <- strsplit(l,":")[[1]][2]
      }else if(grepl("calc",l)){
        calc <- strsplit(l,":")[[1]][2]
        indices[[name]] <- calc
        detail[[name]] <- desc
      }
    }
  }
  return(list(calcs=indices,details=detail))
}



