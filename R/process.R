#' @title run_indices
#'
#' @description Run multiple vegetation index calculation over multiple images,
#'      and extract their values per polygon or point as provided in geopackage.
#'
#' @param to_run A vector or list of index names to be run
#' @param indices A named list generator from the read_indices function
#' @param bands A vector or list of ordered band names for the images
#' @param img_dir File path to folder containing images to run on
#' @param proc_dir File path to the processing folder for temporary files
#' @param gpkg File path to a geopackage or an sf object
#'
#' @return An sf dataframe with geopackage data and extracted index information
#'
#' @noRd
#' @import terra
#' @import sf
#' @import exactextractr
#' @import foreach
#' @import doParallel
run_indices <- function(to_run,indices,bands,img_dir,proc_dir,gpkg,
                        save_dir="",points=FALSE){
  if(class(gpkg)[1]=="character"){
    gpkg <- st_read(gpkg)
  }
  clip_dir <- img_clip(img_dir,proc_dir,gpkg)
  imgs <- list.files(path=clip_dir,pattern="(.tif|.tiff|.TIF|.TIFF)$",
                     all.files=FALSE,full.names=FALSE)

  n_cores <- detectCores()
  if(n_cores %/% 2 > length(to_run) && !"RAW" %in% to_run){
    cluster <- makeCluster(length(to_run))
  }else if("RAW" %in% to_run && n_cores %/% 2 > (length(to_run) + length(bands))){
    cluster <- makeCluster(length(to_run) + length(bands))
  }else{
    cluster <- makeCluster(n_cores %/% 2)
  }
  registerDoParallel(cluster)
  for(i in imgs){
    cur_gpkg <- st_read(file.path(proc_dir,paste(tools::file_path_sans_ext(i),".gpkg",sep="")))
    output <- foreach(c=to_run,.export=c("run_calc"),.packages=c("sf","terra","exactextractr")) %dopar% {
      data <- rast(file.path(clip_dir,i))
      if(toupper(c)=="VOLUME"){
        ras <- volume_calc(cur_gpkg,data)
        if(points){
          res <- point_extract(ras,cur_gpkg)
        }else{
          res <- exact_extract(ras,cur_gpkg,'sum')
        }
      }else if(toupper(c)=="RAW"){
        if(points){
          res <- point_extract(data,cur_gpkg)
        }else{
          res <- exact_extract(data,cur_gpkg,'median')
        }
        #for(b in 1:length(bands)){
        #  gpkg[paste(tools::file_path_sans_ext(i),"_",bands[b],sep="")] <- res[b]
        #}
      }else{
        ras <- run_calc(bands,indices[[c]],data)
        if(points){
          res <- point_extract(ras,cur_gpkg)
        }else{
          res <- exact_extract(ras,cur_gpkg,'median')
        }
      }
      #if(toupper(c)!="RAW"){
      #  gpkg[paste(tools::file_path_sans_ext(i),"_",c,sep="")] <- res
      #}
      if(save_dir!=""){
        writeRaster(ras,file.path(save_dir,paste(tools::file_path_sans_ext(i),"_",c,".tif",sep="")),overwrite=TRUE)
      }
      res
    }
    for(n in 1:length(to_run)){
      if(toupper(to_run[n])=="RAW"){
        for(b in 1:length(bands)){
          gpkg[paste(tools::file_path_sans_ext(i),"_",bands[b],sep="")] <- output[[n]][b]
        }
      }else{
        gpkg[paste(tools::file_path_sans_ext(i),"_",to_run[n],sep="")] <- output[n]
      }
    }
  }
  stopCluster(cluster)
  return(gpkg)
}


#' @title point_extract
#'
#' @description Extract point values for a raster
#'
#' @param data A terra SpatRaster object
#' @param gpkg An sf object
#'
#' @return A dataframe with point values for the geopackage points
#'
#' @noRd
#' @import terra
#' @import sf
point_extract <- function(data,gpkg){
  return(extract(data,gpkg))
}


#' @title run_calc
#'
#' @description Run an index calculation on an raster
#'
#' @param bands A vector or list with the raster's band order
#' @param calc A string containing the calculation to be run. Variables in this
#'      calculation are represented like so:
#'      b - blue band
#'      g - green band
#'      r - red band
#'      n - nir band
#'      re - rededge band
#' @param data A terra SpatRaster object
#'
#' @return A terra SpatRaster object with the resulting raster after the
#'      calculation has been performed
#'
#' @noRd
#' @import terra
#' @import sf
run_calc <- function(bands,calc,data){
  if("red" %in% bands){
    r <- data[[match("red",bands)]]
  }else if(grepl("r[^a-zA-Z]",calc)){
    print("Error: no red band!")
    return("error")
  }
  if("green" %in% bands){
    g <- data[[match("green",bands)]]
  }else if(grepl("g[^a-zA-Z]",calc)){
    print("Error: no green band!")
    return("error")
  }
  if("blue" %in% bands){
    b <- data[[match("blue",bands)]]
  }else if(grepl("b[^a-zA-Z]",calc)){
    print("Error: no blue band!")
    return("error")
  }
  if("rededge" %in% bands){
    re <- data[[match("rededge",bands)]]
  }else if(grepl("re[^a-zA-Z]",calc)){
    print("Error: no rededge band!")
    return("error")
  }
  if("nir" %in% bands){
    n <- data[[match("nir",bands)]]
  }else if(grepl("n[^a-zA-Z]",calc)){
    print("Error: no nir band!")
    return("error")
  }
  return(eval(parse(text=calc)))
}

#' @title volume_calc
#'
#' @description Calculate volume for each polygon over a DSM/DEM raster using plane averages
#'
#' @param gpkg File path to a geopackage or an sf object
#' @param data File path to a DSM/DEM tif or a terra SpatRaster object
#'
#' @return A terra SpatRaster object containing volumes for each pixel, to be
#'    extracted using exact_extract
#'
#' @noRd
#' @import terra
#' @import sf
volume_calc <- function(gpkg,data){
  if(class(gpkg)[1]=="character"){
    gpkg <- st_read(gpkg)
  }
  if(class(data)[1]=="character"){
    data <- rast(data)
  }
  alts <- c()
  for(r in 1:nrow(gpkg)){
    xmin <- ext(st_sf(gpkg$geom[r]))$xmin
    xmax <- ext(st_sf(gpkg$geom[r]))$xmax
    ymin <- ext(st_sf(gpkg$geom[r]))$ymin
    ymax <- ext(st_sf(gpkg$geom[r]))$ymax
    corners <- st_sf(geometry=st_sfc(c(st_point(c(xmin,ymin)),st_point(c(xmax,ymin)),st_point(c(xmax,ymax)),st_point(c(xmin,ymax)))),crs=crs(gpkg))
    avg_alt <- sum(extract(data,corners)[2])/4
    alts[r] <- avg_alt
  }
  plane <- rasterize(vect(gpkg$geom),data,field=alts,background=0,touches=TRUE)
  heights <- data - plane
  pix_area <- res(data)[1] * res(data)[2]
  volumes <- heights * pix_area
  volumes <- ifel(volumes > 0, volumes, 0)
  return(volumes)
}


