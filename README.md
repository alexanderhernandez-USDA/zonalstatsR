# zonalstatsR
R package for performing vegetation index calculations, volume calculation, and extraction of calculated and raw data.

## Installation
The package and its dependencies can be installed using the devtools package. You may need to install devtools if it isn't already present on your machine. Download the tar.gz package file for zonalstatsR, and then run the following in R or RStudio:
```
devtools::install_local("/path/to/zonalstatsR_0.1.0.tar.gz")
```
Once the installation has finished, you can load zonalstatsR with
```
library(zonalstatsR)
```

## Dependencies
zonalstatsR depends on the following packages:
- Terra
- SF
- exactextractr
- doParallel
- foreach

## Usage
Using zonalstatsR is pretty straightforward. There is a single function to be run, 'zonal_stats'.
```
zonal_stats(img_dir,bands,to_run,gpkg,out_gpkg = "",save_dir = "",points = FALSE,buffer = 0)
```
#### img_dir
File path to folder with rasters

#### bands
List or vector containing band names in order for rasters

#### to_run
List or vector containing the index names to run. Volume can also be calculated using index VOLUME. Raw extraction of data can be performed using index RAW. All vegetation indices can be run using keyword ALL instead of using a list/vector of indices.

#### gpkg
File path to geopackage or sf object

#### out_gpkg
Optional, path to an output geopackage

#### save_dir
Optional, path to a folder to save calculation rasters in

#### points
Set to FALSE by default, set to TRUE if geopackage uses points

#### buffer
Set to 0 be default, set to a nonzero number to generate buffers around points. When using buffer, please have your geopackage and raster in a CRS which uses meters

After running the `zonal_stats` function, an sf object is returned with values for all polygons for the calculated indices.
