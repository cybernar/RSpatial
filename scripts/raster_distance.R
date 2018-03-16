library(raster)
library(gdalUtils)

gdal_proximity <- function(parameter_values) {
  # gdal_proximity wrapper
  executable <- "gdal_proximity.bat"
  parameter_variables <- list(
    logical = list(NULL),
    vector = list(NULL),
    scalar = list(varnames <- c("srcband","dstband","maxdist","nodata","fixed-buf-val")),
    character = list(varnames <- c("ot","of","values","distunits","use_input_data")),
    repeatable = list(varnames <- c("co")))
  parameter_order <- c(
    "srcfile","dstfile",
    "srcband","dstband","of","co","ot","values","distunits","maxdist","nodata",
    "use_input_data","fixed-buf-val")
  parameter_noflags <- c("srcfile","dstfile")
  # Now assign some parameters:
  cmd<-gdal_cmd_builder(
    executable=executable,
    parameter_variables=parameter_variables,
    parameter_values=parameter_values,
    parameter_order=parameter_order,
    parameter_noflags=parameter_noflags)
  system(cmd)
}

gdal_proximity <- function(pama) {
  parameter_vector <- sapply(parameter_vector,function(x) paste(x,collapse=" "))
  
  cmd <- paste(c(qm(executable),parameter_vector),collapse=" ")
  
  return(cmd)
}


frast_ocsol <- "D:/Support/PierrickD/output/ocsol_50m.tif"

rast_ocsol <- raster(frast_ocsol)

# on cherche la distance par rapport à tout ce qui est eau, cad :
# 5110 (cours d'eau), 5120 (canal), 5210 (etang), 5220 (lagune), 5300 (mer)

# remplacer 5110,5120,5210,5220,5300 par 1 et le reste par NA
df_subs <- data.frame(by=c(5110,5120,5210,5220,5300),which=c(1,1,1,1,1))
rast_ocsol_eau <- subs(rast_ocsol, df_subs)

# calcul distance euclidienne des pixels NA au pixel eau le + proche (-> distance_eau.tif)
distance(rast_ocsol_eau, filename="D:/Support/PierrickD/output/distance_eau.tif")

# gdal_translate .tif -> .asc
gdal_translate("D:/Support/PierrickD/output/distance_eau.tif",
               "D:/Support/PierrickD/output/distance_eau.asc",
               ot="Float32", of="AAIGrid")


parameter_values = list(
  srcfile = "D:/Support/PierrickD/output/ocsol_50m.tif",
  dstfile = "D:/Support/PierrickD/output/R_proximity_50m.tif",
  values = "5110,5120,5210,5220,5300",
  distunits = "GEO",
  nodata = -9999
)

gdal_proximity(parameter_values)


python_exe_path <- normalizePath(list.files(getOption("gdalUtils_gdalPath")[[1]]$path, "python.exe",full.names=TRUE))
proximity_bat_path <- normalizePath(list.files(getOption("gdalUtils_gdalPath")[[1]]$path, "gdal_proximity.py",full.names=TRUE))
srcfile = "D:\\Support\\PierrickD\\output\\ocsol_50m.tif"
dstfile = "D:\\Support\\PierrickD\\output\\R_proximity_50m.tif"

params <- "-values 5110,5120,5210,5220,5300 -distunits GEO -nodata -9999"
cmd <- paste(qm(python_exe_path),qm(proximity_bat_path), qm(srcfile), qm(dstfile), params, collapse=" ")

system(cmd)
