# PIHMgisR --- PIHM GIS tool in R

*Notice*: This version of PIHMgisR will not be updated anymore.  
The newer version of the modeling system is transferred to [SHUD project](https://shud.xyz) via [www.shud.xyz](www.shud.xyz). 


## PIHM
- www.pihm.psu.edu

The Penn State Integrated Hydrologic Model (PIHM) is a multiprocess, multi-scale hydrologic model where the major hydrological processes are fully coupled using the semi-discrete finite volume method. 

This package can be used with the AutoPIHM project, that can build modeling domain automatically.

## Purpose of the package:
1. convert the geospatial data into PIHM format. The tool kit is able to process the raster and vector data, then building the unstructured triangular mesh domain for PIHM.
2. Write/read the PIHM input files.
3. Read the PIHM output files.
4. Generate the calibration parameter set.
5. Time-Series analysis on hydrologic data
6. Two-dimensional and 3-Dimentional plot.
7. GIS analysis. Convert the unstructure data into spatial data (Shapefile or Raster)
8. Download the USGS hydrological data, including discharge, ground water well, sediment, etc.


## Note:
Current PIHMgisR requires different version of RTriangle package. you must install that via github(June 2019):
```
install.packages("devtools")
devtools::install_github("davidcsterratt/RTriangle", subdir="pkg")
```
