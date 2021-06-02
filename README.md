# ETC-ICM trends North Sea

### Contents

* [Import to R](#import-to-r)
* [Data coverage](#data-coverage)
* [CHASE Method](#chase-method)
* [CHASE Results](#chase-results)

### Import to R

The folder [data](/data/) contains the input data for the assessments.

```
folder<-"data/"

fileB<-"ETCICM_1621_NorthSea_biota_20200817.txt"
fileS<-"ETCICM_1621_NorthSea_sediment_20200817.txt"
fileW<-"ETCICM_1621_NorthSea_water_20200817.txt"
fileStn<-"ETCICM_1621_Stations_20200817.txt"

dfStn <- read.table(paste0(folder,fileStn),sep="\t",header=T,quote="",comment.char="",fileEncoding="UTF-8-BOM")
dfS <- read.table(paste0(folder,fileS),sep="\t",header=T,quote="",comment.char="",fileEncoding="UTF-8-BOM")
dfW <- read.table(paste0(folder,fileW),sep="\t",header=T,quote="",comment.char="",fileEncoding="UTF-8-BOM")
dfB <- read.table(paste0(folder,fileB),sep="\t",header=T,quote="",comment.char="",fileEncoding="UTF-8-BOM")

```

### Data coverage

![Station map](png/station_map.png)


![Station count](png/station_count.png)




The file received from METU was a text file `data_from_Mersin.txt`. This is data exported from an odv file. Before importing into R, I used a visual basic script `preprocess.wsf` to manipulate the data into a format more easily read by R. In theory this could all be done directly in R but it was quicker for me to add this extra step outside R.
The script file was run using the windows batch file `preprocess.bat`. The restructured data is saved as the file  `data_from_Mersin_for_R.txt`.

The file  `data_from_Mersin_for_R.txt` is read into an R data frame using [Mersin_read_data.R](/Mersin_read_data.R). The data frame is saved as an R data file `data_from_Mersin.Rda`

### CHASE Method

More details of the CHASE method can be found in [Andersen et al. (2016)](https://link.springer.com/article/10.1007/s10661-016-5121-x)

Very briefly;

* We compare measurements of hazardous substances in three categories (phases): 1) Water, 2) Sediment and 3) Biota.

* The average concentration of each substance is calculated for each year and assessment unit. This is done for the substances for which there is a recognized threshold value [thresholds_v6.txt](data/thresholds_v6.txt). This is done by taking the log-mean average.

* For each substance , the Contamination Ratio is calculated by calculating the ratio of the mean concentration versus the threshold value.

* The Contamination Score (CS) is calculated according to the CHASE method, as the sum of CR values divided by the square-root of the number of substances included/observed.

* The contamination score is caclulated for each of the three categories.

* A score greater than 1.0 indicates a contamination problem area. A score less than 1.0 indicates that this is not a problem area.


### CHASE Results

![Sum of area by CHASE assessment class](png/CHASE_area.png)
![CHASE average Class for North Sea](png/CHASE_timeseries.png)


