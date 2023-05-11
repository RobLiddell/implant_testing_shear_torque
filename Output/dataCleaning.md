*Note: This could probably be written as a script next time as it
doesn’t really much visual output.*

# Goal

This code is for the extraction and cleaning of data found in individual
tab separated value tables saved in .txt files \## Data Extraction Out
first step is extract all the data

``` r
#This is the folder which contains all of the raw data folders

path<-"../Data/rawData"

#Create a vector with all of the paths to each of the individual data folders which have Copy at the end of their name
folders<-dir(path)%>%
  str_subset("Copy$")%>%
  {glue("{path}/{.}")}

#Set the path to the legend document which outlines the testing times/methods and implant types for each animal
legend<-"../Data/rawData/legend/legend.txt"

#This function goes through all of the data files extracting information such as animalID, date of testing, side, and testing method
extractForceData<-function(folders){
  dat<-dir(i)%>%
    str_subset(".txt$")%>%
    as_tibble()%>%
    rename(file=value)%>%
    mutate(testDate=ymd(str_extract(i,"2014 \\d+ \\d+")), #Extract date of testing
           animalID=str_extract(file,"^\\d*"),            #Extract animal ID
           animalSide=str_extract(file,"(?<=\\d)\\D"),    #Extract the side of the animal tested (L/R)
           archSide=str_extract(file,"(?<=\\D)\\D"),     #Extract the side of the arch tested (L/M)
           testNum=if_else(str_detect(file,"-"),str_extract(file,"(?<=-)\\d"),"1"), #Extract which test the implant failed on
           testMeth=if_else(str_detect(file,"S"),"S","T"), #Extract the method of testing
           fullPath=as.character(glue("{folders[1]}/{file}")), #Create the full path
           data=map(fullPath,~read_delim(.x,col_names=F,col_types = "n",delim="\t")))%>% #Extract the data from the file
    mutate(across(animalSide:archSide,~str_to_upper(.x)))
  return(dat)
}


#Load all of the raw Tension/shear values
for(i in folders){
  if(which(folders==i)==1){
    rawData<-extractForceData(i)
  }else{
    rawData<-extractForceData(i)%>%
      bind_rows(rawData)
  }
}

#Load the legend
legendData<-read_tsv(legend,col_types = list(col_character(), #Setting all of the column types
                                      col_character(),
                                      col_character(),
                                      col_date(),
                                      col_date(),
                                      col_character()))%>%
  pivot_longer(cols=c(Left,Right),                           #pivot table longer creating individual rows for 
               names_to = "animalSide",                      #Left and Right Sides
               values_to = "ImplantType")%>%
  mutate(animalSide=if_else(animalSide=="Left","L","R"))%>% 
  select(-Sacrifice_Date,-testMeth)
```

## Data Cleaning

First we extract the peak force from each data file read in. (This and
previous steps could be made less memory intensive by possibly doing in
the previous step). After extracting I left_join with the legend on
animalSide and animalID to include the surgery date and implant type
data. Afterwards the fields are modified making the different values
easier to read.

``` r
data<-rawData%>%
  rowwise()%>%
  mutate(force=max(data$X2))%>% #Find the disruption force from each dataset (due to rowwise above)
  ungroup()%>%
  select(animalID,testDate,animalSide,archSide,testMeth,force)%>% #Select relevant fields
  arrange(as.numeric(animalID))%>%
  left_join(legendData,by=c("animalID","animalSide"))%>% #Join on relevant fields
  mutate(animalSide=factor(animalSide,levels=c("L","R"),labels=c("Left","Right")), #Re-factored names to make more readable
         archSide=factor(archSide,levels=c("L","M"),labels=c("Lateral","Medial")),
         testMethod=factor(testMeth,levels=c("S","T"),labels=c("Shear","Tension")),
         implantType=factor(ImplantType,levels=c("B","A"),labels=c("BAE+DCD","BAE")),
         time=as.numeric(testDate-Surgery_Date),         
         .keep="unused")
```

### Data Inspection

Here I want to look for data values which lie outside expected values

``` r
#Here we have checked for results outside of normal

print("These results are outside of the expected time points")
```

    ## [1] "These results are outside of the expected time points"

``` r
data%>%
  filter(!(time %in% c(5,9,14,28,84,168)))
```

    ## # A tibble: 7 × 7
    ##   animalID animalSide archSide force testMethod implantType  time
    ##   <chr>    <fct>      <fct>    <dbl> <fct>      <fct>       <dbl>
    ## 1 121      Left       Lateral   3.81 Tension    BAE            27
    ## 2 121      Left       Medial    1.42 Tension    BAE            27
    ## 3 121      Right      Lateral   9.81 Tension    BAE+DCD        27
    ## 4 121      Right      Medial   11.0  Tension    BAE+DCD        27
    ## 5 122      Left       Medial   40.7  Shear      BAE+DCD        27
    ## 6 122      Right      Lateral  16.4  Shear      BAE            27
    ## 7 122      Right      Medial   14.6  Shear      BAE            27

``` r
#Correct the finding that some times ar 27 instead of 28

cleanedData<-data%>%
  mutate(time=if_else(time==27,28,time))
```

``` r
saveLoc<-"../Data/formattedData/ST1_Data_Cleaned.txt"

cleanedData%>%
  write_tsv(saveLoc)
```
