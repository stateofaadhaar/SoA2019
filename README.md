# State of Aadhaar, 2019
Data, codes and figures for the State of Aadhaar, 2019

## HOW TO USE THIS FILE 
This readme accompanies the dataset for [State of Aadhaar](www.stateofaadhaar.in), 2019. State of Aadhaar is the largest public dataset on digital identity in the world, and we hope it spurs data-driven discourse on good ID.
The readme highlights the structure of the folder, including accompanying datasets, sources, and how to work with the data

## FOLDER STRUCTURE 

``` bash

Root
|
|--- source.R : R file to setup options, initialize functions and run the analysis and visuals for State of Aadhaar 2019
|
|--- 01. Inputs 
|     | 
|     | 
|     |--- 01. Datasets: State of Aadhaar, pulse and in-depth surveys as csv files
|     |
|     |--- 02. Data for figures: Meta data for some figures, required for analysis of in-depth survey
|     |
|     |--- 03. Survey toolkit: Contains methodology note, questionnaires and codebook
|     |
|     |--- 04. India boundary: Contains shapefiles for India. Sourced from Datameet, India
|
|
|--- 02. Codes 
|     | R files for custom functions, as well as analysis and visualizations
|
|
|--- 03. Outputs 
|     | 
|     | 
|     |--- 01. Data for figures: Folder to store data required for visuals
|     |
|     |--- 02. Figures: Figures created for the State of Aadhaar, 2019 report
|     |
|     |--- 03. Logs: Contains log outputs from analysis
```

## HOW TO SET UP THE DATASET FOR ANALYSIS
Please see the source file for examples on setting up and using the data for analysis

## DISCLAIMER
The India shapefiles do not consider the recent changes in the administrative boundaries of Jammu and Kashmir. Please check for encoding errors upon loading the data files. The data, and all materials, are shared under [Creative Commons Attribution 4.0 International license (CC-BY 4.0)](https://creativecommons.org/licenses/by/4.0/).

### Suggested Citation
Swetha Totapally, Petra Sonderegger, Priti Rao, Jasper Gosselt, Gaurav Gupta *State of Aadhaar Report 2019*. Dalberg, 2019
