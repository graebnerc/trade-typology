# Trade Models in the European Union
Dennis Tamesberger, [Claudius Gräbner](https://claudius-graebner.com), 
Timo Kapelari, 
[Philipp Heimberger](https://wiiw.ac.at/philipp-heimberger-s-1138.html) and 
[Jakob Kapeller](https://jakob-kapeller.org/)

Here we provide all the data and the code to replicate all empirical exercises 
in the abovementioned paper.
You can access a freely available working paper version of the paper here.

The repository contains all code and data to replicate all figures and tables in the paper. 

## Code structure

`src/clustering_functions.R`: contains the function definitions used for the 
clustering and will be called from `src/clustering.R`.

`src/clustering.R` conducts all the clustering exercises and produces figure 2, 
as well as tables 2 and 3.

`src/data_preparation.R` has been used to assemble the data set. Will not be 
called by default since the data set used for the clustering in the paper has 
been saved as `data/clustering_data_used.csv`. But it is provided for reasons
of transparency.

`src/descript-figures.R` creates figures 3, 4, 5 and 6.

`src/system_spec.R` has been used to get the system specification used to
create the figures of the published paper.

## Data 
For more information about the data set see the supplementary material in the 
working paper.

## Structure for working directory
The code assumes that the working directory is structured as follows:
`working-directory/src/` contains all the R files, 
`working-directory/data/` contains the data and 
`working-directory/output/` exists. 
All figures will be saved in this folder.

## Original system specification

The file `output/session_info.txt` contains information about the hardware as 
well as the packages used when the output for the original paper was produced.