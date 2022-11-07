# Trade Models in the European Union
[Claudius Gräbner-Radkowitsch](https://claudius-graebner.com), Dennis Tamesberger, 
[Philipp Heimberger](https://wiiw.ac.at/philipp-heimberger-s-1138.html),
Timo Kapelari, and 
[Jakob Kapeller](https://jakob-kapeller.org/)

Here we provide the data and the code to replicate all empirical exercises 
in the above mentioned paper.
You can also access a freely available working paper version of the paper 
[here](https://www.jku.at/fileadmin/gruppen/108/ICAE_Working_Papers/wp95.pdf).

The paper has also published in the journal *Economic Annals*. The official 
reference is:

Gräbner-Radkowitsch, C., Tamesberger, D., Heimberger, P., Kapelari, T., & 
Kapeller, J. (2022): Trade Models in the European Union. *Economic Annals*, 
Vol. 67(235), forthcoming. DOI: [TBA]().

## Code structure

`src/clustering_functions.R`: contains the function definitions used for the 
clustering and will be called from `src/clustering.R`.

`src/clustering.R` conducts all the clustering exercises and produces figure 2, 
as well as tables 2 and 3.

`src/descript-figures.R` creates figures 3, 4, 5 and 6.

`src/system_spec.R` has been used to get the system specification used to
create the figures of the published paper.

## Data 

For more information about the data set see the supplementary material in the 
working paper. The data has also been published in the 
[Harvard Dataverse](https://doi.org/10.7910/DVN/NADWIL).

## Structure for working directory
The code assumes that the working directory is structured as follows:
`working-directory/src/` contains all the R files, 
`working-directory/data/` contains the data and 
`working-directory/output/` exists. 
All figures will be saved in this folder.

## Original system specification

The file `output/session_info.txt` contains information about the hardware as 
well as the packages used when the output for the original paper was produced.
