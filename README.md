# stability-risk-time-preferences
The (In)Stability of Risk and Time Preferences: Concepts, Evidence, and Measurement
Sabine Liebenhm (Agroscope / University of Saskatchewan) and Nodir Djanibekov (IAMO)
This repository contains the data and R code underlying the empirical synthesis presented in the book chapter The (In)Stability of Risk and Time Preferences: Concepts, Evidence, and Measurement. The repository is intended to support transparency and reproducibility. All variables used in the analysis are defined and described in the chapter, in particular in Section 3 (coding framework).
---
Repository contents
File	Description
`instabRPTPdata.xlsx`	Raw data including full references
`instabRPTPdataForR.xlsx`	Analysis-ready dataset used in R
`instab_greyscale.R`	R script reproducing all figures and tables
`README.md`	This file
---
Data
`instabRPTPdata.xlsx`
The raw dataset underlying the review. It includes full bibliographic references for all 112 studies in the final sample, along with all coded variables. This file is intended as the archival record of the review and coding process.
`instabRPTPdataForR.xlsx`
The analysis-ready version of the dataset, formatted for direct import into R. It contains the same coded variables as the raw data file but without the full reference columns. This is the file read by the R script.
All variables are described in Section 3 of the chapter (coding framework), which covers the four coding dimensions: (i) preference domain and measurement approach, (ii) data structure, event classification, and time horizon, (iii) stability conclusion and direction of change, and (iv) strength of identification and source of instability.
---
Code
`instab_greyscale.R`
A single R script that reproduces all figures reported in Section 4 of the chapter, formatted following the publisher's guidelines. The script reads `instabRPTPdataForR.xlsx`, applies the factor orderings and variable definitions described in the chapter, and exports all figures.
Key dependencies:
`tidyverse`
`ggplot2`
`patchwork`
`ggpattern`
`readxl`
To reproduce the analysis, open `instab_greyscale.R`, set the working directory to the folder containing the data files, and run the script. Package versions are recorded at the end of the script via `sessionInfo()`.
---
Citation
If you use the data or code from this repository, please cite the chapter as:
> Liebenhm, S. and Djanibekov, N. (*forthcoming*). The (In)Stability of Risk and Time Preferences: Concepts, Evidence, and Measurement. In R. Dimova (Ed.), *[book title to be confirmed upon publication]*.
---
License
Data: CC BY 4.0
Code: MIT License
---
Contact
Sabine Liebenhm — sabine.liebenehm@agroscope.admin.ch
