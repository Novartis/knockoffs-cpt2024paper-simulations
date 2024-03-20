# Simulation studies accompanying the paper 'All that glitters is not gold: Type-I error controlled variable selection from clinical trial data' (CPT 2024)


## Description
This repository contains example code to run simulation studies like the ones in [Zimmermann et al. (2024)](http://dx.doi.org/10.1002/cpt.3211).
Due to confidentiality and privacy concerns, the data or code from the real case studies presented in the same paper cannot be shared publicly.


## Installation
To install this repository from GitHub, use:
```R
if(!require(devtools)) install.packages("devtools")
devtools::install_github("Novartis/knockoffs-cpt2024paper-simulations")
```

To use this repository, ensure that the following packages are installed (e.g. from CRAN):
bnlearn, caret, clustermq, dplyr, ggtext, ggthemes, grid, gridExtra, gtools, latex2exp, lemon, recipes, tictoc, and tidyverse

In addition, the [`knockofftools`](https://github.com/Novartis/knockofftools/) repository should be installed. 
To do this, use:
```R
if(!require(devtools)) install.packages("devtools")
devtools::install_github("Novartis/knockofftools")
```

## Usage
This repository is intended to be used in a high performance computing environment as it uses [`clustermq`](https://cran.r-project.org/web/packages/clustermq/index.html) to run simulations in parallel. 

The folder `simulation_studies` contains one subfolder for each simulation-based Figure in [Zimmermann et al. (2024)](http://dx.doi.org/10.1002/cpt.3211). To run the simulations and produce summary plots, run the R-Script(s) within the respective subfolder. This will also generate an .RDS file with the results in the `output` folder. Note that the working directory is intended to be the folder of this repository.


## Authors and references
This repository accompanies the following publication: [Zimmermann et al. (2024)](http://dx.doi.org/10.1002/cpt.3211).
The R implementation of the sequential knockoff algorithm was introduced in [Kormaksson et al. (2021)](https://onlinelibrary.wiley.com/doi/full/10.1002/sim.8955)

The following authors contributed to this repository:
- Matthias Kormaksson, matthias-1.kormaksson_ext@novartis.com
- Konstantinos (Kostas) Sechidis, kostas.sechidis@novartis.com
- Manuela Zimmermann, manuela.zimmermann@novartis.com


## Disclaimer

Please note that the content of this repository is provided for informational purposes only. The authors or Novartis make no representations or warranties of any kind, expressed or implied. Furthermore, the authors or Novartis shall not be held responsible or liable for any errors, omissions, or delays in this information or any losses, injuries, or damages arising from its display, download or use. All information is provided on an as-is basis. 


