# Sekhon and Shem-Tov (2017) replication repository

## R code files

The **estCI** package that contains the *aveCI* function which is used in the replication code can be dowloaded using the command below in the R command shell:
```R
devtools::install_github("yotamshemtov/estCI")
````

#### Real data example
* *Rosenbaum2001.R* and *Rosenbaum2001.Rout*: Figure 1.

#### Monte-Carlo simulations
All of the programs below perform the same simulation for different data generating processes (DGPs) with different parameters. A sample of 1,000 units is drawn from a super-population 1,000 times for each parameter combination. For each sample, 1,000 different treatment assignment allocations are computed and the coverage and confidence/prediction intervals are calculated for each treatment assignment.

On our computer cluster it took 04:19:53 (4hr, 19min, and 53sec) without using parallel computing, to run the three R programs below.     
 
* *binary_outcome_sim.R* and *binary_outcome_sim.Rout*: Figure 3.
* *random_coefficient_sim.R* and *random_coefficient_sim.Rout*: Figure 4. 
* *tobit_outcome_sim.R* and *tobit_outcome_sim.Rout*: Figure 5.

#### Additional files
* *analytical_figures.R* and *analytical_figures.Rout*: Figure 2.


