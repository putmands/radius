# Social Network Structure and the Radius of Risk Sharing
Replication code for "Social Network Structure and the Radius of Risk Sharing" accepted at _Social Network Analysis and Mining_. 

## 1 Data
The paper draws on two empirical illustrations, in Colombia and Tanzania. The Colombia illustration draws on data from Attanasio et al. (2012) and de Weerdt (2025). To replicate this project, download this data from the repositories cited below.

Attanasio, Orazio, Barr, Abigail, Cardenas, Juan Camilo, Genicot, Garance, and Meghir, Costas. Replication data for: Risk Pooling, Risk Preferences, and Social Networks. Nashville, TN: American Economic Association [publisher], 2012. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor], 2019-10-12. https://doi.org/10.3886/E113814V1

De Weerdt, Joachim, 2025, "Nyakatoke Network Data", https://doi.org/10.7910/DVN/U2OJE3, Harvard Dataverse, V1, UNF:6:YcKmqT/wWWrvPW8juI+wFA== [fileUNF]

## 2 Software
The results were produced using R and Stata. For R, I include a set-up script that will install necessary packages. For Stata, the user will need to install NGREG, written by Marcel Fafchamps, to obtain regressions with dyadic robust standard errors. The ado file can be obtained at his [website](https://web.stanford.edu/~fafchamp/resources.html).

## 3 Replication
Place the data from the above repositories in the relevant Ddta directory. The folder structure is already in place.

### 3.1 Colombia Illustration
In ~/radius/Colombia/Code, run:
1. **dta_to_csv.do**
   Saves dta files from Attanasio et al. (2012) as csv files.
2. **dyadic_regression_communities.R**
   Processes dyadic data. Outputs "colombia_dyads_with_communities.csv." Aggregates data to the group level and outputs "defaults_by_group.csv." Generates degree distribution plot (Figure XX).
3. **dyadic_regression_communities.do**
   Inputs "colombia_dyads_with_communities.csv" and "defaults_by_group.csv." Conducts dyadic regression analysis and group default analysis. Saves estimates for radii figures. Saves Tables XX-XX.
4. **muni7plots.R**
   Generates Figures 1, 2, S1, S2, S3, and S4.

### 3.2 Tanzania Illustration
In ~/radius/Tanzania/Code, run:
5. **make tabular data into cvs data**
   Saves dta files from de Weerdt (2025) as csv files.
6. **process_data.R**
   Processes dyadic data. Outputs "tanzania_dyadic_with_communities.csv." Generates network visualization (Figure XX) and degree distribution plots (Figure XX).
7. **dyadic_regressions.R**
   Inputs "tanzania_dyadic_with_communities.csv." Conducts dyadic regression analysis. Saves estimates for radii figures. Saves Tables XX-XX.

### 3.3 Radii of Risk Sharing
After running the files above, in ~/radius/Colombia/Code, run:
8. **radii_plots.R**
   Generates Figures XX-XX.
9. **interactions.do**
  Generates Table XX.

### 3.4 Network Sampling Simulations
In ~/radius/Colombia/Code, run:
simulation.R
plot_sims.R

In ~/radius/Tanzania/Code, run:
simulation.R
plot_sims.R
