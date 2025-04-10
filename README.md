# Social Network Structure and the Radius of Risk Sharing
Replication code for "Social Network Structure and the Radius of Risk Sharing" accepted at _Social Network Analysis and Mining_. 

## 1 Data
The paper draws on two empirical illustrations, in Colombia and Tanzania. The Colombia illustration draws on data from Attanasio et al. (2012) and de Weerdt (2025). To replicate this project, download this data from the repositories cited below.

+ Attanasio, Orazio, Barr, Abigail, Cardenas, Juan Camilo, Genicot, Garance, and Meghir, Costas. Replication data for: Risk Pooling, Risk Preferences, and Social Networks. Nashville, TN: American Economic Association [publisher], 2012. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor], 2019-10-12. https://doi.org/10.3886/E113814V1

+ De Weerdt, Joachim, 2025, "Nyakatoke Network Data", https://doi.org/10.7910/DVN/U2OJE3, Harvard Dataverse, V1, UNF:6:YcKmqT/wWWrvPW8juI+wFA== [fileUNF]

## 2 Software
The results were produced using R and Stata. For R, I include a set-up script that will install necessary packages. For Stata, the user will need to install NGREG, written by Marcel Fafchamps, to obtain regressions with dyadic robust standard errors. The ado file can be obtained at his [website](https://web.stanford.edu/~fafchamp/resources.html).

## 3 Set-Up
Download or pull this repository. Run setup.R (found in ~/radius/). This will install necessary packages in R and will create the correct folder structure. Then place the data from the above repositories in the relevant data directory.

## 4 Replication
To replicate the results of the paper in full, you will need to run 12 programs in the following order. 

### 4.1 Colombia Illustration
In ~/radius/Colombia/Code, run:

1. **dta_to_csv.do**  
   Saves dta files from Attanasio et al. (2012) as csv files.
2. **dyadic_regression_communities.R**  
   Processes dyadic data. Outputs "colombia_dyads_with_communities.csv." Aggregates data to the group level and outputs "defaults_by_group.csv." Generates degree distribution plot (Figure S7a).
3. **dyadic_regression_communities.do**  
   Inputs "colombia_dyads_with_communities.csv" and "defaults_by_group.csv." Conducts dyadic regression analysis and group default analysis. Saves estimates for radii figures. Generates underlying data for Tables 1, S2, and S3 and generates Table 2, S4, S13, S20, S28, and S29. Generates underlying data for Table S21.
4. **muni7plots.R**  
   Generates Figures 1, 2, S1, S2, S3, and S4.
5. **simulation.R**  
   Runs the network sampling simulation in the Colombia illustration. To reproduce the estimates of the paper you will want to run 500 simulations (nsims = 500) dropping 50% of nodes (p_drop = 0.5) and 75% to produce the panels of Tables S11 and S12. To reproduce the results that underly Figure 4b, you will also need to run it dropping 25% of nodes. 

### 4.2 Tanzania Illustration
In ~/radius/Tanzania/Code, run:

6. **dta2csv.do**  
   Saves dta files from and earlier version of the de Weerdt (2025) data which is identical except that it came as dta data. Users who have the newer version of this data will need to save the tab data as a csv in lieu of this step, or adapt the next step to load in the tab data. 
7. **process_data.R**  
   Processes dyadic data. Outputs "tanzania_dyadic_with_communities.csv." Generates network visualization (Figure S5), underlying data for Tables 1, S2, and S3, and degree distribution plot (Figure S7b).
8. **dyadic_regressions.R**  
   Inputs "tanzania_dyadic_with_communities.csv." Conducts dyadic regression analysis. Saves estimates for radii figures. Saves Tables 3, S5, S14, S15, S17-19, S22-S27. Generates underlying data for Table S21.
9. **simulation.R**  
   Runs the network sampling simulation in the Tanzania illustration. To reproduce the estimates of the paper you will want to run 5000 simulations (nsims = 5000) dropping 50% of nodes (p_drop = 0.5), and 75% to produce the panels of Tables S8, S9, and S10. To reproduce the results that underly Figure 4a, you will also need to run it dropping 25% of nodes. 


### 4.3 Radii of Risk Sharing
After running the files above, in ~/radius/Colombia/Code, run:

10. **radii_plots.R**  
   Generates Figures 3 and S6. As a shortcut, this loads the conditional expectations data from xlsx files created by the author.
11. **interactions.do**  
   Generates Table S7.

### 4.4 Network Sampling Simulations
In ~/radius/Colombia/Code, run:

12. **plot_sims.R**  
    Generates Figure 4. As a shortcut, this script has the results from **simulation.R** (from both illustrations) hardcoded in.
