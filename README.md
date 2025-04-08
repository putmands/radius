# Social Network Structure and the Radius of Risk Sharing
Replication code for "Social Network Structure and the Radius of Risk Sharing" accepted at _Social Network Analysis and Mining_. 

## Data
The paper draws on two empirical illustrations, in Colombia and Tanzania. The Colombia illustration draws on data from Attanasio et al. (2012) and de Weerdt (2025). To replicate this project, download this data from the repositories cited below.

Attanasio, Orazio, Barr, Abigail, Cardenas, Juan Camilo, Genicot, Garance, and Meghir, Costas. Replication data for: Risk Pooling, Risk Preferences, and Social Networks. Nashville, TN: American Economic Association [publisher], 2012. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor], 2019-10-12. https://doi.org/10.3886/E113814V1

De Weerdt, Joachim, 2025, "Nyakatoke Network Data", https://doi.org/10.7910/DVN/U2OJE3, Harvard Dataverse, V1, UNF:6:YcKmqT/wWWrvPW8juI+wFA== [fileUNF]

## Software
The results were produced using R and Stata. For R, I include a set-up script that will install necessary packages. For Stata, the user will need to install NGREG, written by Marcel Fafchamps, to obtain regressions with dyadic robust standard errors. The ado file can be obtained at his [website](https://web.stanford.edu/~fafchamp/resources.html).

## Replication

### Colombia Illustration
In ~/radius/Colombia/Code, run:
1. dta_to_csv.do
2. dyadic_regression_communities.R
3. dyadic_regression_communities.do
4. muni7plots.R

### Tanzania Illustration
In ~/radius/Tanzania/Code, run:
5. 

### Radii of Risk Sharing
In ~/radius/Colombia/Code, run:

### Network Sampling Simulations
In ~/radius/Colombia/Code, run:
simulation.R
plot_sims.R

In ~/radius/Tanzania/Code, run:
simulation.R
plot_sims.R
