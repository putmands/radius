## simulations!!
## 

#### 0. Preliminary ####
##### 0.1 Set up workspace #####
#home_dir = "C:/Users/Daniel Putman/Dropbox/Research/Tanzania_Communities/"
home_dir = "~/radius/Tanzania/"

data_ext = "Data"
figure_ext = "Figures"
table_ext = "Tables"
## update packages
#install.packages("readr")
#install.packages("tidyverse")
#install.packages("igraph")
#install.packages("RColorBrewer")
#install.packages("gtools")
#install.packages("stargazer")
#install.packages("ggplot2")
#install.packages("lfe")
## load packages
library(readr)
library(dplyr)
library(igraph)
library(RColorBrewer)
library(gtools)
library(stargazer)
library(ggplot2)
library(lfe)
library(beepr)

n_keep = 90
p_keep = 100*round(n_keep/119,2)
n_drop = 119 - n_keep
nsims = 5000
#### 1 Process Dyadic Data ####
#### 1.1 Load Data ####
#### 1.1.1 Dyadic Data ####
setwd(paste0(home_dir,data_ext))
dyadic_full = read_csv(file = "tanzania_dyadic_with_communities.csv")
nodes = unique(c(dyadic_full$ha,dyadic_full$hb))

set.seed(54301)
#### 1.1.3 Network Data ####
## load directed network and plot
network = read_csv("directed.csv")

##### 1.2 Compute Network Measures #####
## For each municipality:
##    build village network
##    detect communities
##    compute centrality: degree, ev, betweenness
##    find distance-2 connections
##    ...  distance-3 ...
##    collect other statistics at the network level

g_full = graph.data.frame(d = network,directed = F)
# remove any multiple links and self-loops
g_full = simplify(g_full,remove.multiple = T,remove.loops = T)
## make sure graph is undirected
g_full = as.undirected(g_full,mode = "collapse")

## make a new graph with all unique ids
g_nodes = make_empty_graph(n = 0,directed = F) + vertices(nodes)
## take the union to reintroduce singletons
g_full = igraph::union(g_full,g_nodes)

## create storage
bs = rep(NA,nsims)
ba = rep(NA,nsims)
ba2 = rep(NA,nsims)
ba3 = rep(NA,nsims)
bc = rep(NA,nsims)

bs_only = rep(NA,nsims)
ba_only = rep(NA,nsims)
ba2_only = rep(NA,nsims)
ba3_only = rep(NA,nsims)
bc_only = rep(NA,nsims)


## correlation of sampled support with non sampled variables
rho_ss = rep(NA,nsims)
rho_sa = rep(NA,nsims)
rho_sa2 = rep(NA,nsims)
rho_sa3 = rep(NA,nsims)
rho_sc = rep(NA,nsims)

## correlation of sampled distance-1 with non sampled variables
rho_as = rep(NA,nsims)
rho_aa = rep(NA,nsims)
rho_aa2 = rep(NA,nsims)
rho_aa3 = rep(NA,nsims)
rho_ac = rep(NA,nsims)

## correlation of sampled distance-2 with non sampled variables
rho_a2s = rep(NA,nsims)
rho_a2a = rep(NA,nsims)
rho_a2a2 = rep(NA,nsims)
rho_a2a3 = rep(NA,nsims)
rho_a2c = rep(NA,nsims)

## correlation of sampled distance-3 with non sampled variables
rho_a3s = rep(NA,nsims)
rho_a3a = rep(NA,nsims)
rho_a3a2 = rep(NA,nsims)
rho_a3a3 = rep(NA,nsims)
rho_a3c = rep(NA,nsims)

## correlation of sampled communities with non sampled variables
rho_cs = rep(NA,nsims)
rho_ca = rep(NA,nsims)
rho_ca2 = rep(NA,nsims)
rho_ca3 = rep(NA,nsims)
rho_cc = rep(NA,nsims)

## simulation
for(sim in 1:nsims){
  ## drop nodes
  nodes_drop = sample(x = nodes,size = n_drop,replace = F)
  nodes_keep = dplyr::setdiff(x = nodes,y = nodes_drop)
  g = delete_vertices(g_full, as.character(nodes_drop))
  
  ## process data
  ## convert to edgelist
  el = as_edgelist(g)
  el = data.frame(el)
  el$frfam_alt = 1
  el = as_tibble(el)
  names(el) = c("ha","hb","frfam_sampled")
  el$ha = as.double(el$ha)
  el$hb = as.double(el$hb)
  el_b = el
  names(el_b) = c("hb","ha","frfam_sampled")
  el_b = el_b %>% select(ha,hb,frfam_sampled)
  el = bind_rows(el,el_b)
  rm(el_b)
  
  ## run community detection algorithm
  cd = cluster_walktrap(g,steps = 4)
  ## store communities with network centralities
  comm_a = data.frame(ha = as.numeric(V(g)$name),
                      comm_a = cd$membership)
  comm_b = comm_a
  
  ## rename
  names(comm_b) = c("hb","comm_b")
  
  ## number of people in village
  n = dim(comm_a)[1]
  
  ## construct all possible dyads:
  ##    use all permuations of unique id pairs
  ##    repeats = F means no self matches
  comms = permutations(n,2,nodes_keep,repeats = F)
  comms = data.frame(comms)
  names(comms) = c("ha","hb")
  ## join together frame with communities
  comms = left_join(comms,comm_a, by = c("ha"))
  comms = left_join(comms,comm_b, by = c("hb"))
  
  ## move muni network over to adjacency matrix
  a = get.adjacency(g)
  ## square this matrix to get adj matrix of all distance 2 walks
  a2 = a %*% a
  ## take out loops of distance 2
  diag(a2) = 0
  ## ignore multiplicity of walks
  a2[a2!=0] = 1 
  ## can also do av3: walks of distance 3
  a3 = a2 %*% a
  diag(a3) = 0
  a3[a3!=0] = 1
  ## convert these matrix back to igraph objects
  g2 <- graph.adjacency(a2)
  g3 = graph.adjacency(a3)
  ## convert out to edgelist
  el2 = as_edgelist(g2)
  el2 = data.frame(el2)
  names(el2) = c("ha","hb")
  el2$ha = as.double(as.character(el2$ha))
  el2$hb = as.double(as.character(el2$hb))
  ## edgelist of length three connections
  el3 = as_edgelist(g3)
  el3 = data.frame(el3)
  names(el3) = c("ha","hb")
  el3$ha = as.double(as.character(el3$ha))
  el3$hb = as.double(as.character(el3$hb))
  
  ## check if second degree connections exist
  if(dim(el2)[1] == 0){
    ## build empty data frame here
    el2 <- data.frame(ha = double(),
                      hb = double(), 
                      frfam2_sampled = double()) 
  } else {
    ## fill data frame here
    el2$frfam2_sampled = 1
  }
  
  ## check if third degree connections exist (same)
  if(dim(el3)[1] == 0){
    el3 <- data.frame(ha = double(),
                      hb = double(), 
                      frfam3_sampled = double()) 
  } else {
    el3$frfam3_sampled = 1
  }
  
  #### 2 Assemble Dyadic Data with Simulated Data ####
  ##### 2.1 Compile Data ##### 
  ## networks
  comms = as_tibble(comms)
  ## distance-2 connections
  el2 = as_tibble(el2)
  ## distance-3 connections
  el3 = as_tibble(el3)
  
  ##### 2.2 Merge Datasets and Compute Sampled Dyadic Statistics #####
  comms$samecomm_sampled = ifelse(comms$comm_a == comms$comm_b,1,0)
  
  ## make sampled dyadic datset before merging
  dyadic = dyadic_full %>% filter(ha %in% nodes_keep & hb %in% nodes_keep) %>% 
    select(ha,hb,any_transfer,reciprocal_transfer,total_transfers,
           frfam,frfam2,frfam3,supported,samecomm,samegroups)
  
  comms = comms %>% 
    select(ha,hb,samecomm_sampled)
  
  dyadic = left_join(dyadic,comms,by = c("ha","hb"))
  ## distance-1 sampled
  dyadic = left_join(dyadic,el,by = c("ha","hb"))
  dyadic$frfam_sampled = ifelse(is.na(dyadic$frfam_sampled),0,dyadic$frfam_sampled)
  ## does a path of length 2 from i reach j?
  dyadic = left_join(dyadic,el2,by = c("ha","hb"))
  dyadic$frfam2_sampled = ifelse(is.na(dyadic$frfam2_sampled),0,dyadic$frfam2_sampled)
  ## does a path of length 3 from i reach j?
  dyadic = left_join(dyadic,el3,by = c("ha","hb"))
  dyadic$frfam3_sampled = ifelse(is.na(dyadic$frfam3_sampled),0,dyadic$frfam3_sampled)
  ## is ij supported, i.e., directly connected and connected via a path of length 2
  dyadic$supported_sampled = dyadic$frfam_sampled*dyadic$frfam2_sampled
  ## are i and j in any risk sharing groups together?
  dyadic$frfam3_sampled = ifelse(dyadic$frfam2_sampled == 1 | dyadic$frfam_sampled == 1,0,dyadic$frfam3_sampled)
  dyadic$frfam2_sampled = ifelse(dyadic$frfam_sampled == 1,0,dyadic$frfam2_sampled)
  
  #### 3 Simulation Statistics ####
  #### 3.1 Correlations between Sampled and Actual Dyadic Relationships ####
  ## only need the upper left hand corner of the matrix (matrix symmetric, not interested in cross-correlations among sampled variables)
  cor_matrix = cor(dyadic[,c("supported_sampled","frfam_sampled","frfam2_sampled","frfam3_sampled","samecomm_sampled","supported","frfam","frfam2","frfam3","samecomm")])[c("supported_sampled","frfam_sampled","frfam2_sampled","frfam3_sampled","samecomm_sampled"),c("supported","frfam","frfam2","frfam3","samecomm")]
  
  ## code s = support, a = distance-1, a2 = distance-2, a3 = distance-3, c = community
  ## first letter(number) is always computed using the sampled network and second is always actual network
  ## save correlations
  rho_ss[sim] = cor_matrix["supported_sampled","supported"]
  rho_sa[sim] = cor_matrix["supported_sampled","frfam"]
  rho_sa2[sim] = cor_matrix["supported_sampled","frfam2"]
  rho_sa3[sim] = cor_matrix["supported_sampled","frfam3"]
  rho_sc[sim] = cor_matrix["supported_sampled","samecomm"]
  
  rho_as[sim] = cor_matrix["frfam_sampled","supported"]
  rho_aa[sim] = cor_matrix["frfam_sampled","frfam"]
  rho_aa2[sim] = cor_matrix["frfam_sampled","frfam2"]
  rho_aa3[sim] = cor_matrix["frfam_sampled","frfam3"]
  rho_ac[sim] = cor_matrix["frfam_sampled","samecomm"]
  
  rho_a2s[sim] = cor_matrix["frfam2_sampled","supported"]
  rho_a2a[sim] = cor_matrix["frfam2_sampled","frfam"]
  rho_a2a2[sim] = cor_matrix["frfam2_sampled","frfam2"]
  rho_a2a3[sim] = cor_matrix["frfam2_sampled","frfam3"]
  rho_a2c[sim] = cor_matrix["frfam2_sampled","samecomm"]
  
  rho_a3s[sim] = cor_matrix["frfam3_sampled","supported"]
  rho_a3a[sim] = cor_matrix["frfam3_sampled","frfam"]
  rho_a3a2[sim] = cor_matrix["frfam3_sampled","frfam2"]
  rho_a3a3[sim] = cor_matrix["frfam3_sampled","frfam3"]
  rho_a3c[sim] = cor_matrix["frfam3_sampled","samecomm"]
  
  rho_cs[sim] = cor_matrix["samecomm_sampled","supported"]
  rho_ca[sim] = cor_matrix["samecomm_sampled","frfam"]
  rho_ca2[sim] = cor_matrix["samecomm_sampled","frfam2"]
  rho_ca3[sim] = cor_matrix["samecomm_sampled","frfam3"]
  rho_cc[sim] = cor_matrix["samecomm_sampled","samecomm"]
  
  #### 3.2 Regressions with Sampled Networks ####
  ## support and community run alone because that is how they are introduced in paper
  ## however distance-2 and 3 are always run with distance-1 so distance only
  ## no worry about bias in distance-1 so that needs not be run alone
  support_only = lm(data = dyadic,formula = any_transfer ~ supported_sampled)
  distance_only = lm(data = dyadic,formula = any_transfer ~ frfam_sampled + frfam2_sampled + frfam3_sampled)
  comm_only = lm(data = dyadic,formula = any_transfer ~ samecomm_sampled)
  
  ## full model here is the full "longer walks" model
  full_model = lm(data = dyadic,formula = any_transfer ~ supported_sampled + frfam_sampled + frfam2_sampled + frfam3_sampled + samecomm_sampled)
  
  ## save coefficients
  bs[sim] = full_model$coefficients["supported_sampled"]
  ba[sim] = full_model$coefficients["frfam_sampled"]
  ba2[sim] = full_model$coefficients["frfam2_sampled"]
  ba3[sim] = full_model$coefficients["frfam3_sampled"]
  bc[sim] = full_model$coefficients["samecomm_sampled"]
  bs_only[sim] = support_only$coefficients["supported_sampled"]
  ba_only[sim] = distance_only$coefficients["frfam_sampled"]
  ba2_only[sim] = distance_only$coefficients["frfam2_sampled"]
  ba3_only[sim] = distance_only$coefficients["frfam3_sampled"]
  bc_only[sim] = comm_only$coefficients["samecomm_sampled"]
}

#### 4 Tables ####
#### 4.1 Compile simulation data ####
## Correlations
sim_cors = 
  data.frame(rho_ss,rho_sa,rho_sa2,rho_sa3,rho_sc,
             rho_as,rho_aa,rho_aa2,rho_aa3,rho_ac,
             rho_a2s,rho_a2a,rho_a2a2,rho_a2a3,rho_a2c,
             rho_a3s,rho_a3a,rho_a3a2,rho_a3a3,rho_a3c,
             rho_cs,rho_ca,rho_ca2,rho_ca3,rho_cc)

## Coefficients 
sim_coefs = 
    data.frame(
      bs_only,ba_only,ba2_only,ba3_only,bc_only,
      bs,ba,ba2,ba3,bc)


#### 4.2 Build Correlations Table ####
## repopulate correlations matrix
## empty matrix
mean_cor = matrix(data = NA,nrow = 5,ncol = 5)
## same colnames and rownames as before (to make it easy)
varnames = c("supported","frfam","frfam2","frfam3","samecomm")
varnames_sampled = paste0(varnames,"_sampled")
rownames(mean_cor) = varnames_sampled
colnames(mean_cor) = varnames

## unpack mean correlations back into matrix 
mean_cor["supported_sampled","supported"] = round(mean(rho_ss,na.rm = TRUE),2)
mean_cor["supported_sampled","frfam"] = round(mean(rho_sa,na.rm = TRUE),2)
mean_cor["supported_sampled","frfam2"] = round(mean(rho_sa2,na.rm = TRUE),2)
mean_cor["supported_sampled","frfam3"] = round(mean(rho_sa3,na.rm = TRUE),2)
mean_cor["supported_sampled","samecomm"] = round(mean(rho_sc,na.rm = TRUE),2)

mean_cor["frfam_sampled","supported"] = round(mean(rho_as,na.rm = TRUE),2)
mean_cor["frfam_sampled","frfam"] = round(mean(rho_aa,na.rm = TRUE),2)
mean_cor["frfam_sampled","frfam2"] = round(mean(rho_aa2,na.rm = TRUE),2)
mean_cor["frfam_sampled","frfam3"] = round(mean(rho_aa3,na.rm = TRUE),2)
mean_cor["frfam_sampled","samecomm"] = round(mean(rho_ac,na.rm = TRUE),2)

mean_cor["frfam2_sampled","supported"] = round(mean(rho_a2s,na.rm = TRUE),2)
mean_cor["frfam2_sampled","frfam"] = round(mean(rho_a2a,na.rm = TRUE),2)
mean_cor["frfam2_sampled","frfam2"] = round(mean(rho_a2a2,na.rm = TRUE),2)
mean_cor["frfam2_sampled","frfam3"] = round(mean(rho_a2a3,na.rm = TRUE),2)
mean_cor["frfam2_sampled","samecomm"] = round(mean(rho_a2c,na.rm = TRUE),2)

mean_cor["frfam3_sampled","supported"] = round(mean(rho_a3s,na.rm = TRUE),2)
mean_cor["frfam3_sampled","frfam"] = round(mean(rho_a3a,na.rm = TRUE),2)
mean_cor["frfam3_sampled","frfam2"] = round(mean(rho_a3a2,na.rm = TRUE),2)
mean_cor["frfam3_sampled","frfam3"] = round(mean(rho_a3a3,na.rm = TRUE),2)
mean_cor["frfam3_sampled","samecomm"] = round(mean(rho_a3c,na.rm = TRUE),2)

mean_cor["samecomm_sampled","supported"] = round(mean(rho_cs,na.rm = TRUE),2)
mean_cor["samecomm_sampled","frfam"] = round(mean(rho_ca,na.rm = TRUE),2)
mean_cor["samecomm_sampled","frfam2"] = round(mean(rho_ca2,na.rm = TRUE),2)
mean_cor["samecomm_sampled","frfam3"] = round(mean(rho_ca3,na.rm = TRUE),2)
mean_cor["samecomm_sampled","samecomm"] = round(mean(rho_cc,na.rm = TRUE),2)

## now change colnames and rownames to produce table 
varnames = c("Supported","Distance-1","Distance-2","Distance-3","Same Community")
varnames_sampled = paste0(varnames," (Sampled Network)")
rownames(mean_cor) = varnames_sampled
colnames(mean_cor) = varnames

## make title for table
cor_title = paste0("Average Correlation Coefficients for ",n_keep," Household ($approx ",p_keep,"$) Sample of Nyakatoke Network")
cor_filename = paste0("correlations_",n_keep,"_hhs_",nsims,"_sims.tex")
cor_label = paste0("tab:sim",n_keep,"_cor")

setwd(paste0(home_dir,table_ext))
stargazer(data.frame(mean_cor),summary = FALSE,
          title = cor_title,label = cor_label,
          type = "latex",out=cor_filename)

#### 4.3 Build Summary of Regression Coefficients ####
## get coefficients from full sample
## only regressions
support_only = lm(data = dyadic,formula = any_transfer ~ supported)
distance_only = lm(data = dyadic,formula = any_transfer ~ frfam + frfam2 + frfam3)
comm_only = lm(data = dyadic,formula = any_transfer ~ samecomm)

## full model here is the full "longer walks" model
full_model = lm(data = dyadic,formula = any_transfer ~ supported + frfam + frfam2 + frfam3 + samecomm)

## save coefficients
betas = full_model$coefficients["supported"]
betaa = full_model$coefficients["frfam"]
betaa2 = full_model$coefficients["frfam2"]
betaa3 = full_model$coefficients["frfam3"]
betac = full_model$coefficients["samecomm"]
betas_only = support_only$coefficients["supported"]
betaa_only = distance_only$coefficients["frfam"]
betaa2_only = distance_only$coefficients["frfam2"]
betaa3_only = distance_only$coefficients["frfam3"]
betac_only = comm_only$coefficients["samecomm"]

reg_title = paste0("Summary of Regression Coefficients (Outcome: Any Transfer) for ",n_keep," Household ($approx ",p_keep,"$) Sample of Nyakatoke Network")
reg_filename = paste0("regressions_",n_keep,"_hhs_",nsims,"_sims.tex")
reg_label = paste0("tab:sim",n_keep,"_reg")
reg_variables = c("Supported (Single)","Distance 1 (Distance Only Model)","Distance 2 (Distance Only Model)","Distance 3 (Distance Only Model)","Same Community (Single)",
                  "Supported (Full Model)","Distance 1 (Full Model)","Distance 2 (Full Model)","Distance 3 (Full Model)","Same Community (Full Model)")
names(sim_coefs) = reg_variables

stargazer(sim_coefs,type = "latex",label = reg_label,
          summary.stat = c("n","median","mean","sd"),
          title = reg_title,out = reg_filename)

beep(1)