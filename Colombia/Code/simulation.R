## simulations!!
## 

#### 0. Preliminary ####
##### 0.1 Set up workspace #####
home_dir = "C:/Users/putmands/Dropbox/Research/Colombia_Scope/"
#home_dir = "C:/Users/Daniel Putman/Dropbox/Research/Colombia_Scope/"


data_ext = "Data"
results_ext = "Results"
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

p_drop = 3/4
#p_drop = 1/8
nsims = 500
#### 1 Process Dyadic Data ####
#### 1.1 Load Data ####
#### 1.1.1 Dyadic Data ####
setwd(paste0(home_dir,data_ext))
network = read_csv(file = "AttanasioEtAl2011Dyadic.csv") %>% 
  select(iida,iidb,municode,samegroup,frfam) ## restrict columns

dyadic = read_csv(file = "colombia_dyads_with_communities.csv") %>%
  select(iida,iidb,municode,samegroup,frfam_supported,frfam,frfam2,frfam3,samecomm)

villages = c(1:70)
myseed = 54302

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

set.seed(myseed)
for(sim in 1:nsims){
  ## maybe i need these
  network_list = list()
  el2_list = list()
  el3_list = list()
  network_sim = network 
  
  for(v in villages){
    ## building municipality network
    ## take only those in community v
    network_v = network %>% filter(municode == v)
    ## now take unique nodes (in case some have no links)
    nodes_v = as.character(unique(c(network_v$iida,network_v$iidb)))
    ## make a new graph with all unique ids
    g_nodes = make_empty_graph(n = 0,directed = F) + vertices(nodes_v)
    
    ## subset to only include network connections
    network_v = network_v %>% 
      filter(frfam == 1)
    ## make into igraph object
    gv = graph.data.frame(network_v,directed = F)
    # remove any multiple links and self-loops
    gv = igraph::simplify(gv,remove.multiple = T,remove.loops = T)
    ## make sure graph is undirected
    gv = as.undirected(gv,mode = "collapse")
    ## take the union to reintroduce singletons
    gv = igraph::union(gv,g_nodes)
    
    n_nodes = length(nodes_v)
    n_drop = round(n_nodes*p_drop,0)

    ## drop nodes
    nodes_drop_v = sample(x = nodes_v,size = n_drop,replace = F)
    nodes_keep_v = dplyr::setdiff(x = nodes_v,y = nodes_drop_v)
    gv_sim = delete_vertices(gv, as.character(nodes_drop_v))
    
    ## run community detection algorithm for frfam
    cdv = cluster_walktrap(gv_sim,steps = 4)
    comm_a = data.frame(iida = as.numeric(V(gv_sim)$name),
                        comm_a = cdv$membership)
    comm_b = comm_a
    ## rename
    names(comm_b) = c("iidb","comm_b")
    
    ## number of people in village
    nv = dim(comm_a)[1]
    ## construct all possible dyads:
    ##    use all permuations of unique id pairs
    ##    repeats = F means no self matches
    commsv = as_tibble(expand.grid(nodes_keep_v,nodes_keep_v)) 
    names(commsv) = c("iida","iidb")
    commsv = commsv %>% filter(!(iida == iidb)) %>%
      mutate(iida = as.double(as.character(iida)),
             iidb = as.double(as.character(iidb)))
    
    ## join together frame with communities
    commsv = left_join(commsv,comm_a, by = c("iida"))
    commsv = left_join(commsv,comm_b, by = c("iidb"))
    ## add municode
    commsv$municode = v
    ## identify communities differently by village
    ## i.e., comm 1 in village 1 is different than comm 1 in village 2
    commsv$comm_muni_a = commsv$municode*100 + commsv$comm_a 
    commsv$comm_muni_b = commsv$municode*100 + commsv$comm_b
    ## store this data in a list to be compiled later
    network_list[[v]] = commsv
    
    ## move muni network over to adjacency matrix
    av = get.adjacency(gv)
    ## square this matrix to get adj matrix of all distance 2 walks
    av2 = av %*% av
    ## take out loops of distance 2
    diag(av2) = 0
    ## ignore multiplicity of walks
    av2[av2!=0] = 1 
    ## can also do av3: walks of distance 3
    av3 = av2 %*% av
    diag(av3) = 0
    av3[av3!=0] = 1
    ## convert these matrix back to igraph objects
    gv2 <- graph.adjacency(av2)
    gv3 = graph.adjacency(av3)
    
    ## convert out to edgelist
    el2_v = as_edgelist(gv2)
    el2_v = data.frame(el2_v)
    names(el2_v) = c("iida","iidb")
    el2_v$iida = as.double(as.character(el2_v$iida))
    el2_v$iidb = as.double(as.character(el2_v$iidb))
    
    ## edgelist of length three connections
    el3_v = as_edgelist(gv3)
    el3_v = data.frame(el3_v)
    names(el3_v) = c("iida","iidb")
    el3_v$iida = as.double(as.character(el3_v$iida))
    el3_v$iidb = as.double(as.character(el3_v$iidb))
    
    ## check if second degree connections exist
    if(dim(el2_v)[1] == 0){
      ## build empty data frame here
      el2_v <- data.frame(iida = double(),
                          iidb = double(), 
                          municode = double(), 
                          frfam2 = double()) 
    } else {
      ## fill data frame here
      el2_v$municode = v
      el2_v$frfam2 = 1
    }
    
    ## check if third degree connections exist (same)
    if(dim(el3_v)[1] == 0){
      el3_v <- data.frame(iida = double(),
                          iidb = double(), 
                          municode = double(), 
                          frfam3 = double()) 
    } else {
      el3_v$municode = v
      el3_v$frfam3 = 1
    }
    ## store in list
    el2_list[[v]] = el2_v
    el3_list[[v]] = el3_v
  }
  
  ## networks
  comms = bind_rows(network_list)
  comms = as_tibble(comms)
  ## distance-2 connections
  el2 = bind_rows(el2_list)
  el2 = as_tibble(el2)
  ## distance-3 connections
  el3 = bind_rows(el3_list)
  el3 = as_tibble(el3)
  
  ## ISSUE HERE
  comms$samecomm = ifelse(comms$comm_muni_a == comms$comm_muni_b,1,0)
  network_sim = left_join(comms,network,by = c("iida","iidb","municode"))
  network_sim = left_join(network_sim,el2,by = c("iida","iidb","municode"))
  network_sim = left_join(network_sim,el3,by = c("iida","iidb","municode"))
  network_sim = network_sim %>% 
    mutate(frfam2 = ifelse(is.na(frfam2),0,frfam2),
           frfam3 = ifelse(is.na(frfam3),0,frfam3),
           frfam_supported = frfam*frfam2,
           frfam3 = ifelse(frfam + frfam2 > 0,0,frfam3),
           frfam2 = ifelse(frfam > 0,0,frfam2)) %>%
    select(iida,iidb,municode,samegroup,frfam_supported,frfam,frfam2,frfam3,samecomm)
  
  colnames(network_sim) = 
    c("iida","iidb","municode","samegroup","frfam_supported_sampled","frfam_sampled",
      "frfam2_sampled","frfam3_sampled","samecomm_sampled")
  
  network_sim = left_join(network_sim,dyadic,by = c("iida","iidb","municode","samegroup"))
  
  ## compute correlations, save
  cor_matrix = cor(network_sim[,c("frfam_supported","frfam","frfam2","frfam3","samecomm",
                  "frfam_supported_sampled","frfam_sampled","frfam2_sampled","frfam3_sampled","samecomm_sampled")])
  
  
  rho_ss[sim] = cor_matrix["frfam_supported_sampled","frfam_supported"]
  rho_sa[sim] = cor_matrix["frfam_supported_sampled","frfam"]
  rho_sa2[sim] = cor_matrix["frfam_supported_sampled","frfam2"]
  rho_sa3[sim] = cor_matrix["frfam_supported_sampled","frfam3"]
  rho_sc[sim] = cor_matrix["frfam_supported_sampled","samecomm"]
  
  rho_as[sim] = cor_matrix["frfam_sampled","frfam_supported"]
  rho_aa[sim] = cor_matrix["frfam_sampled","frfam"]
  rho_aa2[sim] = cor_matrix["frfam_sampled","frfam2"]
  rho_aa3[sim] = cor_matrix["frfam_sampled","frfam3"]
  rho_ac[sim] = cor_matrix["frfam_sampled","samecomm"]
  
  rho_a2s[sim] = cor_matrix["frfam2_sampled","frfam_supported"]
  rho_a2a[sim] = cor_matrix["frfam2_sampled","frfam"]
  rho_a2a2[sim] = cor_matrix["frfam2_sampled","frfam2"]
  rho_a2a3[sim] = cor_matrix["frfam2_sampled","frfam3"]
  rho_a2c[sim] = cor_matrix["frfam2_sampled","samecomm"]
  
  rho_a3s[sim] = cor_matrix["frfam3_sampled","frfam_supported"]
  rho_a3a[sim] = cor_matrix["frfam3_sampled","frfam"]
  rho_a3a2[sim] = cor_matrix["frfam3_sampled","frfam2"]
  rho_a3a3[sim] = cor_matrix["frfam3_sampled","frfam3"]
  rho_a3c[sim] = cor_matrix["frfam3_sampled","samecomm"]
  
  rho_cs[sim] = cor_matrix["samecomm_sampled","frfam_supported"]
  rho_ca[sim] = cor_matrix["samecomm_sampled","frfam"]
  rho_ca2[sim] = cor_matrix["samecomm_sampled","frfam2"]
  rho_ca3[sim] = cor_matrix["samecomm_sampled","frfam3"]
  rho_cc[sim] = cor_matrix["samecomm_sampled","samecomm"]
  
  
  ## single variable regressions
  support_model = 
    felm(data = network_sim,
         formula = samegroup ~ frfam_supported_sampled | municode | 0 | municode)
  distance_model = 
    felm(data = network_sim,
         formula = samegroup ~ frfam_sampled +  frfam2_sampled + 
           frfam3_sampled | municode | 0 | municode)
  comm_model = 
    felm(data = network_sim,
         formula = samegroup ~ samecomm_sampled | municode | 0 | municode)
  ## multiple regressions
  full_model = 
    felm(data = network_sim,formula = samegroup ~ frfam_supported_sampled + 
           frfam_sampled + frfam2_sampled + frfam3_sampled + samecomm_sampled | 
           municode |  0 | municode)
  
  ## save coefficients
  bs[sim] = full_model$coefficients["frfam_supported_sampled",]
  ba[sim] = full_model$coefficients["frfam_sampled",]
  ba2[sim] = full_model$coefficients["frfam2_sampled",]
  ba3[sim] = full_model$coefficients["frfam3_sampled",]
  bc[sim] = full_model$coefficients["samecomm_sampled",]
  bs_only[sim] = support_model$coefficients["frfam_supported_sampled",]
  ba_only[sim] = distance_model$coefficients["frfam_sampled",]
  ba2_only[sim] = distance_model$coefficients["frfam2_sampled",]
  ba3_only[sim] = distance_model$coefficients["frfam3_sampled",]
  bc_only[sim] = comm_model$coefficients["samecomm_sampled",]

  
}




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
cor_title = paste0("Average Correlation Coefficients for ",100*(1-p_drop),"% Sample of Colombia Session Networks")
cor_filename = paste0("correlations_",100*(1-p_drop),"_percent_sample_",nsims,"_sims.tex")
cor_label = paste0("tab:sim",100*(1-p_drop),"_cor")

setwd(paste0(home_dir,results_ext))
stargazer(data.frame(mean_cor),summary = FALSE,
          title = cor_title,label = cor_label,
          type = "latex",out=cor_filename)



#### 4.3 Build Summary of Regression Coefficients ####
## get coefficients from full sample
## only regressions
## single variable regressions
support_model = 
  felm(data = dyadic,
       formula = samegroup ~ frfam_supported | municode | 0 | municode)
distance_model = 
  felm(data = dyadic,
       formula = samegroup ~ frfam +  frfam2 +  frfam3 | municode | 0 | municode)
comm_model = 
  felm(data = dyadic,
       formula = samegroup ~ samecomm | municode | 0 | municode)
## multiple regressions
full_model = 
  felm(data = dyadic,formula = samegroup ~ frfam_supported + 
         frfam + frfam2 + frfam3 + samecomm | 
         municode |  0 | municode)


## save coefficients
betas = full_model$coefficients["frfam_supported",]
betaa = full_model$coefficients["frfam",]
betaa2 = full_model$coefficients["frfam2",]
betaa3 = full_model$coefficients["frfam3",]
betac = full_model$coefficients["samecomm",]
betas_only = support_model$coefficients["frfam_supported",]
betaa_only = distance_model$coefficients["frfam",]
betaa2_only = distance_model$coefficients["frfam2",]
betaa3_only = distance_model$coefficients["frfam3",]
betac_only = comm_model$coefficients["samecomm",]

reg_title = paste0("Summary of Regression Coefficients (Outcome: Same Group) for ",
                   100*(1-p_drop),"% Sample of Colombia Session Networks")
reg_filename = paste0("regressions_",100*(1-p_drop),"_percent_sample_",nsims,"_sims.tex")
reg_label = paste0("tab:sim",100*(1-p_drop),"_reg")
reg_variables = c("Supported (Single)","Distance 1 (Distance Only Model)","Distance 2 (Distance Only Model)","Distance 3 (Distance Only Model)","Same Community (Single)",
                  "Supported (Full Model)","Distance 1 (Full Model)","Distance 2 (Full Model)","Distance 3 (Full Model)","Same Community (Full Model)")
names(sim_coefs) = reg_variables

stargazer(sim_coefs,type = "latex",label = reg_label,
          summary.stat = c("n","median","mean","sd"),
          title = reg_title,out = reg_filename)

beep(1)
