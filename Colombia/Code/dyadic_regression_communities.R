#### 0. Preliminary ####
##### 0.1 Set up workspace #####
#home_dir = "C:/Users/Daniel Putman/Dropbox/Research/Colombia_Scope/"
#home_dir = "X:/Dropbox/Colombia_Scope/"
home_dir = "~/radius/Colombia/"
code_ext = "Code"
data_ext = "Data"
results_ext = "Results"
setwd(paste0(home_dir,code_ext))

lcu2ppp = 992.94

library(readr)
library(dplyr)
library(igraph)
library(RColorBrewer)
library(gtools)
library(stargazer)
library(ggplot2)
library(lfe)
library(dotwhisker)
library(multcomp)
library(patchwork)
##### 0.2 Functions #####
## function that builds closeness measure for multi-component graph
graph_closeness = function(graph){
  N = length(V(graph))
  d = distances(graph)
  c_total = 0
  n_total = 0
  for(i in 1:N){
    for(j in 1:N){
      if(i != j){
        if(d[i,j] != Inf){
          c_total = c_total + 1/d[i,j]
          n_total = n_total + 1
        }
      }
    }
  }
  c = c_total/n_total
  return(c)
}

exp_payout = function(choices){
  y_good = c(3000,5700,7200,9000,11000,12000)
  y_bad = c(3000,2700,2400,1800,1000,0)
  ey = 0.5*y_good+0.5*y_bad
  vector_ey = ey[choices]
  return(vector_ey)
}

#### 1 Process Dyadic Data ####
##### 1.1 Load Data #####
setwd(paste0("../",data_ext))
network = read_csv("AttanasioEtAl2011Dyadic.csv")

##### 1.2 Create Storage ##### 
villages = c(1:70)
network_list = list()
networkcl_list = list()
el2_list = list()
el3_list = list()
elcl2_list = list()
elcl3_list = list()
graph_stats = list()
graph_statscl = list()
degrees_list = list()

##### 1.3 Compute Network Measures #####
## For each municipality:
##    build munipality network
##    detect communities
##    compute centrality: degree, ev, betweenness
##    find distance-2 connections
##    ...  distance-3 ...
##    collect other statistics at the network level

for(v in villages){
  ## building municipality network
  ## take only those in community v
  network_v = subset(network, subset = municode == v)
  ## now take unique nodes (in case some have no links)
  nodes_v = as.character(unique(c(network_v$iida,network_v$iidb)))
  ## make a new graph with all unique ids
  g_nodes = make_empty_graph(n = 0,directed = F) + vertices(nodes_v)
  
  ## restrict columns
  network_v = network_v[,c("iida","iidb","frfam","frfamcl")]
  ## subset to only include network connections
  network_v = subset(network_v, subset = (frfam == 1))
  ## make into igraph object
  gv = graph.data.frame(network_v,directed = F)
  # remove any multiple links and self-loops
  gv = igraph::simplify(gv,remove.multiple = T,remove.loops = T)
  ## make sure graph is undirected
  gv = as.undirected(gv,mode = "collapse")
  ## take the union to reintroduce singletons
  gv = igraph::union(gv,g_nodes)
  
  ## run community detection algorithm for frfam
  set.seed(54301)
  cdv = cluster_walktrap(gv,steps = 4)
  cdv_eb = cluster_edge_betweenness(gv)
  ## other options give similar results (should be commented out)
  #cdv = cluster_edge_betweenness(gv)
  #cdv = cluster_infomap(gv)
  ## store communities with network centralities
  comm_a = data.frame(iida = as.numeric(V(gv)$name),
                      comm_a = cdv$membership,
                      comm_eb_a = cdv_eb$membership,
                      deg_a = degree(gv), 
                      ev_a = eigen_centrality(gv)$vector, 
                      eb_a = betweenness(gv))
  comm_b = comm_a
  ## rename
  names(comm_b) = c("iidb","comm_b","comm_eb_b","deg_b","ev_b","eb_b")
  ## number of people in village
  nv = dim(comm_a)[1]
  ## construct all possible dyads:
  ##    use all permuations of unique id pairs
  ##    repeats = F means no self matches
  commsv = permutations(nv,2,as.numeric(nodes_v),repeats = F)
  commsv = data.frame(commsv)
  names(commsv) = c("iida","iidb")
  ## hold on to a version for later
  commsclv = commsv 
  ## join together frame with communities
  commsv = left_join(commsv,comm_a, by = c("iida"))
  commsv = left_join(commsv,comm_b, by = c("iidb"))
  ## add municode
  commsv$municode = v
  ## identify communities differently by village
  ## i.e., comm 1 in village 1 is different than comm 1 in village 2
  commsv$comm_muni_a = commsv$municode*100 + commsv$comm_a 
  commsv$comm_muni_b = commsv$municode*100 + commsv$comm_b
  commsv$comm_muni_eb_a = commsv$municode*100 + commsv$comm_eb_a 
  commsv$comm_muni_eb_b = commsv$municode*100 + commsv$comm_eb_b
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
  
  ## ------------------------------------ ##
  ## do again for close friends and family
  networkcl_v = subset(network_v, subset = (frfamcl == 1))
  gclv = graph.data.frame(networkcl_v,directed = F)
  gclv = igraph::simplify(gclv,remove.multiple = T,remove.loops = T)
  gclv = as.undirected(gclv,mode = "collapse")
  gclv = igraph::union(gclv,g_nodes)
  ## run community detection algorithm
  cdclv = cluster_walktrap(gclv,steps = 2)
  ## other options give similar results (should be commented out)
  #cdv = cluster_edge_betweenness(gv)
  #cdv = cluster_infomap(gv)
  ## store communities with network centralities
  commcl_a = data.frame(iida = as.numeric(V(gclv)$name),
                        commcl_a = cdclv$membership,
                        degcl_a = degree(gclv), 
                        evcl_a = eigen_centrality(gclv)$vector, 
                        ebcl_a = betweenness(gclv))
  commcl_b = commcl_a
  ## rename
  names(commcl_b) = c("iidb","commcl_b","degcl_b","evcl_b","ebcl_b")
  ## join together frame with communities
  commsclv = left_join(commsclv,commcl_a, by = c("iida"))
  commsclv = left_join(commsclv,commcl_b, by = c("iidb"))
  ## add municode
  commsclv$municode = v
  ## identify communities differently by village
  ## i.e., comm 1 in village 1 is different than comm 1 in village 2
  commsclv$commcl_muni_a = commsclv$municode*100 + commsclv$commcl_a 
  commsclv$commcl_muni_b = commsclv$municode*100 + commsclv$commcl_b
  ## store this data in a list to be compiled later
  networkcl_list[[v]] = commsclv
  
  aclv = get.adjacency(gclv)
  ## square this matrix to get adj matrix of all distance 2 walks
  aclv2 = aclv %*% aclv
  ## take out loops of distance 2
  diag(aclv2) = 0
  ## ignore multiplicity of walks
  aclv2[aclv2!=0] = 1 
  ## can also do av3: walks of distance 3
  aclv3 = aclv2 %*% aclv
  diag(aclv3) = 0
  aclv3[aclv3!=0] = 1
  ## convert these matrix back to igraph objects
  gclv2 <- graph.adjacency(aclv2)
  gclv3 = graph.adjacency(aclv3)
  
  ## convert out to edgelist
  elcl2_v = as_edgelist(gclv2)
  elcl2_v = data.frame(elcl2_v)
  names(elcl2_v) = c("iida","iidb")
  elcl2_v$iida = as.double(as.character(elcl2_v$iida))
  elcl2_v$iidb = as.double(as.character(elcl2_v$iidb))
  
  ## edgelist of length three connections
  elcl3_v = as_edgelist(gclv3)
  elcl3_v = data.frame(elcl3_v)
  names(elcl3_v) = c("iida","iidb")
  elcl3_v$iida = as.double(as.character(elcl3_v$iida))
  elcl3_v$iidb = as.double(as.character(elcl3_v$iidb))
  
  ## check if second degree connections exist
  if(dim(elcl2_v)[1] == 0){
    ## build empty data frame here
    elcl2_v <- data.frame(iida = double(),
                        iidb = double(), 
                        municode = double(), 
                        frfamcl2 = double()) 
  } else {
    ## fill data frame here
    elcl2_v$municode = v
    elcl2_v$frfamcl2 = 1
  }
  
  ## check if third degree connections exist (same)
  if(dim(elcl3_v)[1] == 0){
    elcl3_v <- data.frame(iida = double(),
                        iidb = double(), 
                        municode = double(), 
                        frfamcl3 = double()) 
  } else {
    elcl3_v$municode = v
    elcl3_v$frfamcl3 = 1
  }
  
  
  ## store in list
  elcl2_list[[v]] = elcl2_v
  elcl3_list[[v]] = elcl3_v
  
  ## graph stats
  graph_stats_v = data.frame(n_nodes = length(V(gv)),
                             n_edges = length(E(gv)),
                             n_triangles = length(triangles(gv))/3,
                             n_comm = max(cdv$membership),
                             mod = max(cdv$modularity),
                             mod_eb = max(cdv_eb$modularity),
                             average_degree = mean(degree(gv)),
                             clustering = transitivity(gv),
                             closeness = graph_closeness(gv))

  graph_stats_v = as_tibble(graph_stats_v)
  graph_stats[[v]] = graph_stats_v

  graph_statscl_v = data.frame(n_nodes = length(V(gclv)),
                             n_edges = length(E(gclv)),
                             n_triangles = length(triangles(gclv))/3,
                             n_comm = max(cdclv$membership),
                             mod = max(cdclv$modularity),
                             average_degree = mean(degree(gclv)),
                             clustering = transitivity(gclv),
                             closeness = graph_closeness(gclv))
  
  graph_statscl_v = as_tibble(graph_statscl_v)
  graph_statscl[[v]] = graph_statscl_v
  
  degrees_list[[v]] = t(c(degree(gv)))
}

#### 2 Assemble and Save Dyadic Data ####
##### 2.1 Compile Data ##### 
## networks
comms = bind_rows(network_list)
comms = as_tibble(comms)
## distance-2 connections
el2 = bind_rows(el2_list)
el2 = as_tibble(el2)
## distance-3 connections
el3 = bind_rows(el3_list)
el3 = as_tibble(el3)

## close friends and family
commscl = bind_rows(networkcl_list)
commscl = as_tibble(commscl)
## distance-2 connections
elcl2 = bind_rows(elcl2_list)
elcl2 = as_tibble(elcl2)
## distance-3 connections
elcl3 = bind_rows(elcl3_list)
elcl3 = as_tibble(elcl3)

##### 2.2 Merge Datasets and Compute Dyadic Statistics #####
###### 2.2.1 Dyadic Data ######
comms$samecomm = ifelse(comms$comm_muni_a == comms$comm_muni_b,1,0)
comms$samecomm_eb = ifelse(comms$comm_muni_eb_a == comms$comm_muni_eb_b,1,0)
network = left_join(network,comms,by = c("iida","iidb","municode"))
network = left_join(network,el2,by = c("iida","iidb","municode"))
network$frfam2 = ifelse(is.na(network$frfam2),0,network$frfam2)
network = left_join(network,el3,by = c("iida","iidb","municode"))
network$frfam3 = ifelse(is.na(network$frfam3),0,network$frfam3)

commscl$samecommcl = ifelse(commscl$commcl_muni_a == commscl$commcl_muni_b,1,0)
network = left_join(network,commscl,by = c("iida","iidb","municode"))
network = left_join(network,elcl2,by = c("iida","iidb","municode"))
network$frfamcl2 = ifelse(is.na(network$frfamcl2),0,network$frfamcl2)
network = left_join(network,elcl3,by = c("iida","iidb","municode"))
network$frfamcl3 = ifelse(is.na(network$frfamcl3),0,network$frfamcl3)

network$deg_sum = network$deg_a + network$deg_b
network$deg_max = ifelse(network$deg_a>=network$deg_b,network$deg_a,network$deg_b)
network$ev_sum = network$ev_a + network$ev_b
network$eb_sum = network$eb_a + network$eb_b

network$deg_diff = abs(network$deg_a - network$deg_b)
network$ev_diff = abs(network$ev_a - network$ev_b)
network$eb_diff = abs(network$eb_a - network$eb_b)

##### 2.2.2 Vector Data #####
covariates = read_csv("AttanasioEtAl2011Vector.csv")
covariatesa = covariates[,c("iid","municode","renegadewin","idgroup","win2","tcons","ysch")]
names(covariatesa) = c("iida","municode","defaulta","idgroupa","wina","tconsa","yscha")
covariatesb = covariates[,c("iid","municode","renegadewin","idgroup","win2","tcons","ysch")]
names(covariatesb) = c("iidb","municode","defaultb","idgroupb","winb","tconsb","yschb")

network = left_join(network,covariatesa,by = c("iida","municode"))
network = left_join(network,covariatesb,by = c("iidb","municode"))
network$anydefault = ifelse(network$defaulta + network$defaultb > 0, 1, 0)
network$anywin = ifelse(network$wina + network$winb > 0, 1, 0)

network$consa_ppp = log(network$tconsa/lcu2ppp)
network$consb_ppp = log(network$tconsb/lcu2ppp)
network$difcons_ppp = abs(network$consa_ppp - network$consb_ppp)
network$sumcons_ppp = network$consa_ppp + network$consb_ppp

network$both_highed = ifelse(network$yscha >= 5 & network$yschb >= 5,1,0)
network$both_lowed = ifelse(network$yscha < 5 & network$yschb < 5,1,0)

network$frfam_supported = network$frfam2*network$frfam
network$frfamcl_supported = network$frfamcl2*network$frfamcl

summary(am_network <- lm(data = network,formula = frfam ~ difyage + sumyage + both_highed + both_lowed + difcons_ppp + sumcons_ppp))
summary(am_comm <- lm(data = network,formula = samecomm ~ difyage + sumyage + both_highed + both_lowed + difcons_ppp + sumcons_ppp))

#### 2.3 Save Dyadic Dataset ####
write_csv(network,path = "colombia_dyads_with_communities.csv", na = "")

#### 3 Defaults Analysis ####
##### 3.1 Within Group Dyadic Data #####
###### 3.1.2 Process Data ###### 
within_group = network %>% 
  filter(., (samegroup == 1)) %>%
  group_by(iida) %>%
  summarise(.,deg = sum(frfamcl),
            deg2 = sum(frfam2),
            deg3 = sum(frfam3),
            supported_deg = sum(frfam_supported),
            samecomm = sum(samecomm),
            n_others = n(),
            eb = mean(eb_a))

names(within_group) = c("iid","deg","deg2","deg3","supported_deg","samecomm","n_others","eb")

within_group = left_join(within_group,covariates,by = c("iid"))
within_group$exp1 = exp_payout(choices = within_group$choice1)
within_group$exp2 = exp_payout(choices = within_group$choice2)

within_group = within_group %>%
  group_by(idgroup) %>%
  mutate(exp_mean1 = mean(exp1),
         exp_mean2 = mean(exp2))

within_group$shock = within_group$win2 - within_group$exp_mean1
within_group$pos_shock = ifelse(within_group$shock > 0,1,0)
within_group$p1 = within_group$deg/within_group$n_others
within_group$p_supported = within_group$supported_deg/within_group$n_others
within_group$pcomm = within_group$samecomm/within_group$n_others

###### 3.1.2 Save Within Group Dyadic Dataset ######
## save data here

##### 3.2 Group Aggregates Data #####
###### 3.2.1 Process Data ###### 
network$iid = network$iida
network2 = left_join(network,covariates, by = c("iid","municode"))

network2$shell2 = ifelse(network2$frfam + network2$frfam2 > 0,1,0)
network2$shell3 = ifelse(network2$shell2 + network2$frfam3 > 0,1,0)

network2$shellcl2 = ifelse(network2$frfamcl + network2$frfamcl2 > 0,1,0)
network2$shellcl3 = ifelse(network2$shellcl2 + network2$frfamcl3 > 0,1,0)


by_group = network2 %>%
  filter(., samegroup == 1) %>%
  group_by(idgroup) %>%
  summarise(.,density = mean(frfam,na.rm = T),density2 = mean(shell2,na.rm = T),
            density3 = mean(shell3,na.rm = T),density_comm = mean(samecomm,na.rm = T),
            density_supported = mean(frfam_supported,na.rm = T),
            densitycl = mean(frfamcl,na.rm = T),densitycl2 = mean(shellcl2,na.rm = T),
            densitycl3 = mean(shellcl3,na.rm = T),densitycl_comm = mean(samecommcl,na.rm = T),
            densitycl_supported = mean(frfamcl_supported,na.rm = T),
            nn = mean(grpmem.x))

by_group2 = covariates %>% 
  group_by(idgroup) %>%
  summarise(.,choice = mean(choice2,na.rm = T),p_female = mean(female,na.rm = T),
            yage = mean(yage,na.rm = T),p_urb = mean(urbr_2,na.rm = T),
            ysch = mean(ysch,na.rm = T),p_married = mean(married,na.rm = T),
            lcons = mean(lcons,na.rm = T),hhsize = mean(survhhsz,na.rm = T)
            ,p_default = mean(renegadewin,na.rm = T)
            ,municode = mean(municode,na.rm = T))

by_group = inner_join(by_group,by_group2,by=c("idgroup"))
by_group$nn_x_supportedcl = by_group$nn*by_group$densitycl_supported
by_group$nn_x_densitycl = by_group$nn*by_group$densitycl
by_group$nn_x_densitycl2 = by_group$nn*by_group$densitycl2
by_group$nn_x_densitycl3 = by_group$nn*by_group$densitycl3
by_group$nn_x_densitycl_comm = by_group$nn*by_group$densitycl_comm

by_group$nn_x_supported = by_group$nn*by_group$density_supported
by_group$nn_x_density = by_group$nn*by_group$density
by_group$nn_x_density2 = by_group$nn*by_group$density2
by_group$nn_x_density3 = by_group$nn*by_group$density3
by_group$nn_x_density_comm = by_group$nn*by_group$density_comm

###### 3.2.2 Save Group Aggregates Dataset ######
write_csv(by_group,path = "defaults_by_group.csv")

#### 4 Finding a Representative Network ####
##### 4.1 Compile Graph Statistics #####
###### 4.1.1 frfam ######
graph_stats_df = bind_rows(graph_stats)
graph_stats_df$density = graph_stats_df$n_edges/(graph_stats_df$n_nodes*(graph_stats_df$n_nodes-1))
graph_stats_df$comm_size = graph_stats_df$n_nodes/graph_stats_df$n_comm
graph_stats_df$clustering = ifelse(is.nan(graph_stats_df$clustering),0,graph_stats_df$clustering)

###### 4.1.2 frfamcl ######
graph_statscl_df = bind_rows(graph_statscl)
graph_statscl_df$density = graph_statscl_df$n_edges/(graph_statscl_df$n_nodes*(graph_statscl_df$n_nodes-1))
graph_statscl_df$comm_size = graph_statscl_df$n_nodes/graph_statscl_df$n_comm
graph_statscl_df$clustering = ifelse(is.nan(graph_statscl_df$clustering),0,graph_statscl_df$clustering)

##### 4.2 Summarize Graph Statistics #####
##### ###### 4.2.1 frfam ######
stargazer(data.frame(graph_stats_df[,c(1,2,6,9,7,8,4,10,5)]),type = "latex", summary = T, omit.summary.stat = c("n","p25","p75","min","max"))

###### 4.2.2 frfamcl ######
stargazer(data.frame(graph_statscl_df[,c(1,9,7,8,10,5)]),type = "latex", summary = T, omit.summary.stat = c("n","p25","p75","min","max"))

##### 4.3 "Mean Squared Error" Anaylsis #####
## measures distance from the average graph using five different statistics
## useful in choosing network to visualize (see plot7)
graph_stats_df$mse = 
  ((graph_stats_df$n_nodes - mean(graph_stats_df$n_nodes))/sd(graph_stats_df$n_nodes))^2 +
  ((graph_stats_df$average_degree - mean(graph_stats_df$average_degree))/sd(graph_stats_df$average_degree))^2 +
  ((graph_stats_df$clustering - mean(graph_stats_df$clustering))/sd(graph_stats_df$clustering))^2 +
  ((graph_stats_df$n_comm - mean(graph_stats_df$n_comm)/sd(graph_stats_df$n_comm)))^2 +
  ((graph_stats_df$closeness - mean(graph_stats_df$closeness))/sd(graph_stats_df$closeness))^2

#### 5 Graphical analysis: strong and weak ties ####
full_model = felm(data = network,
                  formula = samegroup ~ frfam_supported + frfam + 
                    frfam2 + frfam3 + samecomm + 
                    frfam_supported:samecomm + frfam:samecomm +
                    frfam2:samecomm + frfam3:samecomm |
                    municode | 0 | municode)
## Robustness: additional covariates
## + difchoice1 + difwin1 + sumwin1 + sumurban + sumfemale + sumyage +
#  sumysch + nummarried + sumcons + sumhhsize + difurban + diffemale + 
#  difyage + difysch + difmarried + difcons + difhhsize + maxgpffopt +
#  deg_sum + deg_diff + ev_sum + ev_diff + eb_sum + eb_diff

## re-write using stata output -- output differs marginally.
## esttab 
agg_model = felm(data = network,
                  formula = samegroup ~ frfam_supported + frfam + 
                    frfam2 + frfam3 |
                    municode | 0 | municode)

full_h0 <- glht(full_model, linfct = c("frfam_supported + frfam = 0"))
full_h1 <- glht(full_model, linfct = c("frfam = 0"))
full_h2 <- glht(full_model, linfct = c("frfam2 = 0"))
full_h3 <- glht(full_model, linfct = c("frfam3 = 0"))

full_hc0 <- glht(full_model, linfct = c("samecomm + frfam_supported + frfam + frfam_supported:samecomm  + frfam:samecomm = 0"))
full_hc1 <- glht(full_model, linfct = c("samecomm + frfam + frfam:samecomm = 0"))
full_hc2 <- glht(full_model, linfct = c("samecomm + frfam2 + frfam2:samecomm = 0"))
full_hc3 <- glht(full_model, linfct = c("samecomm + frfam3 + frfam3:samecomm = 0"))

## coefs
b0 = coef(full_h0)
b1 = coef(full_h1)
b2 = coef(full_h2)
b3 = coef(full_h3)

bc0 = coef(full_hc0)
bc1 = coef(full_hc1)
bc2 = coef(full_hc2)
bc3 = coef(full_hc3)

## standard errors
se0 = sqrt(vcov(full_h0))
se1 = sqrt(vcov(full_h1))
se2 = sqrt(vcov(full_h2))
se3 = sqrt(vcov(full_h3))

sec0 = sqrt(vcov(full_hc0))
sec1 = sqrt(vcov(full_hc1))
sec2 = sqrt(vcov(full_hc2))
sec3 = sqrt(vcov(full_hc3))

variableNames = c("Distance 1 \n+ Supported","Distance 1 \n+ Unsupported","Distance 2","Distance 3")
betas =c(b0,b1,b2,b3,bc0,bc1,bc2,bc3)
sigmas = c(se0,se1,se2,se3,sec0,sec1,sec2,sec3)
results_df = data.frame(estimate = betas,std.error= sigmas,
                        model = c(rep("Between Communities",4),
                                  rep("Within Communities",4)), 
                        term = rep(variableNames,2))

nudge = -0.1
nudgec = 0.1

g3 = dwplot(results_df,model_order = c("Within Communities","Between Communities"),
            dot_args = list(aes(shape = model,colour = model))) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none") +
        #legend.position = c(0.05, 0.95),
        #legend.justification = c(0, 1), 
        #legend.background = element_rect(colour="grey80"),
        #legend.title = element_blank()) + 
  xlab("Excess Prob. of Co-Membership \nin Risk Pooling Group") +
  ylab("Friends and Family Network") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  geom_segment(aes(x = b3, xend = b2,y = 1+nudge,yend = 2+nudge), linetype = 3, colour = "#F8766D")+
  geom_segment(aes(x = b2, xend = b1,y = 2+nudge,yend = 3+nudge), linetype = 3, colour = "#F8766D") +
  geom_segment(aes(x = b1, xend = b0,y = 3+nudge,yend = 4+nudge), linetype = 3, colour = "#F8766D") +
geom_segment(aes(x = bc3, xend = bc2,y = 1+nudgec,yend = 2+nudgec), linetype = 3, colour = "#00BFC4")+
  geom_segment(aes(x = bc2, xend = bc1,y = 2+nudgec,yend = 3+nudgec), linetype = 3, colour = "#00BFC4") +
  geom_segment(aes(x = bc1, xend = bc0,y = 3+nudgec,yend = 4+nudgec), linetype = 3, colour = "#00BFC4") +
  coord_flip() +
  annotate("rect", ymin = 0.5, ymax = 2.1, xmin = 0.22, xmax = 0.29,
           alpha = 1,fill = "white", colour= "grey80")+
  annotate("text", x = 0.27, y = 0.8, hjust = 0,vjust =0.5, label = "Between Communities") +
  annotate("text", x = 0.24, y = 0.8, hjust = 0,vjust =0.5, label = "Within Communities") +
  annotate("point", x = 0.27, y = 0.7, shape = 19, colour = "#F8766D") +
  annotate("point", x = 0.24, y = 0.7, shape = 17,colour = "#00BFC4")
g3


ggsave("strongweak.pdf",width = 6, height = 4, dpi = 500, units = "in", device='pdf')
ggsave("strongweak.tiff",width = 6, height = 4, dpi = 500, units = "in", device='tiff')
ggsave("strongweak.eps",width = 6,height = 4,dpi = 500,units = 'in',device = "eps")

#### Robustness for ATtentuation Bias

mean(network$samegroup[network$frfam + network$frfam2 + network$frfam3 > 0])
mean(network$samegroup[network$frfam + network$frfam2 + network$frfam3 == 0])

mean(network$samegroup[network$frfam + network$frfam2 + network$frfam3 + network$samecomm > 0])
mean(network$samegroup[network$frfam + network$frfam2 + network$frfam3 + network$samecomm == 0])

mean(network$samegroup[network$frfam == 1])
mean(network$samegroup[network$frfam2 == 1])
mean(network$samegroup[network$frfam3 == 1])
mean(network$samegroup[network$samecomm == 1])


summary(pref_model_s <- felm(data = network,formula = 
                   frfam_supported ~ frfam + frfam2 + frfam3 + samecomm |
                   municode | 0 | municode))

summary(pref_model_a <- felm(data = network,formula = 
                               frfam  ~ frfam_supported + frfam2 + frfam3 + samecomm |
                               municode | 0 | municode))


summary(felm(data = network,formula = 
      samegroup ~ frfam_supported + frfam + frfam2 + frfam3 + samecomm |
       municode | 0 | municode))

setwd(paste0("../",results_ext))
degrees = as_tibble(bind_cols(degrees_list))
gdf = degrees %>% pivot_longer(cols = everything(),names_to = "id",values_to = "degree")
gg = gdf %>%
  ggplot(aes(x = degree)) + geom_histogram(bins = 34) + 
  theme_minimal()
gg
ggsave(filename = "degree_distribuion_colombia.pdf",width = 6,height = 4,device = "pdf",units = "in",dpi = 600)
