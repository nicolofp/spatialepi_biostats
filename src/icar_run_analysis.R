library(cmdstanr)   
library(INLA)

source("src/icar_data_preparation.R")

# Build adjagency matrix
district_nb = poly2nb(distretti$geometry,snap=0.0002)
district_mat = nb2mat(district_nb)
W = rowSums(district_mat != 0) * district_mat 
#A = A[upper.tri(A)]
A = data.table(melt(W))
setkey(A,"Var1","Var2")
A = A[value != 0 & Var2 > Var1]

y = DF$ratio_lungs;
#x = as.matrix(DF[,.(Antimonio,Arsenico,`Cromo VI`,Manganese,Piombo)])
x = as.matrix(DF[,16:17,with=F])
E = DF$h_all * 0.1; #--> does it really need?
K = NCOL(x)

N = NROW(distretti)
node1 = A$Var1
node2 = A$Var2  
N_edges = NROW(A)

# sapply(district_nb,function(i) length(i)) --> count connection per each 
#Build the adjacency matrix using INLA library functions
adj.matrix = sparseMatrix(i=node1,j=node2,x=1,symmetric=TRUE)
#The ICAR precision matrix (note! This is singular)
Q=  Diagonal(N, rowSums(adj.matrix)) - adj.matrix
#Add a small jitter to the diagonal for numerical stability (optional but recommended)
Q_pert = Q + Diagonal(N) * max(diag(Q)) * sqrt(.Machine$double.eps)

# Compute the diagonal elements of the covariance matrix subject to the 
# constraint that the entries of the ICAR sum to zero.
#See the inla.qinv function help for further details.
Q_inv = inla.qinv(Q_pert, constr=list(A = matrix(1,1,N),e=0))

#Compute the geometric mean of the variances, which are on the diagonal of Q.inv
scaling_factor = exp(mean(log(diag(Q_inv))))  
  
data = list(N=N,
            N_edges=N_edges,
            node1=node1,
            node2=node2,
            y=y,
            x=x,
            E=E,
            K = K,
            scaling_factor=scaling_factor);

bym2_model = cmdstan_model("stan/bym/bym2_noE.stan");

bym2_lomb_stanfit = bym2_model$sample(data = data,
                                      parallel_chains = 4,
                                      max_treedepth = 15,
                                      refresh = 0)

bym2_lomb_stanfit$summary(variables = c("beta0","betas"))

# CAR version not IAR
sp_d = list(n = nrow(x),                    # number of observations
            p = ncol(x),                    # number of coefficients
            X = x,                          # design matrix
            y = y,                          # observed number of cases
            log_offset = log(0.1*y + 0.1),  # log(expected) num. cases
            W_n = sum(W) / 2,               # number of neighbor pairs
            W = W)                          # adjacency matrix

sp_model = cmdstan_model('stan/bym/sparse_car.stan')
sp_fit = sp_model$sample(data = sp_d,
                         parallel_chains = 4,
                         #refresh = 0,
                         refresh = 100)

sp_fit$summary(variables = c("beta"))

  