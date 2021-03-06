Path <- setwd("D:/Program Files/RStudio/projects/JST/documents")
K = 3                                                     # number of topics
S = 2                                                     # number of sentiments
alpha = 0.005
beta = 0.005
gumma = 0.005
iteration = 2000                                        
burn = 250
#################################################
MAP_2 <- function(arg){
  MAP <- array(1:(K*S), dim=c(S,K))
  for(s in 1:S){
    for(k in 1:K){
      if(MAP[s,k]==arg){
        newtopic <- k
        newSentiment <- s
      }
    }
  }
  return(c(newSentiment,newtopic))
}
MAP_1 <- function(s,k){     
  temp<- (k*S)-(S-s)
  return(temp)
}
#################################################         # creating (Sentiment_topic) Matrix
na <-c()
Sentiment_topic <- matrix(0,nrow = W, ncol = (S*K))
for(k in 1:(S*K)){
  na <-append(na,paste0("(Sentiment/Topic)-",k),after = length(na))
}
colnames(Sentiment_topic)<- na
#################################################         # finding Dictionary words
R <- which(duplicated(gibbs[,1]) | duplicated(gibbs[,1], fromLast = TRUE))  # repeat words
U <- setdiff(R,which(duplicated(gibbs[,1])))                                # base words that have repeat
R_N <- setdiff(which(gibbs[,1]==gibbs[,1]),R)             # words that have not repeat
V <- gibbs[union(U,R_N),1] 
#################################################         # row names for Sentiment_topic matrix
rownames(Sentiment_topic)<- gibbs[1:W]
#################################################         # initialize of Z,L
gibbs[,3] <- sample(c(1:K),W,replace = TRUE)              
gibbs[,4] <- sample(c(1:S),W,replace = TRUE) 
##################################################        # Estimated matrix
THETA <- matrix(0,nrow = M, ncol = S*K)
PHI <- matrix(0,nrow = S*K, ncol = length(V))
colnames(PHI)<- V
PI <- matrix(0,nrow = M, ncol = S)
#################################################         # Theta matrix
Theta <- array(0, dim=c(M,S,K))
##################################################        # Phi matrix
Phi <- array(0, dim=c(S,K,length(V)))
dimnames(Phi)[[3]] <- V
##################################################        # Pi matrix
Pi <- matrix(0,nrow = M, ncol = S)
##################################################        # initialize Phi, Theta, pi, and N_R matrix
n_S_K <- matrix(0,nrow = S, ncol = K)
for(x in 1:W){
  Theta[as.integer(gibbs[x,2]),as.integer(gibbs[x,4]),as.integer(gibbs[x,3])]<-Theta[as.integer(gibbs[x,2]),as.integer(gibbs[x,4]),as.integer(gibbs[x,3])]+1
  Phi[as.integer(gibbs[x,4]),as.integer(gibbs[x,3]),gibbs[x,1]] <- Phi[as.integer(gibbs[x,4]),as.integer(gibbs[x,3]),gibbs[x,1]] + 1
  Pi[as.integer(gibbs[x,2]),as.integer(gibbs[x,4])] <- Pi[as.integer(gibbs[x,2]),as.integer(gibbs[x,4])] + 1
  n_S_K[as.integer(gibbs[x,4]),as.integer(gibbs[x,3])] <- n_S_K[as.integer(gibbs[x,4]),as.integer(gibbs[x,3])] + 1
  #cat("\r N_M: %",(x/W)*100)
}
Theta <- matrix(Theta, M,S*K)                           # mapping arrays to 2-D matrix
Phi <- matrix(Phi, S*K, length(V))
colnames(Phi)<- V
n_S_K <- as.vector(n_S_K)
##################################################       ## iterations ##
##################################################
n_s_k_w <- c()
n_d_s_k <- c()
n_d_s <- c()
n_s_k <- c()
n_d <- c()
#until <- 0
for(i in 1:iteration){
  #startTime <- Sys.time()
  for(z in 1:W){  
    #startTime <- Sys.time()                                            # loading new data
        n_s_k_w <- Phi[,gibbs[z,1]]
        n_s_k_w[MAP_1(as.integer(gibbs[z,4]),as.integer(gibbs[z,3]))] <- n_s_k_w[MAP_1(as.integer(gibbs[z,4]),as.integer(gibbs[z,3]))] - 1
        n_d_s_k <- Theta[as.integer(gibbs[z,2]),]
        n_d_s_k[MAP_1(as.integer(gibbs[z,4]),as.integer(gibbs[z,3]))] <- n_d_s_k[MAP_1(as.integer(gibbs[z,4]),as.integer(gibbs[z,3]))] - 1
        n_d_s <- Pi[as.integer(gibbs[z,2]),]
        n_d_s[as.integer(gibbs[z,4])] <- n_d_s[as.integer(gibbs[z,4])] - 1
        n_s_k <- n_S_K
        n_s_k[MAP_1(as.integer(gibbs[z,4]),as.integer(gibbs[z,3]))] <- n_s_k[MAP_1(as.integer(gibbs[z,4]),as.integer(gibbs[z,3]))] - 1
        n_d <- docs_len[as.integer(gibbs[z,2])]
            
        n_s_k_w <- n_s_k_w + beta
        n_d_s_k <- n_d_s_k + alpha
        n_d_s_g <- n_d_s + gumma
        n_s_k <- n_s_k + (length(V)*beta)
        n_d_s <- n_d_s + (K*alpha)
        n_d <- n_d + (S*gumma)
            
        p_Z <- (n_s_k_w/n_s_k) * (n_d_s_k/n_d_s) * (n_d_s_g/n_d)
        prob <- sample(c(1:(K*S)),1,prob = p_Z)                   # sampling (Draw)
        #prob <-which(p_Z==max(p_Z))
        if(gibbs[z,4]!=MAP_2(prob)[1] || gibbs[z,3]!=MAP_2(prob)[2]){                                 # updating Theta / Phi / Pi / n_s_k 
          Theta[as.integer(gibbs[z,2]),prob]<-Theta[as.integer(gibbs[z,2]),prob]+1
          Theta[as.integer(gibbs[z,2]),MAP_1(as.integer(gibbs[z,4]),as.integer(gibbs[z,3]))]<-Theta[as.integer(gibbs[z,2]),MAP_1(as.integer(gibbs[z,4]),as.integer(gibbs[z,3]))]-1
          Pi[as.integer(gibbs[z,2]),MAP_2(prob)[1]] <- Pi[as.integer(gibbs[z,2]),MAP_2(prob)[1]] +1
          Pi[as.integer(gibbs[z,2]),as.integer(gibbs[z,4])] <- Pi[as.integer(gibbs[z,2]),as.integer(gibbs[z,4])] -1
          n_S_K[prob] <- n_S_K[prob] +1
          n_S_K[MAP_1(as.integer(gibbs[z,4]),as.integer(gibbs[z,3]))] <- n_S_K[MAP_1(as.integer(gibbs[z,4]),as.integer(gibbs[z,3]))] -1
          Phi[prob,gibbs[z,1]]<-Phi[prob,gibbs[z,1]]+1
          Phi[MAP_1(as.integer(gibbs[z,4]),as.integer(gibbs[z,3])),gibbs[z,1]]<-Phi[MAP_1(as.integer(gibbs[z,4]),as.integer(gibbs[z,3])),gibbs[z,1]]-1
          gibbs[z,3] <- MAP_2(prob)[2]                                     # update z 
          gibbs[z,4] <- MAP_2(prob)[1] 
        }
        if(i > burn){                                        # update Sentiment_topic matrix
          v_4 <- vector("numeric", (S*K))
          v_4[prob]<- 1
          Sentiment_topic[z,] <-Sentiment_topic[z,] + v_4
        }
      }
  #cat("\r learning: ",i)
  if(i > burn){
    #################################################           # estimating PHI matrix (parameter estimation)
    for(e in 1:(S*K)){
      for(r in 1:length(V)){
        PHI[e,r]<- PHI[e,r] + ((Phi[e,r]+beta)/(n_s_k[e]+(length(V)*beta)))
      }
    }
  }
  cat("\r learning: %",(i/iteration)*100)
}
##################################################         # get Sentiment/Topic Label 
for(r in 1:W){                                           
  count <- which(Sentiment_topic[r,]==max(Sentiment_topic[r,]))
  if(length(count)>1){
    temp <- sample(as.integer(count),1,prob = rep((1/length(count)),length(count)))
    gibbs[r,4] <- MAP_2(temp)[1]
    gibbs[r,3] <- MAP_2(temp)[2]
  }else{
    gibbs[r,6] <- MAP_2(count)[1]
    gibbs[r,5] <- MAP_2(count)[2]
  }
}
##################################################          # Sentiment / Topic percentage of each document
for(p in 1:M){
  SS <- c()
  KK <- c()
  x<-subset(gibbs, gibbs[,2]==p)                            # subset of gibbs matrix
  for(s in 1:(S)){
    SS[s]<-length(which(x[,4] == paste0(s)))
  }
  for(k in 1:(K)){
    KK[k]<-length(which(x[,5] == paste0(k)))
  }
  SS <- (SS/sum(SS))*100
  KK <- (KK/sum(KK))*100
  cat("Document-",p, ":", SS[1],"% (Sentiment-1)\n")
  cat("Document-",p, ":", SS[2],"% (Sentiment-2)\n")
  cat("Document-",p, ":", KK[1],"% (Topic-1)\n")
  cat("Document-",p, ":", KK[2],"% (Topic-2)\n")
  cat("Document-",p, ":", KK[3],"% (Topic-3)\n \n")
}
#################################################           # estimating PHI matrix (parameter estimation)
for(i in 1:(S*K)){
  for(r in 1:length(V)){
    PHI[i,r]<-PHI[i,r]/(iteration-burn)
  }
}
#################################################           # getting (Top words) 
TopPHI <- matrix(nrow = length(V), ncol = (S*K))
colnames(TopPHI)<- na
PHI <- t(PHI)
for(i in 1:(S*K)) {
  PHI <- PHI[order(PHI[,i],decreasing = TRUE),]
  TopPHI[,i] <- row.names(PHI)
}

write.csv(TopPHI,file="top10-PHI.csv",row.names = FALSE)

###############################
#Notes:
#  columns odd are sentiment 1
#  columns even are sentiment 2
###############################






