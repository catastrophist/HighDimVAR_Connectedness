
library(lubridate)
library(BigVAR)
library(igraph)
library(ggplot2)
library(dplyr)
#install.packages("wesanderson")
library(wesanderson)
library(huge)
library(igraph) 
#remotes::install_github("rpkgs/Ipaper")
library(Ipaper)
#from github too
library(ForceAtlas2)
dat<- readxl::read_xlsx("use50vol1213.xlsx")
dat$times <- ymd(dat$time)

stklist <- c('601169', '601818', '601166', '600999', '601377', '601088','600000', '600029', '601788', '601390', '601998', '600048','600010', '600050', '601328', '600030', '600887', '601857','601985', '601601', '601989', '601186', '601988', '601766','601800', '600028', '600036', '600016', '600519', '601318','601006', '600893', '601668', '601288', '600109', '601398','601669', '601727', '601688', '601211', '600637', '600111','601628', '600104', '601919', '600518', '600795', '600958','600837', '601336')




df <- dat %>%arrange(times)
ssl <- list()
for (i in 1:50){
  
  ssl <- append(ssl,subset(as.data.frame(df),df$stock==stklist[i]))
  
  
  
}

ssl2 <- list()
for (i in 1:50){
  
  ssl2 <- append(ssl2,subset(as.data.frame(df$vol),df$stock==stklist[i]))
  
  
  
}
names(ssl2) <- stklist
let <- rep(0,50)
s <- 0
for (i in 1:50){
  
  if(lengths(ssl2[i])==486){
    
    s=s+1
  }
}

dfssl<- data.frame(matrix(0,486,s))



n=0
for (i in 1:50){
  
  if(lengths(ssl2[i])==486){
    
    n=n+1 
    dfssl[n] <- ssl2[i]
    
  }
  
  
  
  
}

as.data.frame(dfssl[3])

p <- 10
#model1 <- constructModel(as.matrix(dfssl),p,struct = 'Basic',gran=c(0.2),ownlambdas = TRUE,window.size=120)
#result1 <- BigVAR.est(model1)
#beta <- result1[["B"]]
#beta[,,1]


g12 <- ggplot2::ggplot(dfssl,mapping=aes(X1,X2))+geom_point()+xlab("stockX1")+ylab('stockX2')
g23 <- ggplot2::ggplot(dfssl,mapping=aes(X2,X3))+geom_point()+xlab("stockX2")+ylab('stockX3')
g34 <- ggplot2::ggplot(dfssl,mapping=aes(X3,X4))+geom_point()+xlab("stockX3")+ylab('stockX4')
g45 <- ggplot2::ggplot(dfssl,mapping=aes(X4,X5))+geom_point()+xlab("stockX4")+ylab('stockX5')
g56 <- ggplot2::ggplot(dfssl,mapping=aes(X5,X6))+geom_point()+xlab("stockX5")+ylab('stockX6')
g67 <- ggplot2::ggplot(dfssl,mapping=aes(X6,X7))+geom_point()+xlab("stockX6")+ylab('stockX7')
g78 <- ggplot2::ggplot(dfssl,mapping=aes(X7,X8))+geom_point()+xlab("stockX7")+ylab('stockX8')
g89 <- ggplot2::ggplot(dfssl,mapping=aes(X8,X9))+geom_point()+xlab("stockX8")+ylab('stockX9')

Ipaper::write_fig(g12,file = "graphs\\Rplot_relat_x1x2.pdf")
Ipaper::write_fig(g23,file = "graphs\\Rplot_relat_x2x3.pdf")
Ipaper::write_fig(g34,file = "graphs\\Rplot_relat_x3x4.pdf")
Ipaper::write_fig(g45,file = "graphs\\Rplot_relat_x4x5.pdf")
Ipaper::write_fig(g56,file = "graphs\\Rplot_relat_x5x6.pdf")
Ipaper::write_fig(g67,file = "graphs\\Rplot_relat_x6x7.pdf")
Ipaper::write_fig(g78,file = "graphs\\Rplot_relat_x7x8.pdf")
Ipaper::write_fig(g89,file = "graphs\\Rplot_relat_x8x9.pdf")
 
qplot(c(1:486),dfssl$X1)+geom_line()+xlab("observations")+ylab('stockX1')
qplot(c(1:486),dfssl$X2)+geom_line()+xlab("observations")+ylab('stockX2')
qplot(c(1:486),dfssl$X3)+geom_line()+xlab("observations")+ylab('stockX3')
qplot(c(1:486),dfssl$X4)+geom_line()+xlab("observations")+ylab('stockX4')
qplot(c(1:486),dfssl$X5)+geom_line()+xlab("observations")+ylab('stockX5')
qplot(c(1:486),dfssl$X6)+geom_line()+xlab("observations")+ylab('stockX6')



#CROSS VALIDATION
model2 <- constructModel(as.matrix(dfssl),p,struct = 'Basic',gran=c(50,10),verbose=FALSE)
result2 <- cv.BigVAR(model2)

save(result2,file = "result2.Rdata")







##########
#read result2
load('result2.Rdata')
#######
#plot MSFE
BigVAR::plot(result2@InSampMSFE,y=NULL)
cvmsfe <- result2@InSampMSFE
#gen dataframe to plot
cvmsfe.dat<- as.data.frame(cvmsfe)

#ggplot(data = cvmsfe.dat)+geom_point(aes(x=c(1:163),y=V1,fill='Lag1',color='#000000'))+geom_point(aes(x=c(1:163),y=V2))+
#  geom_point(aes(x=c(1:163),y=V3))+geom_point(aes(x=c(1:163),y=V4))+geom_point(aes(x=c(1:163),y=V5))+
#  geom_point(aes(x=c(1:163),y=V6))+geom_point(aes(x=c(1:163),y=V7))+geom_point(aes(x=c(1:163),y=V8))+
#  geom_point(aes(x=c(1:163),y=V9))+geom_point(aes(x=c(1:163),y=V10))

bp<- ggplot(data = cvmsfe.dat)+geom_point(aes(x=c(-1:-163),y=V1,colour='Lag1'))+
  geom_point(aes(x=c(-1:-163),y=V2,colour='Lag2'))+
  geom_point(aes(x=c(-1:-163),y=V3,colour='Lag3'))+
  geom_point(aes(x=c(-1:-163),y=V4,colour='Lag4'))+
  geom_point(aes(x=c(-1:-163),y=V5,colour='Lag5'))+
  geom_point(aes(x=c(-1:-163),y=V6,colour='Lag6'))+
  geom_point(aes(x=c(-1:-163),y=V7,colour='Lag7'))+
  geom_point(aes(x=c(-1:-163),y=V8,colour='Lag8'))+
  geom_point(aes(x=c(-1:-163),y=V9,colour='Lag9'))+
  geom_point(aes(x=c(-1:-163),y=V10,colour='Lag10'))+ 
  ggtitle('Training loss in validation')+
  ylab('MSFE')+xlab('period')+scale_color_discrete(breaks=c('Lag1','Lag2','Lag3','Lag4',
  'Lag5','Lag6','Lag7','Lag8','Lag9','Lag10'))

Ipaper::write_fig(bp,file = "graphs\\msfe.pdf")


##########plot end #################
# delete the last const value of perdict vector
betacv<- result2@betaPred
vecbeta <- betacv[,1:460]


T <- s

pai <- array(0,dim=c(T,T,p))
for (k in 1:p){
  
  pai[,,k] = vecbeta[,((k-1)*T+1):(T*k)]
  
}   

####write a function cal.Ai to calculate Ai##
# L gupiaogeshu 46
cal.Ai<- function(PAI, L, p, i){
  
  if (i==0){
    return(diag(x=1,L))#return 是一个函数
  }
  else if(i<0){
    return(matrix(0,L,L))
  }
  else{
    sum = matrix(0,L,L)
    for (j in 1:p){
      sum <- sum + pai[,,j]%*%cal.Ai(PAI, L, p, i - j)
    }
    return(sum)
  }
  
}

## get the Ai array

Ai <- array(0,dim=c(T,T,p))


#get e from results@resids

res<- result2@resids
plot(res[1:476,21])
x <- c(1:476)
y <- res[1:476,21]
resplot<- ggplot(data= NULL,aes(x = x, y = y))+geom_point(color = "darkred")+xlab("observations")+ylab("residuals")+ggtitle("VAR results")
Ipaper::write_fig(resplot,file = "graphs\\resplot.pdf")


# take zero point as centre
#plot(resamp1.use)
covres <- cov(res,res)

cal.theta<- function(COV,i,j,H,PAI, p){
  #cov:covariance matrix of error epslion
  #i,j identify the location
  #H the H-step-ahead forecast error
  #pai the calculated beta matrix
  #p lag in VAR
  sig1 <- 0
  sig2 <- 0
  
  #get ei    
  ei<- cbind(rep(0,nrow(COV)))
  ei[i] <- 1
  
  #get ej
  ej<- cbind(rep(0,nrow(COV)))
  ej[j] <- 1
  
  
  for (h in 1:H-1){
    Aij<- cal.Ai(PAI, nrow(COV), p, h)
    sig1 <- sig1 + (COV[j,j]^-1)*(  t(ei)%*%Aij%*%COV%*%ej)^2
    sig2 <- sig2 + (  t(ei)%*%Aij%*%COV%*%t(Aij)%*%ei)    
  }
  
  return(sig1/sig2)
  
  
}

thetaH.10 <- matrix(0,T,T)
for (i in 1:T){
  
  for(j in 1:T){
    
    thetaH.10[i,j] <- cal.theta(covres,i,j,10,pai,p)
    
    
  }
  
}

std.thetaH.10.1 <- matrix(0,T,T)
for (i in 1:T){
  
  for (j in 1:T){
    if(i!=j){
      std.thetaH.10.1[i,j] <- thetaH.10[i,j]
    }
  }
  
}


truetheta<- load('truethetaH.10.RData')


std.thetaH.10.1 <- matrix(0,T,T)
for (i in 1:T){
  
  for (j in 1:T){
    if(i!=j){
      std.thetaH.10.1[i,j] <- thetaH.10[i,j]
    }
  }
  
}
std.thetaH.10 <- matrix(0,T,T)
jsum <- rowSums(std.thetaH.10.1)
for (i in 1:T){
  
  for (j in 1:T){
    if(i!=j){
      std.thetaH.10[i,j] <- std.thetaH.10.1[i,j]/  jsum[i]
    }
  }
  
}
totalconnectedness.from <- rep(0,T)
for(i in 1:T){
  for (j in 1:T){
    if(i!=j){
      totalconnectedness.from[i] <- (1/T)*std.thetaH.10[i,j]+totalconnectedness.from[i]
    }
  }
}
#calculate element connectedness
totalconnectedness.to <- rep(0,T)
for(j in 1:T){
  for (i in 1:T){
    if(i!=j){
      totalconnectedness.to[j] <- (1/T)*std.thetaH.10[i,j]+totalconnectedness.to[j]
    }
  }
}

#cal Cij net connectedness____ pairwise directional connectedness
net.connectednessij <- matrix(0,T,T)
for(i in 1:T){
  for (j in 1:T){
    if(i!=j){
      net.connectednessij[i,j]  <- std.thetaH.10[j,i]-std.thetaH.10[i,j]
    }
  }
}

##########graph paragraph

########################

ig4 <- graph.adjacency(std.thetaH.10,mode="undirected", weighted=TRUE)
layout4 <- layout.forceatlas2(ig4,directed=FALSE,iterations = 110, plotstep = 100)
igraph::plot.igraph(ig4,layout=layout4)
E(ig4)$width <- E(ig4)$weight*30 + min(E(ig4)$weight)*1+ 1 # offset=1
#igraph::plot.igraph(ig4, layout=layout_on_grid(ig4, dim=2))
igraph::plot.igraph(ig4,layout=layout4)




shrink1.std.thetaH.10ij <- matrix(0,T,T)
for(i in 1:T){
  for (j in 1:T){
    if(std.thetaH.10[i,j]>0.06){
      shrink1.std.thetaH.10ij[i,j]  <- std.thetaH.10[i,j]
    }
  }
}


ig5 <- graph.adjacency(shrink1.std.thetaH.10ij,mode="directed", weighted=TRUE)
layout5 <- layout.forceatlas2(ig5,directed=TRUE,iterations = 110, plotstep = 100)
igraph::plot.igraph(ig5,layout=layout5)
E(ig5)$width <- E(ig5)$weight*30 + min(E(ig5)$weight)*1+ 1 # offset=1
#igraph::plot.igraph(ig4, layout=layout_on_grid(ig4, dim=2))
igraph::plot.igraph(ig5,layout=layout5)
dev.off()
pdf("graphs\\ig5without_largeness.pdf")
igraph::plot.igraph(ig5,layout=layout5)
dev.off()





#wrong and waited to be corrected
member<-spinglass.community(ig5,weights=E(ig5)$weight,spins=2)  

#correction of above
##########
weights=E(ig5)$weight
giant       = clusters.giant(weights) ## using the biggest component as an example, you can use the others here.
communities = giant.community_spinglass()





##########

plot(ig5,edge.arrow.size=0.1 ,vertex.label.dist=0,layout=layout5,vertex.color=rainbow(7,alpha=0.3),edge.arrow.mode = "-") 

pdf("graphs\\ig5with_largeness.pdf")
igraph::plot.igraph(ig5,layout=layout5,edge.arrow.size=0.1 ,vertex.label.dist=0,layout=layout5,vertex.color=rainbow(7,alpha=0.3),edge.arrow.mode = "-")
dev.off()




######
plot.membership<-function(graph,membership,main=""){  
  V(graph)$member<-membership  
  mem.col<-rainbow(length(unique(membership)),alpha=0.3)  
  V(graph)$color<-mem.col[membership]  
  plot(graph,edge.width=E(graph)$weight,vertex.color=V(graph)$color,main=main)  
}  
plot.membership(ig5,clusters(ig5)$membership,"Neighborhood finding of Stocks",layout=layout5)  
#######

#设置点大小
V(ig5)$size = 15  
V(ig5)[degree(ig5)>=1]$size = 15  

#设置不同社群颜色
mem.col<-rainbow(length(unique(V(ig5)$member)),alpha = 0.3)  
V(ig5)$color<-mem.col[V(ig5)$member]  


svg(filename=paste("C:/Users/long/Desktop","/1.svg",sep = ""),width = 40,height = 40)  
plot(ig5,layout=layout5,vertex.color=V(ig5)$color,vertex.label=V(ig5)$label,vertex.size=V(ig5)$size,edge.arrow.mode = "-")  
dev.off() 


wc <- spinglass.community(ig5,weights =E(ig5)$weight)
new_cols <- c("white", "red", "black")[membership(wc)]
plot(wc, col=new_cols,mark.border="black",ig5, layout=layout5, vertex.label=toutou,vertex.size=5,  edge.arrow.size=.2,edge.arrow.mode = "-")
svg(filename='sample1.svg',width = 40,height = 40) 




colnames(dfssl) <- toutou

stargazer::stargazer(dfssl,label=toutou,summary=TRUE,title='Basic informations of SSE50 stocks \'svolatility ')

stargazer::stargazer(std.thetaH.10)

colnames(std.thetaH.10) <- toutou
row.names(std.thetaH.10) <- toutou
stargazer::stargazer(std.thetaH.10,title='Connectedness of SSE50 stocks \'s volatility ')



#################################
########################
########################

ig4 <- graph.adjacency(std.thetaH.10,mode="undirected", weighted=TRUE)
layout4 <- layout.forceatlas2(ig4,directed=FALSE,iterations = 110, plotstep = 100)
igraph::plot.igraph(ig4,layout=layout4)
E(ig4)$width <- E(ig4)$weight*30 + min(E(ig4)$weight)*1+ 1 # offset=1
#igraph::plot.igraph(ig4, layout=layout_on_grid(ig4, dim=2))
igraph::plot.igraph(ig4,layout=layout4)
member<-spinglass.community(ig4,weights=E(ig4)$weight,spins=10)  

layout4 <- layout.forceatlas2(ig4,directed=TRUE,iterations = 110, plotstep = 100)
igraph::plot.igraph(ig5,layout=layout4)
dev.off()
E(ig5)$width <- E(ig5)$weight*30 + min(E(ig5)$weight)*1+ 1 # offset=1
#igraph::plot.igraph(ig4, layout=layout_on_grid(ig4, dim=2))
igraph::plot.igraph(ig4,layout=layout5)
dev.off()



plot(ig4,edge.arrow.size=0.1 ,vertex.label.dist=0,layout=layout5,vertex.color=rainbow(7,alpha=0.3),edge.arrow.mode = "-") 





######
plot.membership<-function(graph,membership,main=""){  
  V(graph)$member<-membership  
  mem.col<-rainbow(length(unique(membership)),alpha=0.3)  
  V(graph)$color<-mem.col[membership]  
  plot(graph,edge.width=E(graph)$weight,vertex.color=V(graph)$color,main=main)  
}  
#neighborhood
pdf("graphs\\ig4with_neighborhoods.pdf")
plot.membership(ig4,clusters(ig4)$membership,"Neighborhood finding of Stocks")  
dev.off()
#设置点大小
V(ig4)$size = 15  
V(ig4)[degree(ig5)>=1]$size = 15  

#设置不同社群颜色
mem.col<-rainbow(length(unique(V(ig5)$member)),alpha = 0.3)  
V(ig4)$color<-mem.col[V(ig4)$member]  

dev.off()
pdf("graphs\\ig4withlayout.pdf") 
plot(ig4,layout=layout5,vertex.color=V(ig4)$color,vertex.label=V(ig4)$label,vertex.size=V(ig4)$size,edge.arrow.mode = "-")  
dev.off() 
