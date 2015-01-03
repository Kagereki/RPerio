defcdcaap<-function(x,y){
 #Set all the missing recession to zero
  x[is.na(x)] <- 0
  #Then only select the interproximal sites in the recession data
  x <- x[ which(x$site=='m' | x$site=='M' | x$site=='mesial' |x$site=='MESIAL'| x$site=='Mesial' | x$site=='d' | x$site=='D' | x$site=='distal' |x$site=='DISTAL'| x$site=='Distal'), ]
  x$locus <- paste(x$tooth, x$site,x$side, sep = "_")
  x$tooth<-as.factor(x$tooth)
  x$locus<-as.factor(x$locus)
  x1 <- x[order(x$locus),]
  
  # Set all the missing PD to three (The logic behind this is well thought by Kagee) 
  y[is.na(y)] <- 3
  #Just as the recession data we select only the proximal sites and drop the mid
  y <- y[ which(y$site=='m' | y$site=='M' | y$site=='mesial' |y$site=='MESIAL'| y$site=='Mesial' | y$site=='d' | y$site=='D' | y$site=='distal' |y$site=='DISTAL'| y$site=='Distal'), ]
  y$tooth<-as.factor(y$tooth)
  y$locus <- paste(y$tooth, y$site,y$side, sep = "_")
  y$locus<-as.factor(y$locus)
  y1 <- y[order(y$locus),]
  cols<-y1[sapply(y1,is.numeric)]
  
# Then we calculate the CAL due to the PD
#This is a dataframe called recession
  names<-y1[sapply(y1,is.factor)]
  calpd<-cols-3
  calpd[calpd <0] <- 0
  y2<-cbind(names,calpd)
  
  # Then we look at the pd logical component: For the severe we do not have to have them on different teeth
  colst<-t(cols)
  rownames(colst) <- NULL
  pdsev<-rowSums(colst >= 5)
  pd.1<-as.data.frame(pdsev)
  
  # for the moderate we have to separate the sites:
  pdcol<-by(y1, y1$tooth, tail, n=1)
  pdcolhigh<-do.call("rbind", as.list(pdcol))
  pddata<-pdcolhigh[sapply(pdcolhigh,is.numeric)]
  rownames(pddata) <- NULL
  pddata<-t(pddata)
  pdmod<-rowSums(pddata >= 5)
  pd.2<-as.data.frame(pdmod)
  
  calrecession<-x1[sapply(x1,is.numeric)]
  rownames(calrecession) <- NULL
  y.1<-y2[sapply(y2,is.numeric)]
  rownames(y.1) <- NULL
  x.2<-  names<-x1[sapply(x1,is.factor)]
  rownames(x.2) <- NULL
  y.2<-  names<-y2[sapply(y2,is.factor)]
  rownames(y.2) <- NULL
  cal<-calpd+calrecession
  sum<-cbind(x.2,y.2,cal)
  highest<-by(sum, sum$tooth, tail, n=1)
  highestd<-do.call("rbind", as.list(highest))
  data<-highestd[sapply(highestd,is.numeric)]
  rownames(data) <- NULL
  #data<-highestd[c(-1,-2,-3,-4,-5,-6,-7,-8,-9)]
  data.1<-t(data)
  rownames(data.1) <- NULL
  # We look at the CAL component
  recsev<-rowSums(data.1 >= 6)
  rec.1<-as.data.frame(recsev)
  recmod<-rowSums(data.1 >= 4)
  rec.2<-as.data.frame(recmod)
  recno <-ifelse((rec.1 > 0) | (rec.2 > 0),0 ,1 )
  rec.3<-as.data.frame(recno)
  severe <-ifelse((rec.1 >=2 ) & (pd.1 >= 1),1 ,0 )
  severe<-as.data.frame(severe)
  moderate <-ifelse((rec.2 >=2 ) | (pd.2 >= 2)| (severe==1),1 ,0 )
  moderate<-as.data.frame(moderate)
  mild <-ifelse((severe ==1 ) | (moderate == 1),0 ,1 )
  mild<-as.data.frame(mild)
  dad<-data.frame(rec.1,rec.2,rec.3,pd.1,pd.2,severe,moderate,mild)
  rownames(dad) <- NULL
  names(dad) <-
  c("rec.1","rec.2","rec.3","pd.1","pd.2","severe","moderate","mild")
  dad$disease[dad$severe == 1 ] <- "Severe"
  dad$disease[dad$moderate ==1] <- "Moderate"
  dad$disease[dad$mild ==1 ] <- "Mild/No disease"
  myvars.1 <- c("disease")
  Diseasecat <- dad[myvars.1]
  return(Diseasecat)
}



