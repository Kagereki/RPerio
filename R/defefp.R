efpcase<-function (x,y){
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
    sum<-cbind(x.2,cal)
    #sum<-newdata
    
    tooth11 <- sum[ which(sum$tooth==11),];tooth11<-tooth11[sapply(tooth11,is.numeric)];tooth11<-as.matrix(apply(tooth11, 2, max,na.rm=TRUE))
    tooth12 <- sum[ which(sum$tooth==12),];tooth12<-tooth12[sapply(tooth12,is.numeric)];tooth12<-as.matrix(apply(tooth12, 2, max,na.rm=TRUE))
    tooth13 <- sum[ which(sum$tooth==13),];tooth13<-tooth13[sapply(tooth13,is.numeric)];tooth13<-as.matrix(apply(tooth13, 2, max,na.rm=TRUE))
    tooth14 <- sum[ which(sum$tooth==14),];tooth14<-tooth14[sapply(tooth14,is.numeric)];tooth14<-as.matrix(apply(tooth14, 2, max,na.rm=TRUE))
    tooth15 <- sum[ which(sum$tooth==15),];tooth15<-tooth15[sapply(tooth15,is.numeric)];tooth15<-as.matrix(apply(tooth15, 2, max,na.rm=TRUE))
    tooth16 <- sum[ which(sum$tooth==16),];tooth16<-tooth16[sapply(tooth16,is.numeric)];tooth16<-as.matrix(apply(tooth16, 2, max,na.rm=TRUE))
    tooth17 <- sum[ which(sum$tooth==17),];tooth17<-tooth17[sapply(tooth17,is.numeric)];tooth17<-as.matrix(apply(tooth17, 2, max,na.rm=TRUE))
    
    
    tooth21 <- sum[ which(sum$tooth==21),];tooth21<-tooth21[sapply(tooth21,is.numeric)];tooth21<-as.matrix(apply(tooth21, 2, max,na.rm=TRUE))
    tooth22 <- sum[ which(sum$tooth==22),];tooth22<-tooth22[sapply(tooth22,is.numeric)];tooth22<-as.matrix(apply(tooth22, 2, max,na.rm=TRUE))
    tooth23 <- sum[ which(sum$tooth==23),];tooth23<-tooth23[sapply(tooth23,is.numeric)];tooth23<-as.matrix(apply(tooth23, 2, max,na.rm=TRUE))
    tooth24 <- sum[ which(sum$tooth==24),];tooth24<-tooth24[sapply(tooth24,is.numeric)];tooth24<-as.matrix(apply(tooth24, 2, max,na.rm=TRUE))
    tooth25 <- sum[ which(sum$tooth==25),];tooth25<-tooth25[sapply(tooth25,is.numeric)];tooth25<-as.matrix(apply(tooth25, 2, max,na.rm=TRUE))
    tooth26 <- sum[ which(sum$tooth==26),];tooth26<-tooth26[sapply(tooth26,is.numeric)];tooth26<-as.matrix(apply(tooth26, 2, max,na.rm=TRUE))
    tooth27 <- sum[ which(sum$tooth==27),];tooth27<-tooth27[sapply(tooth27,is.numeric)];tooth27<-as.matrix(apply(tooth27, 2, max,na.rm=TRUE))
    
    
    tooth31 <- sum[ which(sum$tooth==31),];tooth31<-tooth31[sapply(tooth31,is.numeric)];tooth31<-as.matrix(apply(tooth31, 2, max,na.rm=TRUE))
    tooth32 <- sum[ which(sum$tooth==32),];tooth32<-tooth32[sapply(tooth32,is.numeric)];tooth32<-as.matrix(apply(tooth32, 2, max,na.rm=TRUE))
    tooth33 <- sum[ which(sum$tooth==33),];tooth33<-tooth33[sapply(tooth33,is.numeric)];tooth33<-as.matrix(apply(tooth33, 2, max,na.rm=TRUE))
    tooth34 <- sum[ which(sum$tooth==34),];tooth34<-tooth34[sapply(tooth34,is.numeric)];tooth34<-as.matrix(apply(tooth34, 2, max,na.rm=TRUE))
    tooth35 <- sum[ which(sum$tooth==35),];tooth35<-tooth35[sapply(tooth35,is.numeric)];tooth35<-as.matrix(apply(tooth35, 2, max,na.rm=TRUE))
    tooth36 <- sum[ which(sum$tooth==36),];tooth36<-tooth36[sapply(tooth36,is.numeric)];tooth36<-as.matrix(apply(tooth36, 2, max,na.rm=TRUE))
    tooth37 <- sum[ which(sum$tooth==37),];tooth37<-tooth37[sapply(tooth37,is.numeric)];tooth37<-as.matrix(apply(tooth37, 2, max,na.rm=TRUE))
    
    tooth41 <- sum[ which(sum$tooth==41),];tooth41<-tooth41[sapply(tooth41,is.numeric)];tooth41<-as.matrix(apply(tooth41, 2, max,na.rm=TRUE))
    tooth42 <- sum[ which(sum$tooth==42),];tooth42<-tooth42[sapply(tooth42,is.numeric)];tooth42<-as.matrix(apply(tooth42, 2, max,na.rm=TRUE))
    tooth43 <- sum[ which(sum$tooth==43),];tooth43<-tooth43[sapply(tooth43,is.numeric)];tooth43<-as.matrix(apply(tooth43, 2, max,na.rm=TRUE))
    tooth44 <- sum[ which(sum$tooth==44),];tooth44<-tooth44[sapply(tooth44,is.numeric)];tooth44<-as.matrix(apply(tooth44, 2, max,na.rm=TRUE))
    tooth45 <- sum[ which(sum$tooth==45),];tooth45<-tooth45[sapply(tooth45,is.numeric)];tooth45<-as.matrix(apply(tooth45, 2, max,na.rm=TRUE))
    tooth46 <- sum[ which(sum$tooth==46),];tooth46<-tooth46[sapply(tooth46,is.numeric)];tooth46<-as.matrix(apply(tooth46, 2, max,na.rm=TRUE))
    tooth47 <- sum[ which(sum$tooth==47),];tooth47<-tooth47[sapply(tooth47,is.numeric)];tooth47<-as.matrix(apply(tooth47, 2, max,na.rm=TRUE))
    

data<-data.frame(tooth11,tooth12,tooth13,tooth14,tooth15,tooth16,tooth17,
tooth21,tooth22,tooth23,tooth24,tooth25,tooth26,tooth27,
tooth31,tooth32,tooth33,tooth34,tooth35,tooth36,tooth37,
tooth41,tooth42,tooth43,tooth44,tooth45,tooth46,tooth47)

data5<-rowSums(data > 5)

disease1<-ifelse(((data5/20)*100)>30,c("disease"), c("No disease"))
data[data<3]<-0

data17<-data[c(-tooth16)];data17<-rowSums(data17 > 0)
data16<-data[c(-tooth17,-tooth15)];data16<-rowSums(data16 > 0)
data15<-data[c(-tooth16,-tooth14)];data15<-rowSums(data15 > 0)
data14<-data[c(-tooth15,-tooth13)];data14<-rowSums(data14 > 0)
data13<-data[c(-tooth14,-tooth12)];data13<-rowSums(data13 > 0)
data12<-data[c(-tooth13,-tooth11)];data12<-rowSums(data12 > 0)
data11<-data[c(-tooth12,-tooth21)];data11<-rowSums(data11 > 0)


data27<-data[c(-tooth26)];data27<-rowSums(data27 > 0)
data26<-data[c(-tooth27,-tooth25)];data26<-rowSums(data26 > 0)
data25<-data[c(-tooth26,-tooth24)];data25<-rowSums(data25 > 0)
data24<-data[c(-tooth25,-tooth23)];data24<-rowSums(data24 > 0)
data23<-data[c(-tooth24,-tooth22)];data23<-rowSums(data23 > 0)
data22<-data[c(-tooth23,-tooth21)];data22<-rowSums(data22 > 0)
data21<-data[c(-tooth22,-tooth21)];data21<-rowSums(data21 > 0)

data37<-data[c(-tooth36)];data37<-rowSums(data37 > 0)
data36<-data[c(-tooth37,-tooth35)];data36<-rowSums(data36 > 0)
data35<-data[c(-tooth36,-tooth34)];data35<-rowSums(data35 > 0)
data34<-data[c(-tooth35,-tooth33)];data34<-rowSums(data34 > 0)
data33<-data[c(-tooth34,-tooth32)];data33<-rowSums(data33 > 0)
data32<-data[c(-tooth33,-tooth31)];data32<-rowSums(data32 > 0)
data31<-data[c(-tooth32,-tooth31)];data31<-rowSums(data31 > 0)

data47<-data[c(-tooth46)];data47<-rowSums(data47 > 0)
data46<-data[c(-tooth47,-tooth45)];data46<-rowSums(data46 > 0)
data45<-data[c(-tooth46,-tooth44)];data45<-rowSums(data45 > 0)
data44<-data[c(-tooth45,-tooth43)];data44<-rowSums(data44 > 0)
data43<-data[c(-tooth44,-tooth42)];data43<-rowSums(data43 > 0)
data42<-data[c(-tooth43,-tooth41)];data42<-rowSums(data42 > 0)
data41<-data[c(-tooth42,-tooth41)];data41<-rowSums(data41 > 0)
	

disease<-ifelse( data17>2|data16>2|data15>2|data14>2|data13>2|data12>2|data11>2
                |data27>2|data26>2|data25>2|data24>2|data23>2|data22>2|data21>2
                |data37>2|data36>2|data35>2|data34>2|data33>2|data22>2|data31>2
                |data47>2|data46>2|data45>2|data44>2|data43>2|data22>2|data41>2,c("disease"),c("No disease"))


diseasecat<-data.frame(disease,disease1)

category<-ifelse(diseasecat$disease1=="disease"|diseasecat$disease=="disease",c("disease"),c("No disease"))

category<-data.matrix(category)
return(category)
}


