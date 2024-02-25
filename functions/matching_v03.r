matching<-function(data,name.cascon,case.value,id.name,num.controls,var.match,tol,name.pair,seed.cases=321,seed.controls=678){          

# data: R dataFrame
# name.cascon: variable to distinguish cases and controls
# case.value: value assigned to cases in the ‘name.cascon’
# id.name: variable that identifies each individual (no duplicates!)
# num.controls: ratio controls vs. cases
# var.match: a character vector with the names of the variables to be matched
# tol: a numeric vector with the tolerance for each variable in the var.match (with exactly the same order)
# name.pair: name for the variable that identifies the matched pairs
# seed.cases: seed for cases
# seed.controls: seed for control


############ EXAMPLE ##############
# dm <- matching(data = mydata,
#             name.cascon = "parti",
#             case.value=1,
#             id.name="ID",
#             num.controls=2,
#             var.match=c("female", "age", "region" ),
#             tol=c(0, 2, 0),
#             name.pair="pair",
#             seed.cases=09052022,
#             seed.controls=09052022+1)

  require(Hmisc)

  find.matches<-function (x, y, tol, scale = tol, maxmatch = 10)
  {
      rep.int <- rep 
      if (!is.matrix(x)) x <- as.matrix(x)
      n <- nrow(x)
      p <- ncol(x)

      if (!is.list(tol)){
        print("wait: selecting. . . . ")
        temp<-list()
        for (k in 1:length(tol)) temp[[k]]<-c(-tol[k],tol[k])
        tol<-temp 
      }

      if (!is.matrix(y)) y <- as.matrix(y)
      if (p != ncol(y)) stop("number of columns of x and y must match")

      ny <- nrow(y)
      rown <- dimnames(x)[[1]]
      ry <- dimnames(y)[[1]]
      matches <- matrix(if (length(ry)) "" else 0, n, maxmatch, dimnames = list(rown, paste("Match #",1:maxmatch, sep = "")))
      distance <- matrix(NA, n, maxmatch, dimnames = list(rown,paste("Distance #", 1:maxmatch, sep = "")))
      if (length(ry) == 0) ry <- 1:ny
      scale <- unlist(lapply(tol,diff))
      scale <- ifelse(scale == 0, 1, scale)
      mx <- 0
      for (i in 1:n) {
          dif <- y - rep(x[i, ], rep.int(ny, p))
          inside<-NULL
          for (k in 1:p) inside<-cbind(inside,dif[,k]<=tol[[k]][2] & dif[,k]>=tol[[k]][1])
          which <- which(apply(inside,1,all))
          lw <- length(which)
          if (lw) {
              scaled <- dif[which, , drop = FALSE]/rep(scale, rep.int(lw,p))
              dist <- rowSums(scaled^2)
              lw <- min(lw, maxmatch)
              mx <- max(mx, lw)
              d <- order(dist)[1:lw]
              matches[i, 1:lw] <- ry[which[d]]
              distance[i, 1:lw] <- dist[d]
          }
      }
      structure(list(matches = matches[, 1:mx], distance = distance[,1:mx]), class = "find.matches")
  }


  ############  END OF PREVIOUS FUNCTIONS ##############


  CASECON=data[,name.cascon]
  
  print("Starting . . . ")
  
  print(names(data))
  print(c(id.name,var.match))
  
  casos=data[CASECON==case.value,c(id.name,var.match)]
  
  controls=data[!CASECON==case.value,c(id.name,var.match)]



  
  match.res<-find.matches(as.matrix(casos[,-1]),as.matrix(controls[,-1]),tol=tol,maxmatch=nrow(casos))

  cont.matched<-matrix(NA,nrow(casos),num.controls)

  match<-match.res$matches

  dist<-match.res$distance

  if(is.null(rownames(match))) rownames(match)<-1:nrow(match)
  if(is.null(rownames(dist))) rownames(dist)<-1:nrow(dist)

  # aTaking cases in a random order.

  if (!is.null(seed.cases)) set.seed(seed.cases)

  ordre.casos<-sample(rownames(match))

  match2<-match[ordre.casos,]
  dist2<-dist[ordre.casos,]
                                        
  id.cas<-row.names(match2)  

  if (!is.null(seed.controls)) set.seed(seed.controls)

  for (i in 1:nrow(casos)){

  	candidats<-match2[i,]
  	candidats<-candidats[!candidats==""]
  	distancies<-dist2[i,1:length(candidats)]

  	if (i>1){ # Avoiding the candidates we took for the previous cases..
  		elim.anteriors<-!candidats%in%cont.matched[1:(i-1),]
  		candidats<-candidats[elim.anteriors]
  		distancies<-distancies[elim.anteriors]
  	}

  	if (length(candidats)<num.controls) print(paste("ID ",casos[id.cas[i],id.name]," cannot be matched",sep=""))

  	if (length(candidats)==num.controls) cont.matched[i,]=candidats

  	if (length(candidats)>num.controls) {	## they are ordered according to increasing distance
  		## Sorting the candidates randomly within each dist2[i,]
 			candidats<-lapply(split(as.character(candidats),distancies),function(x) {if (length(x)==1) return(x); if (length(x)>1) return(sample(x))})
 			candidats<-as.character(unlist(candidats))
  		cont.matched[i,]<-candidats[1:num.controls]
  	}
  	

  }

  aux.rep<-NULL
  for (i in 1:ncol(cont.matched)) aux.rep<-c(aux.rep,cont.matched[,i])

  # cat("This must sum to zero",sum(table(aux.rep)>1),"\n") ## must always return zero.
  print(paste("From " ,nrow(casos)," cases, ", sum(!is.na(cont.matched[,1]))," individuals have been paired.",sep=""))

  
  no.matched.casos<-id.cas[is.na(cont.matched[,1])]
  no.matched.casos<-casos[no.matched.casos,id.name]

  matched.casos<-id.cas[!is.na(cont.matched[,1])]
  matched.casos<-casos[matched.casos,id.name]
  # matched.casos<-casos[as.numeric(matched.casos),id.name]
  
  
  
  matched.controls<-cont.matched[!is.na(cont.matched[,1]),,drop=FALSE]
  

  if (num.controls==1){ 
    matched.controls<-controls[matched.controls,id.name]
    # matched.controls<-controls[as.numeric(matched.controls),id.name]
  }
  if (num.controls>1){
  	for (i in 1:num.controls){
  		matched.controls[,i]<-controls[matched.controls[,i],id.name]
  		# matched.controls[,i]<-controls[as.numeric(matched.controls[,i]),id.name]
  	}
  }

  taula.match<-data.frame(list(parella=1:length(matched.casos),id_cas=matched.casos,id_cont=matched.controls))

  parella<-rep(taula.match$parella,each=1+num.controls)

  id<-NULL
  for (i in 1:nrow(taula.match)){
  	if (num.controls==1) id=c(id,matched.casos[i],matched.controls[i])
  	if (num.controls>1) id=c(id,matched.casos[i],matched.controls[i,])
  }


  cascon<-rep(c(1,rep(0,num.controls)),nrow(taula.match))

  taula.res<-data.frame(list(ID=I(id),CASCON=cascon,PARELLA=parella))

  names(taula.res)[3]<-name.pair
  names(taula.res)[1]<-id.name
  
  return(taula.res[,-2])

}

















