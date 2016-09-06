library(xtable)
library(reshape)
library(zoo)
library(ggplot2)
options(scipen=22)

annual2mthly <- function(x,y) {100*(((1+x/100)^(1/y))-1)}


percentChange <- function(series){ 
  serieslag <- lag(series ,-1)
  100*(series-serieslag)/serieslag 
}

## Creating monthly returns for equity, nifty and corp bonds 
load("niftytbill.rda")
nifty <- aggregate(nifty, as.yearmon, function(...) tail(... , 1))
r.equity <- percentChange(nifty)
real.rf <- 100*((1 + (coredata(yield.tbill.3m)/100))^(1/3) -1 ) 
monthly.fees.expenses <- annual2mthly(1,12) # 100bps per year
real.cbond <- annual2mthly(9.56,12)


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



# To check : Names and debugging 
WageMatrix <- function(age.entry=25,
                       age.exit=65,
                       wage=list(25000,0.07)){
    years <- (age.exit - age.entry) + 1
    
    if(length(wage)== 2){
        if(is.numeric(wage[[1]]) && length(wage[[1]])==1){
            w <- matrix(NA, years,1)
            rownames(w) <- age.entry:age.exit
            w[1,] <- wage[[1]]
            for(i in 2:length(w)){
                w[i,] <- w[i-1,] + wage[[2]]*w[i-1,]
            }
        }
    }
    if(is.vector(wage) && length(wage)== years){
        w <- data.frame(w=wage[[1]])
        rownames(w) <- age.entry:age.exit
    }
    if(length(wage[[1]])< years && length(wage[[1]])>2) {
        stop("Please enter a single wage entry with a growth rate or enter the complete wage matrix")
    }
    w <- data.frame(age=rownames(w), w)
    w$count <- 12
    final.wage <- untable(df= w[,1:2], num=w[,3])
    return(final.wage)
}

## To check:Debuggig 
contMatrix <- function(age.entry=25,
                       age.exit=65,
                       cont.rate=0.2){
    years <- (age.exit - age.entry) + 1
    if( length(cont.rate)==1){
        contribution <- rep(cont.rate, years*12)
    }
    if( length(cont.rate)== years){
        contribution <- data.frame(age=age.entry:age.exit,cont.rate)
        rownames(contribution) <- age.entry:age.exit
        contribution$count <- 12
        contirbution <- untable(df=contribution[,1:2], num=contribution[,3])
        contribution <- contribution[,2]
    }
    if( length(cont.rate>years) && ( length(cont.rate)<years && length(cont.rate)>1)){
        stop("Enter the correct cont.rate")
    }
    return(contribution) 
}

## To check: We get negative values
invprofile <- function(age.entry, age.exit){
    agemat <- matrix(NA, (age.exit-age.entry)+1, 3)
    rownames(agemat) <- age.entry:age.exit
    first <- matrix(c(10, 25, 65), 1,3)
    agemat[1:11,1] <- first[1,1]
    agemat[1:11,2] <- first[1,2]
    agemat[1:11,3] <- first[1,3]
    for(i in 12:nrow(agemat)){
        agemat[i,1] <- agemat[i-1,1] + 2.8
        agemat[i,2] <- agemat[i-1,2] - 0.6
        agemat[i,3] <- agemat[i-1,3] - 2.2
        
    }
    colnames(agemat) <- c("goi_bonds", "corp_bonds", "equity")
    agemat <- agemat/100
    return(agemat)
}


weightMatrix <- function( weight.matrix="lc",
                         age.entry=25,
                         age.exit=65){
    years <- (age.exit - age.entry) + 1
    if(class(weight.matrix)=="character" && weight.matrix=="lc"){
        x <- invprofile(age.entry, age.exit)
        x <- data.frame(age=rownames(x), x)
        x$count <- 12
        x<- untable(df=x[,1:4], num=x[,5])
    } else if( is.numeric(weight.matrix) &&
              is.vector(weight.matrix) &&
              length(weight.matrix)==3 &&
              sum(weight.matrix)==1){
        x <- matrix( rep( weight.matrix, years), ncol= 3, byrow=TRUE)
        x <- data.frame( age= age.entry:age.exit, x)
        x$count <- 12
        rownames(x) <- age.entry:age.exit
        x<- untable(df=x[,1:4], num=x[,5])
    }else if( is.data.frame(weight.matrix) &&
              ncol(weight.matrix)==3 &&
              nrow(weight.matrix)==years &&
             unique(rowSums(weight.matrix))==1){
        x <- weight.matrix
    }
    else{
        stop("Please enter a correct value")
    }
    return(x)
}

## Nominal 
returnMatrix <- function(sel="auto"){
    if(class(sel)=="character"){
        if( sel== "auto"){
            r.equity <- rnorm(nrow(wages),
                              mean=mean(r.equity),
                              sd=sd(r.equity))/100
                                        # Monthly tbill returns
            real.rf <- rnorm(nrow(wages),
                             mean=mean(real.rf),
                             sd=sd(real.rf))/100
            
                                        # Monthly corporate bond returns
        }
    }
    if(class(sel)=="data.frame"){
        if( ncol(sel)==3 && nrow(sel)==2){
            r.equity <- rnorm(nrow(wages),
                              mean=sel[1,1],
                              sd=sel[1,2])/100
                                        # Monthly tbill returns
            real.rf <- rnorm(nrow(sel[2,1]),
                             mean=meansel[2,2],
                             sd=sd(real.rf))/100
            
                                        # Monthly corporate bond returns
            real.cbond <- rnorm(nrow(sel[3,1]),
                                mean=meansel[3,2],
                                sd=sd(real.rf))/100
        }
    }
    r.portfolio <- cbind(real.rf,
                         real.cbond,
                         r.equity)
    wages$returns <- rowSums( cbind(weights$goi_bonds * r.portfolio[,"real.rf"],
                                    weights$corp_bonds * r.portfolio[,"real.cbond"],
                                    weights$equity * r.portfolio[,"r.equity"])) -
        monthly.fees.expenses
    portfolio <- 0
    for (i in 1:nrow(wages)){
        portfolio <- wages$contwages[i] +
            (1+wages$returns[i]/100)*portfolio 
    }
    return(portfolio)
}

                                        # To check: Names 
annuity <- function( annuityselect=list("price", "DOP"),
                    asset.maagement.tax= 0.03,
                    for.annuity.terminal){
    annuity.terminal.minus.tax <- for.annuity.terminal - (asset.maagement.tax * for.annuity.terminal)
    if(annuityselect[[1]]=="price" &&
       is.numeric(annuityselect[[2]])){
        x <- annuityselect[[2]]
        pension <- (365/12)*annuity.terminal.minus.tax/x
    }else if(annuityselect[[1]]=="factor" &&
       is.numeric(annuityselect[[2]])){
        x <- annuityselect[[2]]
        pension <- (annuity.terminal.minus.tax/x)/12 
    }
    else{
        stop("Please enter the annuity choice and the value in a list")
    }
    return(pension)
}


penCalc <- function(age.entry=25,
                    age.exit=65,
                    wage=list(25000,0.07),
                    cont.rate=0.2,
                    weight.matrix="lc",
                    perc.term=0.2,
                    sel="auto",
                    annuityselect=list("price", 2000),
                    asset.maagement.tax= 0.03){
    
    wages <- WageMatrix( age.entry,
                        age.exit,
                        wage)
    
    wages$contributions <- contMatrix(age.entry,
                                      age.exit,
                                      cont.rate)
    
    wages$contwages <- wages$w * wages$contributions
    weights <- weightMatrix(weight.matrix,
                            age.entry,
                            age.exit)
    
    terminal <- replicate(10000, returnMatrix(sel))
    for.annuity.terminal <- terminal * perc.term
    in.hand.terminal <- terminal - for.annuity.terminal
    pension <- annuity(annuityselect,
                       asset.maagement.tax,
                       for.annuity.terminal)
    lastwage <- tail(wages$w,1)
    replacement <- 100*(pension/lastwage)
    fin.data <- data.frame(pension=pension,
                           replacement=replacement,
                           terminal=terminal)
    p1 <- ggplot(fin.data, aes(x=pension)) + geom_density() 
    p2 <- ggplot(fin.data, aes(x=replacement)) + geom_density()
    p3 <- ggplot(fin.data, aes(x=terminal)) + geom_density()
    pdf(paste("pencalc", ".pdf",sep=""), width=5.6, height=2.4, pointsize=10)
    multiplot(p1,p2,p3)
    dev.off()
    
    
    x <- list(multiplot(p1,p2,p3),
              wages)
    return(x)
}
