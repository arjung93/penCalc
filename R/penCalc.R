## library(xtable)
## library(ggplot2)
## library(reshape)
## library(zoo)

annual2mthly <- function(x,y) {100*(((1+x/100)^(1/y))-1)}


percentChange <- function(series){ 
  serieslag <- lag(series ,-1)
  100*(series-serieslag)/serieslag 
}

## Creating monthly returns for equity, nifty and corp bonds 

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  

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
        colnames(x) <- c("goi_bonds","corp_bonds","equity")
        monthly.fees.expenses
    }
    else{
        stop("Please enter a correct value")
    }
    return(x)
}

#load("../data/niftytbill.rda")

## Nominal 
returnMatrix <- function(sel="auto",wages,weights){
    if(class(sel)=="character"){
        if( sel== "auto"){
            data(niftytbill)
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

#'  A function to find out the monthly pension payments recieved after inversting in an annuity 
#' @usage penCalc(age.entry=25,
#'                age.exit=65,
#'                wage=list(25000,0.07),
#'                cont.rate=0.2,
#'                weight.matrix="lc",
#'                perc.term=0.2,
#'                sel="auto",
#'                annuityselect=list("price", 2000),
#'                asset.maagement.tax= 0.03)
#' @title penCalc
#' @import zoo xtable ggplot2 reshape grid
#' @param age.entry Numeric entry of the age at which the individual starts working. Default=25
#' @param age.exit Numeric entry of the age at which the individual stops working. Default=65
#' @param wage The parameter accepts the following arguments \enumerate{ \item  \bold{wage at entry with growth rate}: A list of two elements. The first element is the age at entry and the second element is the annual wage growth rate. eg. list(35000, 0.07)
#'\item \bold{Complete wage}: Enter the complete the wage structure in a vector. Please make sure that the number of entries in the wage structure is equal to (age.exit-age.entry) +1.}
#' @param cont.rate This argument of the function will tell us the percentage of wage kept aside to invest in a pool of government and corporate bonds and equity. This parameter can accept the follwing arguments:
#' \enumerate{\item Single contribution rate: Numeric value less than 1.This value will be replicated for each salaried year. 
#' \item Contribution vector: Enter a vector of contribution rates.}
#' @param weight.matrix This argument provides the percentage of the contributed salary to invest in a pool of government and corporate bonds and equity. This arguments can accept the follwing options:
#' \enumerate{\item Lifecycle: : "lc" generates the weights on the basis of lifecycle function mentioned in the Deepak Parekh Report. 
#' \item Single Wieghts: To provide static weights please provide a ector of 3 numerics in the follwing order Government, Equity and corporate
#' \item Dynamic Weights: Provide a data frame of 3 columns and the number of rows equal to the (age at entry - age at exit)+1. The columns of the data frame should be in the following order- Government bonds, Corporate bonds, Equity}
#' @param perc.term Percentage of the terminal value to be invested in buying an annuity.
#' @param sel Argument to generate the hypothetical returns on investment. The following two options have been provided:
#'     \enumerate{ \item Character vector "auto": This argument uses the inhouse data of nifty, government and coprotate bonds to generate the hypothetical returns. 
#'         \item A data frame of 3 columns and 2 rows. The rows should highlight the mean and standard deviations and columns should represent the investment instrumnent.}
#' @param annuityselect Type (price or factor) and price/factor. Enter the type of annuity as the first element of the list and enter the price/factor of the annuity as the second element of the list. 
#' @param asset.maagement.tax Numeric percent of the tax to be paid when buying an annuity 
#' @return Distribution of monthly pension, terminal value and the replacement rate
#' @examples  penCalc(age.entry=25,
#'            age.exit=65,
#'                   wage=list(25000,0.07),
#'                   cont.rate=0.2,
#'                   weight.matrix="lc",
#'                   perc.term=0.2,
#'                   sel="auto",
#'                   annuityselect=list("price", 2000),
#'                   asset.maagement.tax= 0.03)
#' @author Renuka Sane, Arjun Gupta
#' @export penCalc
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
    
    terminal <- replicate(10000, returnMatrix(sel,wages=wages,weights=weights))
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
    
    
    
    x <- multiplot(p1,p2,p3)
    return(x)
}

