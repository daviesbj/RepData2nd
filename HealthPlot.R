HealthPlot <- function(
  theFrame,
  eVsets = list( c('HEATWAVE', 'TORNADO', 'FLOOD', 'LIGHTNING', 'RIP CURRENT' ),
                c('TORNADO', 'HEATWAVE', 'FLOOD', 'LIGHTNING', 'TRPCL STORM' ) )
  ){
  
  par(mfrow=c(2,2))
  
  onePlot(theFrame, aggFunc = 'sum', impact = 'FATALITIES',
          evTypes = eVsets[[1]])
  onePlot(theFrame, aggFunc = 'sum', impact = 'INJURIES',
          evTypes = eVsets[[2]])
  onePlot(theFrame, aggFunc = 'length', impact = 'FATALITIES',
          evTypes = eVsets[[1]])
  onePlot(theFrame, aggFunc = 'length', impact = 'INJURIES',
          evTypes = eVsets[[2]])
}


onePlot <- function(
  sourceFrame,
  aggFunc = 'sum',
  impact = 'FATALITIES',
  evTypes = c('HEATWAVE', 'TORNADO', 'FLOOD', 'LIGHTNING', 'RIP CURRENT' )
){
  colors <- c( 'black', 'red', 'green', 'blue', 'orange' )
  Years <- levels(as.factor(theFrame$YEAR))
  plotFrame <- data.frame( YEAR = Years )
  # Assemble data frame for plotting so we can yet scale
  nEvt <- length( evTypes )
  for( iEv in (1:nEvt) ){
    eVtype <- evTypes[iEv]
    message( 'Doing ', eVtype )
    typeFrame <- theFrame[theFrame$EVTYPE_NEW==eVtype,]
    eVFrame <- data.frame(
      YEAR = typeFrame$YEAR,
      X = typeFrame[,impact]
    )
    colnames(eVFrame) <- c( 'YEAR', impact )
    if( aggFunc == 'sum' ) sumFrame <- aggregate(eVFrame, list(eVFrame$YEAR), sum, na.rm=TRUE)
    if( aggFunc == 'length' ) sumFrame <- aggregate(eVFrame, list(eVFrame$YEAR), length)
    colnames(sumFrame) <- c( 'YEAR', 'foo', eVtype )
    sumFrame$YEAR <- as.factor(sumFrame$YEAR)
    plotFrame <- merge( plotFrame, sumFrame[,c('YEAR',eVtype)], by = 'YEAR', all.x = TRUE )
    plotFrame[which(is.na(plotFrame[,eVtype])),eVtype] = 0
  } 
  yMax <- max(plotFrame[,2:(nEvt+1)])
  if( aggFunc == 'sum' ){
    mainTitle <- paste0( "Annual ", impact, " -- Major Event types" )
    yLab <- paste0( "Total ", impact, " in year")
  }
  if( aggFunc == 'length' ){
    mainTitle <- paste0( "Annual frequency of ", impact, "-Causing Event types" )
    yLab <- paste0( "Number in Year" )
  }
  plot(plotFrame$YEAR, plotFrame[,2], type='n', lwd=2, col=colors[iEv],
     xlab = 'YEAR', ylim = c( 0, yMax ), ylab = yLab, main = mainTitle, lwd = 0 )
  for( iEv in (1:nEvt) ){
    points(plotFrame$YEAR, plotFrame[,iEv+1], type='l', lwd=2, col=colors[iEv] ) 
  }
  legend( x = 'topright', evTypes, lwd = 2, col=colors )
}

