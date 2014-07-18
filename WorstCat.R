WorstCat <- function( theFrame, impact, n = 5, format = NULL ){
  subFrame <- theFrame[, c('EVTYPE','N',impact)]
  catOrd <- order( theFrame[,impact], decreasing = TRUE )[1:n]
  nWorstFrame <- subFrame[catOrd,]
  
  nSumFrame <- data.frame(
    EVTYPE = c(paste0( 'TOP ', n)),
    N = c( sum( nWorstFrame$N)),
    IMPACT = c(sum( nWorstFrame[, impact]))    
    )
  colnames(nSumFrame) <- c( 'EVTYPE', 'N', impact )
  
  totalsFrame <- data.frame(
    EVTYPE = c('TOTAL'),
    N = c( sum( subFrame$N)),
    IMPACT = c(sum( subFrame[, impact]))
  )
  colnames(totalsFrame) <- c( 'EVTYPE', 'N', impact )
  
  totalsFrame <- data.frame(
    EVTYPE = c('TOTAL'),
    N = c( sum( subFrame$N)),
    IMPACT = c(sum( subFrame[, impact]))
    )
  colnames(totalsFrame) <- c( 'EVTYPE', 'N', impact )
  
  othersFrame <- data.frame(
    EVTYPE = c('Others'),
    N = c( totalsFrame[1,'N'] - sum( nWorstFrame[,'N']) ),
    IMPACT = c( totalsFrame[1,impact] - sum( nWorstFrame[,impact]) )
    )
  colnames(othersFrame) <- c( 'EVTYPE', 'N', impact )
  
  reportFrame <- rbind(nWorstFrame, nSumFrame, othersFrame, totalsFrame )
  
  if( ! is.null( format )){
    reportFrame[,impact] <- sprintf( format, reportFrame[,impact ] )
  }
  
  rownames( reportFrame ) <- as.character( c(c(1:n), ' ', '  ', '   ') )
  
reportFrame
}
