WorstCat <- function( theFrame, impact, n = 5, format = NULL ){
  subFrame <- theFrame[, c('EVTYPE','N',impact)]
  catOrd <- order( theFrame[,impact], decreasing = TRUE )[1:n]
  nWorstFrame <- subFrame[catOrd,]
  
  nSumFrame <- data.frame(
    EVTYPE = c(''),
    N = c( sum( nWorstFrame$N)),
    IMPACT = c(sum( nWorstFrame[, impact]))    
  )
  colnames(nSumFrame) <- c( 'EVTYPE', 'N', impact )
  
  totalsFrame <- data.frame(
    EVTYPE = c(''),
    N = c( sum( subFrame$N)),
    IMPACT = c(sum( subFrame[, impact]))
  )
  colnames(totalsFrame) <- c( 'EVTYPE', 'N', impact )
  
  othersFrame <- data.frame(
    EVTYPE = c(''),
    N = c( totalsFrame[1,'N'] - sum( nWorstFrame[,'N']) ),
    IMPACT = c( totalsFrame[1,impact] - sum( nWorstFrame[,impact]) )
  )
  colnames(othersFrame) <- c( 'EVTYPE', 'N', impact )
  
  rankFrame <- data.frame(
    RANK = as.character( c( c(1:n),
                            paste0('Top ', n),
                            'Other',
                            'TOTAL' 
    )
    )
  )
  
  reportFrame <- rbind(nWorstFrame, nSumFrame, othersFrame, totalsFrame )
  
  pcTotFrame <- data.frame(
    FOO <- sprintf( '%.1f', reportFrame[,impact]/totalsFrame[1,impact]*100)
  )
  colnames(pcTotFrame) <- c( paste0( '%', impact ) )
  
  reportFrame <- cbind( rankFrame, reportFrame, pcTotFrame )
  
  if( ! is.null( format )){
    reportFrame[,impact] <- sprintf( format, reportFrame[,impact ] )
  }
  
  rownames( reportFrame ) <- as.character( c(c(1:n), ' ', '  ', '   ') )
  
  reportFrame
}