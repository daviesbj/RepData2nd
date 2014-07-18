HealthPlot <- function(
  theFrame,
  eVsets = list( c('HEATWAVE', 'TORNADO', 'FLOOD', 'LIGHTNING', 'RIP CURRENT' ),
                c('TORNADO', 'HEATWAVE', 'FLOOD', 'LIGHTNING', 'TRPCL STORM' ) )
  ){
  

}

singlePlot <- function(
  theFrame,
  field,
  eVset = c('HEATWAVE', 'TORNADO', 'FLOOD', 'LIGHTNING', 'RIP CURRENT' )
  ){
  for( eVtype in eVset){
    eVFrame <- theFrame[EVTYPE==eVtype,]
    sumFrame <- summarize()
    
  }
}