#' highflowmetrics
#'
#' Compute percent error between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @param  month month
#' @param  day day
#' @param  year year
#' @return annual_max_cor


check_maxannual = function(m,o, month, day, year,wy) {

  flow = cbind.data.frame(m,o, month, day, year,wy)
  # first lets get minimum yearly values
  
  tmp = flow %>% group_by(wy) %>% summarize(maxo=max(o), maxm=max(m))

    annual_max_cor = cor(tmp$maxm, tmp$maxo)
  
  
  return(annual_max_cor=annual_max_cor)
}
