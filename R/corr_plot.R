#' corr_plot
#'
#' @param variables
#'
#' @return
#' @export
#'
#' @examples
#'

corr_plot <- function(dataframe,direction=,...) {

  ORDER<-unique(dataframe$sample)
  N_PLOT<-length(ORDER)

  par(mfrow=c(1,N_PLOT))

  log_plot(data=dataframe,well=ORDER[1],correlation=TRUE,...)

  for (i in ORDER[2:N_PLOT]) { # Loop over loop.vector

    log_plot(data=dataframe,well=i,correlation=TRUE,y_scale=FALSE,...)
  }

  par(mfrow=c(1,1))
}
