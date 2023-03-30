#' inv_trans
#' '
#'
#' @param vect column of dataframe with numerical data
#'
#' @return a column of a dataframe ready for cbind with the inverted order of the elements- the major values will be the lower
#' @export
#'
#' @examples
#' a<-c(6:17)
#' inv_trans(a)
#'
inv_trans <- function(dataframe) {

  dataframe %>%
    dplyr::select(one="phi_alter") %>%
    dplyr::mutate(max1=max(one),
                  min1=min(one),
                  dif=max1-min1,
                  dist=one-dif,
                  pen=one-dist,
                  val=pen-dist,
                  value=ifelse(pen %% 2 == 0,val,val+1),
                  phi_alter=value-(pen-1))->DT# %>%
   # dplyr::select(phi_alter)->DT

  cbind(dataframe,DT)

#  cbind(dataframe %>% dplyr::select(-phi_alter),DT)


}

