#' log_plot
#' @description
#' log_plot is wrap-up function to plot a single sedimentary plot
#'
#' @param database
#'        dicionario
#'        well
#'
#' @return
#' @export
#'
#' @examples
logploter<-function(database,well,correlation=FALSE){

    DT<-database %>% dplyr::select(sample,bed,thick,phi,facies,sub) %>%
            mutate(r=cumsum(thick),
             l=r-thick,
             phi_alter=phi+(-min(phi)+1)) %>%
            cbind(phi_alter=stratools::inv_trans(.,column="phi_alter"))

    DT1<-filter(sample==well)

  basic.log <- litholog(l = DT1$l, # This creates a data table of
                        r = DT1$r, # rectangles coordinates for a
                        h = DT1$phi_alter, # basic litholog
                        i = DT1$bed)


  MAX_SIZE_CORR<-(max(DT$phi_alter))
  MAX_PROF_CORR<-ceiling((max(DT$r)))

  MAX_SIZE_LOC<-(max(DT1$phi_alter))
  MAX_PROF_LOC<-ceiling((max(DT1$r)))

  MAX_SIZE_LOGIC<-ifelse(correlation=FALSE,MAX_SIZE_LOC,MAX_SIZE_CORR)
  MAX_PROF_LOGIC<-ifelse(correlation=FALSE,MAX_PROF_LOC,MAX_PROF_CORR)

  title_X<-(MAX_SIZE_LOGIC+5)/2
  title_Y<- MAX_PROF_LOGIC+4

  is.litholog(basic.log)
  plot.new()
  plot.window(xlim = c(0,MAX_SIZE_LOGIC+3), ylim = c(0,MAX_PROF_LOGIC+5))
  minorAxis(2, at.maj = seq(0, MAX_PROF_LOGIC+5, 5), n = 5, las = 1)
  multigons(basic.log$i, basic.log$xy, basic.log$dt,col = DT1$sub)
  bedtext(labels = DT1$facies, l = DT1$l, r = DT1$r,x=MAX_SIZE+4)
  text(title_X, title_Y,well)

}

