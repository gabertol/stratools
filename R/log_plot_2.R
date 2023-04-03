#' log_plot2
#' @description
#' log_plot is wrap-up function to plot a single sedimentary plot
#'
#' @param database
#'        well name of the well to be plot
#'        mode indicates if the plot scale is setted to local, or correlation.
#'
#' @return
#' @export
#'
#' @examples
log_plot2<-function(data,sample,correlation=FALSE,grainsize=TRUE,y_scale=TRUE,x_scale=TRUE,gs_less_size=5){

  low<-(-min(data$phi))+1
  col<-data %>% filter(sample==sample) %>% dplyr::select(bed,any_of("color")) %>%
    mutate(color2="yellow") %>%
    group_by(bed) %>%
    summarise(bed=unique(bed),
              color2=unique(color2),
              color=ifelse("color" %in% names(data),unique(color),color2)) %>%
    ungroup()


  DT<-data %>%
    dplyr::select(well="sample",bed,thick,phi,facies,sub,-any_of("color")) %>%
    group_by(well) %>%
    mutate(r=cumsum(thick),
           l=r-thick) %>%
    ungroup() %>%
    mutate(low_phi=low,
           phi_alter=low_phi+phi) %>%
    inv_trans(.) %>%
    mutate(phi_alter2=gs_less_size)


  DT1<-DT %>%
    filter(well==sample)


  h<-ifelse(isTRUE(grainsize),DT1$phi_alter,DT1$phi_alter2)

  color<-as.vector(ifelse("color" %in% names(data),col$color,col$color2))

  basic.log <- litholog(l = DT1$l, # This creates a data table of
                        r = DT1$r, # rectangles coordinates for a
                        h =h, # basic litholog
                        i = DT1$bed)




  MAX_PROF_CORR<-ceiling((max(DT$r)))

  MAX_SIZE_LOC<-(max(DT1$phi_alter))
  MAX_PROF_LOC<-ceiling((max(DT1$r)))

  MAX_SIZE_LOGIC<-ifelse(isTRUE(grainsize),MAX_SIZE_LOC,5)
  MAX_PROF_LOGIC<-ifelse(isTRUE(correlation),MAX_PROF_CORR,MAX_PROF_LOC)

  title_X<- (-4)
  title_Y<- MAX_PROF_LOGIC+4

  is.litholog(basic.log)
  plot.new()
  plot.window(xlim = c(-5,MAX_SIZE_LOGIC+2), ylim = c(0,MAX_PROF_LOGIC+5))

  if (isTRUE(x_scale)) {
    minorAxis(1, at.maj = seq(0, MAX_SIZE_LOGIC+2, 2), pos=-1,las = 1)
  }
  else {}

  if (isTRUE(y_scale)) {
    minorAxis(2, at.maj = seq(0, MAX_PROF_LOGIC+5, 5), n = 5, las = 1)
  }
  else {}

  multigons(basic.log$i, basic.log$xy, basic.log$dt,col = color)
  bedtext(labels = DT1$facies, l = DT1$l, r = DT1$r,x=-2,ymin=NA)
  text(title_X, title_Y,sample)


}

