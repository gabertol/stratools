#' log_plot
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
log_plot<-function(data,
                   well,
                   sample_name="sample",
                   bed_thick_name="thick",
                   bed_id_name="bed",
                   phi_name="phi",
                   facies1_name="facies",
                   facies2_name="sub",
                   color_name="color",
                   correlation=FALSE,
                   grainsize=TRUE,
                   y_scale=TRUE,
                   x_scale=TRUE,
                   gs_less_size=5){

  # Variables
  sample<-data %>% dplyr::select(sample=sample_name)

  bed_thick<-data %>% dplyr::select(bed_thick=bed_thick_name)

  bed_id<-data %>% dplyr::select(bed_id=bed_id_name)

  phi<-data %>% dplyr::select(phi=phi_name)

  facies1<-data %>% dplyr::select(facies1=facies1_name)

  facies2<-data %>% dplyr::select(facies2=facies2_name)

  color1<-data %>% dplyr::select(any_of(color_name))
  color2<-data %>% dplyr::select(sample=sample_name) %>% mutate(color2="yellow") %>% dplyr::select(-sample)
  col<-tibble(color1,color2) %>% dplyr::select(col=paste(ifelse(color_name %in% colnames(data),"color","color2")))

  DF<- cbind(sample,bed_id,bed_thick,phi,facies1,facies2,col)

  # dataframes
  DF_cor<-DF %>%
    group_by(sample) %>%
    mutate(r=cumsum(bed_thick),
           l=r-bed_thick) %>%
    ungroup() %>%
    inv_trans(.) %>%
    mutate(phi_alter2=gs_less_size)

  DF_loc<- DF_cor %>%
    filter(sample==well) %>%
    dplyr::select(l,r,i="bed_id",col,facies1,facies2,h=paste(ifelse(isTRUE(grainsize),"phi_alter","phi_alter2")))


  # StratigrapheR input

  basic.log <- litholog(l = DF_loc$l, # This creates a data table of
                        r = DF_loc$r, # rectangles coordinates for a
                        h = DF_loc$h, # basic litholog
                        i = DF_loc$i)
  # Logic gates
  MAX_SIZE_CORR<-(max(DF_cor$phi_alter))
  MAX_PROF_CORR<-ceiling((max(DF_cor$r)))

  MAX_SIZE_LOC<-(max(DF_loc$h))
  MAX_PROF_LOC<-ceiling((max(DF_loc$r)))

  MAX_SIZE_LOGIC<-ifelse(isTRUE(grainsize),ifelse(isTRUE(correlation),MAX_SIZE_CORR,MAX_SIZE_LOC),5)
  MAX_PROF_LOGIC<-ifelse(isTRUE(correlation),MAX_PROF_CORR,MAX_PROF_LOC)

  title_X<- (-4)
  title_Y<- MAX_PROF_LOGIC+4

  # Plot

  is.litholog(basic.log)
  plot.new()
  plot.window(xlim = c(-5,MAX_SIZE_LOGIC+2), ylim = c(0,MAX_PROF_LOGIC+5))

  if (isTRUE(x_scale)) {
    minorAxis(1, at.maj = seq(0, MAX_SIZE_LOGIC+2, 1), pos=-1,las = 1)
  }
  else {}

  if (isTRUE(y_scale)) {
    minorAxis(2, at.maj = seq(0, MAX_PROF_LOGIC+5, 5), n = 5, las = 1)
  }
  else {}

  multigons(basic.log$i, basic.log$xy, basic.log$dt,col = DF_loc$col)
  bedtext(labels = DF_loc$facies1, l = DF_loc$l, r = DF_loc$r,x=-2,ymin=NA)
  text(title_X, title_Y,well)

}
