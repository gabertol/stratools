#' log_plot
#'
#' @description
#' log_plot is a wrap-up function to plot a single sedimentary plot
#'
#' @param data
#'        The input dataset containing well data. Default is NULL.
#' @param well
#'        Well name of the well to be plotted. Default is NULL.
#' @param sample_name
#'        Name of the column in data for sample. Default is "sample".
#' @param bed_thick_name
#'        Name of the column in data for bed thickness. Default is "thick".
#' @param bed_id_name
#'        Name of the column in data for bed ID. Default is "bed".
#' @param phi_name
#'        Name of the column in data for phi (porosity). Default is "phi".
#' @param facies1_name
#'        Name of the column in data for facies. Default is "facies".
#' @param facies2_name
#'        Name of the column in data for sub-facies. Default is "sub".
#' @param color_name
#'        Name of the column in data for color. Default is "color".
#' @param correlation
#'        Logical value indicating whether to create a correlation plot. Default is FALSE.
#' @param grainsize
#'        Logical value indicating whether to include grain size information. Default is TRUE.
#' @param y_scale
#'        Logical value indicating whether to scale the y-axis. Default is TRUE.
#' @param x_scale
#'        Logical value indicating whether to scale the x-axis. Default is TRUE.
#' @param gs_less_size
#'        Value used for grain size calculations. Default is 5.
#'

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
                   gs_less_size=5,
                   cex=0.5,
                   srt = 0){

  # Variables
  sample<-data %>% dplyr::select(sample=sample_name)

  bed_thick<-data %>% dplyr::select(bed_thick=bed_thick_name)

  bed_id<-data %>% dplyr::select(bed_id=bed_id_name)

  phi<-data %>% dplyr::select(phi=phi_name)

  facies1<-data %>% dplyr::select(facies1=facies1_name)

  facies2<-data %>% dplyr::select(facies2=facies2_name)

  color1<-data %>% dplyr::select(any_of(color_name))

  color2<-data %>%
    dplyr::select(sample=sample_name) %>%
    mutate(color2="yellow") %>%
    dplyr::select(-sample)

  col<-tibble(color1,color2) %>%
    dplyr::select(col=paste(ifelse(color_name %in% colnames(data),"color","color2")))

  DF<- cbind(sample,bed_id,bed_thick,phi,facies1,facies2,col)

  # dataframes
  DF_cor<-DF %>%
    group_by(sample) %>%
    mutate(r=cumsum(bed_thick),
           l=r-bed_thick) %>%
    ungroup() %>%
    mutate(phi=ifelse(facies1=="COVERED",10,phi)) %>%
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

  title_X<- (-1)
  title_Y<- MAX_PROF_LOGIC+2


  # X scale
  tabela_correspondencia <- tribble(
    ~phi, ~mm, ~gs_name, ~gs_other,~pt_br,~abrev,~simp,
    -12, 4096, "boulder", "boulder","matacão","Mt","NA",
    -11, 2048, "boulder", "boulder","matacão","Mt","NA",
    -10, 1024, "boulder", "boulder","matacão","Mt","NA",
    -9, 512, "boulder", "boulder","matacão","Mt","NA",
    -8, 256.000, "boulder", "boulder","matacão","Mt","NA",
    -7, 128, "boulder", "boulder","matacão","Mt","Boulder",
    -6, 64.000, "cobble", "cobble","seixo","Sx","Cobble",
    -5, 32, "very coarse gravel", "pebble","cascalho","Cb","NA",
    -4, 16, "coarse gravel", "pebble","cascalho","Cb","NA",
    -3, 8, "medium gravel", "pebble","cascalho","Cb","NA",
    -2, 4.000, "fine gravel", "pebble","cascalho","Cb","Pebble",
    -1, 2.00, "very fine gravel", "granules","granulos","Gr","Granules",
    0, 1.000, "very coarse sand", "sand","areia","S","NA",
    1, 0.500, "coarse sand", "sand","areia","S","NA",
    2, 0.250, "medium sand", "sand","areia","S","NA",
    3, 0.125, "fine sand", "sand","areia","S","NA",
    4, 0.060, "very fine sand", "sand","areia","S","Sand",
    5, 0.031, "coarse silt", "silt","silte","St","NA",
    6, 0.016, "medium silt", "silt","silte","St","NA",
    7, 0.008, "fine silt", "silt","silte","St","NA",
    8, 0.004, "very fine silt", "silt","silte","St","Silt",
    9, 0.002, "clay", "mud","argila","Cl","NA",
    10, 0.001, "clay", "mud","argila","Cl","Clay"
  )


  tibble(phi_alter = c(min(DF_cor$phi_alter2):max(DF_cor$phi_alter2)),
         phi = c(max(DF_cor$phi):min(DF_cor$phi))) %>%
    left_join(.,tabela_correspondencia,by="phi") %>%
    pull(gs_name)->x_label

  # Plot

  is.litholog(basic.log)
  plot.new()
  plot.window(xlim = c(-1.0, MAX_SIZE_LOGIC + 2), ylim = c(0, MAX_PROF_LOGIC + 5))

  if (isTRUE(x_scale)) {
    minorAxis(1, at.maj = seq(0, length(x_label) - 1, 1), pos = -1, las = 2, labels = x_label)
  } else {}

  if (isTRUE(y_scale)) {
    minorAxis(2, at.maj = seq(0, MAX_PROF_LOGIC + 5, 5), n = 5, las = 1)
  } else {}

  multigons(basic.log$i, basic.log$xy, basic.log$dt, col = DF_loc$col)
  bedtext(labels = DF_loc$facies1, l = DF_loc$l, r = DF_loc$r, x = -0.5, ymin = NA, arg = list(cex = cex,srt=srt))

  # Calcular a posição central com base na escala x
  x_center <- (par()$usr[1] + par()$usr[2]) / 2

  # Adicione um título personalizado alinhado ao centro
  title <- well
  text(x = x_center, y = title_Y, labels = title, col = "black", font = 2, cex = 1.5, srt = 90, adj = c(0.5, 0.5))

}


