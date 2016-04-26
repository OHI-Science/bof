## PlotMap plots data frame values onto a map with region identifiers, and saves output
## If there are multiple goals, will loop through and map all




library(ggplot2) # install.packages('ggplot2')
library(RColorBrewer) # install.packages('RColorBrewer')

PlotMap <- function(scores          = scores, # dataframe with at least 2 columns: rgn_id and scores/values.
                    fld_value_id    = 'region_id', # likely 'rgn_id' or 'region_id' of map regions
                    fld_value_score = 'score', # likely 'score' or 'value' to display on map
                    dim_choice      = 'score', # choice of "future", "pressures", "resilience", "score", "status", "trend"
                    spatial_regions = spatial_regions,
                    path_figures    = 'reports/figures',
                    # allow fig_png to be NULL and then pass it back as a list of ggplot objects so that you could modify it more on
                    scale_label     = element_blank(),
                    scale_limits    = c(0, 100),
                    overwrite       = TRUE) {
  ## debug ## scores = scores; fld_value_id = 'region_id'; fld_value_score = 'score'; dim_choice = 'score'; spatial_regions = spatial_regions; path_figures = 'reports/figures'; scale_label = element_blank(); scale_limits = c(0, 100); overwrite = TRUE

  ## setup ----

  ## if scores variable doesn't exist, read in scores.csv
  if (!exists("scores")) scores = read.csv('scores.csv')

  ## check field values in scores column names
  if ( !fld_value_score %in% names(scores) | !fld_value_id %in% names(scores) ) {
    stop(sprintf('Column name "%s" or "%s" not found in scores variable, please modify PlotMap() function call.',
                 fld_value_score, fld_value_id))
  }

  ## see if there are multiple goals
  if( 'goal' %in% colnames(scores) ) {
    goals = unique(scores$goal)
  } else {
    goals = NULL
  }

  ## loop over each goal and subgoal ----

  for (g in goals){ # g ='AO'

    print(sprintf('Mapping %s . . .', g))
    fig_png   = sprintf('%s/map_%s.png', path_figures, g)
    map_title = sprintf('Ocean Health Index scores: %s', g)

    ## filter scores for goal g
    scores_g = scores %>%
      filter(goal == g)

    ## if exists, filter dimension for 'score'
    if ( 'dimension' %in% names(scores_g) ) {
      scores_g = scores_g %>%
        filter(dimension == dim_choice)
    }

    ## if exists, remove region_id == 0 for mapping
    # if (0 %in% scores_g[[fld_value_id]]){
    #   scores_g = scores_g[scores_g[[fld_value_id]] != 0, ] # figure this out with filter() someday
    # }

    ## join polygon with scores
    sp_regions <- spatial_regions %>%
      left_join(scores_g, by = 'region_id')


    ## plot map!
    res = 100
    if (overwrite | !file.exists(fig_png)){
      png(fig_png, width=res*7, height=res*7)

      df_plot = ggplot(data = sp_regions,
                       aes(x = long, y = lat, group = group, fill = score)) +
        theme(axis.ticks = element_blank(), axis.text = element_blank(),
              text = element_text(family = 'Helvetica', color = 'gray30', size = 12),
              plot.title = element_text(size = rel(1.5), hjust = 0, face = 'bold'),
              legend.position = 'right') +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        scale_fill_gradientn(colours = brewer.pal(10, 'RdYlBu'), na.value = 'gray80',
                             limits = scale_limits) +
        geom_polygon(color = 'gray80', size = 0.1) +
        labs(title = map_title,
             fill  = element_blank(),
             x = NULL, y = NULL)

      ## if save plots as .pngs

      print(df_plot) # try ggsave(fig_png, df_plot) someday, but was taking forever
      dev.off()

      ## else
      ## return(df_plot) -- save this into a list. create an empty list and add to it.
      # can set the name of the item in the list to be the name of the goal.

    }

  }

}
