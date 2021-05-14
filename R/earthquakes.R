`%>%` <- magrittr::`%>%`

# Data Wrangling: Loading & Cleaning --------------------------------------------------------------

#' @title
#' Returns the package based filename
#'
#' @description
#' The NOAA earthquakes data is available as an archived file having tab
#' delimed data. The function assumes the location of file within the package
#' and returns the filename for loading raw data.
#'
#' @return This function returns the string filename which if exists contain
#'   the data for the earthquakes occurrences
#'
#' @examples
#' \dontrun{make_filename()}
#'
#' # No export
make_filename <- function() {
  system.file("extdata", "earthquakes.tsv.gz", package = "eqvis")
}

# MODULE # 1A - STEPS TO FOLLOW
# The dataset is in tab-delimited format and can be read in using the read_delim() function in the readr package.
# After downloading and reading in the dataset, the overall task for this module is to

#' @title
#' Loads the Data from delimited file
#'
#' @param filename The location of the file with file name
#'
#' @return Data Frame with raw data
#'
#' @importFrom readr read_delim
#' @importFrom dplyr tbl_df
#'
#' @examples
#' eqvis::eq_load_data()
#'
#' @export
eq_load_data <- function(filename=''){
  if (filename == '')
    filename <- make_filename()

  if(!file.exists(filename))
    stop("file '", filename, "' does not exist.")

  data <- suppressMessages({
    readr::read_delim(filename, delim = "\t")
  })

  dplyr::tbl_df(data)
}

# MODULE # 1B - STEPS TO FOLLOW
# In addition, write a function eq_location_clean() that
# >>>>>cleans the LOCATION_NAME column by
# >>>>>stripping out the country name (including the colon) and
# >>>>>converts names to title case (as opposed to all caps).
# >>>>>This will be needed later for annotating visualizations.
# This function should be applied to the raw data to produce a cleaned up version of the LOCATION_NAME column.

#' @title
#' Cleans the LOCATION_NAME column
#'
#' @description
#' This function should be applied to the raw data to produce a cleaned up version
#' of the LOCATION_NAME column.It stripes out the country name (including the colon) and
#' converts the remaining location names into title case.
#'
#' @param df_raw The source data as loaded from the raw files
#'
#' @return The source data having cleaned LOCATION_NAME & factored COUNTRY name
#'
#' @importFrom tidyr separate
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_to_title
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_na
#' @importFrom magrittr "%>%"
#'
#' @examples
#' eqvis::eq_location_clean()
#'
#' @export
eq_location_clean <- function(df_raw = NULL){
  if (is.null(df_raw))
    df_raw <- eqvis::eq_load_data()

  df_raw %>%
    tidyr::separate(col="LOCATION_NAME",into=c('COUNTRY','Location','A','B'), sep=':') %>%
    dplyr::mutate(COUNTRY = as.factor(COUNTRY)) %>%
    dplyr::mutate(
      LOCATION_NAME = trimws(
        stringr::str_to_title(
          stringr::str_replace(
            paste(
              stringr::str_replace_na(Location, replacement = ""),
              stringr::str_replace_na(A, replacement = ""),
              stringr::str_replace_na(B, replacement = ""),
              sep = ':'
            ),
            pattern = "::", replacement = "")
        )
      )
    ) %>%
    dplyr::select(-Location, -A, -B)
}

# MODULE # 1C - STEPS TO FOLLOW
# write a function named eq_clean_data()that
# >>>>>takes raw NOAA data frame and
# >>>>>returns a clean data frame.
# The clean data frame should have the following:
# >>>>>A date column created by uniting the year, month, day and converting it to the Date class
# >>>>>LATITUDE and LONGITUDE columns converted to numeric class

#' @title
#' Cleans the Earthquake raw data
#'
#' @description
#' The NOAA earthquake data is available as tab delimited file. The function assumes
#' that the data is provided in raw form directly after reading the files from I/O.
#'
#' @param df_raw The source data as loaded from the raw files
#'
#' @return This function returns the cleaned data with date format based DATE, appropriate
#'    LOCATION_NAME and other selected fields after been transformed into correct data types
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom lubridate ymd
#' @importFrom stringr str_pad
#' @importFrom magrittr "%>%"
#'
#' @examples
#' eqvis::eq_clean_data()
#'
#' @export
eq_clean_data <- function(df_raw = NULL){
  if (is.null(df_raw))
    df_raw <- eqvis::eq_load_data()

  df_clean <- df_raw %>%
    eq_location_clean() %>%
    dplyr::mutate(
      DATE = lubridate::ymd(
        paste0(
          stringr::str_pad(ifelse(is.na(YEAR), 1900, YEAR), width = 4, pad = '0'),
          stringr::str_pad(ifelse(is.na(MONTH), 1, MONTH), width = 2, pad = '0'),
          stringr::str_pad(ifelse(is.na(DAY), 1, DAY), width = 2, pad = '0')
          )
        )
      ) %>%
    dplyr::mutate(YEAR = as.numeric(YEAR)) %>%
    dplyr::mutate(MONTH = as.numeric(MONTH)) %>%
    dplyr::mutate(DAY = as.numeric(DAY)) %>%
    dplyr::mutate(LATITUDE = as.numeric(LATITUDE)) %>%
    dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE)) %>%
    dplyr::mutate(TOTAL_DEATHS = as.integer(TOTAL_DEATHS)) %>%
    dplyr::mutate(EQ_PRIMARY = as.numeric(EQ_PRIMARY)) %>%
    dplyr::mutate(EQ_MAG_MW = as.numeric(EQ_MAG_MW)) %>%
    dplyr::select(
      DATE, YEAR, MONTH, DAY,
      LATITUDE, LONGITUDE, COUNTRY, LOCATION_NAME,
      INTENSITY, TOTAL_DEATHS, EQ_PRIMARY, EQ_MAG_MW
      )

  df_clean
}


# Visualization # 1: Earthquake Timeline (Geom Functions) --------------------------------------------------------------

# MODULE # 2A - STEPS TO FOLLOW
# Build a geom for ggplot2 called geom_timeline() for plotting a time line of earthquakes
# >>>>>>Ranging from xmin to xmax dates with a point for each earthquake.
# >>>>>>Optional aesthetics include color, size, and alpha (for transparency).
# >>>>>>The x aesthetic is a date and
# >>>>>>An optional y aesthetic is a factor indicating some stratification in which case multiple timelines will be plotted for each level of the factor (e.g. country).

#' @title
#' Compute Group Function for StatTimeline
#'
#' @description
#' The function filters the earthquake data rows such that the date of earthquake occurrence
#' falls between the provided minimum and maximum dates for the time line
#'
#' @param data The data based on required and optional aesthetics of geom
#' @param scales The scales of geom
#'
#' @return The function returns the data as prepared for the geom's draw panel function
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#'
#' # No export
#'
#' @examples
#' \dontrun{
#' Internal function called by Stat when geom is called
#' }
compute_group_timeline <- function(data, scales) {
  # print('Enter-CG')

  data <- tibble::as_tibble(data)
  data <- data %>%
    dplyr::filter(x>=xmin & x<=xmax) %>%
    dplyr::mutate(size)


  # print(data)

  data %>%
    dplyr::select(
      x, y, colour, size, alpha
      )
}

#' @title
#' Draw panel function for ggproto to extend GeomPoint
#'
#' @description
#' This function is called when the custom geom is been drawn on its layer. It returns the
#' PointsGrob, which contains all the points along with its attributes of shape & size.
#'
#' @param data The data that is passed after computation from compute_group function of StatTimeline
#' @param panel_scales The panel scales based on which data is transformed via coord
#' @param coord The coordinates of the ggmap. which has transform function
#'
#' @return PointsGrob object to be drawn on ggplot2 layer
#'
#' @importFrom tibble as_tibble
#' @importFrom tidyr drop_na
#' @importFrom grid gpar
#' @importFrom grid pointsGrob
#' @importFrom scales alpha
#' @importFrom magrittr "%>%"
#'
#' # No export
#'
#' @examples
#' \dontrun{
#' Internal function called by ggproto when this extended GeomPoint is called
#' }
draw_panel_timeline <- function(data, panel_scales, coord){
  # print('Enter-DP')
  # print(data)

  datapts <- tibble::as_tibble(data)
  datapts <- datapts %>%
    tidyr::drop_na()

  coords <- coord$transform(data, panel_scales)

  grid::pointsGrob(
    x = coords$x,
    y = coords$y,
    pch = coords$shape,
    size = grid::unit(coords$size,"char"),
    gp = grid::gpar(col = coords$colour,
                    fill = scales::alpha(coords$fill, coords$alpha),
                    alpha = coords$alpha
                    )
  )
}


#' StatTimeline - A new class that inherits from the Stat and computes Earthquake timeline data
#'
#' compute_group The function that prepares the data based on which geometric object is drawn
#'
#' Required Aesthetics:
#' xmin The MINIMUM date for the earthquake timeline
#' xmax The MAXIMUM date for the earthquake timeline
#' colour The border color for point. Default is GREEN
#' size The size of the point. Default is 2
#' alpha The transparency value for fill color. Default is 0.6
#'
#' Default Aesthetics:
#' y A factor indicating some stratification in which case multiple timelines will be plotted
#'   for each level of the factor (e.g. country).
StatTimeline <- ggplot2::ggproto("StatTimeline",
                                 ggplot2::Stat,
                                 compute_group = compute_group_timeline,
                                 required_aes = c("xmin","xmax","colour","size","alpha"),
                                 default_aes = ggplot2::aes(y=1,
                                                            # colour = "green",
                                                            # size = 2,
                                                            # alpha = 0.6
                                                            )
)

#' @title
#' Uses StatTimeline as stat for generating new ggplot2 Layer
#'
#' @description
#' Each ggplot2 layer provides stat parameter, that can perform required computation
#' on data in order to provide the same to next layer. This function passes StatTimeline,
#' the parameterized ggproto function which uses compute_group_timeline function for
#' earthquake data filtering on min and max values.
#'
#' @param mapping Set of aesthetic mappings created by aes() or aes_(). If specified and
#' inherit.aes = TRUE (the default), it is combined with the default mapping at the top
#' level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#' If NULL, the default, the data is inherited from the plot data as specified
#' in the call to ggplot().
#' A data.frame, or other object, will override the plot data. All objects will be fortified
#' to produce a data frame. See fortify() for which variables will be created.
#' A function will be called with a single argument, the plot data. The return value must be
#' a data.frame, and will be used as the layer data. A function can be created from a
#' formula (e.g. ~ head(.x, 10)).
#' @param geom The geometric object to use display the data on ggplot layer
#' @param position Position adjustment, either as a string, or the result of a call to a
#' position adjustment function, as required for ggplot layer
#' @param show.legend Logical indicator to show the Key Legend on ggplot layer
#' @param inherit.aes Logical indicator whether aesthetics to be inherited from prev layer
#' @param na.rm Logical indicator whether NA to be removed
#' @param outliers Logical
#' @param ... Additional parameters to the stat layer
#'
#' @return Returns the ggplot2::layer Object
#'
#' @importFrom ggplot2 layer
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' Function not used but defined for compatibility and future use
#' }
stat_timeline <- function(mapping = NULL,
                           data = NULL,
                           geom = "point",
                           position = "identity",
                           show.legend = NA,
                           inherit.aes = TRUE,
                           na.rm = FALSE,
                           outliers = TRUE,
                           ...) {
  ggplot2::layer(
    stat = StatTimeline,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  outliers = outliers
                  , ...)
  )
}

#' GeomTimeline - A new class that inherits from the GeomPoint and shows Earthquake timeline
#'
#' Required Aesthetics:
#' x The DATE when the earthquake occurs
#' xmin The MINIMUM date for the earthquake timeline
#' xmax The MAXIMUM date for the earthquake timeline
#'
#' Default Aesthetics:
#' y A factor indicating some stratification in which case multiple timelines will be plotted
#'   for each level of the factor (e.g. country).
#' colour The border color for point. Default is GREEN
#' fill The fill color for point. Default is RED.
#' size The size of the point. Default is 2
#' shape The shape of the point. Default is 2
#' alpha The transparency value for fill color. Default is 0.6
#' draw_key The key value for legend. Default is ggplor2::draw_key_point
#' draw_panel The function that returns the PointsGrob based on data passed
GeomTimeline <- ggplot2::ggproto("GeomTimeline",
                                  ggplot2::GeomPoint,
                                  required_aes = c("x","xmin","xmax"),
                                  default_aes = ggplot2::aes(y=1,
                                                             colour = "green",
                                                             fill = "red",
                                                             size = 2,
                                                             shape = 2,
                                                             alpha = 0.6
                                                             )
                                  ,draw_key = ggplot2::draw_key_point
                                  ,draw_panel = draw_panel_timeline
)

#' @title
#' The main function for GeomPoint extension for Earthquake Timelines visualization
#'
#' @param mapping The mapping of aesthetics with data columns
#' @param data The data that needs to be visualized
#' @param stat Has default value as identity
#' @param position Has default value as identity
#' @param show.legend TRUE indicates legend has to be shown
#' @param inherit.aes TRUE indicates that aesthetics are inherited from base layer
#' @param na.rm FALSE indicates that the NA values needs to be displayed
#' @param ... Any other parameters
#'
#' @return
#'
#' @importFrom ggplot2 layer
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eqvis::eq_load_data() %>%
#' eqvis::eq_clean_data() %>%
#' dplyr::filter(COUNTRY %in% c("TURKEY","CHINA") & lubridate::year(DATE) >= 2000 & lubridate::year(DATE) < 2022 & EQ_MAG_MW>=0) %>%
#' ggplot2::ggplot(mapping = aes(x=DATE, y=COUNTRY)) +
#' eqvis::geom_timeline(ggplot2::aes(xmin=lubridate::ymd(20000101),
#'                           xmax=lubridate::ymd(20220101),
#'                           size=EQ_MAG_MW,
#'                           color=TOTAL_DEATHS,
#'                           shape=COUNTRY,
#'                           alpha=0.6
#' ))
#' }
geom_timeline <- function(mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           show.legend = TRUE,
                           inherit.aes = TRUE,
                           na.rm = FALSE,
                           ...) {
  ggplot2::layer(
    geom = GeomTimeline,
    mapping = mapping,
    data = data,
    stat = StatTimeline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



# Visualization # 2: Earthquake Timeline With Labels (Geom Functions) --------------------------------------------------------------
# MODULE # 2B - STEPS TO FOLLOW
# Build a geom called geom_timeline_label() for adding annotations to the earthquake data.
# >>>>>>>>This geom adds a vertical line to each data point with a text annotation (e.g. the location of the earthquake) attached to each line.
# >>>>>>>>There should be an option to subset to n_max number of earthquakes (ref), where we take the n_max largest (by magnitude) earthquakes.
# >>>>>>>>Aesthetics are x, which is the date of the earthquake and label which takes the column name from which annotations will be obtained.


#' @title
#' Compute Group Function for StatTimelineLabel
#'
#' @description
#' The function filters the earthquake data rows such that the magnitude of earthquake
#' is greater than or equal to the maximum
#'
#' @param data The data based on required and optional aesthetics of geom
#' @param scales The scales of geom
#' @param n_max Number of largest earthquakes (by magnitude), for which labels should be displayed
#'
#' @return The function returns the data which can be used by geom's draw panel function
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#'
#' # No export
#'
#' @examples
#' \dontrun{
#' Internal function called by Stat when geom is called
#' }
compute_group_timelinelabel <- function(data, scales, n_max=4) {
  # print('compute_group_timelinelabel')
  # print(data)

  data <- tibble::as_tibble(data)
  # data <- data %>%
  #   dplyr::filter(size>=n_max)

  data <- data %>%
    # dplyr::group_by(size) %>%
    dplyr::arrange(desc(size), .by_group = TRUE) %>%
    dplyr::mutate(eq_seq = 1:dplyr::n()) %>%
    dplyr::filter(eq_seq <= n_max)

  # print(data)

  data %>%
    dplyr::select(x, y, colour, size, alpha)

  data
}


#' @title
#' Draw panel function for ggproto to extend Geom class
#'
#' @description
#' This function is called when the custom geom function is called to draw trees of new
#' geometric objects on a new layer. It returns the gtree object composed of segmentGrob
#' and textGrob for the filtered earthquakes that are top largest in magnitudes
#'
#' @param data The data that is passed after computation from compute_group function of StatTimelineLabel
#' @param panel_scales The panel scales based on which data is transformed via coord
#' @param coord The coordinates of the ggmap. which has the transform function
#'
#' @return gtree object with 2 children segment & text to show the earthquakes label
#'
#' @importFrom tibble as_tibble
#' @importFrom tidyr drop_na
#' @importFrom grid segmentsGrob
#' @importFrom grid gpar
#' @importFrom grid textGrob
#' @importFrom magrittr "%>%"
#'
#' # No export
#'
#' @examples
#' \dontrun{
#' Internal function called by ggproto when this extended Geom is called
#' }
draw_panel_timelinelabel <- function(data, panel_scales, coord){
  # print('draw_panel_timelinelabel')
  # print(data)

  datapts <- tibble::as_tibble(data)
  datapts <- datapts %>%
    tidyr::drop_na()

  coords <- coord$transform(data, panel_scales)
  # print(coords)

  lbl_seg <- grid::segmentsGrob(
    x0 = coords$x,
    y0 = coords$y,
    x1 = coords$x,
    y1 = coords$y + 0.1,
    gp = grid::gpar(col = "grey")
  )

  # print(grid::unit(coords$y,"npc"))

  lbl_text <- grid::textGrob(
    x = coords$x,
    y = coords$y + 0.1,
    label = coords$label,
    rot = 45,
    just = "left"
  )

  grid::gTree(children = grid::gList(
                                    lbl_seg,
                                    lbl_text
                                    )
              )
}

#' StatTimelineLabel - A new class that inherits from the Stat and computes top largest Earthquakes for labeling
#'
#' compute_group The function that prepares the data based on which geometric object is drawn
#'
#' Required Aesthetics:
#' xmin The MINIMUM date for the earthquake timeline
#' xmax The MAXIMUM date for the earthquake timeline
#' colour The border color for point. Default is GREEN
#' size The size of the point. Default is 2
#' alpha The transparency value for fill color. Default is 0.6
#'
#' Default Aesthetics:
#' y A factor indicating some stratification in which case multiple timelines will be plotted
#'   for each level of the factor (e.g. country).
#'
#' Extra Parameters:
#' na.rm Logical indicator for whether NA values need to be removed or not
#' n_max Number of total earthquakes, largest (by magnitude), which needs to be labeled
StatTimelineLabel <- ggplot2::ggproto("StatTimelineLabel",
                                 ggplot2::Stat,
                                 compute_group = compute_group_timelinelabel,
                                 required_aes = c("xmin",
                                                  "xmax",
                                                  "colour",
                                                  "size",
                                                  "alpha"
                                                  ),
                                 default_aes = ggplot2::aes(y=1,
                                                            ),
                                 extra_params = c("na.rm","n_max")
)

#' @title
#' Uses StatTimelineLabel as stat for generating new ggplot2 Layer
#'
#' @description
#' Each ggplot2 layer provides stat parameter, that can perform required computation
#' on data in order to provide the same to next layer. This function passes StatTimelineLabel,
#' the parameterized ggproto function, which uses compute_group_timelinelabel function for
#' earthquake data that selects the top n_max largest earthquakes (by magnitude).
#'
#' @param mapping Set of aesthetic mappings created by aes() or aes_(). If specified and
#' inherit.aes = TRUE (the default), it is combined with the default mapping at the top
#' level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#' If NULL, the default, the data is inherited from the plot data as specified
#' in the call to ggplot().
#' A data.frame, or other object, will override the plot data. All objects will be fortified
#' to produce a data frame. See fortify() for which variables will be created.
#' A function will be called with a single argument, the plot data. The return value must be
#' a data.frame, and will be used as the layer data. A function can be created from a
#' formula (e.g. ~ head(.x, 10)).
#' @param geom The geometric object to use display the data on ggplot layer
#' @param position Position adjustment, either as a string, or the result of a call to a
#' position adjustment function, as required for ggplot layer
#' @param show.legend Logical indicator to show the Key Legend on ggplot layer
#' @param inherit.aes Logical indicator whether aesthetics to be inherited from prev layer
#' @param na.rm Logical indicator whether NA to be removed
#' @param n_max The top n_max largest earthquakes (by magnitude) that should be selected
#' @param ... Additional parameters to the stat layer
#'
#' @return Returns the ggplot2::layer Object
#'
#' @importFrom ggplot2 layer
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' Function not used but defined for compatibility and future use
#' }
stat_timeline_label <- function(mapping = NULL,
                           data = NULL,
                           geom = "point",
                           position = "identity",
                           show.legend = NA,
                           inherit.aes = TRUE,
                           na.rm = FALSE,
                           n_max = 4,
                           ...) {
  ggplot2::layer(
    stat = StatTimelineLabel,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  n_max = n_max
                  , ...)
  )
}

#' GeomTimelineLabel - A new class that inherits from the Geom and shows Labels for largest Earthquake
#'
#' Required Aesthetics:
#' x The DATE when the earthquake occurs
#' xmin The MINIMUM date for the earthquake timeline
#' xmax The MAXIMUM date for the earthquake timeline
#' label The LABEL text that should be shown
#'
#' Default Aesthetics:
#' y A factor indicating some stratification in which case multiple timelines will be plotted
#'   for each level of the factor (e.g. country).
#' colour The border color for point. Default is GREEN
#' fill The fill color for point. Default is RED.
#' size The size of the point. Default is 2
#' shape The shape of the point. Default is 2
#' alpha The transparency value for fill color. Default is 0.6
#'
#' draw_key The key value for legend. Default is ggplor2::draw_key_blank i-e no Legend
#' draw_panel The function that returns the gtree with SegmentGrob and TextGrob to show label
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel",
                                 ggplot2::Geom,
                                 required_aes = c("x",
                                                  "xmin",
                                                  "xmax",
                                                  "label"
                                                  ),
                                 default_aes = ggplot2::aes(y=1,
                                                            colour = "green",
                                                            fill = "red",
                                                            size = 2,
                                                            shape = 2,
                                                            alpha = 0.6
                                 ),
                                 draw_key = ggplot2::draw_key_blank,
                                 draw_panel = draw_panel_timelinelabel,
                                 extra_params = c("na.rm","n_max")
)

#' @title
#' The main function for Geom extension that adds Earthquake labels on Timelines
#'
#' @param mapping The mapping of aesthetics with data columns
#' @param data The data that needs to be visualized
#' @param stat Has default value as identity
#' @param position Has default value as identity
#' @param show.legend TRUE indicates legend has to be shown
#' @param inherit.aes TRUE indicates that aesthetics are inherited from base layer
#' @param na.rm FALSE indicates that the NA values needs to be displayed
#' @param n_max The total number of earthquakes (largest by magnitude) that needs to be labeled
#' @param ... Any other parameters
#'
#' @return New ggplot2 Layer having values that refer to StatTimelineLabel & GeomTimelineLabel
#'
#' @importFrom ggplot2 layer
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eqvis::eq_load_data() %>%
#' eqvis::eq_clean_data() %>%
#' dplyr::filter(COUNTRY %in% c("TURKEY","CHINA") & lubridate::year(DATE) >= 2000 & lubridate::year(DATE) < 2022 & EQ_MAG_MW>=0) %>%
#' ggplot2::ggplot(mapping = aes(x=DATE, y=COUNTRY)) +
#' eqvis::geom_timeline(ggplot2::aes(xmin=lubridate::ymd(20000101),
#'                           xmax=lubridate::ymd(20220101),
#'                           size=EQ_MAG_MW,
#'                           color=TOTAL_DEATHS,
#'                           shape=COUNTRY,
#'                           alpha=0.6
#' )) +
#' eqvis::geom_timeline_label(ggplot2::aes(xmin=lubridate::ymd(20000101),
#'                           xmax=lubridate::ymd(20220101),
#'                           size=EQ_PRIMARY,
#'                           color=TOTAL_DEATHS,
#'                           shape=COUNTRY,
#'                           alpha=0.6,
#'                           label=LOCATION_NAME
#'                           ),
#'                    n_max=4
#'                    )
#' }
geom_timeline_label <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          show.legend = FALSE,
                          inherit.aes = TRUE,
                          na.rm = FALSE,
                          n_max = 4,
                          ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel,
    mapping = mapping,
    data = data,
    stat = StatTimelineLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  n_max = n_max,
                  ...)
  )
}



# Visualization # 3: Earthquake Mappings with interactive Labels (Leaflet Marker Function)\ --------------------------------------------------------------

# MODULE # 3A - STEPS TO FOLLOW
# Create a function called eq_create_label() that
# >>>>>>takes the dataset as an argument and
# creates an HTML label that can be used as the annotation text in the leaflet map.
# This function should put together a character string for each earthquake that will show
# the cleaned location (as cleaned by the eq_location_clean() function created in Module 1),
# the magnitude (EQ_PRIMARY), and
# the total number of deaths (TOTAL_DEATHS),
# with boldface labels for each ("Location", "Total deaths", and "Magnitude").
# If an earthquake is missing values for any of these, both the label and the value should be skipped for that element of the tag.

#' @title
#' Creates an HTML label that can be used as the annotation text in the leaflet map.
#'
#' @description
#' This function puts together a character string for each earthquake that will show the
#' cleaned location (using eq_location_clean), the magnitude (EQ_PRIMARY), and
#' the total number of deaths (TOTAL_DEATHS). Also, it adds boldface labels for each
#' ("Location: ", "Total deaths: ", and "Magnitude: "). If an earthquake is missing values
#' for any of these, both the label and the value is skipped for that element of the tag.
#'
#' @param df The clean data as returned from eq_clean_data
#'
#' @return A vector that consists of HTML labels for each earthquake
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#' @examples
#' eqvis::eq_load_data() %>%
#'   eqvis::eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   dplyr::select("DATE","LATITUDE","LONGITUDE","LOCATION_NAME","TOTAL_DEATHS","EQ_PRIMARY","popup_text")
eq_create_label <- function(df = NULL){
  if (is.null(df))
    df <- eqvis::eq_clean_data()

  as.vector(
    unlist(
      df %>%
        dplyr::select("LOCATION_NAME","TOTAL_DEATHS","EQ_PRIMARY") %>%
        dplyr::mutate(LABEL = paste0(
          ifelse(is.na(LOCATION_NAME),"",paste0("<b>Location: </b>",LOCATION_NAME,"<br/>")),
          ifelse(is.na(EQ_PRIMARY),"",paste0("<b>Magnitude: </b>",EQ_PRIMARY,"<br/>")),
          ifelse(is.na(TOTAL_DEATHS),"",paste0("<b>Total deaths: </b>",TOTAL_DEATHS,"<br/>"))
          )
          ) %>%
        dplyr::select("LABEL")
      )
    )
}

# MODULE # 3B - STEPS TO FOLLOW
# Build a function called eq_map() that
# >>>>>takes an argument data containing the filtered data frame with earthquakes to visualize.
# >>>>>The function maps the epicenters (LATITUDE/LONGITUDE) and
# >>>>>Annotates each point within a pop up window containing annotation data stored in a column of the data frame.
# >>>>>The user should be able to choose which column is used for the annotation in the pop-up with a function argument named annot_col.
# >>>>>Each earthquake should be shown
# >>>>>>>>>>with a circle, and
# >>>>>>>>>>the radius of the circle should be proportional to the earthquake's magnitude (EQ_PRIMARY).


#' @title
#' Uses leaflet functions to map the epicenters (LATITUDE/LONGITUDE) and also to annotate
#' each point with a pop up window.
#'
#' @param df_map The cleaned earthquake data set, having the column with name matching the value of annot_col
#' @param annot_col The name of the column that contains HTML text to be displayed in popup
#'
#' @return A leaflet map widget showing the epicenters along with popup text
#'
#' @importFrom leaflet colorFactor
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#' @importFrom viridis viridis
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#' @examples
#' eqvis::eq_load_data() %>%
#'   eqvis::eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   dplyr::mutate(popup_text = eqvis::eq_create_label(.)) %>%
#'   eqvis::eq_map(annot_col = "popup_text")
eq_map <- function(df_map = NULL, annot_col = "DATE"){
  if (is.null(df_map))
    df_map <- eqvis::eq_clean_data() %>%
      dplyr::filter(COUNTRY %in% c('MEXICO') & lubridate::year(DATE) >= 2000)

  pal <- leaflet::colorFactor(viridis::viridis(5), df_map$TOTAL_DEATHS)

  df_map %>%
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(#data = df_map,
      radius = ~ ifelse(is.na(EQ_PRIMARY), 2, EQ_PRIMARY),
      lng = ~ LONGITUDE, lat = ~ LATITUDE,
      popup = ~ paste(df_map[[annot_col]]),
      # color = ~ pal(TOTAL_DEATHS)
      stroke = TRUE, fillOpacity =  0.1,
      weight = 2,
    )
    # ) %>%
    # addLegend(pal = pal, values = df_map$TOTAL_DEATHS)
}


# Generate Results --------------------------------------------------------------

#' @title
#' Plots the Timelines and the popup text for earthquakes largest in size
#'
#' @param countries Character vectors of country names
#' @param minymd Integer Year Month Day of timeline start
#' @param maxymd Integer Year Month Day of timeline end
#' @param mineqmag Numeric minimum magnitude of earthquakes to be ploted
#'
#' @return Visualization in form of gemetric objects plotted on 2d chart
#' @export
#'
#' @examples
#' eqvis::eq_timeline()
eq_timeline <- function(countries = c("TURKEY","CHINA"),
                                   minymd = 20000101,
                                   maxymd = 20220101,
                                   mineqmag = 0
){
  make_filename() %>%
    eqvis::eq_load_data() %>%
    eqvis::eq_clean_data() %>%
    dplyr::filter(COUNTRY %in% countries &
                    # lubridate::year(DATE) >= minyear & lubridate::year(DATE) < maxyear &
                    EQ_PRIMARY>=mineqmag
    ) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x=DATE, y=COUNTRY)) +
    eqvis::geom_timeline(ggplot2::aes(xmin=lubridate::ymd(minymd),
                                      xmax=lubridate::ymd(maxymd),
                                      size=EQ_PRIMARY,
                                      color=TOTAL_DEATHS,
                                      shape=COUNTRY,
                                      alpha=0.6
    ))+
    ggplot2::scale_fill_manual(values=c("blue", "red"))
}


#' @title
#' Plots the Timelines and the popup text for earthquakes largest in size
#'
#' @param countries Character vectors of country names
#' @param minymd Integer Year Month Day of timeline start
#' @param maxymd Integer Year Month Day of timeline end
#' @param mineqmag Numeric minimum magnitude of earthquakes to be ploted
#'
#' @return Visualization in form of gemetric objects plotted on 2d chart
#' @export
#'
#' @examples
#' eqvis::eq_timeline_and_labels()
eq_timeline_and_labels <- function(countries = c("TURKEY","CHINA"),
                                   minyear = 2000,
                                   maxyear = 2022,
                                   minymd = 20000101,
                                   maxymd = 20220101,
                                   mineqmag = 0,
                                   toteqlbls = 4
                                   ){
  make_filename() %>%
    eqvis::eq_load_data() %>%
    eqvis::eq_clean_data() %>%
    dplyr::filter(COUNTRY %in% countries &
                    lubridate::year(DATE) >= minyear & lubridate::year(DATE) < maxyear &
                    EQ_PRIMARY>=mineqmag
    ) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x=DATE, y=COUNTRY)) +
    eqvis::geom_timeline(ggplot2::aes(xmin=lubridate::ymd(minymd),
                                      xmax=lubridate::ymd(maxymd),
                                      size=EQ_PRIMARY,
                                      color=TOTAL_DEATHS,
                                      shape=COUNTRY,
                                      alpha=0.6
    ))+
    eqvis::geom_timeline_label(ggplot2::aes(xmin=lubridate::ymd(minymd),
                                         xmax=lubridate::ymd(maxymd),
                                         size=EQ_PRIMARY,
                                         color=TOTAL_DEATHS,
                                         shape=COUNTRY,
                                         alpha=0.6,
                                         label=LOCATION_NAME
          ),
          n_max=4
        ) +
    ggplot2::scale_fill_manual(values=c("blue", "red"))
}

#' @title
#' Map Earthquake Locations showing the magnitude and dates of occurrences
#'
#' @param countries Character vectors of country names
#' @param minyear Integer representing minimum year
#'
#' @return Map showing Earthquake locations along with popup text
#' @export
#'
#' @examples
#' eq_map_with_datelabels()
eq_map_with_datelabels <- function(countries = c("MEXICO"),
                                   minyear = 2000
                                   ){
  make_filename() %>%
    eqvis::eq_load_data() %>%
    eqvis::eq_clean_data() %>%
    dplyr::filter(COUNTRY %in% countries & lubridate::year(DATE) >= minyear) %>%
    dplyr::mutate(popup_text = DATE) %>%
    eqvis::eq_map(annot_col = "DATE")
}

#' @title
#' Map Earthquake Locations along with popup for key data formatted with HTML
#'
#' @param countries Character vectors of country names
#' @param minyear Integer representing minimum year
#'
#' @return Map showing Earthquakes along with popup for Location, Magnitude & Total Deaths
#' @export
#'
#' @examples
#' eq_map_with_htmllabels()
eq_map_with_htmllabels <- function(countries = c("MEXICO"),
                                   minyear = 2000
                                   ){
  make_filename() %>%
    eqvis::eq_load_data() %>%
    eqvis::eq_clean_data() %>%
    dplyr::filter(COUNTRY %in% countries & lubridate::year(DATE) >= minyear) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eqvis::eq_map(annot_col = "popup_text")
}

# grid::grid.draw(eq_timeline)
# eq_map_test1
# eq_map_test2

