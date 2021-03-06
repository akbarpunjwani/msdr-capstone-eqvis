# Test: make_filename()
testthat::test_that("Make filename returns character value and has package path", {
  testthat::expect_equal(class(eqvis::make_filename()), "character")
  testthat::expect_equal(length(strsplit(eqvis::make_filename(),split = path.package())[[1]]), 2)
})

# Test: eq_load_data()
testthat::test_that("raw data have correct shape", {
  testthat::expect_equal(dim(eqvis::eq_load_data()), c(5937,47))
})

# Test: eq_location_clean()
testthat::test_that("data with cleaned location have correct shape", {
  testthat::expect_equal(dim(eqvis::eq_location_clean()), dim(eqvis::eq_load_data()))
})

# Test: eq_clean_data()
testthat::test_that("cleaned data have correct shape", {
  testthat::expect_equal(length(eqvis::eq_clean_data()[[1]]), dim(eqvis::eq_load_data())[1])
  testthat::expect_equal(is.element(c('DATE', 'YEAR', 'MONTH', 'DAY',
                                      'LATITUDE', 'LONGITUDE', 'COUNTRY', 'LOCATION_NAME',
                                      'INTENSITY', 'TOTAL_DEATHS', 'EQ_PRIMARY', 'EQ_MAG_MW')
                                  , colnames(eqvis::eq_clean_data())
                          ),rep(TRUE,12)
                         )
})

# Test: stat_timeline()
testthat::test_that("stat_timeline have correct stat class and aesthetics", {
  testthat::expect_equal(class(stat_timeline()$stat)[1], "StatTimeline")
  testthat::expect_equal(
    is.element(
      c("xmin", "xmax", "colour", "size", "alpha"),
      stat_timeline()$stat$required_aes
      ),rep(TRUE,5)
    )
})

# Test: geom_timeline()
# Test: compute_group_timeline()
# Test: draw_panel_timeline()
testthat::test_that("geom_timeline have correct geom class, stat class and aesthetics", {
  testthat::expect_equal(class(geom_timeline()$geom)[1], "GeomTimeline")
  testthat::expect_equal(
    is.element(
      c("x", "xmin", "xmax"),
      geom_timeline()$geom$required_aes
    ),rep(TRUE,3)
  )
  testthat::expect_equal(class(geom_timeline()$stat)[1], "StatTimeline")
  testthat::expect_equal(
    is.element(
      c("xmin", "xmax", "colour", "size", "alpha"),
      geom_timeline()$stat$required_aes
    ),rep(TRUE,5)
  )
})

# Test: stat_timeline_label()
testthat::test_that("stat_timeline_label have correct stat class and aesthetics", {
  testthat::expect_equal(class(stat_timeline_label()$stat)[1], "StatTimelineLabel")
  testthat::expect_equal(is.element(
      c("xmin", "xmax", "colour", "size", "alpha"),
      stat_timeline()$stat$required_aes
    ),rep(TRUE,5)
  )
  testthat::expect_equal(is.element(
      c("n_max"),
      stat_timeline_label()$stat$extra_params
    ),rep(TRUE,1)
  )
})

# Test: geom_timeline_label()
# Test: compute_group_timelinelabel()
# Test: draw_panel_timelinelabel()
testthat::test_that("geom_timeline_label have correct geom class, stat class and aesthetics", {
  testthat::expect_equal(class(geom_timeline_label()$geom)[1], "GeomTimelineLabel")
  testthat::expect_equal(
    is.element(
      c("x", "xmin", "xmax", "label"),
      geom_timeline_label()$geom$required_aes
    ),rep(TRUE,4)
  )
  testthat::expect_equal(class(geom_timeline_label()$stat)[1], "StatTimelineLabel")
  testthat::expect_equal(
    is.element(
      c("xmin", "xmax", "colour", "size", "alpha"),
      geom_timeline_label()$stat$required_aes
    ),rep(TRUE,5)
  )
})

# Test: eq_timeline()
testthat::test_that("Sample results of Geom Test have correct attributes", {
  testthat::expect_equal(length(ggplot2::ggplot_build(eq_timeline())$data), 1)
  testthat::expect_equal(
    is.element(
      c("size", "x", "y", "xmin", "xmax"),
      colnames(ggplot2::ggplot_build(eq_timeline())$data[[1]])
    ),rep(TRUE,5)
  )
})

# Test: eq_timeline_and_labels()
testthat::test_that("Sample results of Geom with Labels have correct attributes", {
  testthat::expect_equal(length(ggplot2::ggplot_build(eq_timeline_and_labels())$data), 2)
  testthat::expect_equal(
    is.element(
      c("size", "x", "y", "xmin", "xmax", "label", "eq_seq"),
      colnames(ggplot2::ggplot_build(eq_timeline_and_labels())$data[[2]])
    ),rep(TRUE,7)
  )
})

# Test: eq_create_label()
testthat::test_that("HTML Labels for popup text have correct shape", {
  testthat::expect_equal(length(eqvis::eq_create_label(eqvis::eq_clean_data())), dim(eqvis::eq_load_data())[1])
})

# Test: eq_map()
testthat::test_that("Map object have correct class of Leaflet HTML Widget", {
  testthat::expect_equal(class(eq_map()), c('leaflet','htmlwidget'))
})

# Test: eq_map_with_datelabels()
testthat::test_that("Map object with popup showing correct type as Date", {
  testthat::expect_equal(
    class(
      attr(
        eq_map_with_datelabels()$x,
        "leafletData"
        )$popup_text
      ),
    "Date")
})

# Test: eq_map_with_htmllabels()
testthat::test_that("Map object with popup showing correct type as Character having HTML labels", {
  testthat::expect_equal(
    class(
      attr(
        eq_map_with_htmllabels()$x,
        "leafletData"
      )$popup_text
    ),
    "character")
})
