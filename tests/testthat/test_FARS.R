test_that("function test", {
   curr_working_dir <- getwd()
   test_filename <- make_filename(2013)
   expect_that(test_filename, is_a("character"))
   expect_equal(nchar(test_filename), 21)
   test_file <- system.file("extdata", test_filename, package="RPackageCoursera")
   setwd(system.file("extdata", package = "RPackageCoursera"))
   if(file.exists(test_file)) {
      fars_data <- fars_read(test_file)
      expect_that(dplyr::is.tbl(fars_data), is_true())
   }
   if(file.exists("accident_2013.csv.bz2") & file.exists("accident_2014.csv.bz2")) {
      fars_summary <- fars_summarize_years(c(2013, 2014))
      expect_that(dplyr::is.tbl(fars_summary), is_true())
      expect_equal(dim(fars_summary), c(12, 3))
   }
   setwd(curr_working_dir)
})