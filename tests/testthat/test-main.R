# testthat tests are meant for internal functions
# in the package (contents of the R/ folder).
#
# They are not meant to run the whole pipeline as a test
# because when testthat creates a separate environment for
# testing, files outside of test/ and outside of R/ are NOT
# copied.
#
# The "testing" step of continuous integration therefore
# needs to be followed by a "make" stage, to check that
# the pipeline can actually be run with the current version
# of the (tested) package.
#
# While testing, devtools loads the functions defined in
# tests/testthat/helper.R.
# Before running the tests, it also sources the contents
# of tests/testthat/setup.R.

# placeholder test for now
test_that("Addition works", {
    expect_equal(2+3, 5)
})