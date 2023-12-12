# create directory
dir.create(
    here::here("tests/testthat/data/test"),
    showWarnings = F,
    recursive = T
)

# generate a test data set and store it
testset <- generate_test_data() 

# store countingData.csv in two places :
# - tests/testthat/data/test for use while
# devtools::check()ing the package
testset$data %>%
    utils::write.csv(
        file = here::here("tests/testthat/data/test/countingData.csv"),
        quote = TRUE
    )

# data-generation parameters are not used by the
# normal pipeline, we store them in tests/testthat only
testset$parameters %>%
    utils::write.csv(
        file = here::here("tests/testthat/data/test/parameters.csv"),
        quote = TRUE
    )

# clean testset out of the local environment
withr::defer(testset, testthat::teardown_env())