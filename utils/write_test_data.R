devtools::load_all()

# create directory for test files
dir.create(
    here::here("data/test"),
    showWarnings = F,
    recursive = T
)

# generate a test data set and store it
testset <- generate_test_data() 

# write to data/test
testset$data %>%
    utils::write.csv(
        file = here::here("data/test/countingData.csv"),
        quote = TRUE
    )