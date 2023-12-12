#' Generate testing data in data/test
#' 
#' @importFrom magrittr %>%
generate_test_data <- function(
    n_species = 3,
    n_years = 10,
    n_sites = 10, # site
    n_points = 5, # points per site
    habitats = c("A", "B", "C")
) {
    # draw species intercepts (a) and slopes (b)
    df_species  <- data.frame(
        species = 1:3
    ) %>%
    dplyr::mutate(
        a = rnorm(n_species, mean = log(5), sd = 0.1),
        b = rnorm(n_species, mean = 0, sd = 0.05)
    ) %>%
    dplyr::mutate(
        species = paste0("SP", species)
    )

    # ensure at least 1 species with a different trend
    if (all(sign(df_species$b) == sign(df_species$b[1]))) {
        df_species$b[1] <- - (df_species$b[1])
    }

    # draw site-level intercept
    df_sites  <- data.frame(
        expand.grid(
            species = 1:n_species,
            site = 1:n_sites
        )
    ) %>%
    dplyr::mutate(
        a_site = rnorm(nrow(.), mean = 0, sd = 0.1),
        species = paste0("SP", species)
    )

    # draw point intercepts and slopes
    df_points <- data.frame(
        expand.grid(
            species = 1:n_species,
            site = 1:n_sites,
            point = 1:n_points
        )
    ) %>%
    dplyr::mutate(
        a_point = rnorm(nrow(.), mean = 0, sd = 0.05),
        b_point = rnorm(nrow(.), mean = 0, sd = 0.025)
    ) %>%
    dplyr::mutate(
        species = paste0("SP", species)
    )

    # draw habitat by points
    df_habitats  <- data.frame(
        expand.grid(
            site = 1:n_sites,
            point = 1:n_points
        )
    ) %>%
    dplyr::mutate(
        habitat = sample(habitats, nrow(.), replace = T)
    )

    # draw habitat fixed effect
    df_a_habitat <- data.frame(
        a_h = rnorm(length(habitats), mean = 0, sd = 0.1),
        habitat = habitats
    )
    
    df_habitats <-  df_habitats %>%
        dplyr::left_join(
            df_a_habitat,
            by = "habitat"
        )

    parameters <- data.frame(
        expand.grid(
            species = 1:n_species,
            year = 1:n_years,
            site = 1:n_sites,
            point = 1:n_points
        )
    ) %>%
        dplyr::mutate(
            species = paste0("SP", species)
        ) %>%
    dplyr::left_join( # join species-level parameters
        df_species,
        by = "species"
    ) %>%
    dplyr::left_join( # join site-level parameters
        df_sites,
        by = c("species", "site"),
    ) %>% # join point-level parameters
    dplyr::left_join(
        df_habitats,
        by = c("site", "point")
    ) %>%
    dplyr::left_join(
        df_points,
        by = c("species", "site", "point")
    ) %>%
    dplyr::mutate( # draw residuals
        e = rnorm(nrow(.), mean = 0, sd = 0.25),
        lambda = exp(
            a + (b + b_point) * year + a_site + a_point + e + a_h
        )
    )

    output <- parameters %>%
        dplyr::mutate(
            abundance = stats::rpois(nrow(.), lambda = lambda)
        ) %>%
        dplyr::select(
            c(species, year, site, point, habitat, abundance)
        )

    return(
        list(
            data = output,
            parameters = parameters
        )
    )
}
