source_lookup <- function(obs_name, obs_id, obs_tmp_df) {
    if (obs_id != "scene") {
        res <- obs_name
    } else {
        # filter by supplied name to grab sources
        name_tmp <- obs_name
        obs_tmp1 <- filter(obs_tmp_df, name == !!name_tmp) %>%
          tidyr::unnest(sources)

        obs_tmp2 <- filter(obs_tmp_df, name %in% obs_tmp1$sources)

        res <- purrr::map2(obs_tmp2$name, obs_tmp2$id, ~source_lookup(obs_name = .x, obs_id = .y, obs_tmp_df = obs_tmp_df))
    }

    return(res)
}

create_obs_df <- function(json_file) {
    # import scene collection json file
    obs_json <- fromJSON(json_file, simplifyVector = FALSE)

    # goal: obtain the sources within each scene

    # this returns a list of 73 names
    scene_order <- obs_json$scene_order

    # this returns a list of all sources (including scenes that next sources and other scenes)
    sources_tmp <- obs_json$sources

    obs_key_df <- tibble::tibble(
        name = purrr::map_chr(sources_tmp, "name"),
        id = purrr::map_chr(sources_tmp, "id"),
        sources = purrr::map(sources_tmp, ~{
            # grab settings object
            settings <- .[["settings"]]

            if ("items" %in% names(settings)) {
                if (length(settings$items) > 0) {
                    res <- purrr::map_chr(settings$items, "name")
                } else {
                    res <- NA
                }
            } else {
                res <- NA
            }

            return(res)

        }))

    # goal: create a tibble to organize better
    obs_df <- tibble::tibble(
        name = purrr::map_chr(sources_tmp, "name"),
        id = purrr::map_chr(sources_tmp, "id"),
        sources = purrr::map(sources_tmp, ~{
            # grab settings object
            settings <- .[["settings"]]

            if ("items" %in% names(settings)) {
                if (length(settings$items) > 0) {
                    res <- purrr::map_chr(settings$items, "name")
                } else {
                    res <- NA
                }
            } else {
                res <- NA
            }

            return(res)

        })) %>%
        mutate(actual_sources = purrr::map2(name, id, ~source_lookup(obs_name = .x, obs_id = .y, obs_tmp_df = obs_key_df))) %>%
        mutate(actual_sources_flat = purrr::map(actual_sources, ~{
            if (class(.x) == "list") {
                res <- rlang::squash(.x) %>% as.character()
            } else {
                res <- .x
            }
            return(res)
        })) %>%
        mutate(n_actual_sources = purrr::map_dbl(actual_sources_flat, length)) %>%
        mutate(actual_sources_named = purrr::map2(sources, actual_sources, ~{
            res <- .y
            if (class(.y) == "list" && !is.na(.x)) {
                res <- .y
                names(res) <- .x
            }
            
            return(res)
        }))

    return(obs_df)
}
