library(tidyverse)
library(MDShelper)
library(lme4)
library(performance)

# Select a dimension subset that is partially driven by a shared consensus order.
# agreement_strength = 0  -> fully random subset (uniform over dimensions)
# agreement_strength = 1  -> subset follows the shared consensus order exactly
consensus_select_dims <- function(
  n_true_dims,
  n_dims_selected,
  agreement_strength,
  consensus_order
) {
  if (n_true_dims < 2) stop("`n_true_dims` must be >= 2.")
  if (n_dims_selected < 1) stop("`n_dims_selected` must be >= 1.")
  if (agreement_strength < 0 || agreement_strength > 1) {
    stop("`agreement_strength` must be between 0 and 1.")
  }
  if (length(consensus_order) != n_true_dims) {
    stop("`consensus_order` must have length `n_true_dims`.")
  }

  selected       <- integer(0)
  remaining_dims <- seq_len(n_true_dims)

  while (length(selected) < n_dims_selected) {
    if (runif(1) < agreement_strength) {
      # take the earliest remaining dimension in the consensus order
      candidates <- consensus_order[consensus_order %in% remaining_dims]
      next_dim   <- candidates[1]
      if (is.na(next_dim)) {
        next_dim <- sample(remaining_dims, 1)
      }
    } else {
      next_dim <- sample(remaining_dims, 1)
    }

    selected       <- c(selected, next_dim)
    remaining_dims <- setdiff(remaining_dims, next_dim)
  }

  sort(selected)
}

estimate_icc_multilevel <- function(sim_data_long) {
  mod <- lmer(
    dist ~ 1 + (1 | ID) + (1 | ID_pair),
    data = sim_data_long,
    REML = TRUE
  )

  icc_by_group <- performance::icc(mod, by_group = TRUE) %>%
    as.data.frame() %>%
    as_tibble()

  person_icc <- icc_by_group %>%
    filter(Group == "ID")

  item_icc <- icc_by_group %>%
    filter(Group == "ID_pair")

  list(
    model        = mod,
    icc_by_group = icc_by_group,
    person_icc   = person_icc,
    item_icc     = item_icc
  )
}

extract_icc_value <- function(icc_tbl) {
  candidate_cols <- c("ICC_adjusted", "ICC_unadjusted", "ICC")
  present_cols   <- intersect(candidate_cols, names(icc_tbl))

  if (length(present_cols) == 0) {
    stop("No ICC column found in `icc_tbl`.")
  }

  as.numeric(icc_tbl[[present_cols[1]]][1])
}

simulate_similarity_icc <- function(
  n_stimuli                = 80,
  n_true_dims              = 20,
  n_participants           = 100,
  min_dims_per_participant = 2,
  max_dims_per_participant = 6,
  noise_sd                 = 0.10,
  prop_observed            = 0.05,
  agreement_strength       = 0,
  seed                     = 1234,
  simple_res               = FALSE
) {
  set.seed(seed)

  if (min_dims_per_participant < 1) stop("`min_dims_per_participant` must be >= 1.")
  if (max_dims_per_participant > n_true_dims) {
    stop("`max_dims_per_participant` cannot exceed `n_true_dims`.")
  }
  if (min_dims_per_participant > max_dims_per_participant) {
    stop("`min_dims_per_participant` must be <= `max_dims_per_participant`.")
  }
  if (prop_observed <= 0 || prop_observed > 1) {
    stop("`prop_observed` must be in (0, 1].")
  }
  if (!is.logical(simple_res) || length(simple_res) != 1) {
    stop("`simple_res` must be TRUE or FALSE.")
  }

  true_dims <- gen_data_MDS(
    ndims = n_true_dims,
    n     = n_stimuli
  )

  true_coords <- as.matrix(true_dims$latent)
  if (nrow(true_coords) != n_stimuli || ncol(true_coords) != n_true_dims) {
    stop("`true_coords` does not match requested n_stimuli x n_true_dims.")
  }

  consensus_order <- sample(seq_len(n_true_dims))

  pair_idx                <- t(combn(n_stimuli, 2))
  n_pairs                 <- nrow(pair_idx)
  n_pairs_observed        <- max(1, floor(prop_observed * n_pairs))
  sim_long                <- vector("list", length = n_participants)
  participant_dim_choices <- vector("list", length = n_participants)

  for (pid in seq_len(n_participants)) {
    n_dims_selected <- sample(
      x    = min_dims_per_participant:max_dims_per_participant,
      size = 1
    )
    selected_dims <- sort(
      consensus_select_dims(
        n_true_dims        = n_true_dims,
        n_dims_selected    = n_dims_selected,
        agreement_strength = agreement_strength,
        consensus_order    = consensus_order
      )
    )
    participant_dim_choices[[pid]] <- selected_dims

    participant_coords <- true_coords[, selected_dims, drop = FALSE]
    d       <- as.matrix(dist(participant_coords))
    noisy_d <- d + matrix(
      rnorm(n_stimuli * n_stimuli, mean = 0, sd = noise_sd),
      nrow = n_stimuli,
      ncol = n_stimuli
    )

    noisy_d <- (noisy_d + t(noisy_d)) / 2
    diag(noisy_d) <- 0
    noisy_d[noisy_d < 0] <- 0

    full_distances <- noisy_d[cbind(pair_idx[, 1], pair_idx[, 2])]
    keep_idx       <- sample(seq_len(n_pairs), size = n_pairs_observed, replace = FALSE)
    dist_sparse    <- rep(NA_real_, n_pairs)
    dist_sparse[keep_idx] <- full_distances[keep_idx]

    sim_long[[pid]] <- tibble(
      ID         = as.factor(pid),
      stimulus_1 = pair_idx[, 1],
      stimulus_2 = pair_idx[, 2],
      ID_pair    = as.factor(paste0(pair_idx[, 1], "_", pair_idx[, 2])),
      dist       = dist_sparse
    )
  }

  sim_data_long <- bind_rows(sim_long)
  participant_dim_summary <- tibble(
    ID              = as.factor(seq_len(n_participants)),
    n_dims_selected = map_int(participant_dim_choices, length),
    selected_dims   = map_chr(participant_dim_choices, ~ paste(.x, collapse = ","))
  )

  icc_out <- estimate_icc_multilevel(sim_data_long)

  if (simple_res) {
    return(
      list(
        person_icc = icc_out$person_icc,
        item_icc   = icc_out$item_icc
      )
    )
  }

  list(
    settings = list(
      n_stimuli                = n_stimuli,
      n_true_dims              = n_true_dims,
      n_participants           = n_participants,
      min_dims_per_participant = min_dims_per_participant,
      max_dims_per_participant = max_dims_per_participant,
      noise_sd                 = noise_sd,
      prop_observed            = prop_observed,
      agreement_strength       = agreement_strength,
      seed                     = seed,
      simple_res               = simple_res
    ),
    true_coords              = true_coords,
    consensus_order          = consensus_order,
    sim_data_long            = sim_data_long,
    participant_dim_summary = participant_dim_summary,
    icc_model                = icc_out$model,
    icc_by_group             = icc_out$icc_by_group,
    person_icc               = icc_out$person_icc,
    item_icc                 = icc_out$item_icc
  )
}

run_agreement_scenarios <- function(
  n_sims                   = 1000,
  agreement_levels         = c(0, 0.5, 1),
  base_seed                = 1234,
  n_stimuli                = 80,
  n_true_dims             = 20,
  n_participants           = 100,
  min_dims_per_participant = 2,
  max_dims_per_participant = 6,
  noise_sd                 = 0.10,
  prop_observed            = 0.05,
  show_progress            = TRUE
) {
  if (n_sims < 1) stop("`n_sims` must be >= 1.")
  if (any(agreement_levels < 0 | agreement_levels > 1)) {
    stop("All `agreement_levels` must be between 0 and 1.")
  }
  if (!is.logical(show_progress) || length(show_progress) != 1) {
    stop("`show_progress` must be TRUE or FALSE.")
  }

  n_total_runs <- n_sims * length(agreement_levels)
  run_counter  <- 0

  if (show_progress) {
    pb <- txtProgressBar(min = 0, max = n_total_runs, style = 3)
    on.exit(close(pb), add = TRUE)
  }

  raw_results <- map_dfr(seq_len(n_sims), function(sim_i) {
    map_dfr(agreement_levels, function(a) {
      run_counter <<- run_counter + 1
      if (show_progress) {
        setTxtProgressBar(pb, run_counter)
      }

      sim_res <- simulate_similarity_icc(
        n_stimuli                = n_stimuli,
        n_true_dims              = n_true_dims,
        n_participants           = n_participants,
        min_dims_per_participant = min_dims_per_participant,
        max_dims_per_participant = max_dims_per_participant,
        noise_sd                 = noise_sd,
        prop_observed            = prop_observed,
        agreement_strength       = a,
        seed                     = base_seed + sim_i,
        simple_res               = TRUE
      )

      tibble(
        sim_id              = sim_i,
        agreement_strength  = a,
        person_icc_value    = extract_icc_value(sim_res$person_icc),
        item_icc_value      = extract_icc_value(sim_res$item_icc)
      )
    })
  })

  summary_results <- raw_results %>%
    group_by(agreement_strength) %>%
    summarise(
      person_icc_mean = mean(person_icc_value, na.rm = TRUE),
      person_icc_sd   = sd(person_icc_value, na.rm = TRUE),
      item_icc_mean   = mean(item_icc_value, na.rm = TRUE),
      item_icc_sd     = sd(item_icc_value, na.rm = TRUE),
      .groups         = "drop"
    )

  list(
    raw_results     = raw_results,
    summary_results = summary_results
  )
}

# ---- Example run ----
sim_out <- simulate_similarity_icc(
  n_stimuli                = 80,
  n_true_dims              = 10,
  n_participants           = 100,
  min_dims_per_participant = 3,
  max_dims_per_participant = 6,
  noise_sd                 = 0.10,
  prop_observed            = 0.05,
  agreement_strength       = 0.50,
  seed                     = 1234,
  simple_res               = FALSE
)

glimpse(sim_out$sim_data_long)
glimpse(sim_out$participant_dim_summary)
sim_out$icc_by_group
sim_out$person_icc
sim_out$item_icc

# ---- Example 3-scenario summary ----
scenario_out <- run_agreement_scenarios(
  n_sims                   = 1000,
  agreement_levels         = c(0, 0.5, 1),
  base_seed                = 1234,
  n_stimuli                = 80,
  n_true_dims              = 20,
  n_participants           = 100,
  min_dims_per_participant = 2,
  max_dims_per_participant = 5,
  noise_sd                 = 0.01,
  prop_observed            = 0.05,
  show_progress            = TRUE
)

scenario_out$summary_results

# agreement_strength person_icc_mean person_icc_sd item_icc_mean item_icc_sd
# <dbl>           <dbl>         <dbl>         <dbl>       <dbl>
# 0             0.228        0.0163         0.128     0.00992
# 0.5           0.225        0.0186         0.321     0.0296 
# 1             0.224        0.0239         0.643     0.0321 

