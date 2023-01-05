# nocov start
icc <- function(data, ..., group) {
  if (missing(...) || rlang::dots_n(...) == 0) abort("No variables selected to be evaluated")
  if (missing(group)) abort("group may not be missing")

  data <- select(data, {{ group }}, ...)
  group <- colnames(select(data, {{ group }}))
  cols <- colnames(select(data, ...))
  res <- data.frame(Variable = cols, ICC = NA)

  for (i in seq_along(cols)) {
    lmer_res <- lme4::lmer(
      formula = stats::as.formula(paste0(
        "`",
        cols[i], "` ~ 1 + (1 | ", group, " )"
      )),
      data = data,
      na.action = stats::na.omit
    )
    intraccc <- as.data.frame(lme4::VarCorr(lmer_res))$sdcor^2
    res$ICC[i] <- intraccc[1] / sum(intraccc)
  }

  res
}

# Calculate correlations based on multilevel models
multilevel_cor <- function(x,
                           ...,
                           y = NULL,
                           group = NULL,
                           center = TRUE,
                           scale = FALSE,
                           p_adjust = TRUE) {
  # If ... is specified
  if (rlang::dots_n(...) != 0) {
    # Extract and normalise data
    x <- x %>%
      select({{ group }}, ...)

    names_x <- colnames(select(x, ...))
  }

  # group_by has problems with a mix of characters and bares
  if (all(sapply(rlang::enexprs(group), rlang::is_character))) {
    if (scale) {
      x <- x %>%
        group_by(across(all_of(group)))
    }
    group_names <- unlist(group)
  } else {
    if (scale) {
      x <- x %>%
        group_by(across({{ group }}))
    }
    group_names <- colnames(select(x, {{ group }}))
  }

  x <- x %>%
    mutate(across(
      tidyr::everything(),
      ~ scale(.x, scale = scale, center = center)[, 1]
    )) %>%
    ungroup()

  if (!is.null(y)) {
    y <- y %>%
      select(all_of(colnames(x)))

    if (!dplyr::all_equal(select(x, {{ group }}), select(y, {{ group }}))) {
      abort("group is not equal in x and y")
    }

    names_y <- colnames(y)
  } else {
    y <- x
    names_y <- names_x
  }

  if (!is.logical(p_adjust)) abort("p_adjust must be a logical value")

  # Create a tibble for every combination
  res <- tidyr::crossing(var1 = names_x, var2 = names_y)

  # Calculate a multilevel model for each combination
  model_cor <- function(var1, var2, group) {
    dat1 <- x %>%
      select(all_of(c(group, var1)))
    # %>% rename("var1" = all_of(var1))
    dat2 <- y %>%
      select(all_of(c(var2)))
    # %>% rename("var2" = all_of(var2))
    dat <- dplyr::bind_cols(dat1, dat2)

    # Set up random intercept model
    fixed <- paste0(var1, " ~ -1 + ", var2)
    if (length(group) == 0) {
      random <- paste0("(-1 + ", var2, ")")
    } else {
      random <- paste0("(-1 + ", var2, " | ", paste0(group, collapse = "/"), ")")
    }
    formula <- stats::as.formula(paste0(fixed, "+", random))

    lmerTest::lmer(formula = formula, data = dat, na.action = stats::na.omit, REML = TRUE)
  }

  res$cor <- rep(NA_real_, nrow(res))
  res$p.value <- rep(NA_real_, nrow(res))
  res$lower <- rep(NA_real_, nrow(res))
  res$upper <- rep(NA_real_, nrow(res))

  for (i in seq_len(nrow(res))) {
    if (res$var1[[i]] == res$var2[[i]]) {
      res$cor[[i]] <- 1
      res$p.value[[i]] <- 0
      res$lower[[i]] <- 1
      res$upper[[i]] <- 1
    } else {
      mod <- model_cor(res$var1[[i]], res$var2[[i]], group_names)
      res$cor[[i]] <- lme4::fixef(mod)[[1]]
      res$p.value[[i]] <- stats::anova(mod)$`Pr(>F)`
      confint <- stats::confint(mod, method = "Wald")
      res$lower[[i]] <- confint[nrow(confint), 1]
      res$upper[[i]] <- confint[nrow(confint), 2]
    }
  }

  # Apply multiple testing correction
  if (p_adjust && nrow(res) > 1) {
    res$p.value <- stats::p.adjust(res$p.value, n = nrow(res) / 2)
  }

  return(res)
}

multilevel_autocor <- function(data,
                               ...,
                               group = NULL,
                               lag_var = NULL,
                               center = TRUE,
                               scale = FALSE,
                               p_adjust = TRUE) {
  # If ... is specified
  if (rlang::dots_n(...) != 0) {
    # Extract and normalise data
    data <- data %>%
      select({{ group }}, {{ lag_var }}, ...)

    var_names <- colnames(select(data, ...))
  } else {
    var_names <- select(data, -c({{ group }}, {{ lag_var }}))
  }

  # group_by has problems with a mix of characters and bares
  if (all(sapply(rlang::enexprs(group), rlang::is_character))) {
    if (scale) {
      data <- group_by(data, across(all_of(group)))
    }
    group_names <- unlist(group)
  } else {
    if (scale) {
      data <- group_by(data, across(c({{ group }}, {{ lag_var }})))
    }
    group_names <- colnames(select(data, {{ group }}))
  }

  # TODO: standardise AFTER lagging, or else the standardisation is lost
  data <- data %>%
    mutate(across(.fns = ~ scale(.x, center = center, scale = scale)[, 1])) %>%
    ungroup()

  if (!is.logical(p_adjust)) stop("p_adjust must be a logical value")

  # Calculate a multilevel model for each combination
  model_autocor <- function(data, var, lag_var, group) {
    dat <- data %>%
      select(all_of(c(group, var, lag_var))) %>%
      drop_na() %>%
      group_by(across(all_of(c(group, lag_var)))) %>%
      mutate(!!paste0(var, "_lag") := lag(!!rlang::sym(var))) %>% # nolint
      ungroup()

    # Set up random intercept model
    fixed <- paste0(var, " ~ -1 + ", var, "_lag")
    random <- paste0("(-1 + ", var, "_lag | ", paste0(group, collapse = "/"), ")")
    formula <- stats::as.formula(paste0(fixed, "+", random))

    lmerTest::lmer(formula = formula, data = dat, na.action = stats::na.omit, REML = TRUE)
  }

  res <- tibble(var = var_names)
  res$autocor <- rep(NA_real_, nrow(res))
  res$p.value <- rep(NA_real_, nrow(res))
  res$lower <- rep(NA_real_, nrow(res))
  res$upper <- rep(NA_real_, nrow(res))

  for (i in seq_len(nrow(res))) {
    mod <- model_autocor(data, res$var[[i]], lag_var, group_names)
    res$autocor[[i]] <- lme4::fixef(mod)[[1]]
    res$p.value[[i]] <- stats::anova(mod)$`Pr(>F)`
    confint <- stats::confint(mod, method = "Wald")
    res$lower[[i]] <- confint[nrow(confint), 1]
    res$upper[[i]] <- confint[nrow(confint), 2]
  }

  # Apply multiple testing correction
  if (p_adjust && nrow(res) > 1) {
    res$p.value <- stats::p.adjust(res$p.value, n = nrow(res) / 2)
  }

  return(res)
}
# nocov end
