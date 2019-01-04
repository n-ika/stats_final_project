# FUNCTIONS
# STATISTICS FINAL PROJECT FALL 2018
#
# Here are functions, used for the project analysis.
#
#
#
# ******************************************************************
# Function 1
#
# Filters the data [table] according to the language,
# groups the data by phone_TGT and phone_OTH and
# returns a new table with a summary of mean correct
# responses by a specific language group.
# Arguments:
# - data
# - language
# Output: data frame, filtered by language

lang_filter <- function(DATA,
                        LANGUAGE){
  spk <- DATA %>%
    dplyr::filter(subject_language.x == LANGUAGE) %>%
    dplyr::group_by(phone_TGT, phone_OTH) %>%
    dplyr::summarise(correct_resp = mean(user_corr)) %>%
    dplyr::ungroup()
  return(spk)
}

# ******************************************************************
# Function 2
# 
# Plots the summary of the previously filtered
# data by language group. 
# Arguments:
# - data [e.g. English subgroup summary table]
# - the title of the plot [e.g. "English speakers"]
# Output: a confusion matrix of discrimination rate.

plot_corr_resp <- function(DATA,
                           TITLE_PLOT,
                           MID_COLOR){
  theme.size = 15
  
  spk_plot <- ggplot2::ggplot(DATA,
                              ggplot2::aes(x=phone_TGT,
                                           y=phone_OTH)) +
    ggplot2::geom_tile(ggplot2::aes(fill = correct_resp)) +
    ggplot2::scale_fill_gradient2(low = "firebrick3",
                                 mid = "gold",
                                 high = "forestgreen",
                                 midpoint=MID_COLOR) +
    ggplot2::labs(title =TITLE_PLOT, 
                  x = "Target phone", 
                  y = "Other phone", 
                  fill="Correct response") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::theme(axis.text = 
                     ggplot2::element_text(
                       size = theme.size, colour="black")) 
  return(spk_plot)
}

# ******************************************************************
# Function 3
# 
# Will rename the rows of the first column "combo1"
# to map all pairs A_B and B_A as the same.
# Arguments:
# - data
# Output:
# data with renamed pairs

make_triangle <- function(DATA){
  for (i in 1:dim(DATA)[1]){
    pair <- DATA[["combo1"]][i]
    combo2_index <- which(DATA$combo2 == pair)
    DATA[["combo1"]][combo2_index] <- DATA[["combo1"]][i]
  }
  return(DATA)
}


# ******************************************************************
# Function 4
#
# Samples, with replacement, the values from 2 columns
# of a data frame to simulate the data close to observed.
# Performs a logistic model and evaluates the statistical
# power according to the p value - if it is smaller or 
# bigger than a given alpha.
# Arguments:
# - data [e.g. Geomphon pilot data]
# - alpha [e.g. 0.1]
# Output: 
# a data frame with the percent of statistical power

statistical_power <- function(DATA,
                              alpha){
  power_df <- NULL
  SAMPLE_SIZES <- c(10, 50, 100, 400, 1000, 5000)
  for (size in SAMPLE_SIZES) {
    n_tests <- 1000
    n_successes <- 0
    for (i in 1:n_tests) {
      df_permuted <- DATA %>%
        dplyr::group_by(correct_resp) %>%
        dplyr::sample_n(dim(DATA)[1],
                        replace = TRUE) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(user_corr) %>%
        dplyr::sample_n(dim(DATA)[1],
                        replace = TRUE) %>%
        dplyr::ungroup()
      
      fake_logit_m <- glm(user_corr ~ correct_resp, 
                          data = df_permuted, 
                          family=binomial(link='logit'))
      
      pval <-  coef(summary(fake_logit_m))[2,4]

      if (pval < alpha) {
        n_successes <- n_successes + 1
      }
    }
    power <- n_successes/n_tests
    power_df_current <- tibble::data_frame(power=power, 
                                           alpha=alpha,
                                           size=size)
    power_df <- dplyr::bind_rows(power_df, power_df_current)
  }
return(power_df)
}
