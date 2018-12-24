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
# data by language group. Needs as arguments:
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
    ggplot2::scale_fill_gradient2(low = "forestgreen",
                                 mid = "gold",
                                 high = "firebrick3",
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
# to correctly map all pair A_B and B_A as the same

make_triangle <- function(DATA){
  for (i in 1:dim(DATA)[1]){
    pair <- DATA[["combo1"]][i]
    combo2_index <- which(DATA$combo2 == pair)
    DATA[["combo1"]][combo2_index] <- DATA[["combo1"]][i]
  }
  return(DATA)
}