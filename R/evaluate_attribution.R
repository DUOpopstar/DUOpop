#' @title Evaluate Attribution Metrics for Synthetic Data Privacy
#' @description
#' Calculates a set of attribute disclosure metrics to evaluate the privacy of synthetic data.
#' These metrics quantify how much information an attacker could infer about a target variable \code{t}
#' given access to quasi-identifiers \code{q}, under different assumptions.
#'
#' If a holdout set is provided, the metrics can isolate and evaluate only the undesired attribute-level
#' information. This allows the evaluation to exclude desired group-level information and instead focus
#' on the residual, potentially sensitive individual-level information that remains in the synthetic data.
#'
#' @param orig_df A data frame containing the original data set.
#' @param syn_df A data frame containing the synthetic data set.
#' @param keys A character vector of column names representing quasi-identifiers (\code{q}).
#' @param target A character string specifying the name of the target column (\code{t}).
#' @param train_df Optional: A data frame containing the data used to base the synthetic data on in the case of a holdout set. When a training data set is passed, the original data set will not be used in this function.
#' @param holdout_df Optional: A data frame containing the out-of-sample (real) data not used during synthesis. The training data also needs to be provided.
#' @param na_as_value Logical; if \code{TRUE} (default), treat \code{NA} as a separate category/value in both target and key columns.
#'
#' @details
#' By default, \code{NA} values are treated as valid categories for both target and key columns (i.e., treated as any other value in the frequency tables).
#' This gives higher scores compared to when they are not treated as their own values.
#' Typically missing values are not considered a privacy risk. But there are cases where they should be counted towards key or target values.
#' This distinction is important when \code{NA} represents a true category (e.g., "not applicable") rather than a missing value.
#' If \code{use_na_as_value = FALSE}, then \code{NA} values in the target or quasi-identifiers are not treated as actual values, but all rows—including
#' those with \code{NA}—are still included in the denominators.
#'
#' Empty strings ("") are treated as values.
#'
#' Metrics include:
#' - \strong{BaseCAP}: Baseline disclosure based only on the marginal distribution of \code{t} (see: \code{\link{calculate_basecap}})
#' - \strong{CAPo}: Disclosure in the original data when the attacker knows \code{q} (see: \code{\link{calculate_capo}})
#' - \strong{CAPs}: Disclosure in the synthetic data, combining group and attribute info (see: \code{\link{calculate_caps}})
#' - \strong{hCAPs}: Group-level info retained by the synthetic data (see: \code{\link{calculate_hcaps}})
#' - Disclosure reduction metrics comparing CAPs to CAPo
#'
#' @return A named list with attribution metrics:
#' \describe{
#'   \item{• BaseCAP}{Minimal disclosure when only the marginal distribution of \code{t} is known. Let \eqn{p_t} be the relative frequency of target value \eqn{t}:
#'    \deqn{\text{BaseCAP} = \sum_{t \in T} p_t^2}}
#'   \item{• CAPo}{Maximum disclosure in the original data, assuming full access:
#'    \deqn{\text{CAPo} = \sum_{q,t} \frac{n_{qt}}{n_q} \times \frac{n_{qt}}{N}}}
#'   \item{• CAPs}{Total disclosure, including group and attribution, in the synthetic data:
#'     \deqn{\text{CAPS} = \sum_{q,t} \frac{n_{qt}^{syn}}{n_q^{syn}} \times \frac{n_{qt}^{orig}}{N_{orig}}}}
#'   \item{• hCAPs}{(If \code{holdout_df} is supplied) Desired group-level signal in synthetic data:
#'     \deqn{\text{hCAPs} = \sum_{q,t} \frac{n_{qt}^{syn}}{n_q^{syn}} \times \frac{n_{qt}^{hold}}{N_{hold}}}}
#'   \item{• Disc_Plot}{A plot visualising the four disclosure metrics.}
#'}
#'
#' If \code{holdout_df} is supplied:
#' \describe{
#'   \item{• Upperbound_Attr}{\eqn{CAPo - hCAPs}, upper bound on attribute info in original data}
#'   \item{• Undesired_Attr}{\eqn{CAPs - hCAPs}, residual attribute info in synthetic data}
#'   \item{• Abs_Attr_Reduction}{\eqn{Upperbound\_Attr - Undesired\_Attr}}
#'   \item{• Rel_Attr_Reduction}{\eqn{1 - \frac{Undesired\_Attr}{Upperbound\_Attr}}, proportional reduction: how much of the original attribute disclosure is hidden in the synthetic data}
#' }
#'
#' If \code{holdout_df} is not supplied:
#' \describe{
#'   \item{• Abs_Reduction}{\eqn{CAPo - CAPs}, total disclosure drop as percentage points}
#'   \item{• Rel_Reduction}{\eqn{\frac{CAPo - CAPs}{CAPo}}, total disclosure drop as a percentage}
#'   \item{• Normalized_Disc}{\eqn{\frac{CAPs - BaseCAP}{CAPo - BaseCAP}}, normalized disclosure drop}
#' }
#'
#' @examplesIf requireNamespace("synthpop", quietly = TRUE)
#' vars <- c("age", "sex", "marital", "income", "ls", "smoke")
#' orig_df <- synthpop::SD2011[, vars]
#'
#' set.seed(123)
#' idx <- sample(seq_len(nrow(orig_df)), size = 0.8*nrow(orig_df))
#' train_df <- orig_df[idx, ]
#' holdout_df <- orig_df[-idx, ]
#' syn_df <- synthpop::syn(train_df)$syn
#'
#' attr <- evaluate_attribution(orig_df, syn_df, keys = c("age", "sex"), target = "marital", train_df, holdout_df, na_as_value = TRUE)
#'
#' @import ggplot2 dplyr
#' @importFrom patchwork plot_layout
#' @export
#'

evaluate_attribution <- function(orig_df, syn_df, keys, target, train_df = NULL, holdout_df = NULL, na_as_value = TRUE) {

  #error handling
  check_input_not_empty(data = orig_df, keys = keys, target = target)
  check_input_not_empty(data = syn_df, keys = keys, target = target)
  if(!is.null(train_df)) {
    check_input_not_empty(data = train_df, keys = keys, target = target)
  }
  if(!is.null(holdout_df)) {
    check_input_not_empty(data = holdout_df, keys = keys, target = target)
  }
  if (!is.null(train_df) && is.null(holdout_df)) {
    stop("If you provide a train_df, you must also provide a holdout_df.")
  }
  if (!is.null(holdout_df) && is.null(train_df)) {
    stop("If you provide a holdout_df, you must also provide a train_df.")
  }

  # ─────────────────────────────────────────────
  # 1. BaseCAP
  # ─────────────────────────────────────────────
  if(!is.null(train_df)) {
    basecap <- calculate_basecap(train_df, target, na_as_value)
  } else {
    basecap <- calculate_basecap(orig_df, target, na_as_value)
  }

  # ─────────────────────────────────────────────
  # 2. CAPo (original data)
  # ─────────────────────────────────────────────
  if(!is.null(train_df)) {
    capo <- calculate_capo(train_df, keys, target, na_as_value)
  } else {
    capo <- calculate_capo(orig_df, keys, target, na_as_value)
  }

  # ─────────────────────────────────────────────
  # 3. CAPs (synthetic data with (q,t) from original data)
  # ─────────────────────────────────────────────
  if(!is.null(train_df)) {
    caps <- calculate_caps(train_df, syn_df, keys, target, na_as_value)
  } else {
    caps <- calculate_caps(orig_df, syn_df, keys, target, na_as_value)
  }

  # ─────────────────────────────────────────────
  # 4. hCAPs (synthetic data with (q,t) from holdout data)
  # ─────────────────────────────────────────────
  if (!is.null(holdout_df)){
    hcaps <- calculate_hcaps(holdout_df, syn_df, keys, target, na_as_value)
  } else {
    hcaps <- "No holdout data set supplied"
  }
  # ─────────────────────────────────────────────
  # 5. Disclosure reduction
  # ─────────────────────────────────────────────
  if (!is.null(holdout_df)){
    upperbound_attr <- capo - hcaps
    undesired_attr <- caps - hcaps
    abs_attr_red <- (upperbound_attr) - (undesired_attr)

    if (upperbound_attr == 0) {
      rel_attr_red <- 0 #handle division by zero
    } else {
      rel_attr_red <- 1 - (undesired_attr)/(upperbound_attr)
    }
  } else {
    abs_reduction <- capo - caps
    rel_reduction <- (capo - caps) / capo

    denom <- capo - basecap
    if (denom == 0) {
      normalized_disc <- 0
    } else {
      normalized_disc <- (caps - basecap) / (capo - basecap)
    }
  }

  # ─────────────────────────────────────────────
  # 6. Plots
  # ─────────────────────────────────────────────
  if (!is.null(holdout_df)) {
    values_for_plot <- data.frame(
      Metric = c("CAPo", "CAPs", "hCAPs", "BaseCAP"),
      Value = c(capo, caps, hcaps, basecap)
    ) %>% arrange(Value)

    caption_text = paste0(
      "Attribute disclosure reduced by up to ",
      min(100, round(rel_attr_red * 100, 2)),
      "%, compared to the original data set"
    )
  } else {
    values_for_plot <- data.frame(
      Metric = c("CAPo", "CAPs", "BaseCAP"),
      Value = c(capo, caps, basecap)
    ) %>% arrange(Value)

    caption_text <- paste0(
      "Remaining Normalized Disclosure: ", max(0, round(normalized_disc, 2)), "\n",
      " (0 = no attribute disclosure, 1 = no improvement over the original data)"
    )
  }
  vjust_values <- rep(c(1.9, -1.1), length.out = nrow(values_for_plot))
  values_for_plot$vjust_values <- vjust_values

  main_plot <- ggplot(values_for_plot, aes(x = Value, y = 0)) +
    geom_point(aes(color = Metric), size = 10) +
    geom_text(aes(label = Metric), vjust = vjust_values, size = 4, family = font_vinden_safe()) +
    scale_color_manual(values = IP_kleuren_safe) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    labs(title = "Attribute Disclosure", x = "Disclosure (0 = low, 1 = high)",
         subtitle= caption_text,
         y = NULL) +
    theme_minimal(base_size = 14, base_family = font_vinden_safe()) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "none",
      axis.title.x = element_text(size = 8, margin = margin(t = 10)),
      plot.title = element_text(size = 12, face = "plain"),
      plot.subtitle = element_text(size = 9)
    )

  diffs <- diff(values_for_plot$Value)
  threshold <- 0.05
  close_pairs <- which(diffs < threshold)

  if (length(close_pairs) > 0) {
    cluster_idx <- sort(unique(c(close_pairs, close_pairs + 1)))
    cluster_min <- min(values_for_plot$Value[cluster_idx])
    cluster_max <- max(values_for_plot$Value[cluster_idx])

    padding <- (cluster_max - cluster_min) * 0.2
    zoom_range <- c(cluster_min - padding, cluster_max + padding)
  }

  if (length(close_pairs) > 0) {
    zoom_plot <- ggplot(values_for_plot, aes(x = Value, y = 0)) +
      geom_point(aes(color = Metric), size = 6) +
      geom_text(aes(label = Metric), vjust = vjust_values, size = 4, family = font_vinden_safe()) +
      scale_color_manual(values = IP_kleuren_safe) +
      coord_cartesian(xlim = zoom_range) +
      labs(title = "Zoomed View", x = NULL, y = NULL) +
      theme_minimal(base_size = 12, base_family = font_vinden_safe()) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 10, face = "plain")
      )

    final_plot <- main_plot / zoom_plot + patchwork::plot_layout(heights = c(2, 1))
    #final_plot <- plot_grid(main_plot, zoom_plot, ncol = 1, rel_heights = c(2, 1)) #cowplot version
  } else {
    final_plot <- main_plot
  }

  # ─────────────────────────────────────────────
  # Output as named list
  # ─────────────────────────────────────────────

  if (!is.null(holdout_df)){
    list(
      BaseCAP = basecap,
      CAPo = capo,
      CAPs = caps,
      hCAPs = hcaps,
      Upperbound_Attr = upperbound_attr,
      Undesired_Attr = undesired_attr,
      Abs_Attr_Reduction = abs_attr_red,
      Rel_Attr_Reduction = rel_attr_red,
      Disc_Plot = final_plot
    )
  } else {
    list(
      BaseCAP = basecap,
      CAPo = capo,
      CAPs = caps,
      Abs_Reduction = abs_reduction,
      Rel_Reduction = rel_reduction,
      Normalized_Disc = normalized_disc,
      Disc_Plot = final_plot
    )
  }
}
