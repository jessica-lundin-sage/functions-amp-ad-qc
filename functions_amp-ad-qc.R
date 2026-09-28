## functions for amp-ad QC Markdown files
# Jessica Lundin, Jaclyn Beck
# Sept 28 2026

# load libraryies
library(dplyr)
library(ggplot2)
library(pROC)


# demographics ----
make_bar_plot <- function(metadata, var_of_interest, facet_var = "tissue") {
  ord <- metadata |>
    mutate(variable = fct_infreq(factor(get(var_of_interest)))) |>
    pull(variable) |>
    levels()
  
  meta_tmp <- metadata |>
    group_by_at(c(var_of_interest, facet_var)) |>
    dplyr::count() |>
    mutate(variable = factor(get(var_of_interest), levels = ord))
  
  # Add a little extra space for the number label at the top of each bar
  max_y <- ceiling(max(meta_tmp$n) * 1.02)
  
  # Decrease bar label text size because a large number of facets makes the
  # graphs shorter. Also add even more space for the number label to account
  # for shorter graphs
  if (!is.null(facet_var) && length(unique(meta_tmp[[facet_var]])) > 3) {
    text_size = 3
    max_y <- ceiling(max_y * 1.2)
  } else if (length(unique(metadata[[var_of_interest]])) > 4) {
    text_size = 3
  } else {
    text_size = 4
  }
  
  plt <- ggplot(meta_tmp, aes(x = variable, y = n, fill = variable)) +
    geom_col() +
    geom_text(aes(label = n), vjust = -0.5, size = text_size) +
    theme_bw() +
    xlab(NULL) +
    ylab("count") +
    ylim(0, max_y) +
    labs(fill = var_of_interest) +
    scale_fill_viridis(discrete = TRUE, begin = 0.2) +
    theme(strip.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(face = "bold"),
          title = element_text(face = "bold"))
  
  if (!is.null(facet_var)) {
    plt <- plt + facet_wrap(facet_var) +
      labs(title = paste(facet_var, "vs", var_of_interest))
  } else {
    plt <- plt + labs(title = var_of_interest)
  }
  
  # Shorten legend title for dataContributionGroup to save space
  if (var_of_interest == "dataContributionGroup") {
    plt <- plt + labs(fill = "Contrib. Group")
  }
  
  plt
}




make_bar_plot_batch <- function(metadata, var_of_interest, facet_var = "tissue") {
  #ord <- metadata |>
  #  mutate(variable = var_of_interest) |>
  #  pull(variable) |>
  #  levels()
  
  meta_tmp <- metadata |>
    group_by_at(c(var_of_interest, facet_var)) |>
    dplyr::count() |>
    mutate(variable = factor(get(var_of_interest)))
  
  # Add a little extra space for the number label at the top of each bar
  max_y <- ceiling(max(meta_tmp$n) * 1.02)
  
  # Decrease bar label text size because a large number of facets makes the
  # graphs shorter. Also add even more space for the number label to account
  # for shorter graphs
  if (!is.null(facet_var) && length(unique(meta_tmp[[facet_var]])) > 3) {
    text_size = 3
    max_y <- ceiling(max_y * 1.2)
  } else if (length(unique(metadata[[var_of_interest]])) > 4) {
    text_size = 3
  } else {
    text_size = 4
  }
  
  plt <- ggplot(meta_tmp, aes(x = variable, y = n, fill = variable)) +
    geom_col() +
    geom_text(aes(label = n), vjust = -0.5, size = text_size) +
    theme_bw() +
    xlab(NULL) +
    ylab("count") +
    ylim(0, max_y) +
    labs(fill = var_of_interest) +
    scale_fill_viridis(discrete = TRUE, begin = 0.2) +
    theme(strip.background = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1),
          strip.text = element_text(face = "bold"),
          title = element_text(face = "bold"))
  
  if (!is.null(facet_var)) {
    plt <- plt + facet_wrap(facet_var) +
      labs(title = paste(facet_var, "vs", var_of_interest))
  } else {
    plt <- plt + labs(title = var_of_interest)
  }
  
  # Shorten legend title for dataContributionGroup to save space
  if (var_of_interest == "dataContributionGroup") {
    plt <- plt + labs(fill = "Contrib. Group")
  }
  
  plt
}



make_bar_plot_batch_fillvar <- function(metadata, var_of_interest, facet_var = "tissue", fill_var = NULL, color_scale = "viridis") {
  group_vars <- c(var_of_interest, facet_var, fill_var)
  
  meta_tmp <- metadata |>
    group_by_at(group_vars) |>
    dplyr::count() |>
    mutate(variable = factor(get(var_of_interest)))
  
  #max_y <- ceiling(max(meta_tmp$n) * 1.02)
  
  if (!is.null(facet_var) && length(unique(meta_tmp[[facet_var]])) > 3) {
    text_size = 3
    #  max_y <- ceiling(max_y * 1.2)
  } else if (length(unique(metadata[[var_of_interest]])) > 4) {
    text_size = 3
  } else {
    text_size = 4
  }
  
  fill_aes <- if (!is.null(fill_var)) fill_var else "variable"
  fill_label <- if (!is.null(fill_var)) fill_var else var_of_interest
  
  plt <- ggplot(meta_tmp, aes(x = variable, y = n, fill = .data[[fill_aes]])) +
    geom_col() +
    geom_text(aes(label = n), vjust = -0.5, size = text_size) +
    theme_bw() +
    xlab(NULL) +
    ylab("count") +
    #  ylim(0, max_y) +
    labs(fill = fill_label) +
    scale_fill_viridis(discrete = TRUE, begin = 0.2) +
    theme(strip.background = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1),
          strip.text = element_text(face = "bold"),
          title = element_text(face = "bold"))
  
  # Dynamic Color Scale Injection
  if (color_scale == "viridis") {
    plt <- plt + scale_fill_viridis(discrete = TRUE, begin = 0.2)
  } else if (color_scale == "brewer") {
    plt <- plt + scale_fill_brewer(palette = "Set2") # Adjust default palette as desired
  } else if (color_scale == "manual") {
    # Expects user to add scale manually after the function call, 
    # or you can pass a custom palette argument if needed.
    NULL 
  } else {
    # Default fallback to standard ggplot colors if an unrecognized string is passed
    NULL 
  }
  
  if (!is.null(facet_var)) {
    plt <- plt + facet_wrap(facet_var) +
      labs(title = paste(facet_var, "vs", var_of_interest))
  } else {
    plt <- plt + labs(title = var_of_interest)
  }
  
  if (var_of_interest == "dataContributionGroup") {
    plt <- plt + labs(fill = "Contrib. Group")
  }
  plt <- plt + facet_wrap(facet_var, scales = "free_y")
  plt
}



make_bar_plot_batch_fillvar_single <- function(metadata, var_of_interest, fill_var = NULL, color_scale = "viridis") {
  group_vars <- c(var_of_interest, fill_var)
  
  meta_tmp <- metadata |>
    group_by_at(group_vars) |>
    dplyr::count() |>
    mutate(variable = factor(get(var_of_interest)))
  
  max_y <- ceiling(max(meta_tmp$n) * 1.02)
  
  if (length(unique(metadata[[var_of_interest]])) > 4) {
    text_size = 3
  } else {
    text_size = 4
  }
  
  fill_aes <- if (!is.null(fill_var)) fill_var else "variable"
  fill_label <- if (!is.null(fill_var)) fill_var else var_of_interest
  
  plt <- ggplot(meta_tmp, aes(x = variable, y = n, fill = .data[[fill_aes]])) +
    geom_col() +
    geom_text(aes(label = n), vjust = -0.5, size = text_size) +
    theme_bw() +
    xlab(NULL) +
    ylab("count") +
    ylim(0, max_y) +
    labs(fill = fill_label) +
    #    scale_fill_viridis(discrete = TRUE, begin = 0.2) +
    theme(strip.background = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1),
          strip.text = element_text(face = "bold"),
          title = element_text(face = "bold"))
  
  # Dynamic Color Scale Injection
  if (color_scale == "viridis") {
    plt <- plt + scale_fill_viridis_d(begin = 0.2) # Note: scale_fill_viridis_d is preferred for discrete data
  } else if (color_scale == "brewer") {
    plt <- plt + scale_fill_brewer(palette = "Paired") # Adjust default palette as desired
  } else if (color_scale == "manual") {
    # Expects user to add scale manually after the function call, 
    # or you can pass a custom palette argument if needed.
    # Keeps default ggplot2 colors, allowing user to + scale_fill_manual() outside the function
    NULL 
  } else {
    # Default fallback to standard ggplot colors if an unrecognized string is passed
    NULL 
  }
  plt <- plt + labs(title = paste(var_of_interest, "vs", fill_var))
  
  if (var_of_interest == "dataContributionGroup") {
    plt <- plt + labs(fill = "Contrib. Group")
  }
  
  plt
}

format_histogram_plot <- function(plt) {
  plt + geom_histogram(aes(fill = after_stat(count)), bins = 30, na.rm = TRUE) +
    theme_bw() +
    facet_wrap(~tissue) +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          title = element_text(face = "bold"))
}


# inferred sex ----
SEX_MARKERS <- list(
  y_chr = c(
    RPS4Y1  = "ENSG00000129824",
    DDX3Y   = "ENSG00000067048",
    KDM5D   = "ENSG00000012817",
    EIF1AY  = "ENSG00000198692"
  ),
  x_inact = c(
    XIST    = "ENSG00000229807"
  )
)

## Classifies using y_score alone (XIST is ignored at this step) compared to 2^threshold.
predict_sex2 <- function(expr_mat,
                         threshold = NULL,
                         reported  = NULL,
                         verbose   = TRUE,
                          predict_sex = c("sex_score","y_score")) {
  
  expr_mat <- as.matrix(expr_mat)
  gene_ids  <- rownames(expr_mat)
  
  if (is.null(gene_ids)) stop("expr_mat must have ENSEMBL IDs as rownames.")
  
  # ── Find available markers ─────────────────────────────────────────────────
  y_present    <- intersect(SEX_MARKERS$y_chr,   gene_ids)
  xist_present <- intersect(SEX_MARKERS$x_inact, gene_ids)
  
  if (verbose) {
    message(sprintf(
      "Y-chr markers found: %d/%d  |  XIST found: %s",
      length(y_present),
      length(SEX_MARKERS$y_chr),
      ifelse(length(xist_present) > 0, "yes", "NO — Y-only scoring used")
    ))
    if (length(y_present) == 0)
      stop("No Y-chromosome markers found. Check that rownames are ENSEMBL IDs.")
  }
  
  # ── Compute per-sample scores ──────────────────────────────────────────────
  #  y_score <- if (length(y_present) > 0) {
  #   colMeans(expr_mat[y_present, , drop = FALSE], na.rm = TRUE)
  #  } else {
  #    rep(0, ncol(expr_mat))
  # }
  # from gemini to fix returns of NaN when 0/0 (because remove low gene counts)
  y_score <- if (length(y_present) > 0) {
    # 1. Calculate the column means
    means <- colMeans(expr_mat[y_present, , drop = FALSE], na.rm = TRUE)
    
    # 2. Replace any NaN values (caused by all-NA columns) with 0
    means[is.nan(means)] <- 0
    
    # 3. Return the cleaned means vector
    means
  } else {
    rep(0, ncol(expr_mat))
  }
  
  xist_score <- if (length(xist_present) > 0) {
    as.numeric(expr_mat[xist_present[1], ])
  } else {
    rep(0, ncol(expr_mat))
  }
  
  # sex_score > 0  →  more Y-like  →  Male
  sex_score <- y_score - xist_score
  # sex_score <- y_score 
  
  result <- data.frame(
    sample     = colnames(expr_mat),
    y_score    = y_score,
    xist_score = xist_score,
    sex_score  = sex_score,
    stringsAsFactors = FALSE
  )
  
  # ── Determine threshold ────────────────────────────────────────────────────
  if (is.null(threshold)) {
    if (is.null(reported)) {
      # Fallback: midpoint between the two modes (unsupervised)
      threshold <- find_threshold_unsupervised(sex_score, verbose)
    } else {
      threshold <- find_threshold_supervised(sex_score, reported, verbose)
    }
  }

  if (predict_sex == "y_score"){
  result$predicted_sex <- ifelse(result$y_score >= 2^threshold, "male", "female")
    }
  if (predict_sex == "sex_score"){
  result$predicted_sex <- ifelse(result$sex_score >= threshold, "male", "female")
    }
              
  # ── Merge self-reported sex ────────────────────────────────────────────────
  if (!is.null(reported)) {
    reported_df <- data.frame(
      sample       = names(reported),
      reported_sex = as.character(reported),
      stringsAsFactors = FALSE
    )
    result <- left_join(result, reported_df, by = "sample")
    result$concordant <- result$predicted_sex == result$reported_sex
  }
  
  attr(result, "threshold") <- threshold
  class(result) <- c("sex_prediction", "data.frame")
  result
}



#' Scatter plot: Y-score vs XIST-score coloured by predicted sex
plot_sex_scores <- function(x, label_discordant = TRUE) {
  thr <- attr(x, "threshold")
  
  x <- x %>%
    arrange(desc(concordant))
  
  p <- ggplot(x, aes(x = log2(xist_score), y = log2(y_score),
                     colour = reported_sex,
                     shape  = if ("concordant" %in% names(x)) concordant else NULL)) +
    geom_point(size = 2.5, alpha = 0.8) +
    scale_colour_manual(values = c(female = "purple3", male = "green3")) +
    labs(
      title   = "Sex prediction: Y-chr score vs XIST",
      x       = "log2 XIST expression",
      y       = "log2 Mean Y-chromosome gene expression",
      colour  = "Reported sex",
      shape   = "Concordant"
    ) +
    theme_bw() +
    facet_wrap(~tissue)
  p
}


# FastQC, MultiQC ----

is_outlier_IQR <- function(data, tail = "both", IQR_mult = 1.5) {
  iqr <- stats::IQR(data, na.rm = TRUE) * IQR_mult
  q1 <- stats::quantile(data, 0.25, na.rm = TRUE)
  q3 <- stats::quantile(data, 0.75, na.rm = TRUE)
  
  switch(
    tail,
    "both" = (data < q1 - iqr) | (data > q3 + iqr),
    "upper" = data > q3 + iqr,
    "lower" = data < q1 - iqr
  ) | is.na(data)
}



# RNA metrics PCs ----

qc_diagnosis_assoc <- function(metrics,
                               metadata,
                               diagnosis_col    = "diagnosis",
                               diagnosis_fun    = NULL,   # optional: function(metadata) -> vector of labels
                               diagnosis_levels = NULL,   # optional: first level = reference
                               id_col           = "specimenID",
                               samples          = NULL,   # e.g. colnames(counts)
                               test             = c("either", "anova", "kruskal"),
                               fdr_cutoff       = 0.05) {
  test <- match.arg(test)
  
  # IDs
  if (!id_col %in% colnames(metrics))  metrics[[id_col]]  <- rownames(metrics)
  if (!id_col %in% colnames(metadata)) metadata[[id_col]] <- rownames(metadata)
  if (!is.null(samples)) metrics <- metrics[metrics[[id_col]] %in% samples, ]
  
  # diagnosis definition 
  metadata$.dx <- if (is.function(diagnosis_fun)) diagnosis_fun(metadata) else metadata[[diagnosis_col]]
  metadata$.dx <- if (is.null(diagnosis_levels)) factor(metadata$.dx) else
    factor(metadata$.dx, levels = diagnosis_levels)
  
  # numeric metric columns (strip "%" first)
  met <- metrics
  met[] <- lapply(met, function(x) {
    if (is.character(x)) { y <- suppressWarnings(as.numeric(gsub("%", "", x)))
    if (all(is.na(y[!is.na(x)]))) x else y } else x
  })
  metric_vars <- setdiff(names(met)[sapply(met, is.numeric)], id_col)
  
  dat <- inner_join(met[, c(id_col, metric_vars)],
                    metadata[, c(id_col, ".dx")], by = id_col) %>%
    filter(!is.na(.dx)) %>% droplevels()
  if (nlevels(dat$.dx) < 2) stop("Diagnosis has < 2 levels after filtering.")
  
  # est each metric 
  res <- lapply(metric_vars, function(m) {
    d <- dat[!is.na(dat[[m]]), c(m, ".dx")]
    if (nrow(d) < 3 || var(d[[m]]) == 0 || nlevels(droplevels(d$.dx)) < 2) return(NULL)
    fit <- lm(reformulate(".dx", response = m), data = d)
    grp <- tapply(d[[m]], d$.dx, median)
    data.frame(metric    = m,
               n         = nrow(d),
               R2        = summary(fit)$r.squared,
               p_anova   = anova(fit)[["Pr(>F)"]][1],
               p_kruskal = kruskal.test(d[[m]] ~ d$.dx)$p.value,
               as.list(setNames(round(grp, 3), paste0("median_", names(grp)))),
               check.names = FALSE)
  })
  
  res <- bind_rows(res) %>%
    mutate(fdr_anova   = p.adjust(p_anova, "BH"),
           fdr_kruskal = p.adjust(p_kruskal, "BH"),
           significant = switch(test,
                                either  = fdr_anova < fdr_cutoff | fdr_kruskal < fdr_cutoff,
                                anova   = fdr_anova < fdr_cutoff,
                                kruskal = fdr_kruskal < fdr_cutoff)) %>%
    arrange(p_anova)
  
  sig_metrics  <- res$metric[res$significant]
  metrics_keep <- metrics[, !colnames(metrics) %in% sig_metrics, drop = FALSE]
  rownames(metrics_keep) <- metrics_keep[[id_col]]
  
  list(results      = res,
       sig          = filter(res, significant),
       not_sig      = filter(res, !significant),
       metrics_keep = metrics_keep,
       dx_table     = table(dat$.dx))
}


select_technical_vars <- function(counts,
                                  metadata,
                                  metrics,
                                  sample_filter      = NULL,   # function(md) -> logical, e.g. function(md) md$tissue2 != "PC"
                                  exclude_genes      = NULL,   # e.g. delete_var2
                                  exclude_metrics    = NULL,   # metric columns to never consider
                                  sample_id_col      = "specimenID",
                                  gene_id_col        = "gene_id",
                                  n_top_genes        = 2000,
                                  n_pcs              = 5,
                                  min_var_explained  = 0.10,
                                  cor_cutoff         = 0.80,
                                  transform          = c("log", "log1p", "none"),
                                  plots              = TRUE,
                                  copy_to_clipboard  = FALSE,
                                  verbose            = TRUE) {
  transform <- match.arg(transform)
  say <- function(...) if (verbose) message(...)
  
  ## 1. counts prep 
  counts <- as.data.frame(counts, check.names = FALSE)
  if (gene_id_col %in% colnames(counts)) {
    rownames(counts) <- counts[[gene_id_col]]
    counts[[gene_id_col]] <- NULL
  }
  if (!is.null(exclude_genes)) counts <- counts[!(rownames(counts) %in% exclude_genes), , drop = FALSE]
  
  if (!is.null(sample_filter)) {
    keep <- sample_filter(metadata)
    metadata <- metadata[!is.na(keep) & keep, , drop = FALSE]
  }
  
  samples <- Reduce(intersect, list(metadata[[sample_id_col]],
                                    colnames(counts),
                                    metrics[[sample_id_col]]))
  if (length(samples) < 3) stop("Fewer than 3 samples shared by counts, metadata and metrics")
  counts <- as.matrix(counts[, samples, drop = FALSE])
  storage.mode(counts) <- "double"
  say(sprintf("Counts: %d genes x %d samples", nrow(counts), ncol(counts)))
  
  ## 2. log2 CPM + PCA 
  # same as log2(edgeR::cpm(counts) + 1) for a plain matrix
  log_counts <- log2(t(t(counts) / colSums(counts)) * 1e6 + 1)
  
  gene_vars <- apply(log_counts, 1, var)
  gene_vars <- gene_vars[gene_vars > 0]                     # scale. = TRUE fails on constant genes
  top_genes <- names(head(sort(gene_vars, decreasing = TRUE), n_top_genes))
  pca_fit   <- prcomp(t(log_counts[top_genes, ]), center = TRUE, scale. = TRUE)
  
  n_pcs         <- min(n_pcs, ncol(pca_fit$x))
  var_explained <- pca_fit$sdev^2 / sum(pca_fit$sdev^2)     # proportion
  pc_cols       <- paste0("PC", seq_len(n_pcs))
  
  ## 3. PC ~ metric R^2 
  metrics_vars <- metrics[metrics[[sample_id_col]] %in% samples, , drop = FALSE]
  rownames(metrics_vars) <- metrics_vars[[sample_id_col]]
  metrics_vars <- metrics_vars[, sapply(metrics_vars, is.numeric), drop = FALSE]
  metrics_vars <- metrics_vars[, !(colnames(metrics_vars) %in% exclude_metrics), drop = FALSE]
  # constant / all-NA metrics give no information (and break lm / cor)
  informative  <- sapply(metrics_vars, function(x) sum(!is.na(x)) > 2 && var(x, na.rm = TRUE) > 0)
  if (any(!informative)) say("Dropping constant/empty metrics: ", paste(names(which(!informative)), collapse = ", "))
  metrics_vars <- metrics_vars[, informative, drop = FALSE]
  tech_metrics <- colnames(metrics_vars)
  
  pc_scores <- as.data.frame(pca_fit$x[, pc_cols, drop = FALSE])
  pc_scores[[sample_id_col]] <- rownames(pc_scores)
  metrics_vars[[sample_id_col]] <- rownames(metrics_vars)
  pc_meta <- merge(pc_scores, metrics_vars, by = sample_id_col)
  
  r2_matrix <- sapply(tech_metrics, function(m) {
    sapply(pc_cols, function(pc) {
      summary(lm(reformulate(sprintf("`%s`", m), response = pc), data = pc_meta))$r.squared
    })
  })
  r2_matrix <- matrix(r2_matrix, nrow = n_pcs, dimnames = list(
    sprintf("PC%d (%.1f%%)", seq_len(n_pcs), 100 * var_explained[seq_len(n_pcs)]),
    tech_metrics))
  if (verbose) print(round(r2_matrix, 3))
  
  ## 4-5. weighted R^2 and selection 
  weighted_r2 <- sort(colSums(r2_matrix * var_explained[seq_len(n_pcs)]), decreasing = TRUE)
  if (verbose) { cat("\n% of total variation explained (top PCs):\n"); print(round(100 * weighted_r2, 2)) }
  
  selected_metrics <- names(weighted_r2)[weighted_r2 >= min_var_explained]
  say(sprintf("%d metrics >= %.0f%%: %s", length(selected_metrics), 100 * min_var_explained,
              paste(selected_metrics, collapse = ", ")))
  
  out <- list(selected_metrics = selected_metrics, final_metrics = selected_metrics,
              weighted_r2 = weighted_r2, r2_matrix = r2_matrix,
              var_explained = var_explained, pca_fit = pca_fit, pc_meta = pc_meta,
              high_cor = NULL, to_drop = character(), samples = samples)
  if (length(selected_metrics) < 2) {
    say("Fewer than 2 metrics selected; skipping correlation filtering")
    if (copy_to_clipboard) clipr::write_clip(out$final_metrics)
    return(invisible(out))
  }
  
  ## 6. transform + correlation filter 
  mm <- pc_meta[, selected_metrics, drop = FALSE]
  if (transform != "none") {
    nonpos <- selected_metrics[sapply(mm, function(x) any(x <= 0 & !is.na(x)))]
    if (transform == "log" && length(nonpos)) {
      warning("log() of values <= 0 gives -Inf/NaN for: ", paste(nonpos, collapse = ", "),
              ". Consider transform = 'log1p'.")
    }
    fun <- if (transform == "log") log else log1p
    mm[] <- lapply(mm, fun)
  }
  mm[] <- lapply(mm, function(x) { x[!is.finite(x)] <- NA; x })
  
  # if (plots) psych::pairs.panels(mm, method = "pearson", hist.col = "#00AFBB",
  #                                density = TRUE, ellipses = TRUE, main = "Selected metrics")
  
  cor_matrix <- cor(mm, use = "pairwise.complete.obs", method = "pearson")
  cor_upper  <- cor_matrix
  cor_upper[lower.tri(cor_upper, diag = TRUE)] <- NA
  hits <- which(abs(cor_upper) > cor_cutoff, arr.ind = TRUE)
  high_cor <- data.frame(var1 = rownames(cor_upper)[hits[, "row"]],
                         var2 = colnames(cor_upper)[hits[, "col"]],
                         correlation = cor_upper[hits])
  high_cor <- high_cor[order(-abs(high_cor$correlation)), , drop = FALSE]
  rownames(high_cor) <- NULL
  if (verbose) { cat(sprintf("\nPairs with |r| > %.2f:\n", cor_cutoff)); print(high_cor) }
  
  to_drop <- caret::findCorrelation(cor_matrix, cutoff = cor_cutoff, names = TRUE, exact = TRUE)
  final_metrics <- setdiff(selected_metrics, to_drop)
  say("Dropped for correlation: ", if (length(to_drop)) paste(to_drop, collapse = ", ") else "none")
  say("Final metrics: ", paste(final_metrics, collapse = ", "))
  
  if (plots && length(final_metrics) > 1) {
    psych::pairs.panels(mm[, final_metrics, drop = FALSE], method = "pearson", hist.col = "#00AFBB",
                        density = TRUE, ellipses = TRUE, main = "Final metrics")
  }
  if (copy_to_clipboard) clipr::write_clip(final_metrics)
  
  out$final_metrics <- final_metrics
  out$high_cor      <- high_cor
  out$to_drop       <- to_drop
  out$cor_matrix    <- cor_matrix
  invisible(out)
}


run_pca_outliers2 <- function(counts,
                              metadata,
                              sd_threshold = 4,
                              var_cutoff = 10,
                              center = TRUE,
                              scale = TRUE) {
  
  # ── Subset to tissue ────────────────────────────────────────────────────────
  meta_t  <- metadata
  samples <- intersect(meta_t$specimenID, colnames(counts))
  
  if (length(samples) < 3)
    stop(sprintf("'%s' has only %d samples in both objects — need at least 3.",
                 length(samples)))
  
  n_missing <- nrow(meta_t) - length(samples)
  if (n_missing > 0)
    message(sprintf("  [%s] %d metadata samples absent from count matrix.",
                    n_missing))
  
  mat    <- counts[, samples, drop = FALSE]
  meta_t <- meta_t[meta_t$specimenID %in% samples, ]
  n_use  <- nrow(mat)
  
  pca_input <- t(mat)        # prcomp expects samples × genes
  
  # ── PCA ──────────────────────────────────────────────────────────────────────
  pca      <- prcomp(pca_input, center = center, scale. = scale)
  vexp     <- summary(pca)$importance["Proportion of Variance", ] * 100
  scores   <- as.data.frame(pca$x[, 1:min(10, ncol(pca$x))])
  loadings <- pca$rotation
  scores$specimenID <- rownames(scores)
  
  # ── Select PCs with > var_cutoff% variance ─────────────────────────────────
  pcs_screened <- names(vexp)[vexp > var_cutoff]
  
  if (length(pcs_screened) == 0) {
    warning("No PCs explained > ", var_cutoff, "% variance. Defaulting to PC1.")
    pcs_screened <- "PC1"
  }
  
  pc_cols <- intersect(pcs_screened, names(scores))
  
  # ── Outlier detection ───────────────────────────────────────────────────────
  sd_stats <- do.call(rbind, lapply(pc_cols, function(pc) {
    m <- mean(scores[[pc]])
    s <- sd(scores[[pc]])
    data.frame(PC = pc, mean = m, sd = s,
               lower = m - sd_threshold * s,
               upper = m + sd_threshold * s)
  }))
  
  for (pc in pc_cols) {
    lwr <- sd_stats$lower[sd_stats$PC == pc]
    upr <- sd_stats$upper[sd_stats$PC == pc]
    scores[[paste0(pc, "_outlier")]] <- scores[[pc]] < lwr | scores[[pc]] > upr
  }
  
  outlier_cols <- paste0(pc_cols, "_outlier")
  scores$is_outlier <- rowSums(scores[, outlier_cols, drop = FALSE]) > 0
  scores <- left_join(scores, meta_t, by = "specimenID")
  
  outliers <- scores[scores$is_outlier,
                     c("specimenID", pc_cols, outlier_cols)]
  
  list(
    scores        = scores,
    outliers      = outliers,
    sd_stats      = sd_stats,
    loadings      = loadings,
    var_explained = vexp,
    pcs_screened  = pc_cols,
    sd_threshold  = sd_threshold,
    n_genes_used  = n_use
  )
}


plot_pca <- function(pca_result,
                     x_pc       = "PC1",
                     y_pc       = "PC2",
                     colour_var = NULL, title_var=NULL) {
  
  s    <- pca_result$scores
  thr  <- pca_result$sd_threshold
  vexp <- pca_result$var_explained
  
  sd_x <- pca_result$sd_stats$sd[pca_result$sd_stats$PC == x_pc]
  sd_y <- pca_result$sd_stats$sd[pca_result$sd_stats$PC == y_pc]
  mu_x <- pca_result$sd_stats$mean[pca_result$sd_stats$PC == x_pc]
  mu_y <- pca_result$sd_stats$mean[pca_result$sd_stats$PC == y_pc]
  
  # Generate points for the circle/ellipse path
  t <- seq(0, 2 * pi, length.out = 100)
  path_x <- mu_x + (thr * sd_x) * cos(t)
  path_y <- mu_y + (thr * sd_y) * sin(t)
  
  base_aes <- if (!is.null(colour_var) && colour_var %in% names(s)) {
    aes(x = .data[[x_pc]], y = .data[[y_pc]], colour = .data[[colour_var]])
  } else {
    aes(x = .data[[x_pc]], y = .data[[y_pc]])
  }
  
  p <- ggplot(s, base_aes) +
    annotate("rect",
             xmin = mu_x - thr * sd_x, xmax = mu_x + thr * sd_x,
             ymin = mu_y - thr * sd_y, ymax = mu_y + thr * sd_y,
             fill = NA, colour = "firebrick", linetype = "dashed", linewidth = 0.6) +
    geom_point(aes(shape = is_outlier), size = 2, alpha = 0.75) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17),
                       labels = c(`FALSE` = "Pass", `TRUE` = "Outlier"),
                       name   = NULL) +
    geom_label_repel(
      data         = s[s$is_outlier, ],
      aes(label    = specimenID),
      size         = 2.5,
      max.overlaps = 20,
      show.legend  = FALSE
    ) +
    labs(
      x = sprintf("%s (%.1f%%)", x_pc, vexp[x_pc]),
      y = sprintf("%s (%.1f%%)", y_pc, vexp[y_pc])
    ) +
    theme_bw() +
    theme(plot.title = element_text(face = "bold")) 
  # theme(legend.position = "none")
  
  if (!is.null(colour_var) && colour_var %in% names(s))
    p <- p + scale_color_viridis_d(option = "turbo")
  p <- p + ggtitle(title_var)
  p
}



plot_pca_loadings3 <- function(pca_result, pcs = c("PC1", "PC2", "PC3")) {
  # 1. Extract loadings and convert matrix to data frame/tibble
  loadings_mat <- if ("rotation" %in% names(pca_result)) {
    pca_result$rotation  # prcomp standard
  } else {
    pca_result$loadings  # princomp or factanal standard
  }
  
  as_tibble(loadings_mat, rownames = "metric") %>%
    pivot_longer(cols = all_of(pcs), names_to = "PC", values_to = "loading") %>%
    # Ensure PC factor levels follow the input order
    mutate(PC = factor(PC, levels = pcs)) %>%
    # Group by metric to reorder based on maximum absolute loading across selected PCs
    mutate(metric = fct_reorder(metric, abs(loading), .fun = max)) %>%
    ggplot(aes(x = loading, y = metric, fill = loading > 0)) +
    geom_col(show.legend = TRUE) +
    facet_wrap(~PC, ncol = 3) +
    scale_fill_manual(
      values = c("TRUE" = "steelblue", "FALSE" = "tomato"),
      labels = c("TRUE" = "Positive", "FALSE" = "Negative")
    ) +
    labs(
      title = "PCA Loadings — Metric Contributions",
      x = "Loading", 
      y = NULL, 
      fill = "Direction"
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      # Add vertical padding to text and facet spacing
      axis.text.y = element_text(margin = margin(r = 5), size = 10),
      panel.spacing.y = unit(1, "lines")
    )
}



plot_scree <- function(pca_result) {
  var_exp <- pca_result$var_exp
  
  tibble(
    PC      = factor(paste0("PC", seq_along(var_exp)),
                     levels = paste0("PC", seq_along(var_exp))),
    var_exp = var_exp,
    cumvar  = cumsum(var_exp)
  ) %>%
    ggplot(aes(x = PC)) +
    geom_col(aes(y = var_exp), fill = "steelblue", alpha = 0.7) +
    geom_line(aes(y = cumvar, group = 1), color = "firebrick", linewidth = 1) +
    geom_point(aes(y = cumvar), color = "firebrick", size = 2) +
    #scale_y_continuous(labels = scales::percent) +
    labs(title = "Scree Plot", x = NULL,
         y = "Variance Explained (bars = per PC, line = cumulative)") +
    theme_bw()
}



# residual PCA ----

run_pca_res <- function(mat, meta_df, label) {
  pca   <- prcomp(t(mat), center = TRUE, scale. = TRUE)
  pct   <- round(summary(pca)$importance[2, 1:10] * 100, 1)
  df    <- as.data.frame(pca$x[, 1:5]) |>
    rownames_to_column("sample2") |>
    left_join(rownames_to_column(meta_df, "sample2"), by = "sample2") |>
    mutate(dataset = label)
  list(df = df, pct = pct)
}

pca_scatter_res <- function(df, pct, colour_var, title,
                            pc_x = "PC1", pc_y = "PC2") {
  xlab <- paste0(pc_x, " (", pct[as.integer(sub("PC", "", pc_x))], "%)")
  ylab <- paste0(pc_y, " (", pct[as.integer(sub("PC", "", pc_y))], "%)")
  ggplot(df, aes(x = .data[[pc_x]], y = .data[[pc_y]],
                 colour = .data[[colour_var]])) +
    geom_point(size = 2.5, alpha = 0.85) +
    stat_ellipse(aes(group = .data[[colour_var]]), type = "norm",
                 linetype = 2, linewidth = 0.5, level = 0.9) +
    labs(title = title, x = xlab, y = ylab, colour = colour_var) +
    theme_bw(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 11))     
}


