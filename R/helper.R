##########################################################
# Project: Some utility scripts for my Quarto books
# Author: Peter Baumgartner
# Edit date: May 19, 2024
# CONTENT:
## - my_glance_data: glance at a specified number of random data
## - my_qq_plot: create histogram with overlaid dnorm curve
## - my_scatter: create scatterplot with lm and loess curve
## - list_plotter: plot color list as a palette
## - save_data_file: save data file
## - pkgs_dl: package downloads
## - t_col: transparent colors
##########################################################



library(glossary)

glossary::glossary_path("../glossary-pb/glossary.yml")


##########################################################
# my_glance_data: Glance at a specified number of random data
# Purpose:
  # To prevent possible bias with head()/tail() or
  # other function that print some data excerpts
# Used in "Statistics with R"
# See: https://bookdown.org/pbaumgartner/swr-harris/
##########################################################

# df   = dataframe or tibble
# N    = number of records chosen randomly
# seed = set.seed for reproducibility

my_glance_data <- function(df, N = 8, seed = 42){
    df_temp <- first_and_last_row(df)

    set.seed(seed)
    df |>
        dplyr::mutate(obs = dplyr::row_number()) |>
        dplyr::relocate(obs) |>
        dplyr::slice_sample(n = N) |>
        dplyr::bind_rows(df_temp) |>
        dplyr::arrange(obs)
}

first_and_last_row <-  function(df) {
    df |>
        dplyr::mutate(obs = dplyr::row_number()) |>
        dplyr::filter(dplyr::row_number() %in% base::c(1, dplyr::n()))
}

##########################################################
# : Create histogram with overlaid dnorm curve
# Purpose:
# Compare histogram with normal distribution and density
# Author: Peter Baumgartner
# Used in my personal notes on "Statistics with R"
# See: https://bookdown.org/pbaumgartner/swr-harris/
##########################################################

# df        = data.frame or tibble
# v         = character: numerical column of data.frame:
#             syntax for call = df$v (NA's are allowed)
# x_label   = character: title for x-axis
# nbins     = numeric: number of bins
# col_fill  = character: fill color
# col_color = character: border color of bins
# col_dens  = character: color of density curve
# col_dnorm = character: color of dnorm curve

my_hist_dnorm <- function(df, v, n_bins = 30,
                       col_fill = "gray90",
                       col_color = "black",
                       col_dnorm = "Normal",
                       col_dens = "Density",
                       x_label = "x") {
    p <- df |>
        ggplot2::ggplot(ggplot2::aes(v)) +
        ggplot2::geom_histogram(
            ggplot2::aes(y = ggplot2::after_stat(density)),
            bins = n_bins,
            fill = col_fill,
            color = col_color,
            na.rm = TRUE) +
        ggplot2::geom_density(
            na.rm = TRUE,
            ggplot2::aes(color = col_dens),
            linewidth = 1,
            ) +
        ggplot2::stat_function(
            fun = dnorm,
            args = c(mean = mean(v, na.rm = TRUE),
                    sd = sd(v, na.rm = TRUE)),
            ggplot2::aes(color = col_dnorm),
            na.rm = TRUE,
            linewidth = 1,
            ) +
        ggplot2::theme_bw() +
        ggplot2::xlab(x_label) +
        ggplot2::scale_color_manual(
            name = "Colors",
            values = c("steelblue", "tomato")
        ) +
        ggplot2::theme(legend.position = "top")


    p

}

##########################################################
# my_qq_plot: Create q-q-plot
# Purpose:
# Generate check normality assumption
# Author: Peter Baumgartner
# Used in my personal notes on "Statistics with R"
# See: https://bookdown.org/pbaumgartner/swr-harris/
##########################################################

# df        = data.frame or tibble
# v         = character: numerical column of data.frame:
#             syntax for call = df$v (NA's are allowed)
# x_label   = character: title for x-axis
# y_label   = character: title for y-axis
# col_qq    = character: color of data
# line_qq.  = character: color of theoretical normal distribution


my_qq_plot <- function(
        df,
        v,
        col_qq = "Data distributed",
        line_qq = "Normally distributed",
        x_label = "x",
        y_label = "y"
        ) {
    p <- df |>
    ggplot2::ggplot(
        ggplot2::aes(sample = v)
    ) +
    ggplot2::stat_qq(
        ggplot2::aes(color = col_qq),
        na.rm = TRUE
    ) +
    ggplot2::stat_qq_line(
        ggplot2::aes(linetype = line_qq),
        linewidth = 1,
        na.rm = TRUE
    ) +
    ggplot2::labs(
        x = x_label,
        y = y_label
    ) +
    ggplot2::scale_color_manual(
        name = "",
        values = ("purple3")
    ) +
    ggplot2::scale_linetype_manual(
        name = "",
        values = ("solid")
    ) +
    ggplot2::guides(
        color = ggplot2::guide_legend(order = 1),
        linetype = ggplot2::guide_legend(order = 2)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")

    p
}

##########################################################
# my_scatter: Create scatterplot with lm and loess curve
# Purpose:
# Generate check
# Author: Peter Baumgartner
# Used in my personal notes on "Statistics with R"
# See: https://bookdown.org/pbaumgartner/swr-harris/
##########################################################

# df        = data.frame or tibble
# v         = character: numerical column of data.frame:
#             syntax for call = df$v (NA's are allowed)
# w         = character: numerical column of data.frame
#             syntax for call = df$w (NA's are allowed)
# x_label   = character: title for x-axis
# y_label   = character: title for y-axis
# col_point = character: color of points
# col_lm    = character: color of linear model
# col_loess = character: color of loess curve


my_scatter <- function(
        df,
        v,
        w,
        col_point = "Point",
        col_lm = "Linear",
        col_loess = "Loess",
        x_label = "x",
        y_label = "y"
) {
    p <- df |>
        ggplot2::ggplot(
            ggplot2::aes(
                x = v,
                y = w
            )
        ) +
        ggplot2::geom_point(
            alpha = 0.6,
            ggplot2::aes(color = col_point),
            na.rm = TRUE
        ) +
        ggplot2::geom_smooth(
            formula = y ~ x,
            method = "lm",
            se = FALSE,
            ggplot2::aes(color = col_lm),
            na.rm = TRUE
        ) +
        ggplot2::geom_smooth(
            formula = y ~ x,
            method = "loess",
            se = FALSE,
            ggplot2::aes(color = col_loess),
            na.rm = TRUE
        ) +
        ggplot2::labs(
            x = x_label,
            y = y_label
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "top") +
        ggplot2::scale_color_manual(
            name = "",
            values = c("purple3", "black", "tomato"),
            breaks = c(col_point, col_lm, col_loess)
        )

    p
}
################################################################
# list_plotter: Plot color list as a palette
# Purpose:
# Display different color palettes for easy comparison
# Author: Emil Hvitfeldt
# Developed for r-color-palettes and {paletteer} package
# See: https://github.com/EmilHvitfeldt/r-color-palettes/blob/main/R/list_plotter.R
# I have used it in my personal notes on "Statistics with R"
# # See: https://bookdown.org/pbaumgartner/swr-harris/
################################################################



list_plotter <- function(color_list, names, package_name) {
    par(mar = c(0, 0, 0, 0) + 0.1)

    plot(
        0,
        0,
        type = "n",
        axes = FALSE,
        bty = "n",
        xlab = "",
        ylab = "",
        xlim = c(0, 1),
        ylim = c(-length(color_list) - 1, 0)
    )

    title(package_name, line = -3)
    for (i in seq_len(length(color_list))) {
        colors_len <- length(color_list[[i]])
        breaks <- seq(from = 0,
                      to = 1,
                      length = colors_len + 1)


        text(0, -i, names[i], pos = 4)
        rect(
            xleft = breaks[1:colors_len],
            xright = breaks[1:colors_len + 1],
            ytop = -0.15 - i,
            ybottom = -0.8 - i,
            col = color_list[[i]],
            border = NA
        )
    }
}

################################################################
# pb_create_folder:
# Purpose:
# check if folder already exists at parameter "path"
# if not, then create folder
# Author: Peter Baumgartner
# path = character string:
#                example: "/Users/xxyyzz/Documents/my-data/"
################################################################
pb_create_folder <- function(path){

  if (!base::file.exists(path))
    {base::dir.create(path)}
}


################################################################
# save_data_file: Save data file for the specified chapter
# Purpose:
# If folder not exists, create it and save object as .rds file
# Author: Peter Baumgartner
# chapter_folder = character: folder inside "data" folder
#                  example "chap05"
# object = data to save
# file_name = character: example "xy_object.rds"
# I have used the function in my notes on "Statistics with R"
# # See: https://bookdown.org/pbaumgartner/swr-harris/
################################################################

pb_save_data_file <- function(chapter_folder, object, file_name){
    data_folder <- base::paste0(here::here(), "/data/")
    if (!base::file.exists(data_folder))
    {base::dir.create(data_folder)}

    chap_folder <-
        base::paste0(
            here::here(),
            paste0("/data/", chapter_folder, "/")
        )
    if (!base::file.exists(chap_folder))
    {base::dir.create(chap_folder)}

    base::saveRDS(object = object,
                  file = paste0(chap_folder, "/", file_name))
}


################################################################
# pkgs_downloads: Get number of downloads from RStudio CRAN Mirror
# Purpose:
# Compare popularity of different packages
# Author: Peter Baumgartner
# pkgs = character vector of package names
# period = "last-day" "last-week", "last-month"
# days: period days = 1, 7, 30
# returns: tibble with packages sorted by download figures
# I have used the function in my notes on "Statistics with R"
# # See: https://bookdown.org/pbaumgartner/swr-harris/
################################################################
pkgs_dl <-  function(pkgs, period = "last-week", days = 7) {
    dl_pkgs <- cranlogs::cran_downloads(when = period, packages = pkgs)

    start_date = base::min(dl_pkgs$date)
    end_date = base::max(dl_pkgs$date)

    dl_pkgs |>
        dplyr::group_by(package) |>
        dplyr::summarize(average = trunc(sum(count) / days)) |>
        dplyr::arrange(desc(average)) |>
        dplyr::mutate(
            from = start_date,
            to = end_date
            )
}


## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk

t_col <- function(color, percent = 50, name = NULL) {
    #      color = color name
    #    percent = % transparency
    #       name = an optional name for the color

    ## Get RGB values for named color
    rgb.val <- col2rgb(color)

    ## Make new color using input color as base and alpha set by transparency
    t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
                 max = 255,
                 alpha = (100 - percent) * 255 / 100,
                 names = name)

    ## Save the color
    invisible(t.col)
}

### revised sf:::plot_sf()

sf_plot <- function (x, y, ..., main, pal = NULL, nbreaks = 10, breaks = "pretty",
                     max.plot = getOption("sf_max.plot", default = 9), key.pos = get_key_pos(x,
                                                                                             ...), key.length = 0.618, key.width = kw_dflt(x, key.pos),
                     reset = TRUE, logz = FALSE, extent = x, xlim = sf::st_bbox(extent)[c(1,
                                                                                          3)], ylim = sf::st_bbox(extent)[c(2, 4)], compact = FALSE)
{
  stopifnot(missing(y))
  nbreaks.missing = missing(nbreaks)
  key.pos.missing = missing(key.pos)
  max_plot_missing = missing(max.plot)
  dots = list(...)
  col_missing = is.null(dots$col)
  breaks_numeric = is.numeric(breaks)
  reset_layout_needed = reset
  x = sf:::swap_axes_if_needed(x)
  opar = par(no.readonly = TRUE)
  if (ncol(x) > 2 && !isTRUE(dots$add)) {
    cols = setdiff(names(x), attr(x, "sf_column"))
    lt = .get_layout(sf::st_bbox(x), min(max.plot, length(cols)),
                     par("din"), key.pos[1], key.width)
    if (key.pos.missing || key.pos[1] == -1)
      key.pos = lt$key.pos
    layout(lt$m, widths = lt$widths, heights = lt$heights,
           respect = compact)
    if (isTRUE(dots$axes))
      par(mar = c(2.1, 2.1, 1.2, 0))
    else par(mar = c(0, 0, 1.2, 0))
    if (max_plot_missing)
      max.plot = prod(lt$mfrow)
    if (isTRUE(is.finite(max.plot)) && ncol(x) - 1 > max.plot &&
        max_plot_missing && is.null(options("sf_max.plot")[[1]]))
      warning(paste("plotting the first", max.plot, "out of",
                    ncol(x) - 1, "attributes; use max.plot =", ncol(x) -
                      1, "to plot all"), call. = FALSE)
    cols = setdiff(names(x), attr(x, "sf_column"))
    if (length(cols) > max.plot)
      cols = cols[1:max.plot]
    if (!is.null(key.pos)) {
      values = do.call(c, as.data.frame(x)[cols])
      if (is.character(values))
        values = as.factor(values)
      if (logz)
        values = log10(values)
      if (is.character(breaks) && is.numeric(values)) {
        v0 = values[!is.na(values)]
        n.unq = length(unique(v0))
        breaks = if (!all(is.na(values)) && n.unq > 1)
          classInt::classIntervals(v0, min(nbreaks, n.unq),
                                   breaks, warnSmallN = FALSE)$brks
        else range(values, na.rm = TRUE)
        nbreaks = length(breaks) - 1
      }
    }
    if (nbreaks.missing && is.numeric(breaks))
      nbreaks = length(breaks) - 1
    lapply(cols, function(cname) plot(x[, cname], main = cname,
                                      pal = pal, nbreaks = nbreaks, breaks = breaks, key.pos = NULL,
                                      reset = FALSE, logz = logz, xlim = xlim, ylim = ylim,
                                      ...))
    for (i in seq_len(prod(lt$mfrow) - length(cols))) plot.new()
    if (!is.null(key.pos) && key.pos[1] != 0 && col_missing) {
      if (is.null(pal))
        pal = function(n) sf.colors(n, categorical = is.factor(values))
      colors = if (is.function(pal))
        pal(nbreaks)
      else pal
      if (is.factor(values))
        .image_scale_factor(levels(values), colors, key.pos = key.pos,
                            key.width = key.width, key.length = key.length,
                            ...)
      else .image_scale(values, colors, breaks = breaks,
                        key.pos = key.pos, key.length = key.length, logz = logz,
                        ...)
    }
  }
  else {
    if (isTRUE(x) && ncol(x) == 1) { # added "isTRUE(x) &&"
      plot(st_geometry(x), xlim = xlim, ylim = ylim, ...)
      reset_layout_needed = FALSE
    }
    else {
      if (isTRUE(x) && ncol(x) > 2) { # added "isTRUE(x) &&"
        warning("ignoring all but the first attribute")
        x = x[, 1]
      }
      values = x[[setdiff(names(x), attr(x, "sf_column"))]] # error: "attempt to select less than one element in get1index"
      if (is.list(values))
        stop("plotting list-columns not supported")
      if (is.character(values))
        values = as.factor(values)
      else if (logz)
        values = log10(as.numeric(values))
      if (is.null(pal))
        pal = function(n) sf.colors(n, categorical = is.factor(values))
      else if (!col_missing)
        stop("specify only one of `col' and `pal'")
      if (col_missing) {
        col = if (is.factor(values)) {
          if (key.pos.missing && nlevels(values) > 30)
            key.pos = NULL
          colors = if (is.function(pal))
            pal(nlevels(values))
          else pal
          colors[as.numeric(values)]
        }
        else {
          if (!inherits(values, c("POSIXt", "Date")))
            values = as.numeric(values)
          if (is.character(breaks)) {
            v0 = values[!is.na(values)]
            n.unq = length(unique(v0))
            breaks = if (!all(is.na(values)) && n.unq >
                         1)
              classInt::classIntervals(v0, min(nbreaks,
                                               n.unq), breaks, warnSmallN = FALSE)$brks
            else range(values, na.rm = TRUE)
          }
          nbreaks = length(breaks) - 1
          cuts = if (all(is.na(values)))
            rep(NA_integer_, length(values))
          else if (!breaks_numeric && diff(range(values,
                                                 na.rm = TRUE)) == 0)
            ifelse(is.na(values), NA_integer_, 1L)
          else cut(values, breaks, include.lowest = TRUE)
          colors = if (is.function(pal))
            pal(nbreaks)
          else pal
          colors[cuts]
        }
      }
      else {
        col = dots$col
        if (length(col) != 1 && length(col) != nrow(x))
          warning("col is not of length 1 or nrow(x): colors will be recycled; use pal to specify a color palette")
        key.pos = NULL
      }
      if (!isTRUE(dots$add) && !is.null(key.pos) && !all(is.na(values)) &&
          (is.factor(values) || length(unique(na.omit(values))) >
           1 || breaks_numeric) && length(col) > 1) {
        switch(key.pos[1], layout(matrix(c(2, 1), nrow = 2,
                                         ncol = 1), widths = 1, heights = c(1, key.width)),
               layout(matrix(c(1, 2), nrow = 1, ncol = 2),
                      widths = c(key.width, 1), heights = 1), layout(matrix(c(1,
                                                                              2), nrow = 2, ncol = 1), widths = 1, heights = c(key.width,
                                                                                                                               1)), layout(matrix(c(2, 1), nrow = 1, ncol = 2),
                                                                                                                                           widths = c(1, key.width), heights = 1))
        if (is.factor(values)) {
          .image_scale_factor(levels(values), colors,
                              key.pos = key.pos, key.width = key.width,
                              key.length = key.length, ...)
        }
        else .image_scale(values, colors, breaks = breaks,
                          key.pos = key.pos, key.length = key.length,
                          logz = logz, ...)
      }
      else reset_layout_needed = FALSE
      if (!isTRUE(dots$add)) {
        mar = c(1, 1, 1.2, 1)
        if (isTRUE(dots$axes))
          mar[1:2] = 2.1
        par(mar = mar)
      }
      if (col_missing)
        plot(st_geometry(x), col = col, xlim = xlim,
             ylim = ylim, ...)
      else plot(st_geometry(x), xlim = xlim, ylim = ylim,
                ...)
    }
    if (!isTRUE(dots$add)) {
      if (missing(main)) {
        main = setdiff(names(x), attr(x, "sf_column"))
        if (length(main) && inherits(x[[main]], "units"))
          main = make_unit_label(main, x[[main]])
      }
      localTitle <- function(..., extent, col, bg, pch,
                             cex, lty, lwd, axes, type, bgMap, border, graticule,
                             xlim, ylim, asp, bgc, xaxs, yaxs, lab, setParUsrBB,
                             expandBB, col_graticule, at, lon, lat, crs, datum,
                             ndiscr, margin) title(...)
      localTitle(main, ...)
    }
  }
  if (!isTRUE(dots$add) && reset) {
    if (reset_layout_needed)
      layout(matrix(1))
    par(opar)
  }
  invisible()
}


## END

