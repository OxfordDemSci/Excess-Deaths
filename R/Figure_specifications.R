# figure specs
fig_spec <- list()

# ggplot theme ----------------------------------------------------

# ggplot theme by Jonas Schöley
fig_spec$MyGGplotTheme <-
  function (
    size = 8,
    family = 'sans',
    scaler = 1,
    no_axes = FALSE,
    panel_border = FALSE,
    hgrid = FALSE,
    vgrid = FALSE,
    show_legend = TRUE,
    ar = NA
  ) {

    size_med = size*scaler
    size_sml = round(size*0.7)*scaler
    base_linesize = 0.3*scaler

    list(
      theme_classic(base_size = size_med, base_family = family),
      theme(
        # basic
        text = element_text(color = 'black'),
        line = element_line(size = base_linesize, lineend = 'square'),
        # axis
        #axis.line.y = element_blank(),
        axis.title = element_text(size = size_med, face = 'bold'),
        axis.ticks = element_line(size = rel(0.5), color = 'black'),
        axis.text = element_text(size = size_med, color = 'black'),
        # strips
        strip.text = element_text(color = 'black', size = size_med),
        strip.background = element_blank(),
        # plot
        title = element_text(face = 'bold'),
        plot.subtitle = element_text(color = 'black', size = size_med, face = 'bold'),
        plot.caption = element_text(color = 'black', size = size_sml, face = 'plain'),
        plot.background = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(1, 0.1, 0.5, 0.5), units = 'mm'),
      ),
      if (isTRUE(hgrid)) {
        theme(panel.grid.major.y =
                element_line(linetype = 3, color = 'grey80'))
      },
      if (isTRUE(vgrid)) {
        theme(panel.grid.major.x =
                element_line(linetype = 3, color = 'grey80'))
      },
      if (isTRUE(panel_border)) {
        theme(
          panel.border =
            element_rect(fill = NA)
        )
      },
      if (!isTRUE(show_legend)) {
        theme(legend.position = 'none')
      },
      if (isTRUE(no_axes)) {
        theme(
          axis.line = element_blank()
        )
      },
      if (!is.na(ar)) {
        theme(
          aspect.ratio = ar
        )
      }
    )
  }

# Dimensions ------------------------------------------------------

# figure width (mm)
fig_spec$width = 170

# Colors ----------------------------------------------------------

# color palette
fig_spec$discrete_colors <-
  c('#D23737', # red
    '#3191C9', # blue
    '#D2BC2D', # yellow
    '#4EC93B', # green
    '#881F93', # purple
    '#C5752B') # orange
fig_spec$discrete_colors_light <-
  c('#FCB3B3', # red
    '#A7DDFC', # blue
    '#FAEC8E'  # yellow
    )

fig_spec$sex_colors <-
  c(`m` = '#806152', `f` = '#1E8B8F')

# Export function -------------------------------------------------

fig_spec$ExportPDF <-
  function (figure, filename, path, ...) {
    ggsave(
      filename = paste0(filename, '.pdf'),
      plot = figure,
      path = path,
      units = 'mm',
      dpi = 300,
      useDingbats = FALSE,
      ...
    )
  }

fig_spec$ExportPNG <-
  function (figure, filename, path, ...) {
    ggsave(
      filename = paste0(filename, '.png'),
      plot = figure,
      path = path,
      units = 'mm',
      dpi = 300,
      ...
    )
  }
