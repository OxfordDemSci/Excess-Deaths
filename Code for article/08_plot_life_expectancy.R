
rm(list=ls())

library(data.table)
library(tidyverse)

fig <- list()

source('R/Figure_specifications.R')

# Load data -------------------------------------------------------

load('Data/LifeExpectancyInequality.RData')

# Plot cumulative excess deaths by sex ----------------------------
fig.data <- EW.results[sex!='both']

fig$figure3 <-
  fig.data  %>%
  ggplot() +
  ggtitle('A) Life expectancy at birth')+
  geom_ribbon(
    aes(
      x = year,
      ymin = lower.ex,
      ymax = upper.ex,
      fill = sex
    ),
    alpha = 0.3
  ) +
  geom_line(
    aes(x = year, y = ex, color = sex)
  ) +
  geom_point(
    aes(x = year, y = ex, color = sex),
    size = 1
  ) +
  scale_color_manual(values = c("#1E8B8F","#806152")) +
  scale_fill_manual(values = c("#1E8B8F","#806152")) +
  labs(
    x = NULL,
    y = 'Years'
  ) +
  annotate("text", x = 2004, y = c(82.5,78.2),
           label = c('Females','Males'),
           angle = c(30,38),
           color = c("#1E8B8F","#806152"),
           size = 6.5, hjust = 0, vjust = 1) +
  fig_spec$MyGGplotTheme(
    grid = 'xy', scaler = 1.3, show_legend = FALSE,
    axis = 'x'
  )

fig$figure4 <-
  fig.data  %>%
  ggplot() +
  ggtitle('B) Lifespan inequality')+
  geom_ribbon(
    aes(
      x = year,
      ymin = lower.ex.1,
      ymax = upper.ex.1,
      fill = sex
    ),
    alpha = 0.3
  ) +
  geom_line(
    aes(x = year, y = sd, color = sex)
  ) +
  geom_point(
    aes(x = year, y = sd, color = sex),
    size = 1
  ) +
  #scale_y_continuous(labels = scales::label_comma()) +
  scale_color_manual(values = c("#1E8B8F","#806152")) +
  scale_fill_manual(values = c("#1E8B8F","#806152")) +
  labs(
    x = NULL,
    y = NULL
  ) +
  fig_spec$MyGGplotTheme(
    grid = 'xy', scaler = 1.3, show_legend = FALSE,
    axis = 'x'
  )

library(patchwork)

fig_spec$ExportPDF(
  fig$figure3+fig$figure4, filename = 'Figure_3', path = 'Figures',
  width = fig_spec$width, height = 0.6*fig_spec$width, scale = 1.21
)
fig_spec$ExportPNG(
  fig$figure3+fig$figure4, filename = 'Figure_3', path = 'Figures',
  width = fig_spec$width, height = 0.6*fig_spec$width, scale = 1.21
)



#Checks with offical lifetables and life expectancy function

# ggplot(EW.results,aes(x=year, y = e0))+
#   ggtitle('A) Life expectancy at birth')+
#   geom_ribbon(aes(x=year, y = e0, ymin = lower.e0, ymax = upper.e0, fill = sex), show.legend = F, alpha = 1/4)+
#   geom_path(aes(year, e0, color = sex), size = 1,show.legend = F, lineend = "round")+
#   geom_line(data = EW.results.2,aes(year,V1,color = sex))+
#   geom_point(data = life.tables.EW.1982.2018[x == 0 & upper.year %in% 2001:2018], aes(upper.year,ex,color = sex))+
#   scale_color_manual('Sex' , values = c("#72B2B4", "#B4A097",'#615652'),labels = c('Females','Males','Both'))+
#   scale_fill_manual('Sex' , values = c("#72B2B4", "#B4A097",'#615652'),labels = c('Females','Males','Both'))+
#   theme(
#     aspect.ratio = 1,
#     text = element_text(face = 1)
#   )+
#   labs(
#     x = NULL,
#     y = "Years"
#   )



#life expectancy results in the manuscript
cbind(EW.results[order(sex,year),1:2],round(EW.results[order(sex,year),3:8],1))






