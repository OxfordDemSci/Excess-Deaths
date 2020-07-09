
library(patchwork)
library(data.table)
library(ggplot2)

rm(list=ls())

load('Data/LifeExpectancyInequality.RData')

EW.results[,sex := factor(sex,levels = c('females' ,'males' ,'both'))]

EW.results <- EW.results[sex != 'both']

Figure.1 <- ggplot(EW.results,aes(x=year, y = e0))+
  ggtitle('A) Life expectancy at birth')+
  geom_ribbon(aes(x=year, y = e0, ymin = lower.e0, ymax = upper.e0, fill = sex), show.legend = F, alpha = 1/4)+
    geom_path(aes(year, e0, color = sex), size = 1,show.legend = F, lineend = "round")+
  scale_color_manual('Sex' , values = c("#72B2B4", "#B4A097",'#615652'),labels = c('Females','Males','Both'))+
    scale_fill_manual('Sex' , values = c("#72B2B4", "#B4A097",'#615652'),labels = c('Females','Males','Both'))+
  theme(
    aspect.ratio = 1,
    text = element_text(face = 1)
  )+
  labs(
    x = NULL,
    y = "Years"
  )
  
Figure.1

Figure.2 <- ggplot(EW.results,aes(x=year, y = sigma))+
  ggtitle('B) Lifespan inequality (standard deviation)')+
  geom_ribbon(aes(x=year, y = sigma, ymin = lower.sigma, ymax = upper.sigma, fill = sex), show.legend = F, alpha = 1/4)+
  geom_path(aes(year, sigma, color = sex), size = 1,show.legend = T, lineend = "round")+
  scale_color_manual('Sex' , values = c("#72B2B4", "#B4A097",'#615652'),labels = c('Females','Males','Both'))+
  scale_fill_manual('Sex' , values = c("#72B2B4", "#B4A097",'#615652'),labels = c('Females','Males','Both'))+
  theme(
    aspect.ratio = 1,
    text = element_text(face = 1)
  )+
  labs(
    x = NULL,
    y = NULL
  )

Figure.2

Figure.3 <- Figure.1|Figure.2


pdf(file = 'Figures/Figure_3.pdf',width = 9 ,height = 7)
Figure.3
dev.off()

## A table with summary measures
library(reshape2)

cbind(EW.results[year %in% c(2005,2010,2015,2019,2020),1:2],
      round(EW.results[year %in% c(2005,2010,2015,2019,2020),3:8],1))


#Checks with offical lifetables and life expectancy function

ggplot(EW.results,aes(x=year, y = e0))+
  ggtitle('A) Life expectancy at birth')+
  geom_ribbon(aes(x=year, y = e0, ymin = lower.e0, ymax = upper.e0, fill = sex), show.legend = F, alpha = 1/4)+
  geom_path(aes(year, e0, color = sex), size = 1,show.legend = F, lineend = "round")+
  geom_line(data = EW.results.2,aes(year,V1,color = sex))+
  geom_point(data = life.tables.EW.1982.2018[x == 0 & upper.year %in% 2001:2018], aes(upper.year,ex,color = sex))+
  scale_color_manual('Sex' , values = c("#72B2B4", "#B4A097",'#615652'),labels = c('Females','Males','Both'))+
  scale_fill_manual('Sex' , values = c("#72B2B4", "#B4A097",'#615652'),labels = c('Females','Males','Both'))+
  theme(
    aspect.ratio = 1,
    text = element_text(face = 1)
  )+
  labs(
    x = NULL,
    y = "Years"
  )


