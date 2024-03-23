library(tidyverse)

# Create a data frame (a tibble) with one row for each
# coordinate in your grid

n.cells<-25 #Number of vertical and horizontal cells

combos <- expand.grid(x = seq((1/n.cells)/2, 1-(1/n.cells)/2, length.out=n.cells), 
                      y = seq((1/n.cells)/2, 1-(1/n.cells)/2, length.out=n.cells)) %>%
  as_tibble()

#Target percent cover values

cover <- seq(10,90,by=10)

#Draw random samples from the data without replacement

grid_sample <- data.frame()

for (i in cover){
  
  grid_sample_i <- sample_n(combos, 
                            size = n.cells*n.cells*(i/100), 
                            replace = FALSE)
  grid_sample <- rbind(grid_sample,
                       data.frame(grid_sample_i,
                                  cover=rep(i, nrow(grid_sample_i))))
}

#Plot cover stickers
#Add a square on the bottom left of each panel to show an area that represents
#X% of the total area

grid_sample$cover <- paste(grid_sample$cover, "% cover", sep="")

data_lines_h<-data.frame(x=rep(0, 9),
                         y=sqrt(n.cells*n.cells*(cover/100))/n.cells,
                         xend=sqrt(n.cells*n.cells*(cover/100))/n.cells,
                         yend=sqrt(n.cells*n.cells*(cover/100))/n.cells,
                         cover=paste(cover, "% cover", sep=""))

data_lines_v<-data.frame(x=sqrt(n.cells*n.cells*(cover/100))/n.cells,
                         y=rep(0, 9),
                         xend=sqrt(n.cells*n.cells*(cover/100))/n.cells,
                         yend=sqrt(n.cells*n.cells*(cover/100))/n.cells,
                         cover=paste(cover, "% cover", sep=""))

data_lines <- rbind(data_lines_v, data_lines_h)

ggplot(grid_sample,
       aes(x=x,
           y=y))+
  geom_point(shape=15,
             size=2.05,
             color=viridis::viridis(3)[2])+
  facet_wrap(~cover)+
  theme_minimal()+
  geom_segment(data=data_lines,
               aes(x=x,
                   y=y,
                   xend=xend,
                   yend=yend),
               inherit.aes = FALSE,
               color=viridis::viridis(3)[1],
               size=1)+
  geom_segment(data=data.frame(x=c(0,0,1,1),
                               y=c(0,1,1,0),
                               xend=c(0,1,1,0),
                               yend=c(1,1,0,0)),
               aes(x=x,
                   y=y,
                   xend=xend,
                   yend=yend),
               inherit.aes = FALSE)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(size=14),
        panel.background = element_rect(fill=alpha(viridis::viridis(3)[3], alpha=0.1)))+
  coord_fixed()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))

#Export high resolution image

ggsave("Cover_stickers.jpg", 
       dpi=1000,
       width = 7.07,
       height = 6.48)






