library(ggplot2)
library(ggrepel)
library(plotly)

results = data.frame(Service = c("Emergency Management", "Fleet", "Patrol", "Administration", "Weatherization", "Facilities", "Cybersecurity", "Revenue Collections"),
                     Equity_Rating = c(2,3,4,3,4,5,5,4),
                     Impact_Rating = c(3,1,4,4,4,3,4,5),
                     Resiliency_Rating = c(5,4,3,2,1,5,4,3),
                     Steward_Rating = c(1,3,5, 4,5,3,2,4),
                     Budget = c(66000, 23456, 5000, 50000, 75000, 80000,100000, 20000)) %>%
  rowwise() %>%
  mutate(Total_Score = sum(Equity_Rating, Impact_Rating, Resiliency_Rating, Steward_Rating, na.rm = TRUE))

bar_group = pivot_longer(results, 
                         cols = c("Equity_Rating", "Impact_Rating", "Resiliency_Rating", "Steward_Rating"), 
                         names_to = "Criteria",
                         values_to = "Rating") %>%
  select(-`Budget`, -`Total_Score`)

##traditional option ===============
ggplot(bar_group, aes(y = Service, x = Rating, fill = Criteria)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer() + 
  labs(title = "Proposal Scores") +
  geom_text(aes(label = Rating), position = position_stack(vjust = .5), color = "black", size = 5)


##fancy option ===========
fig <- plot_ly(x=results$Equity_Rating, 
               y=results$Impact_Rating, 
               z=results$Resiliency_Rating, 
               text = results$Service,
               name = "Stewardship Score",
               hoverinfo = "text",
               type="scatter3d", 
               mode="text", 
               marker = list(symbol = "circle", 
                             sizemode = "diameter", 
                             size = results$Budget/5000,
                             colorbar = list(title = "Stewardship Score")),
               color=results$Steward_Rating,
               colors = c("red", "orange", "yellow", "green", "blue")) %>%
  add_trace(type = "mesh3d",
            name = "Best Score",
            x = c(5, 5, 4, 4, 5, 5, 4, 4),
            y = c(5, 4, 4, 5, 5, 4, 4, 5),
            z = c(5, 5, 5, 5, 4, 4, 4, 4),
            i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
            j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
            k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
            facecolor = rep("blue", 12),
            opacity = .2,
            text = "Best Score", 
            showlegend = FALSE) %>%
  add_text(x = 5, y = 5, z = 5, text = "Best Score", showlegend = FALSE)

fig <- fig %>% layout(title = "Proposal Scores",
                      scene = list(
                        aspectratio = list(
                          x = 1,
                          y = 1,
                          z = 1
                        ),
                        camera = list(
                          center = list(
                            x = 0,
                            y = 0,
                            z = 0
                          ),
                          eye = list(
                            x = 1.96903462608,
                            y = -1.09022831971,
                            z = 0.405345349304
                          ),
                          up = list(
                            x = 0,
                            y = 0,
                            z = 1
                          )
                        ),
                        dragmode = "turntable",
                        xaxis = list(title = 'Equity Score', range = c(5,0)),
                        yaxis = list(title = 'Impact Score', range = c(0,5)),
                        zaxis = list(title = 'Resiliency Score', range = c(5,0))),
                      legend = list(title = list(text= '<b> Stewardship Score </b>')),
                      show_legend = TRUE)

fig

##unfinished plot =======
ggplot(results,aes(x = Equity_Rating, 
                   y = Impact_Rating, 
                   label = Service)) +
  geom_point(size = results$Budget / 10000,
                                color = "orange") +
  # geom_point(data = results, aes(x = Resiliency_Rating,
  #                                y = Steward_Rating,
  #                                label = Service,
  #                                size = Budget / 10000,
  #                                color = "yellow")) +
  xlim(-5,5) +
  ylim(-5,5) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  scale_x_continuous(limits = c(-5, 5), breaks = c(-3,3),
                     labels = c("-3" = "Impact", "3" = "Resiliency")) +
  scale_y_continuous(limits = c(-5, 5), breaks = c(-3,3),
                     labels = c("-3" = "Stewardship", "3" = "Equity")) +
  labs(title = "Proposal Rankings",
       x = "Impact / Resiliency",
       y = "Stewardship / Equity") +
  geom_label_repel(size = 3,
                   fill = "deepskyblue",
                   colour = "black",
                   min.segment.length = unit(0, "lines")) +
  annotate(geom = "rect", xmin = -1, xmax = 1, ymin = -1, ymax = 1, alpha = .2, fill = "green") +
  geom_text(x = 0, y = 0, label = "Best Score", color = "Black") +
  theme_bw() +
  theme(axis.line = element_line(color = "gray"),
           panel.border = element_blank(),
           panel.grid.minor = element_blank())
