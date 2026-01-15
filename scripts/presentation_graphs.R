
obdat <- left_join((inatcumob %>% filter(park == "ACAD")), (ebdcumob %>% filter(park == "ACAD")),
     by = "year") %>% 
  mutate(cumsum = cumsum.x + cumsum.y)


obdat %>% 
  ggplot(aes(x = year, y = cumsum)) + 
  geom_line(linewidth = 0.8) +
  geom_dl(data = subset(obdat, year == 2023), 
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.45, dl.trans(y = y, x = x - 2.1), "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "Observers") +
  scale_y_continuous(labels = comma, breaks = seq(0, 14000, by = 3000)) +
  scale_x_continuous(limits = c(1980, 2024), breaks = seq(1980, 2024, by = 5)) +
  theme(legend.position = "none",
        legend.background = element_rect(color = "black", linewidth = 0.4),
        legend.title = element_text(face = "bold", size = 17),
        legend.text = element_text(color = "black", size = 17,  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = 17),
        axis.title = element_text(color = "black", size = 17),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")))


ggsave("outputs/acad_plot_users.png", height = 5.28, width = 8, units = "in", dpi = 500)





obsdat <- left_join((totalinat %>% filter(park == "ACAD")), (totalebd %>% filter(park == "ACAD")),
                   by = "year") %>% 
  mutate(cumsum = cumsum.x + cumsum.y)


obsdat %>% 
  ggplot(aes(x = year, y = cumsum)) + 
  geom_line(linewidth = 0.8) +
  geom_dl(data = subset(obsdat, year == 2023),
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.45, dl.trans(y = y, x = x - 2.5), "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "Observations") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits = c(1980, 2024), breaks = seq(1980, 2024, by = 5)) +
  theme(legend.position = "none", #c(0.18, 0.85),
        legend.background = element_rect(color = "black", linewidth = 0.4),
        legend.title = element_text(face = "bold", size = 17),
        legend.text = element_text(color = "black", size = 17,  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = 17),
        axis.title = element_text(color = "black", size = 17),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")))


ggsave("outputs/acad_plot_obs.png", height = 5.28, width = 8, units = "in", dpi = 500)
