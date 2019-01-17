load("./data/processed_data.Rdata")


library(ggplot2)
library(viridis)
library(dplyr)
library(rgeos)


poly_ggplot <- gBuffer(poly, byid = TRUE, width = 0)
poly_ggplot$ovw <- substr(poly_ggplot$NUTS_ID, 1, 2)
poly_ggplot_ovw <- fortify(poly_ggplot, region = "ovw")
poly_ggplot <- fortify(poly_ggplot, region = "NUTS_ID")


poly_plot_ovw <- poly_ggplot
poly_plot_ovw$Country <- as.factor(substr(poly_ggplot$id, 1, 2))

p_ovw <- ggplot() + 
  geom_polygon(data = poly_plot_ovw, 
               aes(fill = Country, x = long, y = lat, group = group)) +
  geom_path(data = poly_ggplot_ovw, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.1) + 
#  geom_path(data = poly_ggplot_ovw, aes(x = long, y = lat, group = group), 
#            color = 'red', size = 0.25) +
  coord_equal() + 
  labs(x = NULL, y = NULL) + 
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),axis.title.y = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)) + 
  scale_fill_viridis(option = "plasma", discrete = TRUE) + 
  theme(plot.margin = unit(c(0,0.5,0,0.5), "cm")) + 
  theme(legend.position = "bottom", legend.justification = "center", 
        legend.direction = "horizontal", 
        legend.background = element_rect(fill="transparent"), 
        legend.title = element_blank()) + 
  guides(fill = guide_legend(nrow = 2,byrow = TRUE))






gdp_pc <- depvar_list$gdp_pc

quant_gdp_pc_00 <- quantile(gdp_pc$GDP_pc_2000, probs = seq(0, 1, by = 1/6))
quant_gdp_pc_07 <- quantile(gdp_pc$GDP_pc_2007, probs = seq(0, 1, by = 1/6))
quant_gdp_pc_08 <- quantile(gdp_pc$GDP_pc_2008, probs = seq(0, 1, by = 1/6))
quant_gdp_pc_15 <- quantile(gdp_pc$GDP_pc_2015, probs = seq(0, 1, by = 1/6))

equal_gdp_pc_00 <- seq(min(gdp_pc$GDP_pc_2000), max(gdp_pc$GDP_pc_2000), 
                       by = (max(gdp_pc$GDP_pc_2000) - min(gdp_pc$GDP_pc_2000)))
equal_gdp_pc_07 <- seq(min(gdp_pc$GDP_pc_2007), max(gdp_pc$GDP_pc_2007), 
                       by = (max(gdp_pc$GDP_pc_2007) - min(gdp_pc$GDP_pc_2007)))
equal_gdp_pc_08 <- seq(min(gdp_pc$GDP_pc_2008), max(gdp_pc$GDP_pc_2008), 
                       by = (max(gdp_pc$GDP_pc_2008) - min(gdp_pc$GDP_pc_2008)))
equal_gdp_pc_15 <- seq(min(gdp_pc$GDP_pc_2015), max(gdp_pc$GDP_pc_2015), 
                       by = (max(gdp_pc$GDP_pc_2015) - min(gdp_pc$GDP_pc_2015)))

gdp_pc$quant_gdp_pc_00 <- cut(gdp_pc$GDP_pc_2000, breaks = quant_gdp_pc_00, include_lowest = T)
gdp_pc$quant_gdp_pc_07 <- cut(gdp_pc$GDP_pc_2007, breaks = quant_gdp_pc_07, include_lowest = T)
gdp_pc$quant_gdp_pc_08 <- cut(gdp_pc$GDP_pc_2008, breaks = quant_gdp_pc_08, include_lowest = T)
gdp_pc$quant_gdp_pc_15 <- cut(gdp_pc$GDP_pc_2015, breaks = quant_gdp_pc_15, include_lowest = T)

gdp_pc$equal_gdp_pc_00 <- cut(gdp_pc$GDP_pc_2000, breaks = equal_gdp_pc_00, include_lowest = T)
gdp_pc$equal_gdp_pc_07 <- cut(gdp_pc$GDP_pc_2007, breaks = equal_gdp_pc_07, include_lowest = T)
gdp_pc$equal_gdp_pc_08 <- cut(gdp_pc$GDP_pc_2008, breaks = equal_gdp_pc_08, include_lowest = T)
gdp_pc$equal_gdp_pc_15 <- cut(gdp_pc$GDP_pc_2015, breaks = equal_gdp_pc_15, include_lowest = T)


poly_plot_gdp_pc <- na.omit(left_join(poly_ggplot, gdp_pc[ , c(1, 38:ncol(gdp_pc))], by = c("id" = "NUTS_ID")))


gdp_pc_plot_list <- list()
plot_names <- names(poly_plot_gdp_pc)[8:ncol(poly_plot_gdp_pc)]

for(i in 1:length(plot_names)){
  gdp_pc_plot_list[[plot_names[i]]]  <- ggplot() + 
    geom_polygon(data = poly_plot_gdp_pc, 
                 aes_string(fill = plot_names[i], 
                            x = "long", y = "lat", group = "group")) + 
    geom_path(data = poly_plot_gdp_pc, aes(x = long, y = lat, group = group), 
              color = "black", size = 0.05) + 
    labs(x = NULL, y = NULL) + 
    theme(axis.line = element_blank(), 
          axis.text.x=element_blank(), axis.text.y = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA)) + 
    coord_equal() + 
    scale_fill_viridis(option = "plasma", direction = -1, discrete = TRUE) + 
    theme(legend.position = "none", legend.justification = c(0,1), 
          legend.background = element_rect(fill="transparent"), 
          legend.title = element_blank())
}




gdp_gr <- depvar_list$gdp_pc_gr

quant_gdp_gr_0015 <- quantile(gdp_gr$GDP_gr_0015, probs = seq(0, 1, by = 1/6))
quant_gdp_gr_0007 <- quantile(gdp_gr$GDP_gr_0007, probs = seq(0, 1, by = 1/6))
quant_gdp_gr_0815 <- quantile(gdp_gr$GDP_gr_0815, probs = seq(0, 1, by = 1/6))

equal_gdp_gr_0015 <- seq(min(gdp_gr$GDP_gr_0015), max(gdp_gr$GDP_gr_0015), 
                       by = (max(gdp_gr$GDP_gr_0015) - min(gdp_gr$GDP_gr_0015)))
equal_gdp_gr_0007 <- seq(min(gdp_gr$GDP_gr_0007), max(gdp_gr$GDP_gr_0007), 
                       by = (max(gdp_gr$GDP_gr_0007) - min(gdp_gr$GDP_gr_0007)))
equal_gdp_gr_0815 <- seq(min(gdp_gr$GDP_gr_0815), max(gdp_gr$GDP_gr_0815), 
                       by = (max(gdp_gr$GDP_gr_0815) - min(gdp_gr$GDP_gr_0815)))

gdp_gr$quant_gdp_gr_0015 <- cut(gdp_gr$GDP_gr_0015, breaks = quant_gdp_gr_0015, include_lowest = T)
gdp_gr$quant_gdp_gr_0007 <- cut(gdp_gr$GDP_gr_0007, breaks = quant_gdp_gr_0007, include_lowest = T)
gdp_gr$quant_gdp_gr_0815 <- cut(gdp_gr$GDP_gr_0815, breaks = quant_gdp_gr_0815, include_lowest = T)

gdp_gr$equal_gdp_gr_0015 <- cut(gdp_gr$GDP_gr_0015, breaks = equal_gdp_gr_0015, include_lowest = T)
gdp_gr$equal_gdp_gr_0007 <- cut(gdp_gr$GDP_gr_0007, breaks = equal_gdp_gr_0007, include_lowest = T)
gdp_gr$equal_gdp_gr_0815 <- cut(gdp_gr$GDP_gr_0815, breaks = equal_gdp_gr_0815, include_lowest = T)


poly_plot_gdp_gr <- na.omit(left_join(poly_ggplot, gdp_gr[ , c(1, 10:ncol(gdp_gr))], by = c("id" = "NUTS_ID")))


gdp_gr_plot_list <- list()
plot_names <- names(poly_plot_gdp_gr)[8:ncol(poly_plot_gdp_gr)]

for(i in 1:length(plot_names)){
  gdp_gr_plot_list[[plot_names[i]]]  <- ggplot() + 
    geom_polygon(data = poly_plot_gdp_gr, 
                 aes_string(fill = plot_names[i], 
                            x = "long", y = "lat", group = "group")) + 
    geom_path(data = poly_plot_gdp_gr, aes(x = long, y = lat, group = group), 
              color = "black", size = 0.05) + 
    labs(x = NULL, y = NULL) + 
    theme(axis.line = element_blank(), 
          axis.text.x=element_blank(), axis.text.y = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA)) + 
    coord_equal() + 
    scale_fill_viridis(option = "plasma", direction = -1, discrete = TRUE) + 
    theme(legend.position = "none", legend.justification = c(0,1), 
          legend.background = element_rect(fill="transparent"), 
          legend.title = element_blank())
}


#####
# GVA plots in case necessary

# gva_pc <- depvar_list$gva_pc
# 
# quant_gva_pc_00 <- quantile(gva_pc$GVA_pc_2000, probs = seq(0, 1, by = 1/6))
# quant_gva_pc_07 <- quantile(gva_pc$GVA_pc_2007, probs = seq(0, 1, by = 1/6))
# quant_gva_pc_08 <- quantile(gva_pc$GVA_pc_2008, probs = seq(0, 1, by = 1/6))
# quant_gva_pc_15 <- quantile(gva_pc$GVA_pc_2015, probs = seq(0, 1, by = 1/6))
# 
# equal_gva_pc_00 <- seq(min(gva_pc$GVA_pc_2000), max(gva_pc$GVA_pc_2000), 
#                        by = (max(gva_pc$GVA_pc_2000) - min(gva_pc$GVA_pc_2000)))
# equal_gva_pc_07 <- seq(min(gva_pc$GVA_pc_2007), max(gva_pc$GVA_pc_2007), 
#                        by = (max(gva_pc$GVA_pc_2007) - min(gva_pc$GVA_pc_2007)))
# equal_gva_pc_08 <- seq(min(gva_pc$GVA_pc_2008), max(gva_pc$GVA_pc_2008), 
#                        by = (max(gva_pc$GVA_pc_2008) - min(gva_pc$GVA_pc_2008)))
# equal_gva_pc_15 <- seq(min(gva_pc$GVA_pc_2015), max(gva_pc$GVA_pc_2015), 
#                        by = (max(gva_pc$GVA_pc_2015) - min(gva_pc$GVA_pc_2015)))
# 
# gva_pc$quant_gva_pc_00 <- cut(gva_pc$GVA_pc_2000, breaks = quant_gva_pc_00, include_lowest = T)
# gva_pc$quant_gva_pc_07 <- cut(gva_pc$GVA_pc_2007, breaks = quant_gva_pc_07, include_lowest = T)
# gva_pc$quant_gva_pc_08 <- cut(gva_pc$GVA_pc_2008, breaks = quant_gva_pc_08, include_lowest = T)
# gva_pc$quant_gva_pc_15 <- cut(gva_pc$GVA_pc_2015, breaks = quant_gva_pc_15, include_lowest = T)
# 
# gva_pc$equal_gva_pc_00 <- cut(gva_pc$GVA_pc_2000, breaks = equal_gva_pc_00, include_lowest = T)
# gva_pc$equal_gva_pc_07 <- cut(gva_pc$GVA_pc_2007, breaks = equal_gva_pc_07, include_lowest = T)
# gva_pc$equal_gva_pc_08 <- cut(gva_pc$GVA_pc_2008, breaks = equal_gva_pc_08, include_lowest = T)
# gva_pc$equal_gva_pc_15 <- cut(gva_pc$GVA_pc_2015, breaks = equal_gva_pc_15, include_lowest = T)
# 
# 
# poly_plot_gva_pc <- na.omit(left_join(poly_ggplot, gva_pc[ , c(1, 38:ncol(gva_pc))], by = c("id" = "NUTS_ID")))
# 
# 
# gva_pc_plot_list <- list()
# plot_names <- names(poly_plot_gva_pc)[8:ncol(poly_plot_gva_pc)]
# 
# for(i in 1:length(plot_names)){
#   gva_pc_plot_list[[plot_names[i]]]  <- ggplot() + 
#     geom_polygon(data = poly_plot_gva_pc, 
#                  aes_string(fill = plot_names[i], 
#                             x = "long", y = "lat", group = "group")) + 
#     geom_path(data = poly_plot_gva_pc, aes(x = long, y = lat, group = group), 
#               color = "black", size = 0.05) + 
#     labs(x = NULL, y = NULL) + 
#     theme(axis.line = element_blank(), 
#           axis.text.x=element_blank(), axis.text.y = element_blank(), 
#           axis.ticks = element_blank(), 
#           axis.title.x = element_blank(), axis.title.y = element_blank()) + 
#     coord_equal() + 
#     scale_fill_viridis(option = "plasma", direction = -1, discrete = TRUE) + 
#     theme(legend.position = "none", legend.justification = c(0,1), 
#           legend.background = element_rect(fill="transparent"), 
#           legend.title = element_blank())
# }
# 
# 
# 
# 
# gva_gr <- depvar_list$gva_pc_gr
# 
# quant_gva_gr_0015 <- quantile(gva_gr$GVA_gr_0015, probs = seq(0, 1, by = 1/6))
# quant_gva_gr_0007 <- quantile(gva_gr$GVA_gr_0007, probs = seq(0, 1, by = 1/6))
# quant_gva_gr_0815 <- quantile(gva_gr$GVA_gr_0815, probs = seq(0, 1, by = 1/6))
# 
# equal_gva_gr_0015 <- seq(min(gva_gr$GVA_gr_0015), max(gva_gr$GVA_gr_0015), 
#                          by = (max(gva_gr$GVA_gr_0015) - min(gva_gr$GVA_gr_0015)))
# equal_gva_gr_0007 <- seq(min(gva_gr$GVA_gr_0007), max(gva_gr$GVA_gr_0007), 
#                          by = (max(gva_gr$GVA_gr_0007) - min(gva_gr$GVA_gr_0007)))
# equal_gva_gr_0815 <- seq(min(gva_gr$GVA_gr_0815), max(gva_gr$GVA_gr_0815), 
#                          by = (max(gva_gr$GVA_gr_0815) - min(gva_gr$GVA_gr_0815)))
# 
# gva_gr$quant_gva_gr_0015 <- cut(gva_gr$GVA_gr_0015, breaks = quant_gva_gr_0015, include_lowest = T)
# gva_gr$quant_gva_gr_0007 <- cut(gva_gr$GVA_gr_0007, breaks = quant_gva_gr_0007, include_lowest = T)
# gva_gr$quant_gva_gr_0815 <- cut(gva_gr$GVA_gr_0815, breaks = quant_gva_gr_0815, include_lowest = T)
# 
# gva_gr$equal_gva_gr_0015 <- cut(gva_gr$GVA_gr_0015, breaks = equal_gva_gr_0015, include_lowest = T)
# gva_gr$equal_gva_gr_0007 <- cut(gva_gr$GVA_gr_0007, breaks = equal_gva_gr_0007, include_lowest = T)
# gva_gr$equal_gva_gr_0815 <- cut(gva_gr$GVA_gr_0815, breaks = equal_gva_gr_0815, include_lowest = T)
# 
# 
# poly_plot_gva_gr <- na.omit(left_join(poly_ggplot, gva_gr[ , c(1, 10:ncol(gva_gr))], by = c("id" = "NUTS_ID")))
# 
# 
# gva_gr_plot_list <- list()
# plot_names <- names(poly_plot_gva_gr)[8:ncol(poly_plot_gva_gr)]
# 
# for(i in 1:length(plot_names)){
#   gva_gr_plot_list[[plot_names[i]]]  <- ggplot() + 
#     geom_polygon(data = poly_plot_gva_gr, 
#                  aes_string(fill = plot_names[i], 
#                             x = "long", y = "lat", group = "group")) + 
#     geom_path(data = poly_plot_gva_gr, aes(x = long, y = lat, group = group), 
#               color = "black", size = 0.05) + 
#     labs(x = NULL, y = NULL) + 
#     theme(axis.line = element_blank(), 
#           axis.text.x=element_blank(), axis.text.y = element_blank(), 
#           axis.ticks = element_blank(), 
#           axis.title.x = element_blank(), axis.title.y = element_blank()) + 
#     coord_equal() + 
#     scale_fill_viridis(option = "plasma", direction = -1, discrete = TRUE) + 
#     theme(legend.position = "none", legend.justification = c(0,1), 
#           legend.background = element_rect(fill="transparent"), 
#           legend.title = element_blank())
# }
# 
# save(poly_plot_gdp_pc, gdp_pc_plot_list, 
#      poly_plot_gdp_gr, gdp_gr_plot_list, 
#      poly_plot_gva_pc, gva_pc_plot_list, 
#      poly_plot_gva_gr, gva_gr_plot_list, 
#      file = "./data/plots.Rda")
#####





load("./output/run.Rda")


poly_plot_clubs <- left_join(poly_ggplot, 
                             cbind(rownames(data),
                                   data[ , c("clubs_lm", "clubs_sar", "clubs_sem")]), 
                             by = c("id" = "rownames(data)"))

p_clubs_lm <- ggplot() + 
  geom_polygon(data = poly_plot_clubs, 
               aes(fill = clubs_lm, x = long, y = lat, group = group)) + 
  geom_path(data = poly_plot_clubs, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.1) + 
  coord_equal() + 
  labs(x = NULL, y = NULL, 
       title = "Convergence clubs NUTS 2", 
       subtitle = "Unfiltered data") + 
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)) + 
  scale_fill_viridis(option = "viridis", discrete = TRUE) + 
  theme(plot.margin = unit(c(0.5,0,0,0), "cm")) + 
  theme(legend.position = "bottom", legend.justification = "center", 
        legend.direction = "horizontal", 
        legend.background = element_rect(fill="transparent"), 
        legend.title = element_blank()) + 
  guides(fill = guide_legend(nrow = 2,byrow = TRUE))


p_clubs_sar <- ggplot() + 
  geom_polygon(data = poly_plot_clubs, 
               aes(fill = clubs_sar, x = long, y = lat, group = group)) + 
  geom_path(data = poly_plot_clubs, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.1) + 
  coord_equal() + 
  labs(x = NULL, y = NULL, 
       title = "Convergence clubs NUTS 2", 
       subtitle = "SAR-filtered data") + 
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),axis.title.y = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)) + 
  scale_fill_viridis(option = "viridis", discrete = TRUE) + 
  theme(plot.margin = unit(c(0.5,0,0,0), "cm")) + 
  theme(legend.position = "bottom", legend.justification = "center", 
        legend.direction = "horizontal", 
        legend.background = element_rect(fill="transparent"), 
        legend.title = element_blank()) + 
  guides(fill = guide_legend(nrow = 2,byrow = TRUE))


p_clubs_sem <- ggplot() + 
  geom_polygon(data = poly_plot_clubs, 
               aes(fill = clubs_sem, x = long, y = lat, group = group)) + 
  geom_path(data = poly_plot_clubs, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.1) + 
  coord_equal() + 
  labs(x = NULL, y = NULL, 
       title = "Convergence clubs NUTS 2", 
       subtitle = "SEM-filtered data") +
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)) + 
  scale_fill_viridis(option = "viridis", discrete = TRUE) + 
  theme(plot.margin = unit(c(0.5,0,0,0), "cm")) + 
  theme(legend.position = "bottom", legend.justification = "center", 
        legend.direction = "horizontal", 
        legend.background = element_rect(fill="transparent"), 
        legend.title = element_blank()) + 
  guides(fill = guide_legend(nrow = 2,byrow = TRUE))



# save(poly_plot_gdp_pc, gdp_pc_plot_list, 
#      poly_plot_gdp_gr, gdp_gr_plot_list,
#      poly_ggplot_ovw, p_ovw,
#      poly_plot_clubs, p_clubs_lm, p_clubs_sar, p_clubs_sem, 
#      file = "./data/plots.Rda")


plot_gdp_pc <- gdp_pc_plot_list$quant_gdp_pc_00 + 
  labs(title = "GDP p.c. in 2000", subtitle = "Quantile map")
plot_gdp_gr <- gdp_gr_plot_list$quant_gdp_gr_0015 + 
  labs(title = "GDP p.c. growth 2000-15", subtitle = "Quantile map")

save(poly_plot_gdp_pc, plot_gdp_pc, 
     poly_plot_gdp_gr, plot_gdp_gr,
     poly_ggplot_ovw, p_ovw,
     poly_plot_clubs, p_clubs_lm, p_clubs_sar, p_clubs_sem, 
     file = "./data/plots_pres.Rda")
