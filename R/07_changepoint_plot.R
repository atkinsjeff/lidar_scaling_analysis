# packages
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(cowplot)

# change point plot
cp <- read.csv("./changepoint.csv")

cp %>% 
    filter( !grepl('.cv', metric)) %>%
    data.frame() -> means


cp %>% 
    filter( grepl('.cv', metric)) %>%
    data.frame() -> cv


# FIRSTS
cp %>%
    group_by(metric) %>%
    filter( !grepl('.cv', metric)) %>%
    summarize(first.cp = median(FirstCP, na.rm = TRUE),
              first.cp.iqr = IQR(FirstCP, na.rm = TRUE),
              first.cp.25 = quantile(FirstCP, c(0.25), na.rm = TRUE),
              first.cp.75 = quantile(FirstCP, c(0.75), na.rm = TRUE),
              first.cp.min = min(FirstCP, na.rm = TRUE),
              first.cp.max = max(FirstCP, na.rm = TRUE)) %>%
    data.frame() %>%
    summary()

cp %>%
    group_by(metric) %>%
    filter( grepl('.cv', metric)) %>%
    summarize(first.cp = median(FirstCP, na.rm = TRUE),
              first.cp.iqr = IQR(FirstCP, na.rm = TRUE),
              first.cp.25 = quantile(FirstCP, c(0.25), na.rm = TRUE),
              first.cp.75 = quantile(FirstCP, c(0.75), na.rm = TRUE),
              first.cp.min = min(FirstCP, na.rm = TRUE),
              first.cp.max = max(FirstCP, na.rm = TRUE)) %>%
    data.frame() %>%
    summary()


### SECONDS
cp %>%
    group_by(metric) %>%
    summarize(second.cp = median(SecondCP, na.rm = TRUE),
              second.cp.iqr = IQR(SecondCP, na.rm = TRUE),
              second.cp.25 = quantile(SecondCP, c(0.25), na.rm = TRUE),
              second.cp.75 = quantile(SecondCP, c(0.75), na.rm = TRUE),
              second.cp.min = min(SecondCP, na.rm = TRUE),
              second.cp.max = max(SecondCP, na.rm = TRUE)) %>%
    data.frame()


cp %>% 
    filter( grepl('.cv', metric)) %>%
    group_by(metric) %>%
    summarize(first.cp = median(FirstCP, na.rm = TRUE),
              first.cp.iqr = IQR(FirstCP, na.rm = TRUE),
              first.cp.25 = quantile(FirstCP, c(0.25), na.rm = TRUE),
              first.cp.75 = quantile(FirstCP, c(0.75), na.rm = TRUE),
              first.cp.min = min(FirstCP, na.rm = TRUE),
              first.cp.max = max(FirstCP, na.rm = TRUE))
    data.frame() -> cv


# labels
# New facet label names for dose variable
metric.labs <- c("Canopy Cover (CC)", "Canopy Relief Ratio (CRR)", "Foliar Height Diversity (FHD)", "Leaf Area Index (LAI)",
                 "Mean Height", "Median Height", "Mean Outer Canopy Height (MOCH)", "Top Rugosity (Rt)")
names(metric.labs) <- c("CC", "CRR", "FHD", "LAI", "MeanHt", "MedHt", "MOCH", "TopRug")
#####
my_labeller <- as_labeller(c(CC = "Canopy Cover (CC)", CRR = "Canopy Relief Ratio (CRR)", FHD = "Foliar Height Diversty (FHD)",
                             LAI = "Leaf Area Index (LAI)", MeanHt = "Mean Height ("*bar(H)*")", MedHt = "Median Height ("*tilde(H)*")",
                             MOCH = "Mean Outer Canopy Height (MOCH)", TopRug = "Top Rugosity (R[T]"),
                           default = label_parsed)


p1 <- ggplot(means, aes( x = as.factor(site), y = FirstCP, color = as.factor(site)))+
    geom_point(size = 2)+
    #scale_colour_brewer(palette = "BrBG")+
    geom_errorbar(aes(ymin = FirstCI5, ymax = FirstCI95), 
                  width=.2,
                  position=position_dodge(0.05))+
    theme_bw()+
    ylab(expression(paste(First~Changepoint~"["~m^2~"]")))+
    xlab("")+
    theme(legend.position="none")+
    facet_wrap(metric ~ ., ncol = 2,
               labeller = labeller(metric = metric.labs))


p1 <- ggplot(means, aes( x = metric, y = FirstCP, fill = metric))+
    geom_point(size = 2)+
    geom_boxplot(alpha = 0.8) +
    scale_fill_brewer(palette="BrBG")+
    #scale_colour_brewer(palette = "BrBG")+
    # geom_errorbar(aes(ymin = FirstCI5, ymax = FirstCI95), 
    #               width=.2,
    #               position=position_dodge(0.05))+
    theme_bw()+
    ylab(expression(paste(First~Changepoint~"["~m~"]")))+
    xlab("")+
    theme(legend.position="none")



p2 <- ggplot(means, aes( x = metric, y = SecondCP, fill = metric))+
    geom_point(size = 2)+
    geom_boxplot(alpha = 0.8) +
    scale_fill_brewer(palette="BrBG")+
    #scale_colour_brewer(palette = "BrBG")+
    # geom_errorbar(aes(ymin = FirstCI5, ymax = FirstCI95), 
    #               width=.2,
    #               position=position_dodge(0.05))+
    theme_bw()+
    ylab(expression(paste(Second~Changepoint~"["~m~"]")))+
    xlab("")+
    theme(legend.position="none")

x11(width = 8, height = 6)
plot_grid( p1, p2,
           labels = "AUTO", ncol = 1)

#### CV PLOTS
p3 <- ggplot(cv, aes( x = metric, y = FirstCP, fill = metric))+
    geom_point(size = 2)+
    geom_boxplot(alpha = 0.8) +
    scale_fill_brewer(palette="BrBG")+
    #scale_colour_brewer(palette = "BrBG")+
    # geom_errorbar(aes(ymin = FirstCI5, ymax = FirstCI95), 
    #               width=.2,
    #               position=position_dodge(0.05))+
    theme_bw()+
    ylab(expression(paste(First~Changepoint~"["~m~"]")))+
    xlab("")+
    theme(legend.position="none")



p4 <- ggplot(cv, aes( x = metric, y = SecondCP, fill = metric))+
    geom_point(size = 2)+
    geom_boxplot(alpha = 0.8) +
    scale_fill_brewer(palette="BrBG")+
    #scale_colour_brewer(palette = "BrBG")+
    # geom_errorbar(aes(ymin = FirstCI5, ymax = FirstCI95), 
    #               width=.2,
    #               position=position_dodge(0.05))+
    theme_bw()+
    ylab(expression(paste(Second~Changepoint~"["~m~"]")))+
    xlab("")+
    theme(legend.position="none")

x11(width = 8, height = 6)
plot_grid( p3, p4,
           labels = "AUTO", ncol = 1)
# 
# 
# x11(width = 10, height = 8)
# ggplot(means, aes( x = as.factor(site), y = FirstCP))+
#     geom_point(size = 2, color = "black")+
#     #scale_colour_brewer(palette = "BrBG")+
#     geom_errorbar(aes(ymin = FirstCI5, ymax = FirstCI95), 
#                   width=.2,
#                   position=position_dodge(0.05))+
#     theme_bw()+
#     ylab(expression(paste(First~Changepoint~"["~m^2~"]")))+
#     xlab("")+
#     theme(legend.position="none")+
#     #geom_hline(yintercept = 25, color = "red", alpha = 0.5)+
#     facet_wrap(metric ~ ., ncol = 2,
#                labeller = labeller(metric = metric.labs))
# 
# 
# 
# x11(width = 10, height = 8)
# ggplot(cv, aes( x = as.factor(site), y = FirstCP))+
#     geom_point(size = 2, color = "black")+
#     #scale_colour_brewer(palette = "BrBG")+
#     geom_errorbar(aes(ymin = FirstCI5, ymax = FirstCI95), 
#                   width=.2,
#                   position=position_dodge(0.05))+
#     theme_bw()+
#     ylab(expression(paste(First~Changepoint~"["~m^2~"]")))+
#     xlab("")+
#     theme(legend.position="none")+
#     #geom_hline(yintercept = 25, color = "red", alpha = 0.5)+
#     facet_wrap(metric ~ ., ncol = 2,
#                labeller = labeller(metric = metric.labs))
# 
# 
# ylab(expression(Anthropogenic~SO[4]^{"2-"}~(ngm^-3))) + 
# 
# 




### site info
site.info <- read.csv("./data/neon_site_info.csv")

means <- merge(means, site.info)
cv <- merge(cv, site.info)

#
require(randomForest)

# sort down
means %>%
    dplyr::select(siteID, metric, FirstCP, PFT) %>%
    na.omit() %>%
    data.frame() -> bob

# sort down
cv %>%
    dplyr::select(siteID, metric, FirstCP, PFT) %>%
    na.omit() %>%
    data.frame() -> carl
# For classification
randomForest(FirstCP ~ PFT + metric, 
                        data = bob, 
                        ntree = 2000,
                        mtry = 1,
                        importance = TRUE)

#

x11(width = 8, height = 6)
p5 <-ggplot(bob, aes( x = metric, y = FirstCP, fill = PFT))+
    geom_point(size = 2)+
    geom_boxplot(alpha = 0.8) +
    scale_fill_brewer(palette="Dark2")+
    #scale_colour_brewer(palette = "BrBG")+
    # geom_errorbar(aes(ymin = FirstCI5, ymax = FirstCI95), 
    #               width=.2,
    #               position=position_dodge(0.05))+
    theme_bw()+
    ylab(expression(paste(First~Changepoint~"["~m~"]")))+
    xlab("")+
    theme(legend.position = c(0.93, 0.7),
          legend.background = element_rect(fill = "white", color = "black"))

p6 <- ggplot(carl, aes( x = metric, y = FirstCP, fill = PFT ))+
    geom_point(size = 2)+
    geom_boxplot(alpha = 0.8) +
    scale_fill_brewer(palette="Dark2")+
    #scale_colour_brewer(palette = "BrBG")+
    # geom_errorbar(aes(ymin = FirstCI5, ymax = FirstCI95), 
    #               width=.2,
    #               position=position_dodge(0.05))+
    theme_bw()+
    ylab(expression(paste(First~Changepoint~"["~m~"]")))+
    xlab("")+
    theme(legend.position="none")

x11(width = 8, height = 6)
plot_grid( p5, p6,
           labels = "AUTO", ncol = 1)



















