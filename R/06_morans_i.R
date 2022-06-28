#
library(ape)
# bring in data
df <- read.csv("grsm_processed_metrics.csv")

# sort data to sampling structure. 
df %>%
    dplyr::filter(CC >= 25  & MOCH >= 5) %>%
    data.frame() -> df

# set up random sample
df %>% 
    filter(res <= 250) %>%
    group_by(res)%>% 
    sample_n(100) -> df.250

df %>%
    filter(res >= 500) %>%
    data.frame() -> df.500

samp.data <- rbind(df.250, df.500)


####
df %>%
    filter(res == 5) %>%
    group_by(siteID, res) %>%
    mutate_at(.funs = list(sd = ~sd(.)), .vars = c("CC", "CRR", "FHD", "Mean_ht", "Median_ht", "MOCH", "Top_Rug")) %>%
    mutate_at(.funs = list(log10 = ~log10(.)), .vars = c("CC", "CRR", "FHD", "Mean_ht", "Median_ht", "MOCH", "Top_Rug")) %>%
    data.frame() -> df.5


df %>%
    dplyr::select(siteID, res, CC, CRR, FHD, Mean_ht, Median_ht, MOCH, Top_Rug) %>%
    filter(res == 5) %>%
    data.frame() -> df.5

#### MORAN'S I
x <- as.matrix(dist(cbind(df.5$x, df.5$y)))
x.inv <- 1/x
diag(x.inv) <- 0
#x.inv

 <- Moran.I(df.5$CC, x.inv, scaled = FALSE, na.rm = TRUE)

#
# created vector with 5 characters
columns= c("siteID", "res", "CC", "CRR", "FHD", "Mean_ht", "Median_ht", "MOCH", "Top_Rug") 
columns.p = c("siteID", "res", "CC", "CRR", "FHD", "Mean_ht", "Median_ht", "MOCH", "Top_Rug") 
# pass this vector length to ncol parameter
# and nrow with 0
stat.table = data.frame(matrix(nrow = length(res.list), ncol = length(columns))) 
stat.table.p = data.frame(matrix(nrow = length(res.list), ncol = length(columns.p))) 

# assign column names
colnames(stat.table) = columns
colnames(stat.table.p) = columns.p
# here some stuff. 
stat.table$siteID = "GRSM"
stat.table$res = res.list
stat.table.p$siteID = "GRSM"
stat.table.p$res = res.list
# display
print(myData)

#### 5 m
#### MORAN'S I
x <- as.matrix(dist(cbind(df.5$x, df.5$y)))
x.inv <- 1/x
diag(x.inv) <- 0
# 
# for (i in 3:length(columns)){
#     #### MORAN'S I
#     xx <- Moran.I(df.5[, i], x.inv)
#     stat.table[1, i] <- xx$observed
#     
# }
# 
# 10 m
df %>%
    dplyr::select(x, y, siteID, res, CC, CRR, FHD, Mean_ht, Median_ht, MOCH, Top_Rug) %>%
    data.frame() -> df.x


#nested for loop
# for loop
for (j in 1:length(stat.table$res)){
        df.x %>%
        filter(res == stat.table$res[j]) %>%
        data.frame() -> bob
        
    
    
    for (i in 3:length(stat.table)){
        x <- as.matrix(dist(cbind(bob$x, bob$y)))
        x.inv <- 1/x
        diag(x.inv) <- 0
        #### MORAN'S I
        gg <- as.vector(bob[[i+2]])
        xx <- Moran.I(gg, x.inv)
        stat.table[j, i] <- xx$observed
        stat.table.p[j, i] <- xx$p.value
  }
}

write.csv(stat.table, "grsm_moran_output.csv")

x11()
ggplot(stat.table, aes(x = res, y = CC))+
    geom_point()+
    geom_hline(yintercept = 0)+
    stat_smooth(method = "loess")

stat.long <- gather(stat.table, metric, z.score, CC:Top_Rug, factor_key=TRUE)
stat.p.long <- gather(stat.table.p, metric, p.value, CC:Top_Rug, factor_key=TRUE)

summary <- merge(stat.long, stat.p.long)
summary$significant <- ifelse(summary$p.value < 0.05, TRUE, FALSE)
summary$labels <- ifelse(summary$p.value < 0.05, "", "X")

summary$Z.Score <- round(summary$z.score, 2)
x11()
ggplot(summary, aes(x = as.factor(res), y = metric,  fill = labels)) +                           # Create heatmap with ggplot2
    geom_tile(aes(fill = z.score), color = "black")+
    scale_fill_gradientn(limits = c(-0.05, 0.05),
                         colours=c("#FDE725FF", "#22A884FF", "#2A788EFF", "#414487FF", "#440154FF"), 
                         breaks = c(-0.05, -0.025, 0, 0.025, 0.05), labels = format(c(-0.05, -0.025, 0, 0.025, 0.05)))+
    geom_text(aes(label = labels), color = "black", size = 1)+
    ylab("Moran's I")+
    xlab("Grain Size")+
    ggtitle("GRSM")+
    #scale_alpha_manual(values=c(0.1, 1), guide = FALSE)+
    coord_fixed()





scale_fill_gradient(low = "yellow", high = "red", na.value = NA)
x11()
ggplot(summary, aes(x = as.factor(res), y = metric, shape = significant)) +                           # Create heatmap with ggplot2
    geom_point(size = 2)
    scale_shape_manual(values = c(15, 21)) +
    ylab("Moran's I")+
    xlab("Grain Size")+
    ggtitle("GRSM")+
    geom_hline(yintercept = 0)

x11()
ggplot(summary, aes(x = as.factor(res), y = metric, alpha = significant)) +                           # Create heatmap with ggplot2
    geom_tile(aes(fill = z.score), color = "black")+
    scale_fill_gradientn(limits = c(-0.05, 0.05),
                         colours=c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'), 
                         breaks = c(-0.05, -0.025, 0, 0.025, 0.05), labels = format(c(-0.05, -0.025, 0, 0.025, 0.05)))+
    ylab("Moran's I")+
    xlab("Grain Size")+
    ggtitle("GRSM")+
    scale_alpha_manual(values=c(0, 1), guide = FALSE)+
    coord_fixed()

x11()
ggplot(summary, aes(x = as.factor(res), y = metric, alpha = significant)) +                           # Create heatmap with ggplot2
    geom_tile(aes(fill = z.score), color = "black")+
    scale_fill_gradientn(colours=topo.colors(7),na.value = "transparent",
                         breaks=c(0,0.5,1),labels=c("Minimum",0.5,"Maximum"),
                         limits=c(0,1))+
    ylab("Moran's I")+
    xlab("Grain Size")+
    ggtitle("GRSM")+
    scale_alpha_manual(values=c(0, 1), guide = FALSE)+
    coord_fixed()

['#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6']
#######
#
# # created vector with 5 characters
# columns= c("siteID", "res", "CC", "CRR", "FHD", "Mean_ht", "Median_ht", "MOCH", "Top_Rug") 
# columns.p = c("siteID", "res", "CC_p", "CRR_p", "FHD_p", "Mean_ht_p", "Median_ht_p", "MOCH_p", "Top_Rug_p") 
# # pass this vector length to ncol parameter
# # and nrow with 0
# stat.table = data.frame(matrix(nrow = length(res.list), ncol = length(columns))) 
# stat.table.p = data.frame(matrix(nrow = length(res.list), ncol = length(columns.p))) 
# 
# # assign column names
# colnames(stat.table) = columns
# colnames(stat.table.p) = columns.p
# # here some stuff. 
# stat.table$siteID = "GRSM"
# stat.table$res = res.list
# stat.table.p$siteID = "GRSM"
# stat.table.p$res = res.list
# # display
# print(myData)
# 
# #### 5 m
# #### MORAN'S I
# x <- as.matrix(dist(cbind(df.5$x, df.5$y)))
# x.inv <- 1/x
# diag(x.inv) <- 0
# # 
# # for (i in 3:length(columns)){
# #     #### MORAN'S I
# #     xx <- Moran.I(df.5[, i], x.inv)
# #     stat.table[1, i] <- xx$observed
# #     
# # }
# # 
# # 10 m
# df %>%
#     dplyr::select(x, y, siteID, res, CC, CRR, FHD, Mean_ht, Median_ht, MOCH, Top_Rug) %>%
#     data.frame() -> df.x
# 
# 
# #nested for loop
# # for loop
# for (j in 1:length(stat.table$res)){
#     df.x %>%
#         filter(res == stat.table$res[j]) %>%
#         data.frame() -> bob
#     
#     
#     
#     for (i in 3:length(stat.table)){
#         x <- as.matrix(dist(cbind(bob$x, bob$y)))
#         x.inv <- 1/x
#         diag(x.inv) <- 0
#         #### MORAN'S I
#         gg <- as.vector(bob[[i+2]])
#         xx <- Moran.I(gg, x.inv)
#         stat.table[j, i] <- xx$observed
#         stat.table.p[j, i] <- xx$p.value
#     }
# }
# 
