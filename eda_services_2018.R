source("util.R")

d <- get_data("2018_final.csv")

serv_alloc <- melt(d, id="StFCID",service_cols)[, sum(value > 1), by=variable]
serv_alloc[, pct := V1/length(unique(d$StFCID))]
serv_alloc[, var_name := mapvalues(variable, service_cols,as.vector(service_mapping))]
p0 <- ggplot(serv_alloc, aes(reorder(var_name,pct), pct))+
  geom_bar(stat='identity')+
  scale_y_continuous("% of Youth Receiving Service",labels=percent)+coord_flip()+
  xlab("Service") +
  theme_classic(13) +
  theme(plot.margin = margin(10, 20, 10, 10))
ggsave("img/sv_per_sv.pdf",p0,h=4,w=6)

per_youth <- d[, .N, by=(AllServices_Binary)]
per_youth[, pct := N / length(unique(d$StFCID))]
p1 <- ggplot(per_youth, aes(AllServices_Binary, pct))+
  geom_bar(stat='identity')+
  scale_y_continuous("% of Youth Receiving\nGiven Number of Services",
                     labels=percent) + 
  scale_x_continuous("Number of Services", breaks=c(0:15)) +
  theme_classic(20)
ggsave("img/sv_per_youth.pdf",p1,h=4,w=6)
per_youth <- per_youth[order(AllServices_Binary)]
per_youth[, pct_cum := cumsum(pct)]
per_youth

per_youth <- per_youth[2:nrow(per_youth)]
per_youth[, pct := N / length(unique(d[AllServices_Binary > 0]$StFCID))]
per_youth[, pct_cum := cumsum(pct)]
per_youth

d_cum <- d[order(-AllServices_Binary),.(AllServices_Binary)]
d_cum[, tot := cumsum(AllServices_Binary)/sum(AllServices_Binary)]
d_cum[, pct_youth := 1/nrow(d_cum)]
d_cum[, pct_youth := cumsum(pct_youth)]
p2 <- ggplot(d_cum, aes(pct_youth,tot))+ geom_line(size=1.2) +
  theme_classic(20) +
  geom_hline(yintercept = .8,color='red',linetype='dotted')+
  geom_vline(xintercept = .25,color='red',linetype='dotted')+
  scale_x_continuous("% of Youth",labels=percent)+ 
  scale_y_continuous("Total % of\nServices Allocated", labels=percent)
ggsave("img/ccdf.pdf",p2,h=4,w=6)


library(heatmaply)
library(proxy)
dst <- dist(as.matrix(d[,service_cols,with=F]), method="jaccard", by_rows=F)
heatmaply(as.matrix(dst),
          symm=T, 
          labRow = as.vector(service_mapping),
          labCol = as.vector(service_mapping),
          seriate="OLO", nodetype="scatter",
          colors = c("blue","white"))





