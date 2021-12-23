source("util.r")
shap <- fread("df_with_shap.csv")
library(ggbeeswarm)
### First Shap plot

mt <- melt(shap[, names(shap)[grepl("shap_",names(shap))],with=F])
top <-  as.character(mt[,mean(abs(value)),by=variable][order(-V1)][1:25]$variable)
top_df <- data.table()
for(t in top){
  top_df <- rbind(top_df,
                  shap[, c(t,sub("shap_","",t)),
                      with=F][,v := t],
                  use.names=F
  )
}
setnames(top_df, c("shap","feature_val","v"))

top_df[, name := mapvalues(v, names(factor_names), as.vector(factor_names))]
top_df[, name := factor(name, levels=as.vector(factor_names))]
top_df[, rescaled_feature := rescale(feature_val,c(0,1)), by=name]

p_res <- ggplot(top_df[, sample_n(.SD, 5000), by=name], 
                aes(name, shap,color=rescaled_feature)) + 
  geom_quasirandom(varwidth=T) + 
  scale_color_gradientn("Feature Value\n(Rescaled 0-1)",
                        colors=c("blue","grey","orange")) + 
  theme(legend.position="none") +
  ylab("SHAP Value")+
  xlab("Top 25 Most Important Predictors")+
  theme_classic(14)+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        plot.margin = margin(10, 10, 10, 50))+
  geom_hline(yintercept = 0, color='black',size=1.2)
ggsave("img/shaply.pdf",p_res,h=6,w=11)



 ##### State Plot
states <- sub("^St_","",names(shap)[grepl("^St",names(shap))])
state_df <- data.table()
for(state in states){
  n = sum(shap[,get(paste0("St_",state))])
  state_df <- rbind(state_df, shap[, c(paste0("St_",state),
                                       paste0("shap_St_",state)),
                                   with=F][, st := state][, n := n],
                    use.names=F)
  
}
setnames(state_df, c("value","shap","state","n"))


state_sp <- spread(state_df[, mean(shap), 
                            by=.(value,state,n) ], value,V1)[, diff := `1`-`0`]

state_sp <- state_df[value == 1, list(diff=mean(shap)), by=.(value,state,n) ]#state_df[, mean(shap), by=.(value,state,n) ], value,V1)[, diff := `1`-`0`]
# 

library(maps)
library(ggthemes)
library(sf)
library(sp)
library(spdep)
mg <- st_as_sf(merge( merge(state_sp, data.frame(NAME= state.name,state= state.abb), 
            by="state", all.y=T),
            us_states, by="NAME"))
p <- ggplot() + geom_sf( data=mg, 
                       aes(fill = diff), 
                       color="white", size = 0.2) + 
  scale_fill_viridis_c("SHAP Value", option = "plasma", begin=1,end=0) +
  theme_map()
ggsave("img/stateloc.pdf",p, h=4,w=7)
# 
# mg <- mg[!is.na(mg$value),]
# neighbours_sf <- poly2nb(mg)
# listw <- nb2listw(neighbours_sf)
# moran <- moran.plot(mg$diff, listw = nb2listw(neighbours_sf, style = "W"))
# globalMoran <- moran.test(mg$diff, listw)
# globalMoran
# 
# 
# 
# local <- localmoran(x = mg$diff, 
#                     listw = nb2listw(neighbours_sf, style = "W"))
# 
# quadrant <- vector(mode="numeric",length=nrow(local))
# 
# # centers the variable of interest around its mean
# m.qualification <- mg$diff - mean(mg$diff)     
# 
# # centers the local Moran's around the mean
# m.local <- local[,1] - mean(local[,1])    
# 
# # significance threshold
# signif <- 0.1 
# 
# # builds a data quadrant
# quadrant[m.qualification >0 & m.local>0] <- 4  
# quadrant[m.qualification <0 & m.local<0] <- 1      
# quadrant[m.qualification <0 & m.local>0] <- 2
# quadrant[m.qualification >0 & m.local<0] <- 3
# quadrant[local[,5]>signif] <- 0   
# 
# # plot in r
# brks <- c(0,1,2,3,4)
# colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
# plot(mg,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
# box()
# legend("bottomleft", legend = c("insignificant","low-low","low-high","high-low","high-high"),
#        fill=colors,bty="n")
# 
# moran.map <- cbind(mg, local)
# tm_shape(moran.map) +
#   tm_fill(col = "Ii",
#           style = "quantile",
#           title = "local moran statistic") 


state_plt <- ggplot(state_sp, aes(reorder(state,diff), diff, size=n,color=diff > 0))+ 
  geom_segment(size=1 ,aes( xend=reorder(state,diff), y=0, yend=diff)) +
  geom_point() +
  ylab("Mean SHAP Value\nWhen Youth Is From State")+
  xlab("State") + theme_minimal(14) +
  scale_color_manual(values=c("orange","blue"),guide=F) + 
  scale_size_continuous("Number of\nYouth\nIn Care") +
  theme(axis.text.x=element_text(angle=45,hjust=1))
ggsave("img/shaply_state.pdf",state_plt,h=4,w=12)


p <- ggplot(shap, aes(latest_removal_date_int/365,
                      shap_latest_removal_date_int)) + 
  geom_jitter(alpha=.3)+
  scale_x_continuous(limits=c(0.5,5))+
  xlab("Years Since Last Removal") +ylab("SHAP Value")
ggsave("img/shaply_los.pdf",p,h=4,w=7)
# state_plt <- ggplot(state_sp, aes(log(n), diff, label=state))+ 
#   geom_point() + geom_text_repel() + 
#   geom_hline(yintercept = mean(state_sp$diff),color='red') + 
#   geom_vline(xintercept=mean(log(state_sp$n)), color='red') + 
#   ylab("Mean Shapley Value\nWhen Youth Is From State")+
#   xlab("Log(Total Number of Youth In Care In State)") 
# ggsave("img/shaply_state.pdf",state_plt,h=5,w=8)
# 
# 
# p_state <- ggplot(state_sp, aes(reorder(state,-diff), diff))+
#   geom_bar(stat='identity') + 
#   theme_classic()+ 
#   ylab("Mean Difference In Shapley Values") + 
#   xlab("State")


shap <- shap[RU13 > 0]


k <- shap[, as.list(smean.cl.boot(shap_RU13)), 
          by=.(RU13,RaceEthn_2.0)]
ggplot(k, aes(RU13,Mean,ymin=Lower,ymax=Upper,color=factor(RaceEthn_2.0)))+geom_pointrange()

k <- shap[, as.list(smean.cl.boot(shap_RU13)), 
          by=.(RU13,RaceEthn_1.0)]
ggplot(k, aes(RU13,Mean,ymin=Lower,ymax=Upper,
              color=factor(RaceEthn_1.0)))+geom_pointrange()



pdf("k.pdf")
for(f in names(shap)[!grepl("shap_",names(shap))]){
  print(f)
  if(f == "age_19+"){
    next;
  }
  print(ggplot(shap, aes_string(f,paste0("shap_",f))) + geom_jitter(alpha=.3)+
    scale_y_continuous(limits=c(-2,2)))
}
dev.off()

