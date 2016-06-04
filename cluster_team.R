set.seed(100)
library(cluster)
library(HSAUR)
#install.packages("fpc")
#install.packages("DEoptimR")
library(fpc)

setwd("/Users/admin/Downloads/avnet_csvs/final/")
team_data <- read.csv("export_team_cluster_master.csv", head=T)
colnames(team_data)
head(team_data)
## v_scaled <- scale(team_data[,2:5], center = T, scale = T)
v_scaled <- scale(team_data[,2:6], center = T, scale = T)
team_cluster <- kmeans(v_scaled,8, nstart=100, algorithm = "Hartigan-Wong")

table(team_cluster$cluster)

dev.new(width=10, height=8)
clusplot(v_scaled, team_cluster$cluster, color=T, shade = T, labels=2, lines=0, main = "Clusters - Teams")
team_cluster$cluster
## c_team_data <- cbind(team_data[,1:5], team_cluster$cluster)
c_team_data <- cbind(team_data[,1:6], team_cluster$cluster)
## colnames(c_team_data)
colnames(c_team_data) <- c("team","tot_units", "tot_cost", "tot_time", "tot_orders", "tot_resale", "assignment")
write.csv(c_team_data[,1:7],"clustering_team.csv",row.names = F, quote = F)

