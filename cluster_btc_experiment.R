set.seed(100)
library(cluster)
library(HSAUR)
#install.packages("fpc")
#install.packages("DEoptimR")
library(fpc)

setwd("/Users/admin/Downloads/avnet_csvs/final/")
btc_data <- read.csv("export_bill_to_cust_cluster_master.csv", head=T)
colnames(btc_data)
head(btc_data)
v_scaled <- scale(btc_data[,2:6], center = T, scale = T)
btc_cluster <- kmeans(v_scaled,15, nstart=100, algorithm = "Hartigan-Wong")

table(btc_cluster$cluster)

dev.new(width=10, height=8)
clusplot(v_scaled, btc_cluster$cluster, color=T, shade = T, labels=2, lines=0, main = "Clusters - Bill to Customer")
btc_cluster$cluster
## colnames(c_btc_data)
c_btc_data <- cbind(btc_data[,1:6], btc_cluster$cluster)
colnames(c_btc_data) <- c("bill_to_customer","tot_units", "tot_cost", "tot_time", "tot_orders", "tot_resale", "assignment")
write.csv(c_btc_data[,1:7],"clustering_btc.csv",row.names = F, quote = F)


