#! /bin/ksh

sed -f var_replace.sed load_cluster_btc > load_cluster_btc.run
sed -f var_replace.sed load_cluster_team > load_cluster_team.run
sed -f var_replace.sed load_stop_orders > load_stop_orders.run
sed -f var_replace.sed load_total_orders > load_total_orders.run
sed -f var_replace.sed export_bill_to_cust_cluster > export_bill_to_cust_cluster.run
sed -f var_replace.sed export_team_cluster > export_team_cluster.run
sed -f var_replace.sed export_wrk_shipped_orders_agg2 > export_wrk_shipped_orders_agg2.run
