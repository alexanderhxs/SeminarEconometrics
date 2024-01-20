library(dplyr)

#compute rolling window measures
r_sq_monthly = ff(data_monthly[,1],data_monthly[,-1])
#r_sq_biweekly = ff(data_biweekly[,1], data_biweekly[,-1])
#r_sq_weekly = ff(data_weekly[,1], data_weekly[,-1])
r_sq_daily = ff(data[,1], data[,-1])

#look at data
head(r_sq_monthly)
#head(r_sq_biweekly)
#head(r_sq_weekly)
head(r_sq_daily)

#plot time series
plot_ts(r_sq_monthly)
#plot_ts(r_sq_biweekly)
#plot_ts(r_sq_weekly)
plot_ts(r_sq_daily)

#create different time frames
data_monthly_at_fc = data_monthly %>% filter(Month <= '2008-09-01')
data_monthly_3b_fc = data_monthly %>% filter(Month <= '2008-06-01')
data_monthly_3a_fc = data_monthly %>% filter(Month <= '2008-12-01')

#data_dayly_at_fc = data[(20*200):nrow(data),] %>% filter(X <= '2008-09-15')
#data_dayly_3b_fc = data %>% filter(X <= '2008-06-15')
#data_dayly_3a_fc = data %>% filter(X <= '2008-12-15')

#data_dayly_3db_fc = data[(20*200):nrow(data),] %>% filter(X <= '2008-09-10')#3 trade days before
#data_dayly_3da_fc = data[(20*200):nrow(data),] %>% filter(X <= '2008-09-18')#3 trade days after

#compute tvp-var for diffrent time frames
r_sq_monthly_2 = gg(data_monthly[,1],data_monthly[,-1])
r_sq_monthly_2_at_fc = gg(data_monthly_at_fc[,1],data_monthly_at_fc[,-1])
r_sq_monthly_2_3b_fc = gg(data_monthly_3b_fc[,1],data_monthly_3b_fc[,-1])
r_sq_monthly_2_3a_fc = gg(data_monthly_3a_fc[,1],data_monthly_3a_fc[,-1])

#compute tvp-var with daily data at fc
index_fc = which(data == '2008-09-15')
start_index = index_fc - 300
end_index = index_fc + 299
r_sq_daily_2_at_fc = gg(data[start_index:end_index,1],
                        data[start_index:end_index,-1])

#merge data frames to compare monthly rw and tvp
compare_month = merge(r_sq_monthly, r_sq_monthly_2, by = 'Time',
                      suffixes = c('_rw','_tvp'), all = TRUE)
head(compare_month)
tail(compare_month)

plot_ts2(compare_month, mark_shocks = TRUE)

#merge data frames to compare daily and monthly rw
compare_dm_rw = merge(r_sq_daily,r_sq_monthly_2, by = 'Time', suffixes =c('_dy','_mon'),all = TRUE)
start_monthly = r_sq_monthly[1,1]
plot_ts2(compare_dm_rw[start_monthly:nrow(compare_dm_rw),], mark_shocks = TRUE)

#create on3 compare data frame monthly for fc
compare_df = merge(r_sq_monthly_2, r_sq_monthly_2_at_fc,
                   by = 'Time', all = TRUE, suffixes = c('_all','_at_fc')) 

plot_ts2(compare_df, mark_shocks = TRUE)
plot_ts2(compare_df %>% filter((Time >= '2007-01-01') & (Time <= '2010-03-01')), mark_shocks = TRUE)

compare_df_2 = merge(compare_df, r_sq_monthly_2_3b_fc, by = 'Time', all = TRUE)
compare_df_2 = merge(compare_df_2, r_sq_monthly_2_3a_fc, by = 'Time', all = TRUE)

colnames(compare_df_2) = c('Time','all_tvp','to_fc','3_before_fc','3_after_fc')
index_mask = which(('2000-01-01' <= compare_df_2$Time) & (compare_df_2$Time<= '2015-12-31'))
plot_ts2(compare_df_2[index_mask,], mark_shocks = TRUE)

#compare daily and monthly tvp
compare_daily = merge(r_sq_daily,r_sq_daily_2, by = 'Time', all = TRUE, suffixes = c('_rw','_tvp'))
plot_ts2(compare_daily)

#daily rw vs monthly tvp
compare_drw_mtp = merge(r_sq_daily, r_sq_monthly_2, by = 'Time', all = TRUE, suffixes = c('_rw','_tvp'))
start_monthly = r_sq_monthly_2[1,1]
plot_ts2(compare_drw_mtp[start_monthly:nrow(compare_drw_mtp),], mark_shocks = TRUE)

#compare at fc
compare_tvp_rw_fc = merge(r_sq_daily_2_at_fc, r_sq_daily, by = 'Time', all.x = TRUE,
                          suffixes = c('_TVP','_RW'))
plot_ts2(compare_tvp_rw_fc, mark_shocks = TRUE)
compare_tvp_rw_fc = merge(compare_tvp_rw_fc,r_sq_monthly_2, by = 'Time', all.x = TRUE)
colnames(compare_tvp_rw_fc) = c('Time','Correlation_TVP','Correlation_RW','Correlation_TVP_monthly')
plot_ts2(compare_tvp_rw_fc, mark_shocks = TRUE)
#directed connectedness
con_fin_daily = ff2(data[,1], data[,-1], var_index = 1)
con_oil_daily = ff2(data[,1], data[,-1], var_index = 2)
con_drugs_daily = ff2(data[,1], data[,-1], var_index = 3)
con_food_daily = ff2(data[,1], data[,-1], var_index = 4)
con_guns_daily = ff2(data[,1], data[,-1], var_index = 5)

compare_directed = merge(con_fin_daily, con_oil_daily, by = 'Time', all = TRUE)
compare_directed = merge(compare_directed, con_drugs_daily, by = 'Time', all = TRUE)
compare_directed = merge(compare_directed, con_food_daily, by = 'Time', all = TRUE)
compare_directed = merge(compare_directed, con_guns_daily, by = 'Time', all = TRUE)

compare_to_others = compare_directed[,c(1,3,5,7,9,11)]
compare_from_others = compare_directed[,c(1,2,4,6,8,10)]

colnames(compare_to_others) = c('Time','Fin','Oil','Drugs','Food','Guns')
colnames(compare_from_others) =  c('Time','Fin','Oil','Drugs','Food','Guns')


index_mask = index_mask = which(('2018-01-01' <= compare_to_others$Time) & (compare_to_others$Time<= '2023-12-31'))


plot_ts2(compare_to_others[index_mask,c(1,4,5)], mark_shocks = TRUE)
plot_ts2(compare_to_others[index_mask,c(1,3,6)], mark_shocks = TRUE)
plot_ts2(compare_to_others[index_mask,], mark_shocks = TRUE)











compare_df_fc = merge(compare_df_2, r_sq_daily, by = 'Time', all = TRUE)
compare_df_fc = compare_df_fc[,c(1,6,2,3,4,5)]
index_mask = which(('2000-01-01' <= compare_df_fc$Time) & (compare_df_fc$Time<= '2015-12-31'))
compare_df_fc = compare_df_fc[index_mask,]
colnames(compare_df_fc) = c('Time','daily','all_tvp','to_fc','3_before_fc','3_after_fc')
#set parameters
K = ncol(compare_df_fc)
names = colnames(compare_df_fc)

#get scale
min = min(compare_df_fc[,-1], na.rm = TRUE)
max = max(compare_df_fc[,-1], na.rm = TRUE)
print(paste(min,max))
#plot base graph
plot(compare_df_fc[,1], compare_df_fc[,2], type = 'l', col = 'grey' , xlab = 'Time', ylab = 'Correlation Measure', ylim = c(min,max))

#add lines
for (i in 3:(K)){
  line_data = na.omit(compare_df_fc[,c(1,i)])
  lines(line_data[,1], line_data[,2] , type = 'l', col = i-1, lw = 1)
}
for (shock in shocks){
  rect(xleft = as.Date(shock[2]), xright = as.Date(shock[3]), ybottom = min,
        ytop = max, density = 5, col = rgb(0,0,1,1))
  text(x = as.Date(shock[2]), y = min, labels = shock[1], cex = .4)
}

legend('topleft', legend = names[-1], text.col = seq(1,K,1), cex = .5)

