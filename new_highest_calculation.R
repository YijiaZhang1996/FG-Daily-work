setwd("C:/Users/Administrator/Documents/bond_fund")
library(WindR)

t1<-proc.time()

end_date<-"2020-8-31"

# 全部成立满三年的基金
fund_code<-c("160128.OF","161117.OF","164810.OF","519118.OF","166902.OF","050028.OF","100072.OF","163210.OF","161911.OF","000105.OF","000116.OF",
             "000074.OF","000078.OF","000064.OF","000200.OF","000197.OF","000086.OF","000246.OF","000111.OF","000137.OF","000225.OF","166904.OF",
             "000235.OF","160515.OF","000277.OF","000271.OF","000201.OF","000372.OF","000351.OF","160131.OF","519973.OF","000465.OF","000552.OF",
             "000469.OF","000415.OF","000265.OF","000817.OF","001246.OF","519955.OF","519953.OF","519945.OF","002048.OF","002356.OF","001859.OF",
             "519941.OF","002476.OF","519320.OF","002483.OF","002904.OF","002948.OF","002858.OF","003159.OF","003239.OF","003324.OF","003564.OF",
             "519326.OF","003832.OF","003776.OF","004030.OF","004021.OF","501100.OF","004123.OF","004122.OF","004438.OF","003770.OF","004141.OF",
             "004386.OF","004254.OF","003841.OF","004464.OF","003931.OF","004723.OF","004722.OF","004911.OF","004978.OF","004681.OF","005070.OF",
             "000516.OF","004919.OF")

result_list<-data.frame(matrix(NA, ncol=6, nrow = 0))


for (j in 1:length(fund_code)) {
  fund_setup_date<-as.Date(w.wss(fund_code[j],'fund_setupdate')$Data$FUND_SETUPDATE,origin="1899-12-30")
  nav<-w.wsd(fund_code[j],"NAV_adj",fund_setup_date,end_date)$Data
  num_of_trade_date<-length(nav$DATETIME)
  
  # 计算成立以来到之后每一天的净值最大值
  for (i in 1:num_of_trade_date) {
    nav$max[i]<-max(nav[1:i,2])
  }
  
  # 计算创新高的天数和概率
  num_of_new_high<-sum(nav$NAV_ADJ==nav$max)
  proportion_of_new_high<-num_of_new_high/num_of_trade_date
  
  result_list[j,1]<-fund_code[j]
  result_list[j,2]<-proportion_of_new_high
  result_list[j,3]<-num_of_new_high
  result_list[j,4]<-num_of_trade_date
  
}

# 创新高的概率排名
for (j in 1:length(fund_code)) {
  result_list[,5]<-paste(rank(-result_list$X2),length(result_list$X2),sep="/")
  result_list[,6]<-rank(-result_list$X2)/length(result_list$X2)
  
}
colnames(result_list)<-c("基金代码","创新高天数占比","创新高天数","成立以来总交易日天数","同类排名","同类排名百分比")

# 保存结果
write.csv(result_list, "new_highest_result.csv")


t2<-proc.time()
t<-t2-t1
print(paste0('Total running time：',t[3][[1]],'s'))
