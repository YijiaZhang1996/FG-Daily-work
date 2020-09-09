setwd("C:/Users/Administrator/Documents/bond_fund")
library(lubridate)

t1<-proc.time()

end_date<-"2020-8-31"

# ����������Ļ���
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
  # ��ȡ��������ÿ�������յĸ�Ȩ��λ��ֵ
  fund_setup_date<-as.Date(w.wss(fund_code[j],'fund_setupdate')$Data$FUND_SETUPDATE,origin="1899-12-30")
  nav<-w.wsd(fund_code[j],"NAV_adj",fund_setup_date,end_date)$Data
  num_of_trade_date<-length(nav$DATETIME)
  num_of_trade_date_until_0531<-num_of_trade_date-64
  
  for (i in 1:(num_of_trade_date_until_0531)) {
    # ��ÿһ�����ڶ�+3���£���Ϊ��ֹ���ڣ���ֹ�������������
    end_date_3m<-nav$DATETIME[i]+months(3)
    
    # ���1��+3���º�����ڻ��ǽ����գ�ֱ������ֹ������ȡ�������漴��
    if(is.element(end_date_3m,nav$DATETIME)){
      interval_return<-(nav$NAV_ADJ[nav$DATETIME==end_date_3m]-nav$NAV_ADJ[nav$DATETIME==nav$DATETIME[i]])/nav$NAV_ADJ[nav$DATETIME==nav$DATETIME[i]]
    }
    
    # ���2��+3����֮��������ǿ�ֵ������2011-11-29��3���º�û�ж�Ӧ���ڣ�ʹ��wind�ӿڻ��3���º�Ľ���������Ϊ��ֹ��
    else if(is.na(end_date_3m)){
      end_date_3m<-w.tdaysoffset(3,nav$DATETIME[i],"Period=M")$Data$DATETIME 
      interval_return<-(nav$NAV_ADJ[nav$DATETIME==end_date_3m]-nav$NAV_ADJ[nav$DATETIME==nav$DATETIME[i]])/nav$NAV_ADJ[nav$DATETIME==nav$DATETIME[i]]
    }
    # ���3��+3����֮������ڲ��ǽ����գ������ھ�ֵ������û�����ݣ�����Ҫ�ҵ������ֹ������ĸ����һ����������Ϊ��ֹ��
    else{
      end_date_3m<-max(nav[nav$DATETIME<end_date_3m,1])
      interval_return<-(nav$NAV_ADJ[nav$DATETIME==end_date_3m]-nav$NAV_ADJ[nav$DATETIME==nav$DATETIME[i]])/nav$NAV_ADJ[nav$DATETIME==nav$DATETIME[i]]
    }
    
    nav$return_holding_3m[i]<-interval_return
  }
  num_of_positive_return<-sum(nav$return_holding_3m[1:num_of_trade_date_until_0531]>0)
  winning_probability<-num_of_positive_return/num_of_trade_date_until_0531
  
  result_list[j,1]<-fund_code[j]
  result_list[j,2]<-winning_probability
  result_list[j,3]<-num_of_positive_return
  result_list[j,4]<-num_of_trade_date_until_0531
  
}

# ����������ʤ������
for (j in 1:length(fund_code)) {
  result_list[,5]<-paste(rank(-result_list$X2),length(result_list$X2),sep="/")
  result_list[,6]<-rank(-result_list$X2)/length(result_list$X2)
  
}
colnames(result_list)<-c("�������","����������ʤ��","���������¾�ֵ�����ʴ���0�Ľ�������","������2020��5��31���ܽ���������","ͬ������","ͬ�������ٷֱ�")
write.csv(result_list, "winning_probability_all.csv")

t2<-proc.time()
t<-t2-t1
print(paste0('Total running time��',t[3][[1]],'s'))
       