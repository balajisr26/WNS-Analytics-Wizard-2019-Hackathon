gc();
library(data.table)
library(lubridate)
log<-fread('view_log.csv')
str(log)
item<-fread('item_data.csv')
str(item)
nrow(log)
item_log<-merge(log,item,by='item_id',all.x = T)
str(item_log)
nrow(item_log)
item_log$server_time<-ymd_hms(item_log$server_time)
item_log$hour<-hour(item_log$server_time)
item_log$wday<-wday(item_log$server_time)
item_stats_total<-item_log[,list(total_item_cnt=.N
             ,total_item_sessions=length(unique(session_id))
             ,total_device_type=length(unique(device_type))),by='item_id']
summary(item_stats_total)
user_stats_total<-item_log[,list(total_user_cnt=.N
                                 ,total_user_sessions=length(unique(session_id))
                                 ,total_device_type=length(unique(device_type))
                                 ,total_user_price=sum(item_price)),by='user_id']
summary(user_stats_total)
category1_stats_total<-item_log[,list(total_category1_cnt=.N
                                 ,total_user_cnt_category1=length(unique(user_id))
                                 ,total_session_cnt_category1=length(unique(session_id))
                                 ),by='category_1']
summary(category1_stats_total)
category2_stats_total<-item_log[,list(total_category2_cnt=.N
                                      ,total_user_cnt_category2=length(unique(user_id))
                                      ,total_session_cnt_category2=length(unique(session_id))
                               ),by='category_2']
summary(category2_stats_total)
category3_stats_total<-item_log[,list(total_category3_cnt=.N
                                      ,total_user_cnt_category3=length(unique(user_id))
                                      ,total_session_cnt_category3=length(unique(session_id))
                               ),by='category_3']
summary(category3_stats_total)
product_type_stats_total<-item_log[,list(total_product_type_cnt=.N
                                      ,total_user_cnt_productype=length(unique(user_id))
                                      ,total_session_cnt_productype=length(unique(session_id))
                              ),by='product_type']
summary(product_type_stats_total)
user_items_total<-item_log[,list(user_items_cnt_total=.N,user_items_price=sum(item_price)
                                ,total_user_items_session_cnt=length(unique(session_id))
                                ),by=c('user_id','item_id')]
summary(user_items_total)
session_stats_total<-item_log[,list(session_duration=as.numeric(max(server_time)-min(server_time))
                                    ,no_of_items_session=length(unique(item_id)))
                              ,by='session_id']
summary(session_stats_total)

##############################################################


train<-fread('train.csv')
test<-fread('test.csv')
train$ind<-1
test$ind<-2
nrow(train)#237609
nrow(test)#90675
train_test<-rbind(train,test,fill=T)

train_test$impression_time<-ymd_hms(train_test$impression_time)
nrow(train_test)
train_test<-merge(train_test,user_stats_total,by='user_id',all.x = T)
str(train_test)
train_test<-merge(train_test,item_log,by='user_id',all.x=T,allow.cartesian = T)
train_test$flag<-ifelse(train_test$impression_time>train_test$server_time,1,0)
train_test<-subset(train_test,flag==1)
str(train_test)

########First Item ID#############
train_test<-train_test[,rank:=frankv(server_time,order=-1,ties.method='dense'),by='impression_id']
df<-subset(train_test,rank==1)
str(df)

df<-df[,dup_rank:=frankv(impression_time,ties.method='random'),by='impression_id']
df<-subset(df,dup_rank==1)
str(df)
df$time_lag<-as.numeric(df$impression_time-df$server_time)
df$hour_imp<-hour(df$impression_time)
df$wday_imp<-wday(df$impression_time)
df<-merge(df,item_stats_total,by.x='item_id',by.y='item_id',all.x = T)
df<-merge(df,category1_stats_total,by='category_1',all.x = T)
df<-merge(df,category2_stats_total,by='category_2',all.x = T)
df<-merge(df,category3_stats_total,by='category_3',all.x = T)
df<-merge(df,product_type_stats_total,by='product_type',all.x = T)
df<-merge(df,user_items_total,by=c('user_id','item_id'),all.x = T)
df<-merge(df,session_stats_total,by=c('session_id'),all.x = T)
colnames(df)<-paste0(names(df),"_","first")

########Second Item#############
df_second<-subset(train_test,rank==2)
colnames(df_second)<-paste0(names(df_second),"_","second")
str(df_second)

df_second<-df_second[,dup_rank:=frankv(impression_time_second,ties.method='random'),by='impression_id_second']
df_second<-subset(df_second,dup_rank==1)
str(df_second)
df_second$time_lag_second<-as.numeric(df_second$impression_time_second-df_second$server_time_second)
df_second$hour_imp_second<-hour(df_second$impression_time_second)
df_second$wday_imp_second<-wday(df_second$impression_time_second)
df_second<-merge(df_second,item_stats_total,by.x='item_id_second',by.y='item_id',all.x = T)
df_second<-merge(df_second,category1_stats_total,by.x='category_1_second',by.y='category_1',all.x = T)
df_second<-merge(df_second,category2_stats_total,by.x='category_2_second',by.y='category_2',all.x = T)
df_second<-merge(df_second,category3_stats_total,by.x='category_3_second',by.y='category_3',all.x = T)
df_second<-merge(df_second,product_type_stats_total,by.x='product_type_second',by.y='product_type',all.x = T)
df_second<-merge(df_second,user_items_total,by.x=c('user_id_second','item_id_second'),by.y=c('user_id','item_id'),all.x = T)
df_second<-merge(df_second,session_stats_total,by.x=c('session_id_second'),by.y=c('session_id'),all.x = T)
df_second$session_duration_second<-df_second$session_duration
df_second$no_of_items_session_second<-df_second$no_of_items_session
df_second$session_duration<-NULL
df_second$no_of_items_session<-NULL
str(df_second)
colnames(df_second)<-paste0(names(df_second),"_","second")


df<-merge(df,df_second,by.x='impression_id_first',by.y='impression_id_second_second',all.x=T)
df<-df[,dup_rank:=frankv(impression_time_first,ties.method='random'),by='impression_id_first']
df<-subset(df,dup_rank==1)

str(df)

########Third Item#############
df_third<-subset(train_test,rank==3)
colnames(df_third)<-paste0(names(df_third),"_","third")
str(df_third)

df_third<-df_third[,dup_rank:=frankv(impression_time_third,ties.method='random'),by='impression_id_third']
df_third<-subset(df_third,dup_rank==1)
str(df_third)
df_third$time_lag_third<-as.numeric(df_third$impression_time_third-df_third$server_time_third)
df_third$hour_imp_third<-hour(df_third$impression_time_third)
df_third$wday_imp_third<-wday(df_third$impression_time_third)
df_third<-merge(df_third,item_stats_total,by.x='item_id_third',by.y='item_id',all.x = T)
df_third<-merge(df_third,category1_stats_total,by.x='category_1_third',by.y='category_1',all.x = T)
df_third<-merge(df_third,category2_stats_total,by.x='category_2_third',by.y='category_2',all.x = T)
df_third<-merge(df_third,category3_stats_total,by.x='category_3_third',by.y='category_3',all.x = T)
df_third<-merge(df_third,product_type_stats_total,by.x='product_type_third',by.y='product_type',all.x = T)
df_third<-merge(df_third,user_items_total,by.x=c('user_id_third','item_id_third'),by.y=c('user_id','item_id'),all.x = T)
df_third<-merge(df_third,session_stats_total,by.x=c('session_id_third'),by.y=c('session_id'),all.x = T)
df_third$session_duration_third<-df_third$session_duration
df_third$no_of_items_session_third<-df_third$no_of_items_session
df_third$session_duration<-NULL
df_third$no_of_items_session<-NULL
str(df_third)
colnames(df_third)<-paste0(names(df_third),"_","third")


df<-merge(df,df_third,by.x='impression_id_first',by.y='impression_id_third_third',all.x=T)
df<-df[,dup_rank:=frankv(impression_time_first,ties.method='random'),by='impression_id_first']
df<-subset(df,dup_rank==1)

########Fourth Item#############
df_fourth<-subset(train_test,rank==4)
colnames(df_fourth)<-paste0(names(df_fourth),"_","fourth")
str(df_fourth)

df_fourth<-df_fourth[,dup_rank:=frankv(impression_time_fourth,ties.method='random'),by='impression_id_fourth']
df_fourth<-subset(df_fourth,dup_rank==1)
str(df_fourth)
df_fourth$time_lag_fourth<-as.numeric(df_fourth$impression_time_fourth-df_fourth$server_time_fourth)
df_fourth$hour_imp_fourth<-hour(df_fourth$impression_time_fourth)
df_fourth$wday_imp_fourth<-wday(df_fourth$impression_time_fourth)
df_fourth<-merge(df_fourth,item_stats_total,by.x='item_id_fourth',by.y='item_id',all.x = T)
df_fourth<-merge(df_fourth,category1_stats_total,by.x='category_1_fourth',by.y='category_1',all.x = T)
df_fourth<-merge(df_fourth,category2_stats_total,by.x='category_2_fourth',by.y='category_2',all.x = T)
df_fourth<-merge(df_fourth,category3_stats_total,by.x='category_3_fourth',by.y='category_3',all.x = T)
df_fourth<-merge(df_fourth,product_type_stats_total,by.x='product_type_fourth',by.y='product_type',all.x = T)
df_fourth<-merge(df_fourth,user_items_total,by.x=c('user_id_fourth','item_id_fourth'),by.y=c('user_id','item_id'),all.x = T)
df_fourth<-merge(df_fourth,session_stats_total,by.x=c('session_id_fourth'),by.y=c('session_id'),all.x = T)
df_fourth$session_duration_fourth<-df_fourth$session_duration
df_fourth$no_of_items_session_fourth<-df_fourth$no_of_items_session
df_fourth$session_duration<-NULL
df_fourth$no_of_items_session<-NULL
str(df_fourth)
colnames(df_fourth)<-paste0(names(df_fourth),"_","fourth")

df<-merge(df,df_fourth,by.x='impression_id_first',by.y='impression_id_fourth_fourth',all.x=T)
df<-df[,dup_rank:=frankv(impression_time_first,ties.method='random'),by='impression_id_first']
df<-subset(df,dup_rank==1)

########Fifth Item#############
df_fifth<-subset(train_test,rank==5)
colnames(df_fifth)<-paste0(names(df_fifth),"_","fifth")
str(df_fifth)

df_fifth<-df_fifth[,dup_rank:=frankv(impression_time_fifth,ties.method='random'),by='impression_id_fifth']
df_fifth<-subset(df_fifth,dup_rank==1)
str(df_fifth)
df_fifth$time_lag_fifth<-as.numeric(df_fifth$impression_time_fifth-df_fifth$server_time_fifth)
df_fifth$hour_imp_fifth<-hour(df_fifth$impression_time_fifth)
df_fifth$wday_imp_fifth<-wday(df_fifth$impression_time_fifth)
df_fifth<-merge(df_fifth,item_stats_total,by.x='item_id_fifth',by.y='item_id',all.x = T)
df_fifth<-merge(df_fifth,category1_stats_total,by.x='category_1_fifth',by.y='category_1',all.x = T)
df_fifth<-merge(df_fifth,category2_stats_total,by.x='category_2_fifth',by.y='category_2',all.x = T)
df_fifth<-merge(df_fifth,category3_stats_total,by.x='category_3_fifth',by.y='category_3',all.x = T)
df_fifth<-merge(df_fifth,product_type_stats_total,by.x='product_type_fifth',by.y='product_type',all.x = T)
df_fifth<-merge(df_fifth,user_items_total,by.x=c('user_id_fifth','item_id_fifth'),by.y=c('user_id','item_id'),all.x = T)
df_fifth<-merge(df_fifth,session_stats_total,by.x=c('session_id_fifth'),by.y=c('session_id'),all.x = T)
df_fifth$session_duration_fifth<-df_fifth$session_duration
df_fifth$no_of_items_session_fifth<-df_fifth$no_of_items_session
df_fifth$session_duration<-NULL
df_fifth$no_of_items_session<-NULL
str(df_fifth)
colnames(df_fifth)<-paste0(names(df_fifth),"_","fifth")


str(df)


df<-merge(df,df_fifth,by.x='impression_id_first',by.y='impression_id_fifth_fifth',all.x=T)
df<-df[,dup_rank:=frankv(impression_time_first,ties.method='random'),by='impression_id_first']
df<-subset(df,dup_rank==1)


train<-subset(df,ind_first==1)
test<-subset(df,ind_first==2)
nrow(train)#235988
nrow(test)#90675

fwrite(train,'train_prepared3.csv')
fwrite(test,'test_prepared3.csv')

str(train)
