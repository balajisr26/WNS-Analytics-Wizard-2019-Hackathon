gc();
library(catboost)
library(data.table)
library(lubridate)
train<-fread('train_prepared3.csv',data.table=F)
test<-fread('test_prepared3.csv',data.table=F)
train$ind<-1
test$ind<-2
nrow(train)
nrow(test)
train_test<-rbind(train,test,fill=T)
nrow(train_test)
str(train_test)
train_test$app_code_first<-as.factor(train_test$app_code_first)
train_test$is_4G_first<-as.factor(train_test$is_4G_first)
train_test$session_id_first<-as.factor(train_test$session_id_first)
train_test$item_id_first<-as.factor(train_test$item_id_first)
train_test$category_1_first<-as.factor(train_test$category_1_first)
train_test$category_2_first<-as.factor(train_test$category_2_first)
train_test$category_3_first<-as.factor(train_test$category_3_first)
train_test$product_type_first<-as.factor(train_test$product_type_first)
train_test$os_version_first<-as.factor(train_test$os_version_first)
train_test$device_type_first<-as.factor(train_test$device_type_first)
train_test$user_id_first<-as.factor(train_test$user_id_first)
train_test$impression_time_first<-ymd_hms(train_test$impression_time_first)

train_test$app_code_second_second<-as.factor(train_test$app_code_second_second)
train_test$is_4G_second_second<-as.factor(train_test$is_4G_second_second)
train_test$session_id_second_second<-as.factor(train_test$session_id_second_second)
train_test$item_id_second_second<-as.factor(train_test$item_id_second_second)
train_test$category_1_second_second<-as.factor(train_test$category_1_second_second)
train_test$category_2_second_second<-as.factor(train_test$category_2_second_second)
train_test$category_3_second_second<-as.factor(train_test$category_3_second_second)
train_test$product_type_second_second<-as.factor(train_test$product_type_second_second)
train_test$os_version_second_second<-as.factor(train_test$os_version_second_second)
train_test$device_type_second_second<-as.factor(train_test$device_type_second_second)
train_test$user_id_second_second<-as.factor(train_test$user_id_second_second)
train_test$impression_time_second_second<-ymd_hms(train_test$impression_time_second_second)

train_test$app_code_third_third<-as.factor(train_test$app_code_third_third)
train_test$is_4G_third_third<-as.factor(train_test$is_4G_third_third)
train_test$session_id_third_third<-as.factor(train_test$session_id_third_third)
train_test$item_id_third_third<-as.factor(train_test$item_id_third_third)
train_test$category_1_third_third<-as.factor(train_test$category_1_third_third)
train_test$category_2_third_third<-as.factor(train_test$category_2_third_third)
train_test$category_3_third_third<-as.factor(train_test$category_3_third_third)
train_test$product_type_third_third<-as.factor(train_test$product_type_third_third)
train_test$os_version_third_third<-as.factor(train_test$os_version_third_third)
train_test$device_type_third_third<-as.factor(train_test$device_type_third_third)
train_test$user_id_third_third<-as.factor(train_test$user_id_third_third)
train_test$impression_time_third_third<-ymd_hms(train_test$impression_time_third_third)

train_test$app_code_fourth_fourth<-as.factor(train_test$app_code_fourth_fourth)
train_test$is_4G_fourth_fourth<-as.factor(train_test$is_4G_fourth_fourth)
train_test$session_id_fourth_fourth<-as.factor(train_test$session_id_fourth_fourth)
train_test$item_id_fourth_fourth<-as.factor(train_test$item_id_fourth_fourth)
train_test$category_1_fourth_fourth<-as.factor(train_test$category_1_fourth_fourth)
train_test$category_2_fourth_fourth<-as.factor(train_test$category_2_fourth_fourth)
train_test$category_3_fourth_fourth<-as.factor(train_test$category_3_fourth_fourth)
train_test$product_type_fourth_fourth<-as.factor(train_test$product_type_fourth_fourth)
train_test$os_version_fourth_fourth<-as.factor(train_test$os_version_fourth_fourth)
train_test$device_type_fourth_fourth<-as.factor(train_test$device_type_fourth_fourth)
train_test$user_id_fourth_fourth<-as.factor(train_test$user_id_fourth_fourth)
train_test$impression_time_fourth_fourth<-ymd_hms(train_test$impression_time_fourth_fourth)

train_test$app_code_fifth_fifth<-as.factor(train_test$app_code_fifth_fifth)
train_test$is_4G_fifth_fifth<-as.factor(train_test$is_4G_fifth_fifth)
train_test$session_id_fifth_fifth<-as.factor(train_test$session_id_fifth_fifth)
train_test$item_id_fifth_fifth<-as.factor(train_test$item_id_fifth_fifth)
train_test$category_1_fifth_fifth<-as.factor(train_test$category_1_fifth_fifth)
train_test$category_2_fifth_fifth<-as.factor(train_test$category_2_fifth_fifth)
train_test$category_3_fifth_fifth<-as.factor(train_test$category_3_fifth_fifth)
train_test$product_type_fifth_fifth<-as.factor(train_test$product_type_fifth_fifth)
train_test$os_version_fifth_fifth<-as.factor(train_test$os_version_fifth_fifth)
train_test$device_type_fifth_fifth<-as.factor(train_test$device_type_fifth_fifth)
train_test$user_id_fifth_fifth<-as.factor(train_test$user_id_fifth_fifth)
train_test$impression_time_fifth_fifth<-ymd_hms(train_test$impression_time_fifth_fifth)


train<-subset(train_test,ind==1)
test<-subset(train_test,ind==2)

str(train)
feature_names<-setdiff(names(train),c('session_id_first','impression_id_first'
                                      ,'is_click_first','ind','server_time_first'
                                      ,'flag_first','rank_first','dup_rank_first'
                                      ,'impression_time_first'
                                      ,'session_id_second_second','impression_id_second_second'
                                      ,'is_click_second_second','ind','server_time_second_second'
                                      ,'flag_second_second','rank_second_second','dup_rank_second_second'
                                      ,'impression_time_second_second'
                                      ,'session_id_third_third','impression_id_third_third'
                                      ,'is_click_third_third','ind','server_time_third_third'
                                      ,'flag_third_third','rank_third_third','dup_rank_third_third'
                                      ,'impression_time_third_third'
                                      ,'session_id_fourth_fourth','impression_id_fourth_fourth'
                                      ,'is_click_fourth_fourth','ind','server_time_fourth_fourth'
                                      ,'flag_third_fourth_fourth','rank_fourth_fourth','dup_rank_fourth_fourth'
                                      ,'impression_time_fourth_fourth'
                                      ,'session_id_fifth_fifth','impression_id_fifth_fifth'
                                      ,'is_click_fifth_fifth','ind','server_time_fifth_fifth'
                                      ,'flag_third_fifth_fifth','rank_fifth_fifth','dup_rank_fifth_fifth'
                                      ,'impression_time_fifth_fifth'
                                      ))




dtrain<-catboost.load_pool(data=train[,feature_names],label=train$is_click_first
)


dtest<-catboost.load_pool(data=test[,feature_names],label=test$is_click_first
)


fit_params <- list(iterations =150, 
                   thread_count = 8,
                   loss_function = 'CrossEntropy',
                   depth = 5,
                   rsm=1,
                   learning_rate = 0.1,
                   eval_metric='AUC',
                   od_type='Iter',
                   od_wait=100,
                   random_seed=88
)

?c

model <- catboost.train(dtrain,test_pool = NULL, fit_params)

predtrain<-catboost.predict(model,dtrain,prediction_type = "Probability")
predtest<-catboost.predict(model,dtest,prediction_type = "Probability")

summary(test[,feature_names])

summary(predtest)

catboost.get_feature_importance(model,dtest)

df<-data.frame(impression_id=test$impression_id,is_click=predtest)
fwrite(df,'catboost1_4.csv')
