#require(devtools)
#devtools::install_github("richardjtelford/ggbiplot", ref = "experimental")
source('../normality/functions/load_wtdata.R')

db_config_data<- data.frame(user='user',
                            password='password',
                            dbname='yourHistoricalBD',
                            host='127.0.0.1',
                            port=3306)

library(ggbiplot)
prcomp.recon <- function(pca, pcs=NULL){
    if(is.null(pcs)) pcs <- seq(pca$sdev)
    recon <- as.matrix(pca$x[,pcs]) %*% t(as.matrix(pca$rotation[,pcs]))
    if(pca$scale[1] != FALSE){
        recon <- scale(recon , center=FALSE, scale=1/pca$scale)
    }
    if(pca$center[1] != FALSE){
        recon <- scale(recon , center=-pca$center, scale=FALSE)
    }
    return(recon)
}


if(!file.exists("wtdata.RData")){
    ld_id=126
    wp_id=19
    wp_code='escamb'
    fault='gbox2'
    array_id_walm='1806,794,1823,732,1848,1839'
    array_ot='10003,10015,10029,10045,10050'
    power_condition=NULL
    include_variables=''
    exclude_variables=''
    unix_timestamp_ini=1388534400
    unix_timestamp_end=1420070399
    freq_dat_med_min=10
    seconds_to_aggregate=86400
    seconds_offset=0
    date_time_name='date_time'
    target='alarm'
    table_cast_park_dic='1_cast_park_table_dic'
    table_filter_config='1_filter_config'
    filter='fq8,fclean,fnzv'
    filter_exclude=paste("date_time,ld_id,alarm,alarm_block_code,alarm_all,alarm_all_block_code,ot,ot_block_code,ot_all,ot_all_block_code,n1,weekly_n1,weekly_power",sep=",")
    
rs  <-  load_wtdata(ld_id=ld_id,
                    wp_id=wp_id,
                    wp_code=wp_code,
                    fault=fault,
                    array_id_walm=array_id_walm,
                    array_ot=array_ot,
                    power_condition=power_condition,
                    include_variables=include_variables,
                    exclude_variables=exclude_variables,
                    unix_timestamp_ini=unix_timestamp_ini,
                    unix_timestamp_end=unix_timestamp_end,
                    freq_dat_med_min=freq_dat_med_min,
                    seconds_to_aggregate=seconds_to_aggregate,
                    seconds_offset=seconds_offset,
                    date_time_name=date_time_name,
                    target=target,
                    table_cast_park_dic=table_cast_park_dic,
                    table_filter_config=table_filter_config,
                    filter=filter,
                    filter_exclude=filter_exclude,
                    update_filter_ranges=FALSE,
                    db_config=db_config_data)
if(rs$error) {
    return(list(error=TRUE,data=NULL,msg=output_msg))
}
wtdata <- rs$data$wtdata
outliers<- rs$data$outliers
wtdata0 <- rs$data$wtdata0

save(wtdata,wtdata0,outliers,ld_id,wp_id,wp_code,fault,array_id_walm,array_ot,power_condition,include_variables,exclude_variables,unix_timestamp_ini,unix_timestamp_end,freq_dat_med_min,
     seconds_to_aggregate,seconds_offset,date_time_name,target,table_cast_park_dic,table_filter_config,filter,filter_exclude,file="wtdata.RData")
}else{
    load("wtdata10min.RData")
    #load("wtdata1day.RData")
}
d<-wtdata
dt<-d$date_time
alarm<-d[,c("date_time","alarm")]
alarm_daybefore<-alarm
#alarm_on<-alarm_daybefore$date_time[alarm_daybefore$alarm==1]-as.difftime(7, units="days")
alarm_on<-alarm_daybefore$date_time[alarm_daybefore$alarm==1]

seconds_to_aggregate<-86400
alarm_on<-as.POSIXct((as.numeric(alarm_on)%/%seconds_to_aggregate)*seconds_to_aggregate,origin='1970-01-01')
alarm_daybefore$date_time<-as.POSIXct((as.numeric(alarm_daybefore$date_time)%/%seconds_to_aggregate)*seconds_to_aggregate,origin='1970-01-01')

alarm_daybefore$alarm<-0
alarm_daybefore$alarm[which(alarm_daybefore$date_time %in% alarm_on)]<-1

d$date_time<-NULL
d$alarm<-NULL
d$alarm_all<-NULL
d$alarm_block_code<-NULL
d$alarm_all_block_code<-NULL
d$ot<-NULL
d$ot_all<-NULL
d$ot_all_block_code<-NULL
d$ot_block_code<-NULL
#remove columns with Cont
d<-d[,!grepl("Cont", names(d))]
#remove columns with _sdv
d<-d[,!grepl("_sdv", names(d))]
#remove columns with _max
d<-d[,!grepl("_max", names(d))]

#remove columns with _min
d<-d[,!grepl("_min", names(d))]


sdv<-apply(d,2,function(col) sd(x = col,na.rm = TRUE))
nazerosdv<-which(is.na(sdv)|sdv==0)
if(length(nazerosdv)>0) d<-d[,-nazerosdv]
na_count <-sapply(d, function(y) sum(length(which(is.na(y)))))
if(length(na_count)>0 && any(na_count>nrow(d)*0.2)) d<-d[,-which(na_count>nrow(d)*0.2)]#Remove columns with more than 20% NA
keep<-complete.cases(d)
d<-d[keep,]
dt<-dt[keep]
alarm<-alarm[keep,]
alarm_daybefore<-alarm_daybefore[keep,]
d<-d[,apply(d, 2, var, na.rm=TRUE) != 0]
pr<-prcomp(x=d,scale=TRUE,center=TRUE)
#biplot(x = pr)
#ggbiplot(pr,obs.scale = 1, var.scale=1,groups=alarm,ellipse=FALSE,circle=FALSE,alpha = 0.1,varname.size = 3)

alarm_fact<-as.factor(alarm_daybefore$alarm)
size_val<-ifelse(alarm_daybefore$alarm, 0.6, 0.5)
alpha_val<-as.factor(ifelse(alarm_daybefore$alarm, 1, 0.1))
df <- fortify(pr, scale = 1, equalize = FALSE)
ggplot(df, aes(x = PC1, PC2)) + 
       geom_point(aes(color = alarm_fact,alpha=alpha_val)) +
       geom_axis(data = attr(df, "basis"), aes(label = .name))

pc1<-pr$x[,1]
pc2<-pr$x[,2]
correlations <- t(pr$rotation)*pr$sdev

variables_pc1_correlation_sorted<-sort(correlations[1,],decreasing = TRUE)
variables_pc2_correlation_sorted<-sort(correlations[2,],decreasing = TRUE)


var<-pr$sdev^2
pve<-var/sum(var)


plot(var, xlab = "Principal Component",
        ylab = "Proportion of Variance Explained",
        ylim = c(0, 1), type = "b")

plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")


d_recons<-prcomp.recon(pr,50)

error_column<-abs(d-d_recons)
error_by_column<-apply(error_column,2,sum)
sorted_error_by_column<-sort(x = error_by_column,decreasing = TRUE)
p<-plot_ly()
for(col in 1:5){
    p<-add_trace(p,data=d,x=1:nrow(d),y=d[,names(sorted_error_by_column)[col]],name=paste0(names(sorted_error_by_column)[col],' original'), type='scatter',mode='lines')
    p<-add_trace(p,data=d_recons,x=1:nrow(d_recons),y=d_recons[,names(sorted_error_by_column)[col]],name=paste0(names(sorted_error_by_column)[col],' recons'), type='scatter',mode='lines')
}
p
