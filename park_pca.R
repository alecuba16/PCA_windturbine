#require(devtools)
#devtools::install_github("richardjtelford/ggbiplot", ref = "experimental")

#library(ggbiplot)

Sys.setenv(TZ='UTC')
db_config<- data.frame(user='user',
                       password='password',
                       dbname='yourHistoricalBD',
                       host='127.0.0.1',
                       port=3306)
if(!exists("dependencyLoader")){
    if(!file.exists('functions/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions/dependencyLoader.R")));
    source('functions/dependencyLoader.R')
}

if(!exists("createFile.R")){
    if(!file.exists('functions/createFile.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions/createFile.R")));
    source('functions/createFile.R')
}

if(!exists("dirname2")){
    if(!file.exists('functions/dirname2.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions/dirname2.R")));
    source('functions/dirname2.R')
}

# Sources
libraries<-c('RMySQL','plyr','dplyr','rmarkdown','ggbiplot')
sources<-paste0("functions/",
                c('load_wtdata.R','close_protocol.R','filter_custom.R'))
dep<-dependencyLoader(c(libraries,sources))
if(dep$error)  return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))

debug_mode<-TRUE

#2014->2015
#dates<-matrix(c(ini=1388534400,ini=1420070400,end=1420070399,end=1451606399),nrow=2,ncol=2)
dates<-matrix(c(ini=1448928000,ini=1451606400,end=1451606399,end=1454284800),nrow=2,ncol=2)


seconds_to_aggregate<-600
template_file<-"park_pca.Rmd"

#escamb
 parks<-data.frame(
    ld_id=seq(119,134),
    #ld_id=seq(124,126),
    wp_id=19,
    wp_code='escamb',
    fault='Gbox1',
    array_id_walm="1806,794,1823,732,1848,1839",
    array_ot="10003,10015,10029,10045,10050",
    target_name='alarm',stringsAsFactors=FALSE)

# parks<-data.frame(
#     ld_id=seq(119,134),
#     #ld_id=seq(124,126),
#     wp_id=19,
#     wp_code='escamb',
#     fault='Gen1',
#     array_id_walm="706,727,728,729,1836,1837,1838,1844,1845,1846,3076,3077",
#     array_ot="10004,10006,10038,10042,10047,10061,10062",
#     target_name='alarm',stringsAsFactors=FALSE)

#izco
# parks<-data.frame(
# ld_id=seq(167,216),
# wp_id=20,
# wp_code='Izco',
# fault='gbox1',
# array_id_walm='607,608,613,631,659',
# array_ot='10067,10068',
# target_name='alarm',stringsAsFactors=FALSE)
#parks<-ifelse(exists("parks"),rbind(parks,izco),izco)

#moncayuelo
# parks<-data.frame(
#     ld_id=seq(135,166),
#     wp_id=21,
#     wp_code='Moncay',
#     fault='gbox1',
#     array_id_walm="",
#     array_ot="10065,10067,10068,10069",
#     target_name='ot',stringsAsFactors=FALSE)
# parks<-ifelse(exists("parks"),rbind(parks,moncay),moncay)

park_list<-unique(parks$wp_id)
for(pid in 1:length(park_list)){
    for(date in 1:nrow(dates)){
        park_turbines<-parks[parks$wp_id==park_list[pid],]
        turbines<- list()
        pos<-1
        fault<-park_turbines$fault[1]
        target_name<-park_turbines$target_name[1]
        for(pos in 1:length(park_turbines$ld_id)){
            ld_id<-park_turbines$ld_id[pos]
            current_turbine<-park_turbines[pos,]
            cat(paste0("Turbine ",ld_id))
            rs  <-  load_wtdata(
                ld_id=ld_id,
                wp_id=current_turbine$wp_id,
                wp_code=current_turbine$wp_code,
                fault=current_turbine$fault,
                array_id_walm=current_turbine$array_id_walm,
                array_ot=current_turbine$array_ot,
                power_condition='',
                include_variables='',
                exclude_variables='model,fake_data,n1',
                unix_timestamp_ini=dates[date,1],
                unix_timestamp_end=dates[date,2],
                freq_dat_med_min=10,
                seconds_to_aggregate=seconds_to_aggregate,
                seconds_offset=0,
                date_time_name='date_time',
                target=current_turbine$target_name,
                table_cast_park_dic='1_cast_park_table_dic',
                table_filter_config='1_filter_config',
                filter='frange,fclean,fnzv',
                filter_exclude=paste("date_time,ld_id,alarm,alarm_block_code,alarm_all,alarm_all_block_code,ot,ot_block_code,ot_all,ot_all_block_code,n1,weekly_n1,weekly_power",sep=","),
                update_filter_ranges=FALSE,
                db_config=db_config)
            if(rs$error) {
                output_msg <- paste0("\n",iam,":on call load_wtdata\n\t",rs$msg)
                close_protocol(output_msg, iam, debug_mode)
                return(list(error=TRUE,data=NULL,msg=output_msg))
            }
            wtdata <- rs$data$wtdata
            
            #PCA
            dt<-wtdata$date_time
            target<-wtdata[,c("date_time",target_name)]
            
            wtdata$date_time<-NULL
            wtdata$alarm<-NULL
            wtdata$alarm_all<-NULL
            wtdata$alarm_block_code<-NULL
            wtdata$alarm_all_block_code<-NULL
            wtdata$ot<-NULL
            wtdata$ot_all<-NULL
            wtdata$ot_all_block_code<-NULL
            wtdata$ot_block_code<-NULL
            #remove columns with Cont
            wtdata<-wtdata[,!grepl("Cont", names(wtdata))]
            #remove columns with _sdv
            wtdata<-wtdata[,!grepl("_sdv", names(wtdata))]
            #remove columns with _max
            wtdata<-wtdata[,!grepl("_max", names(wtdata))]
            #remove columns with _min
            wtdata<-wtdata[,!grepl("_min", names(wtdata))]
            sdv<-apply(wtdata,2,function(col) sd(x = col,na.rm = TRUE))
            nazerosdv<-which(is.na(sdv)|sdv==0)
            if(length(nazerosdv)>0) wtdata<-wtdata[,-nazerosdv]
            na_count <-sapply(wtdata, function(y) sum(length(which(is.na(y)))))
            if(length(na_count)>0 && any(na_count>nrow(wtdata)*0.2)) wtdata<-wtdata[,-which(na_count>nrow(wtdata)*0.2)]#Remove columns with more than 20% NA
            keep<-complete.cases(wtdata)
            wtdata<-wtdata[keep,]
            dt<-dt[keep]
            target<-target[keep,]
            
            wtdata<-wtdata[,apply(wtdata, 2, var, na.rm=TRUE) != 0]
            
            #diff test
            #wtdata<- apply(wtdata,2,diff)
            pr<-prcomp(x=wtdata,scale=TRUE,center=TRUE)
            #biplot(x = pr)
            #ggbiplot(pr,obs.scale = 1, var.scale=1,groups=alarm,ellipse=FALSE,circle=FALSE,alpha = 0.1,varname.size = 3)
        
            turbines[[pos]]<-list(ld_id=ld_id,pca=pr,target=target)
            #endpca
            pos<-pos+1
            rm(pr)
        }
        report_env <- new.env()
        report_env$turbines <- turbines
        report_env$fault<-fault
        report_env$target_name<-target_name
        report_env$seconds_to_aggregate<-seconds_to_aggregate
        wp_code<-parks$wp_code[parks$wp_id==park_list[pid]][1]
        report_env$wp_code<-wp_code
        report_env$parent_directory <- getwd()
        year<-format(as.POSIXct(dates[date,1],origin='1970-01-01',TZ='UTC'),"%Y")
        path_report <- paste0(wp_code,"_",year,"_pca.html")
        cat(paste0("Making report:",path_report))
        #For testing
        save(path_report,template_file,report_env,file="backup.RData")
        
        report_ok <- try(render(input = template_file,
                                output_format = "html_document",
                                output_file = path_report,
                                encoding = "UTF-8",
                                envir = report_env))
        rm(report_env)
        turbines<-NULL
    }
}
