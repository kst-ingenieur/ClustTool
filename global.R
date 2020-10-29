print(getwd())


packages = c("shinydashboard","leaflet","dplyr","shiny",
              "Rtsne","dplyr","leaflet.extras",
              "tidyverse","osmdata","sf","readxl",
              "factoextra","NbClust","fpc","cluster",
              "ggmap","ggplot2","mclust","DT",
              "schoolmath","factoextra","xfun",
              "Rtsne","e1071","fclust",
              "heatmaply","plotly",
              "shinycssloaders",
              "robCompositions","clValid","shinyjs",
              "rlist","shinyBS","EnvStats","writexl")
for (mylib in packages){
  print(mylib)
  if(mylib %in% rownames(installed.packages()) == FALSE) {install.packages(mylib, repos="http://cran.rstudio.com/")}
  library(mylib, character.only = T)
}



nb=function(daten,methode,dist,nonnum,agglomeration_methode,d_matrix){
  
  if (methode=="kmeans"){
    nbclust=NbClust(daten,distance = dist,method = methode,index ="all")
    fviznbclust=fviz_nbclust(nbclust)
    listnb=list(fviznbclust$data$Number_clusters[which( 
      fviznbclust$data$freq==max(fviznbclust$data$freq)
    )],
    nbclust,fviznbclust)
  }
  
  else if( methode=="clara") {
    fviznbclust=fviz_nbclust(daten,FUNcluster = cluster::clara,method = "silhouette")
    listnb=list(as.numeric(fviznbclust$data$clusters[which(
      fviznbclust$data$y==max(fviznbclust$data$y))
      ]),
      fviznbclust )
  }
  
  else if(methode=="cmeans"){
    sil_width <- c(NA)
    for(i in 2:15){  
      cmeans_fit <- e1071::cmeans(daten,centers =i, method="cmeans",dist = dist) 
      sil=silhouette(as.numeric(cmeans_fit$cluster),dist(daten,method=dist))  
      sil_width[i] <- mean(sil[,3])
    }
    
    my_number_of_cluster=which(sil_width[-1]==max(sil_width[-1]))+1
    
    listnb=list(my_number_of_cluster,sil_width)
    
  }
  
  else if (methode=="pam"){
    
    sil_width <- c(NA)
    d=d_matrix
    for(i in 2:15){  
      pam_fit <- pam(d, diss = TRUE, k = i)  
      sil_width[i] <- pam_fit$silinfo$avg.width  
    }
    my_number_of_cluster=which(sil_width[-1]==max(sil_width[-1]))+1
    
    listnb=list(my_number_of_cluster,sil_width)
  }#pam
  
  else if(methode=="hclust"){
    if(length(nonnum)>0) 
    {
      sil_width <- c(NA)
      d=d_matrix
      for(i in 2:15){  
        dv2=hcut(d,isdiss = TRUE,k=i,hc_func="hclust",hc_method=agglomeration_methode,
                 hc_metric = distance)
        cl=dv2$cluster
        sil=silhouette(cl,d) 
        sil_width[i] <- mean(sil[,3])
      }
      my_number_of_cluster=which(sil_width[-1]==max(sil_width[-1]))+1
      
      listnb=list(my_number_of_cluster,sil_width)
    }#hclust and nonnum>0
    if(length(nonnum)==0){
      nbclust=NbClust(data=daten,diss=d_matrix,distance =NULL ,method = agglomeration_methode,index ="all")
      fviznbclust=fviz_nbclust(nbclust)
      listnb=list(fviznbclust$data$Number_clusters[which(
        fviznbclust$data$freq==max(fviznbclust$data$freq)
      )],
      nbclust,fviznbclust)
    }#hclust and nonnum==0
  }#hclust
  
  else if(methode=="diana"){
    #if(length(nonnum)>0){
    sil_width <- c(NA)
    #gowerdistance=daisy(na.omit(daten),metric = "gower")
    d=d_matrix
    for(i in 2:15){  
      dv2=hcut(d,isdiss = TRUE,k=i,hc_func="diana",
               hc_metric = distance)
      cl=dv2$cluster
      sil=silhouette(cl,d) 
      sil_width[i] <- mean(sil[,3])
    }
    
    my_number_of_cluster=which(sil_width[-1]==max(sil_width[-1]))+1
    
    listnb=list(my_number_of_cluster,sil_width)
    #}#diana and length nonnum>0
    
  }#diana
  
  if (methode=="agnes"){
    if(length(nonnum)>0){
      sil_width <- c(NA)
      #gowerdistance=daisy(na.omit(daten),metric = "gower")
      d=d_matrix
      for(i in 2:15){  
        dv2=hcut(d,isdiss = TRUE,k=i,hc_func="agnes",hc_method=agglomeration_methode,
                 hc_metric = distance)
        cl=dv2$cluster
        sil=silhouette(cl,d) 
        sil_width[i] <- mean(sil[,3])
      }
      my_number_of_cluster=which(sil_width[-1]==max(sil_width[-1]))+1
      
      listnb=list(my_number_of_cluster,sil_width)
    }#agnes and nonum>0
    
    if(length(nonnum)==0){
      nbclust=NbClust(data=daten,diss=d_matrix,distance =NULL ,method = agglomeration_methode,index ="all")
      fviznbclust=fviz_nbclust(nbclust)
      listnb=list(fviznbclust$data$Number_clusters[which(
        fviznbclust$data$freq==max(fviznbclust$data$freq)
      )],
      nbclust,fviznbclust)
    }#agnes and nonum==0
  }#agnes
  
  return(listnb)
} # function nb


#' 

"prepare" <- function(x,scaling,transformation,powers="none"){
  ## ---------------------------------------------------------------------------
  ## x ... matrix or data frame
  ## scaling: 
  ##          - Classical
  ##          - Robust (median)
  ##          - Select_Scaling
  ## transformation: 
  ##                 - nonetrans
  ##                 - Logarithm
  ##                 - Root_Transformation
  ##                 - logcentered
  ##                 - alr (additiv-log-ratio)
  ##                 - BoxCox (with optimal powers)
  ##                 - Centred_Log_Ratio
  ##                 - Isometric_Pivot_Log_Ratio (isometric log ratio)
  ## powers ... vector of powers (default = "none")
  ## ---------------------------------------------------------------------------
  ### scaling:
  Select_caling=function(x){x} 
  Classical <- function(x){ scale(x) }
  Robust <- function(x){ 
    
    t(t((t(t(x) - apply(x, 2, median))))/apply(x,2,mad)) }
  
  
  Select_Scaling <- function(x){x}
  ### transformation:
  Logarithm <- function(x){ log(x)
  }
  
  Root_Transformation<-function(x){
    x=sqrt(x)
  }
  
  
  BoxCox <- function(x){
    for (i in 1:ncol(x)){
      x_new=as.vector(x[,i])
      opt=EnvStats::boxcox(x_new,optimize = TRUE)
      x[,i]=boxcoxTransform(x_new,opt$lambda)
    }
    x
  }
  
  
  Select_Transformation=function(x){x}
  nonetrans <- function(x){
    x
  }
  Logcentered <- function(x){ 
    x=as.matrix(x)
    xgeom=10^apply(log10(x),1,mean)
    x2=x/xgeom
    x2
  }
  
  Additive_Log_Ratio <- function(x) {
    x=as.matrix(x)
    col_name=colnames(x)
    x_new=Rfast::Log( x[, -1]/x[, 1] ) 
    colnames(x_new)=col_name[1:ncol(x)-1]
    x_new
  }
  
  Centred_Log_Ratio <- function(x, offset = 0){
    x=as.matrix(x)
    if(any(x==0) & offset ==0)
      stop("make sure you use pseudo counts before normalisation to avoid 0
           values with log ratio transformation")
    
    # KA added
    #offset = min(x[which(x != 0)])*0.01
    
    
    #if (dim(x)[2] < 2) stop("data must be of dimension greater equal 2")
    if (dim(x)[2] == 1)
    {
      res = list(x.clr = x, gm = rep(1, dim(x)[1]))
    } else{
      geometricmean = function (x) {
        #       if (any(na.omit(x == 0)))
        #         0
        #       else exp(mean(log(unclass(x)[is.finite(x) & x > 0])))
        #     }
        # KA changed to
        exp(mean(log(x + offset)))
      }
      gm = apply(x, 1, geometricmean)
      # KA changed
      x.clr = log((x + offset) / (gm))
      res = x.clr #list(x.clr = x.clr, gm = gm)
    }
    #class(res) = "clr"
    return(res)
  }
  
  Isometric_Pivot_Log_Ratio <- function(x){
    
    # PF, 05.04.2007, fast version MT, 05.05.2007
    # isometric transformation
    # INPUT:
    # x ... compositional data
    # OUTPUT
    # x.iso ... 2 columns matrix 
    # isometric transformation according to paper:  
    x=as.matrix(x)
    col_name=colnames(x)
    x.iso=matrix(NA,nrow=nrow(x),ncol=ncol(x)-1)
    for (i in 1:ncol(x.iso)){
      x.iso[,i]=sqrt((i)/(i+1))*log(((apply(
        as.matrix(x[,1:i]), 1, prod))^(1/i))/(x[,i+1]))
    }
    colnames(x.iso)=col_name[1:ncol(x)-1]
    x.iso=as.data.frame(x.iso)
    return(x.iso)
  }
  get(scaling)(get(transformation)(x))
}




# prepare dataset 2 with results
datensatz2<-function(lon,lat,id,daten_values, method,distance,amount_k,nonnum,
                     agglomeration_method,d_all_comp=NULL,adist_used="null", 
                     weight_count_fact=NULL,adist_data=NULL){ 

  d="null"
  
  #if an id would be choosen in the sidebar the sidebar ide would be used. In the case,
  # that the user havent choosen any id we will append each clusterpoint an id 
  #von 1:length(daten_values[,1])
  if(length(id)==0){
    id=as.vector(1:length(daten_values[,1]))
    id=as.data.frame(id)
  }
  if(length(lon)==0){
    lon=as.vector(rep("Na",length(daten_values[,1])))
    lon=as.data.frame(lon)
  }
  if(length(lat)==0){
    lat=as.vector(rep("Na",length(daten_values[,1])))
    lat=as.data.frame(lat)
  }
  
  daten2=data.frame(c(lon,lat,id,daten_values))                               
  names(daten2)[1]="lon"
  names(daten2)[2]="lat"
  names(daten2)[3]="id" 
  daten2$Cluster= rep(NA,length(lon))
  daten2$size=rep(NA,length(lon))
  daten2$sil=rep(NA,length(lon))
  daten2$Average_Silhouette_Width=rep(NA,length(lon))
  # for mean plot prepare 
  daten2$variable_name_plotmeans=rep(NA,length(lon))
  daten2$cluster_number_plotmeans=rep(NA,length(lon))
  daten2$cluster_center_plotmeans=rep(NA,length(lon))
  daten2$cluster_id_plotmeans=rep(NA,length(lon))
  length_variable=length(daten_values[1,])
  
  if (method=="cmeans"){
    
    cl=e1071::cmeans(daten_values,centers =amount_k, method="cmeans",dist = distance)
    method_out=cl
    if(length(cl$cluster)!=length(daten2$id)){
      print("The listed variables contain missing values resulting in an error. 
            Recheck data set for NA entries.")
      print(apply(daten_values,2,function(x)length(x[is.na(x)==TRUE])))
    }
    
    daten2$Cluster=cl$cluster
    daten2$size[1:length(unique(cl$cluster))]=as.numeric(
      as.character(table(cl$cluster)))
    sil=silhouette(as.numeric(cl$cluster),dist(daten_values,method=distance))  
    daten2$sil = sil[,3] 
    sil_avg=summary(sil)
    d=dist(daten_values,method=distance)
    as=cluster.stats(d,cl$cluster)
  }# cmeans
  
  if (method=="pam"){
    
    if(adist_used==TRUE ){
      if(length(nonnum)>0){
        d=daisy(na.omit(select(daten_values,setdiff(names(daten_values),
                                                    names(adist_data)))),
                metric = distance)
      }#lenght(nonum>0)
      if(length(nonnum)==0){
        d=factoextra::get_dist(na.omit(select(daten_values,setdiff(names(daten_values),
                                                                   names(adist_data)))),
                               method = distance)
      }
      d_all=d*weight_count_fact+d_all_comp
      d=d_all
    }
    
    else if(length(nonnum)>0){
      d=daisy(na.omit(daten_values),metric = distance)
    }
    else if(length(nonnum)==0){
      d=factoextra::get_dist(na.omit(daten_values),method = distance)
    }
    pam <- pam(d, diss = TRUE, k = amount_k)  
    pam$data=daten_values
    method_out=pam
    cl=pam
    if(length(cl$cluster)!=length(daten2$id)){
      print("The listed variables contain missing values resulting in an error. 
            Recheck data set for NA entries.")
      print(apply(daten_values,2,function(x)length(x[is.na(x)==TRUE])))
    }
    daten2$Cluster=cl$clustering
    daten2$size[1:length(unique(cl$clustering))]=as.numeric(
      as.character(table(cl$clustering)))
    sil=silhouette(as.numeric(cl$clustering),d)    
    daten2$sil = sil[,3] 
    sil_avg=summary(sil)
    as=cluster.stats(d,cl$cluster)
  }#pam
  
  if (method=="clara"){
    
    cl=clara(daten_values,k=amount_k,metric = distance)#euc ,man, jaccar
    method_out=cl
    if(length(cl$cluster)!=length(daten2$id)){
      print("The listed variables contain missing values resulting in an error. 
            Recheck data set for NA entries.")
      print(apply(daten_values,2,function(x)length(x[is.na(x)==TRUE])))
    }
    
    daten2$Cluster=cl$clustering
    
    daten2$size[1:length(unique(cl$clustering))]=as.numeric(
      as.character(table(cl$clustering)))
    sil=silhouette(as.numeric(cl$clustering),dist(daten_values,method = distance))    
    daten2$sil = sil[,3] 
    sil_avg=summary(sil)
    d=dist(daten_values,method=distance)
    as=cluster.stats(d,cl$cluster)
    
  }#clara
  
  
  if (method=="kmeans"){
    cl=kmeans(na.omit(daten_values),centers=amount_k)
    method_out=cl
    
    if(length(cl$cluster)!=length(daten2$id)){
      print("The listed variables contain missing values resulting in an error. 
            Recheck data set for NA entries.")
      print(apply(daten_values,2,function(x)length(x[is.na(x)==TRUE])))
    }
    
    daten2$Cluster= cl$cluster
    daten2$size[1:length(unique(cl$cluster))]=cl$size
    sil=silhouette(as.numeric(cl$cluster),dist(daten_values,method = distance))    
    daten2$sil = sil[,3] 
    sil_avg=summary(sil)
    d=dist(daten_values,method=distance)
    as=cluster.stats(d,cl$cluster)
    
  }#kmeans
  
  
  if (method=="Mclust"){
    daten2$BIC=rep(NA,length(lon))
    daten2$model=rep(NA,length(lon))
    cl=Mclust(na.omit(daten_values),amount_k) 
    method_out=cl
    
    if(length(cl$classification)!=length(daten2$id)){
      # dataset should also work with na because rows with na would be deleted
      print("The listed variables contain missing values resulting in an error. 
            Recheck data set for NA entries.")
      print(apply(daten_values,2,function(x)length(x[is.na(x)==TRUE])))
    }
    #cluster belonging
    daten2$Cluster <- cl$classification  
    #cluster size
    daten2$size[1:length(unique(cl$classification))] <- table(cl$classification) 
    #clust$bic <- a$bic
    daten2$BIC <- cl$bic # ein wert
    daten2$model <- cl$modelName
    sil=silhouette(cl$classification,dist(daten_values,method=distance))
    daten2$sil=sil[,3]
    sil_avg=summary(sil)
    d=dist(daten_values,method=distance)
    as=cluster.stats(d,cl$classification)
  }#Mclust
  
  # Ueberprufen sind keine numerische daten ausgeaehlt
  if (method=="hclust"){
    if(adist_used==TRUE){
      if(length(nonnum)>0){
        d=daisy(na.omit(select(daten_values,setdiff(names(daten_values),
                                                    names(adist_data)))),
                metric = distance)
      }
      if(length(nonnum)==0){
        d=factoextra::get_dist(na.omit(select(daten_values,setdiff(names(daten_values),
                                                                   names(adist_data)))),
                               method = distance)
      }
      d_all=d*weight_count_fact+d_all_comp
      d=d_all
    }
    else if(length(nonnum)>0){
      d=daisy(na.omit(daten_values),metric = distance)
    }
    else if(length(nonnum)==0){
      d=factoextra::get_dist(na.omit(daten_values),method = distance)
    }
    #cl=cutree(hclust(d,method = agglomeration_method),k=amount_k)
    
    dv2=hcut(d,isdiss = TRUE,k=amount_k,hc_func="hclust",hc_method=agglomeration_method,
             hc_metric = distance)
    dv2$data=daten_values
    method_out=dv2
    cl=dv2$cluster
    #macht genau das selbe eventuell effizienter
    #hc.cut <- hcut(d, hc_func = "hclust", k = amount_k,isdiss = inherits(d,"dist"))
    sil=silhouette(cl,d)
    
    if(length(cl)!=length(daten2$id)){
      print("The listed variables contain missing values resulting in an error. 
            Recheck data set for NA entries.")
      print(apply(daten_values,2,function(x)length(x[is.na(x)==TRUE])))
    }
    
    daten2$sil=sil[,3]
    sil_avg=summary(sil)
    #plot cl
    #rect.hclust(a,k=3,border = "red")
    as=cluster.stats(d,cl)
    daten2$size[1:amount_k]=as$cluster.size
    daten2$Cluster=cl
  } #hclust
  
  if (method=="diana"){
    if(adist_used==TRUE ){
      if(length(nonnum)>0){
        d=daisy(na.omit(select(daten_values,setdiff(names(daten_values),
                                                    names(adist_data)))),
                metric = distance)
      }
      if(length(nonnum)==0){
        d=factoextra::get_dist(na.omit(select(daten_values,setdiff(names(daten_values),
                                                                   names(adist_data)))),
                               method = distance)
      }
      d_all=d*weight_count_fact+d_all_comp
      d=d_all
    }
    
    else if(length(nonnum)>0){
      d=daisy(na.omit(daten_values),metric = distance)
    }
    else if(length(nonnum)==0){
      d=factoextra::get_dist(na.omit(daten_values),method = distance)
    }
    
    # dv=diana(x=d, diss=TRUE,stand=FALSE,metric =distance )
    # dv2=cutree(dv,k=amount_k)
    dv2=hcut(d,isdiss = TRUE,k=amount_k,hc_func="diana",
             hc_metric = distance)
    dv2$data=daten_values
    method_out=dv2
    
    cl=dv2$cluster
    sil=silhouette(cl,d) 
    
    if(length(cl)!=length(daten2$id)){
      print("The listed variables contain missing values resulting in an error. 
            Recheck data set for NA entries.")
      print(apply(daten_values,2,function(x)length(x[is.na(x)==TRUE])))
    }
    daten2$sil=sil[,3]
    sil_avg=summary(sil)
    #plot cl
    #rect.hclust(a,k=3,border = "red")
    as=cluster.stats(d,cl)
    daten2$size[1:amount_k]=as$cluster.size
    daten2$Cluster=cl
  }#diana
  
  if (method=="agnes"){
    if(adist_used==TRUE ){
      if(length(nonnum)>0){
        d=daisy(na.omit(select(daten_values,setdiff(names(daten_values),
                                                    names(adist_data)))),
                metric = distance)
      }
      if(length(nonnum)==0){
        d=factoextra::get_dist(na.omit(select(daten_values,setdiff(names(daten_values),
                                                                   names(adist_data)))),
                               method = distance)
      }
      d_all=d*weight_count_fact+d_all_comp
      d=d_all
    }
    else if(length(nonnum)>0){
      d=daisy(na.omit(daten_values),metric = distance)
    }
    else if(length(nonnum)==0){
      d=factoextra::get_dist(na.omit(daten_values),method = distance)
    }
    # dv=agnes(x=d, diss=TRUE,stand=FALSE, method = agglomeration_method)
    # dv2=cutree(dv,k=amount_k) #wie Beispiel help(diana)
    dv2=hcut(d,isdiss = TRUE,k=amount_k,hc_func="agnes",hc_method=agglomeration_method,
             hc_metric = distance)
    dv2$data=daten_values
    method_out=dv2
    cl=dv2$cluster
    sil=silhouette(cl,d) 
    
    # Wenn na vorhanden sind in den ausgesuchten Values, sind cl$cluster 
    # und der Datensatz2 ungleich in ihrer groesse.
    # wenn dann die Cluster classificationen dem Datensatz 2 hinzugefuegt werden, 
    # entsteht ein error.Man koennte das ganze so gestallten,
    # dass man die Variablen mit Na nicht zur Auswahl hat 
    # oder wenn der USer Variablen brauchen moechte mit Na 
    # muessten die Zeilen mit Na geloescht werden.
    
    if(length(cl)!=length(daten2$id)){
      print("The listed variables contain missing values resulting in an error. 
            Recheck data set for NA entries.")
      print(apply(daten_values,2,function(x)length(x[is.na(x)==TRUE])))
    }
    daten2$sil=sil[,3]
    sil_avg=summary(sil)
    #plot cl
    #rect.hclust(a,k=3,border = "red")
    as=cluster.stats(d,cl)
    daten2$size[1:amount_k]=as$cluster.size
    daten2$Cluster=cl
  }#agnes
  
  #zuweisung zu jedem cluster den avg.sil.widts
  for (i in 1:length(unique(daten2$Cluster))){
    daten2$Average_Silhouette_Width[which(daten2$Cluster==i)]=round(
      as.numeric(as.character(sil_avg$clus.avg.widths)),3)[i]}
  
  #Guetemasse Global dataframe
  mati=matrix(c(
    round(as$avg.silwidth,digits = 3),
    round(as$average.between,digits = 3),
    round(as$average.within,digits = 3),
    round(as$max.diameter,digits = 3),
    round(as$pearsongamma,digits = 3),
    round(as$dunn,digits = 3),
    round(as$dunn2,digits=3),
    round(as$entropy,digits = 3),
    round(as$wb.ratio,digits=3),
    round(as$widestgap,digits = 3))
    ,nrow=10)
  
  colnames(mati)="Global_Validity_Measures"
  rownames(mati)= c("Average_Silwidth","Average_Between","Average_Within","Max_Diameter",
                    "Pearsongamma","Dunn","Dunn2","Entropy",
                    "Wb_Ratio","Widestap")
  guete_global=as.data.frame(mati)
  
  #Guetemasse Lokal Dataframe
  mati2=matrix(c(
    round(as.numeric(as.character(sil_avg$clus.avg.widths)),3),
    round(as$average.distance,digits = 3),
    round(as$median.distance,digits = 3),
    round(as$diameter,digits = 3),
    round(as$cluster.size,digits = 3),
    round(as$separation,digits = 3),
    round(as$average.toother,digits = 3),
    round(as$cwidegap,digits = 3)
  ),
  nrow=8,
  byrow=TRUE)
  
  colnames(mati2)=paste("Cluster",1:as$cluster.number,sep="_")
  rownames(mati2)=c("Average_Silhouette_Width","Average_Distance","Median_Distance",
                    "Diameter","cluster_Size",
                    "separation","Average_Toother","Cwidegap")
  guete_lokal=as.data.frame(mati2)
  data_validity<-matrix(rep(NA,11*(length(unique(daten2$Cluster))+1+2)),nrow=11)
  data_validity[2:9,1]=row.names(guete_lokal)
  data_validity[1,1]="validity measures local"
  for (i in 1:length(unique(daten2$Cluster))){
    data_validity[1,i+1]=paste("cluster",i,sep = "-")
    data_validity[2:9,i+1]=guete_lokal[,i]
  }
  
  data_validity[1,length(unique(daten2$Cluster))+1+1+1]="Validity Measure Global"
  data_validity[1,length(unique(daten2$Cluster))+1+1]="Measures"
  data_validity[2:11,length(unique(daten2$Cluster))+1+1] =row.names(guete_global)
  data_validity[2:11,(length(unique(daten2$Cluster))+1+1+1)]=guete_global[,1]
  col_names<-data_validity[1,]
  data_validity=data_validity[-1,]
  data_validity=as.data.frame(data_validity)
  colnames(data_validity)<-col_names
  
  d_mat=d
  liste=list(daten2,cl,sil,guete_global,guete_lokal,length_variable,data_validity,
             d_mat,method_out)
  return(liste)
  }