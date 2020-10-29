library(shiny)

server <- function(input, output,session){ 
  
 
  
  
  
  
  observeEvent(input$selectvariableselecttrans,{
    if(input$check_transformation=="Prepare Compositional Data"){
      if (length(input$selectvariableselecttrans)>1){
        enable("buttontrans")}
      else{
        disable("buttontrans")
      }
    }
    if(input$check_transformation=="Prepare Non-Compositional Data"){
    if (length(input$selectvariableselecttrans)==0)
  disable("buttontrans")
    else{
      enable("buttontrans")
    }}
  },ignoreNULL=FALSE)
  
  
  
  observeEvent(input$sidebarmenu,{
    shinyjs::runjs("window.scrollTo(0,0)")
  })
  
  observe(if(is.null(dat_preview$dat)){
    Sys.sleep(1)
    output$trigger <- renderUI({
      tags$script("$('#button').popover('toggle');")
    })
  })
  
  observe(if(!is.null(dat_preview$dat)){
    removePopover(session, "button")
    enable("button")
  })
  
  output$impdata=renderTable({req(input$file1)
    if(input$choosedatatyp=="From CSV")
    {
      dat <- read.csv(input$file1$datapath,
                      header = input$header,
                      sep = input$sep,
                      quote = input$quote)
      dat_preview$dat=dat}
    if(input$choosedatatyp=="From Excel"){
      dat <- read_xlsx(input$file1$datapath)
      dat_preview$dat=dat
    }
    if (input$choosedatatyp=="Global Environment" ){
      dat=get(input$globalvar)
      dat_preview$dat=dat
    }
    if(input$disp == "head") {
      return(head(dat))
    }
    else {
      return(dat)
    }
  })
  
  #F?r Globales Abspeichern Datensatz prepared
  observeEvent(input$save_global,{
    assign( input$globalsave, daten2$dat,envir = .GlobalEnv)
  })
  
  #F?r Globales Abspeichern validity measures
  observeEvent(input$save_global_validity,{
    # daten$gueteglob<-guete$guete_lokal
    # daten$guetelok<-guete$guete_global
    # print(daten)
    assign( input$globalsave_validity, 
            daten_val$dat,
            envir = .GlobalEnv)
  })
  
  
  validitymeasures=reactiveValues()
  amount_k_validity=reactiveValues()
  observeEvent(input$append_list,{
    input.name=input$reactive_validity #input name
    validitymeasures[[input.name]]=daten_val$dat
    amount_k_validity[[input.name]]=input$num
    updateSelectInput(session,"Secondary_validity",
                      "Select from previously saved results",
                      choices=names(validitymeasures))
  })
  
  # Downloadable csv of selected dataset prepared
  output$downloadData=downloadHandler(
    filename = function() {
      paste(input$globalsave_csv, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(daten2$dat, file, row.names = FALSE)
    }
  )
  
  # Downloadable xlsx of selected dataset prepared
  output$downloadData2=downloadHandler(
    filename = function() {
      paste(input$globalsave_xlsx, ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(daten2$dat, file)
    }
  )
  
  # Downloadable csv of selected dataset validity
  output$downloadData_validity=downloadHandler(
    # daten_val$gueteglob<-guete$guete_lokal,
    # daten_val$guetelok<-guete$guete_global,
    filename = function() {
      paste(input$globalsave_csv_validity, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(daten_val$dat, file, row.names = FALSE)
    }
  )
  
  # Downloadable xlsx of selected dataset validity
  output$downloadData2_validity=downloadHandler(
    # daten_val$gueteglob<-guete$guete_lokal,
    # daten_val$guetelok<-guete$guete_global,
    filename = function() {
      paste(input$globalsave_xlsx_validity, ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(daten_val$dat, file)
    }
  )
  
  # erstellen von reaktivevalues
  dat_preview=reactiveValues(dat=NULL)
  dataset_with_na=reactiveValues() #to give a message which attribute contains na
  used_variables=reactiveValues()
  daten=reactiveValues(dat=NULL) #original data
  daten2=reactiveValues(dat=0) #results 
  daten_val=reactiveValues(dat=0)
  cl=reactiveValues(cl=0) #cluster values
  sil=reactiveValues(sil=0) # sil values
  guete=reactiveValues(guete_global=0) #validity measures global
  guete=reactiveValues(guete_lokal=0) #validity measures local
  zero_list=reactiveValues(zerolist=0) # values containig 0
  non_numeric_list=reactiveValues(nonnumericlist=0) #nonnummeric
  length_variable=reactiveValues(dat2=0) 
  nblist=reactiveValues(list=0) # return from function nb
  names_transformed_attribute=reactiveValues(list="")#names of allready transformed data
  ilr_names=reactiveValues(name=NULL)
  update_num_reaktiv=reactiveValues(num=0) # best number of k (k values)
  comp=reactiveValues(list=0) # copdata
  not_negative_values=reactiveValues() #values which are not negative
  eliminated_dimension=reactiveValues(list="") #if used iso transformation
  dim_count=reactiveValues(count=0)
  distance_all_comp=reactiveValues() # distance of the comp data if used adist
  adist_used=reactiveValues(list=FALSE) #if adist is used or not
  weight_count_fact=reactiveValues(values=0) #weight non compdata if adist used
  adist_names=reactiveValues() #names adist variables
  num1=reactiveValues() 
  num2=reactiveValues()
  num3=reactiveValues()
  num4=reactiveValues()
  d_matrix=reactiveValues() #distance matrix 
  method_out=reactiveValues() #ouput from method zb kmeans or mclust...
  plot_mean=reactiveValues()
  plot=reactiveValues(plot=NULL)
  
  # Information Icon in the header 
  observeEvent(input$openModal, {
    showModal(
      modalDialog(title = "Vignette",
                  p("ClustToolApp was developed and introduced as a Bachelor Thesis at the
                    Zurich University of Applied Sciences by Kerim Alqadi and Kevin Steffen,
                    with special thanks to Dr. Matthias Templ in providing assistance and being part of this contribution."),
                  includeHTML("documentation/vignette/DESCRIPTION.html")
                  , size = "l", easyClose = TRUE
      )#modaldialog
    )#showmodal
  })#observeevent
  
  #reaktive value not numeric
  observeEvent(input$selectize,{
    non_numeric_list$nonnumericlist=names(which(sapply(select(daten$dat,list.append(
      input$selectize,input$selectize_comp)),
      is.numeric)==FALSE))
  })#observeEvent inputselectize
  
  observeEvent(input$check_composition,{
    if (input$check_composition=="No"){
      #update variable selectizeinput
      #i have no compositional variable are in the beginning 
      #selected before we have insertet a dataset
      #so we first have to check if a dataset exists. after that it will be 
      #updated somwhere else until 
      #the user change the checkbox and goes back to i have no compositional data
      updateSelectizeInput(session, 'selectize',
                           label="Variables Of Non-Compositional Data:",
                           choices =if(!(is.null(daten$dat))){names(daten$dat)}
                           else{list()}, server = TRUE)}#if input$check_composition no
    if (input$check_composition=="Yes"){
      #update variable selectizeinput change header
      updateSelectizeInput(session, 'selectize',
                           label="Variables Of Non-Compositional Data:",
                           choices = names(daten$dat), server = TRUE)
      updateSelectizeInput(session, 'selectize_comp',
                           label="Variables Of Compositional Data (Select Multiple):",
                           choices =names(daten$dat), server = TRUE)
      comp$list=input$selectize_comp
    }#input$check_composition==Yes
  })#observeevent check_composition
  
  observeEvent({input$selectize},
               if (input$check_composition=="Yes"){
                 updateSelectizeInput(session, 'selectize_comp',
                                      label="Variables Of Compositional Data (Select Multiple)",
                                      choices =names(daten$dat)[!names(daten$dat) %in% 
                                                                  input$selectize],
                                      selected =input$selectize_comp )
               },ignoreNULL=FALSE) #observeEvent
  observeEvent({input$selectize_comp} ,{ 
    
    if (input$check_composition=="Yes"){
      updateSelectizeInput(session, 'selectize',
                           label="Variables Of Non-Compositional Data:",
                           choices = names(daten$dat)[!names(daten$dat) %in% 
                                                        input$selectize_comp],
                           selected = input$selectize)
      comp$list=input$selectize_comp
    } },ignoreNULL=FALSE
  )
  
  observeEvent(input$check_box_adist,{
    updateSelectizeInput(session,"selectvariable1_num1",label="Composition 1",
                         choices=input$selectize_comp,server = TRUE)
  })#observeevent input$check_box_adist
  
  observeEvent(input$num_comp,{
      if(is.numeric(input$num_comp)){
        if(input$num_comp<1 || input$num_comp>4){
          updateNumericInput(session,"num_comp",label = "Number Of Compositions",
                             value = 1,min=1)}
        else{
       
    
    if(input$num_comp==1){
      num1$value=TRUE
      num2$value=FALSE
      num3$value=FALSE
      num4$value=FALSE}
    if(input$num_comp==2){
      num1$value=TRUE
      num2$value=TRUE
      num3$value=FALSE
      num4$value=FALSE}
    if(input$num_comp==3){
      num1$value=TRUE
      num2$value=TRUE
      num3$value=TRUE
      num4$value=FALSE}
    if(input$num_comp==4){
      num1$value=TRUE
      num2$value=TRUE
      num3$value=TRUE
      num4$value=TRUE}}}
    
    else{updateNumericInput(session,"num_comp",label = "Number Of Compositions",
                            value = 1,min=1)}
  })
  
  # For each variaten of complist 1:4 we gave them diffrent names to avoid errors
  observeEvent(input$num_comp,{
  
    updateSelectizeInput(session,"selectvariable1_num1",
                         label="Composition 1",choices=input$selectize_comp)
    updateSelectizeInput(session,"selectvariable1_num2",
                         label="Composition 1",choices=input$selectize_comp)
    updateSelectizeInput(session,"selectvariable2_num2",
                         label="Composition 2",choices=input$selectize_comp)
    updateSelectizeInput(session,"selectvariable1_num3",
                         label="Composition 1",choices=input$selectize_comp)
    updateSelectizeInput(session,"selectvariable2_num3",
                         label="Composition 2",choices=input$selectize_comp)
    updateSelectizeInput(session,"selectvariable3_num3",
                         label="Composition 3",choices=input$selectize_comp)
    updateSelectizeInput(session,"selectvariable1_num4",
                         label="Composition 1",choices=input$selectize_comp)
    updateSelectizeInput(session,"selectvariable2_num4",
                         label="Composition 2",choices=input$selectize_comp)
    updateSelectizeInput(session,"selectvariable3_num4",
                         label="Composition 3",choices=input$selectize_comp)
    updateSelectizeInput(session,"selectvariable4_num4",
                         label="Composition 4",choices=input$selectize_comp)
  })
  
  observeEvent(
    input$button_adist_reset,{
      updateSelectizeInput(session,"selectvariable1_num1",
                           label="Composition 1",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable1_num2",
                           label="Composition 1",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable2_num2",
                           label="Composition 2",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable1_num3",
                           label="Composition 1",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable2_num3",
                           label="Composition 2",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable3_num3",
                           label="Composition 3",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable1_num4",
                           label="Composition 1",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable2_num4",
                           label="Composition 2",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable3_num4",
                           label="Composition 3",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable4_num4",
                           label="Composition 4",choices=input$selectize_comp)
    })
  
  observeEvent(
    input$buttondatareset,{
      updateSelectizeInput(session,"selectvariable1_num1",
                           label="Composition 1",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable1_num2",
                           label="Composition 1",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable2_num2",
                           label="Composition 2",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable1_num3",
                           label="Composition 1",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable2_num3",
                           label="Composition 2",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable3_num3",
                           label="Composition 3",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable1_num4",
                           label="Composition 1",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable2_num4",
                           label="Composition 2",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable3_num4",
                           label="Composition 3",choices=input$selectize_comp)
      updateSelectizeInput(session,"selectvariable4_num4",
                           label="Composition 4",choices=input$selectize_comp)
    })
  
  observeEvent(input$check_box_adist,{
    disable("button_adist")
  })
  
  observeEvent({input$selectvariable1_num1},{
    if (length(input$selectvariable1_num1)==0)
      disable("button_adist")
    else{
      enable("button_adist")
    }
  },ignoreNULL=FALSE) 
  
  observeEvent(input$selectvariable1_num2,{
    if (length(input$selectvariable1_num2)==0)
      disable("button_adist")
    else{
      enable("button_adist")
    }
  },ignoreNULL=FALSE) 
  
  observeEvent(input$selectvariable2_num2,ignoreInit = TRUE,{
    if (length(input$selectvariable2_num2)==0)
      disable("button_adist")
    else{enable("button_adist")}
  },ignoreNULL=FALSE) 
  
  observeEvent(input$selectvariable1_num3,{
    if (length(input$selectvariable1_num3)==0)
      disable("button_adist")
    else{enable("button_adist")}
  },ignoreNULL=FALSE) 
  
  observeEvent(input$selectvariable2_num3,{
    if (length(input$selectvariable2_num3)==0)
      disable("button_adist")
    else{enable("button_adist")}
  },ignoreNULL=FALSE)   
  
  observeEvent(input$selectvariable3_num3,{
    if (length(input$selectvariable3_num3)==0)
      disable("button_adist")
    else{enable("button_adist")}
  },ignoreNULL=FALSE)  
  
  observeEvent(input$selectvariable1_num4,{
    if (length(input$selectvariable1_num4)==0)
      disable("button_adist")
    else{enable("button_adist")}
  },ignoreNULL=FALSE)  
  
  observeEvent(input$selectvariable2_num4,{
    if (length(input$selectvariable2_num4)==0)
      disable("button_adist")
    else{enable("button_adist")}
  },ignoreNULL=FALSE) 
  
  observeEvent(input$selectvariable3_num4,{
    if (length(input$selectvariable3_num4)==0)
      disable("button_adist")
    else{enable("button_adist")}
  },ignoreNULL=FALSE) 
  
  observeEvent(input$selectvariable4_num4,{
    if (length(input$selectvariable4_num4)==0)
      disable("button_adist")
    else{enable("button_adist")}
  },ignoreNULL=FALSE) 
  
  #with 4 complists
  observeEvent({input$selectvariable2_num4
    input$selectvariable3_num4
    input$selectvariable4_num4},{
      
      updateSelectizeInput(session, 'selectvariable1_num4',
                           label="Choose Variables:",
                           choices =input$selectize_comp[!input$selectize_comp %in%
                                                           c(input$selectvariable2_num4,
                                                             input$selectvariable3_num4,
                                                             input$selectvariable4_num4)],
                           selected = input$selectvariable1_num4)
    },ignoreNULL=FALSE)
  observeEvent({input$selectvariable1_num4
    input$selectvariable3_num4
    input$selectvariable4_num4},{
      
      updateSelectizeInput(session, 'selectvariable2_num4',
                           label="Choose Variables:",
                           choices =input$selectize_comp[!input$selectize_comp %in% 
                                                           c(input$selectvariable1_num4,
                                                             input$selectvariable3_num4,
                                                             input$selectvariable4_num4)],
                           selected = input$selectvariable2_num4)
    },ignoreNULL=FALSE)
  observeEvent({input$selectvariable1_num4
    input$selectvariable2_num4
    input$selectvariable4_num4},{
      
      updateSelectizeInput(session, 'selectvariable3_num4',
                           label="Choose Variables:",
                           choices =input$selectize_comp[!input$selectize_comp %in%
                                                           c(input$selectvariable1_num4,
                                                             input$selectvariable2_num4,
                                                             input$selectvariable4_num4)],
                           selected = input$selectvariable3_num4)
    },ignoreNULL=FALSE)
  observeEvent({input$selectvariable1_num4
    input$selectvariable2_num4
    input$selectvariable3_num4},{
      
      updateSelectizeInput(session, 'selectvariable4_num4',
                           label="Choose Variables:",
                           choices =input$selectize_comp[!input$selectize_comp %in% 
                                                           c(input$selectvariable1_num4,
                                                             input$selectvariable2_num4,
                                                             input$selectvariable3_num4)],
                           selected = input$selectvariable4_num4)
    },ignoreNULL=FALSE)
  #with thre comp_lists 
  observeEvent({input$selectvariable2_num3
    input$selectvariable3_num3},{
      updateSelectizeInput(session, 'selectvariable1_num3',label="Choose Variables:",
                           choices =input$selectize_comp[!input$selectize_comp %in% 
                                                           c(input$selectvariable2_num3,
                                                             input$selectvariable3_num3)],
                           selected = input$selectvariable1_num3)
    },ignoreNULL = FALSE)
  observeEvent({input$selectvariable3_num3
    input$selectvariable1_num3},{
      updateSelectizeInput(session, 'selectvariable2_num3',label="Choose Variables:",
                           choices =input$selectize_comp[!input$selectize_comp %in% 
                                                           c(input$selectvariable1_num3,
                                                             input$selectvariable3_num3)],
                           selected = input$selectvariable2_num3)
    },ignoreNULL=FALSE )
  observeEvent({input$selectvariable2_num3
    input$selectvariable1_num3},{
      updateSelectizeInput(session, 'selectvariable3_num3',label="Choose Variables:",
                           choices =input$selectize_comp[!input$selectize_comp %in% 
                                                           c(input$selectvariable1_num3,
                                                             input$selectvariable2_num3)],
                           selected = input$selectvariable3_num3)
    },ignoreNULL=FALSE)
  #with two complists
  observeEvent({input$selectvariable2_num2},{
    
    updateSelectizeInput(session, 'selectvariable1_num2',label="Choose Variables:",
                         choices =input$selectize_comp[!input$selectize_comp %in% 
                                                         input$selectvariable2_num2],
                         selected = input$selectvariable1_num2)
  },ignoreNULL=FALSE)
  observeEvent({input$selectvariable1_num2},{
    
    updateSelectizeInput(session, 'selectvariable2_num2',label="Choose Variables:",
                         choices =input$selectize_comp[!input$selectize_comp %in% 
                                                         input$selectvariable1_num2],
                         selected = input$selectvariable2_num2)
  },ignoreNULL=FALSE)
  
  
  
  observeEvent(input$button_adist,{

    disable("check_box_adist")
    disable("button_adist")
    adist_used$list=TRUE
    
    updateSelectizeInput(session,"selectvariableselecttrans",
                         label="Variable Selection For Transformation And Scaling",
                         choices=  
                           # if checkbox active and then if they are transformed or not 
                           if(input$check_transformation=="Prepare Compositional Data" & 
                              adist_used$list==FALSE)
                           {setdiff(comp$list,names_transformed_attribute$list)}
                         else if(input$check_transformation=="Prepare Compositional Data" &
                                 adist_used$list==TRUE)
                         {""}
                         #{setdiff(comp$list,names_transformed_attribute$list)} 
                         #with nonumeric values
                         else if(length(non_numeric_list$nonnumericlist)!=0)
                         {setdiff(setdiff(input$selectize,non_numeric_list$nonnumericlist),
                                  names_transformed_attribute$list)}
                         else{
                           setdiff(input$selectize,names_transformed_attribute$list)
                         }
    )
    
    
    #change choosen Values 0 with 0.1. 
    newdat=replace(select(daten2$dat,input$selectize_comp),
                   select(daten2$dat,input$selectize_comp)==0,0.1)
    #ueberschreiben der neuen 0 durch 0.1 Variablen
    for (i in input$selectize_comp){
      daten2$dat[i]=newdat[which(i==names(newdat))]
    }
    
    summe=input$controller_1_num2+ifelse(num2$value,input$controller_2_num2,0)+
      ifelse(num3$value,input$controller_3_num2,0)+
      ifelse(num4$value,input$controller_4_num2,0)+
      input$controller_count_factors
    
    
    weight1=input$controller_1_num2/summe
    weight2=ifelse(num2$value,input$controller_2_num2,0)/summe
    weight3=ifelse(num3$value,input$controller_3_num2,0)/summe
    weight4=ifelse(num4$value,input$controller_4_num2,0)/summe
    weight_count_fact$value=input$controller_count_factors/summe
    
    if(input$num_comp==4){
      d1=aDist(select(daten2$dat,input$selectvariable1_num4))
      d2=aDist(select(daten2$dat,input$selectvariable2_num4))
      d3=aDist(select(daten2$dat,input$selectvariable3_num4))
      d4=aDist(select(daten2$dat,input$selectvariable4_num4))
      d_all_comp=d1*weight1+d2*weight2+d3*weight3+d4*weight4
      adist_names$list=c(input$selectvariable1_num4,input$selectvariable2_num4,
                         input$selectvariable3_num4,input$selectvariable4_num4)
    }
    if(input$num_comp==3){
      d1=aDist(select(daten2$dat,input$selectvariable1_num3))
      d2=aDist(select(daten2$dat,input$selectvariable2_num3))
      d3=aDist(select(daten2$dat,input$selectvariable3_num3))
      d_all_comp=d1*weight1+d2*weight2+d3*weight3
      adist_names$list=c(input$selectvariable1_num3,input$selectvariable2_num3,
                         input$selectvariable3_num3)
    }
    if(input$num_comp==2){
      d1=aDist(select(daten2$dat,input$selectvariable1_num2))
      d2=aDist(select(daten2$dat,input$selectvariable2_num2))
      d_all_comp=d1*weight1+d2*weight2
      adist_names$list=c(input$selectvariable1_num2,input$selectvariable2_num2)
    }
    if(input$num_comp==1){
      d1=aDist(select(daten2$dat,input$selectvariable1_num1))
      d_all_comp=d1*weight1
      adist_names$list=input$selectvariable1_num1
    }
    distance_all_comp$list=d_all_comp
    a=datensatz2(select(daten$dat,input$lon),select(daten$dat,input$lat),
                 select(daten$dat,input$id), select(daten2$dat,setdiff(list.append(
                   input$selectize,input$selectize_comp,ilr_names$name),
                   eliminated_dimension$list)),
                 input$methode, input$distance,input$num, 
                 non_numeric_list$nonnumericlist,input$agglomeration_method,
                 distance_all_comp$list,adist_used$list,weight_count_fact$value,
                 select(daten2$dat,adist_names$list))
    
    daten2$dat=a[[1]]
    cl$cl=a[[2]]
    sil$sil=a[[3]]
    guete$guete_global=a[[4]]
    guete$guete_lokal=a[[5]]
    length_variable$dat2=a[[6]]
    daten_val$dat=a[[7]] #dataframe validity measures
    d_matrix$mat=a[[8]]
    method_out$method=a[[9]]
  })
  
  observeEvent(input$num,{
    if(is.numeric(input$num)){
    if(input$num<2){
      updateNumericInput(session,"num",label = "Number Of Cluster",
                         value = 2,min=2)
    }}
    else{updateNumericInput(session,"num",label = "Number Of Cluster",
                            value = 2,min=2)}
    })
  

  
  observeEvent(input$lonlatok,{
    # Wenn zum erstenmal eingelesen wird muss der Datensatz2,
    # mit den ausgewaehlten Variablen vom Datensatz1 erstellt werden.
    if (length(daten2$dat)==1){
      used_variables$list=list.append(input$selectize,input$selectize_comp)
      a=datensatz2(select(daten$dat,input$lon),select(daten$dat,input$lat),
                   select(daten$dat,input$id),select(daten$dat,list.append(
                     input$selectize,input$selectize_comp)),input$methode, 
                   input$distance,input$num,non_numeric_list$nonnumericlist,
                   input$agglomeration_method
      )#function datensatz2
      daten2$dat=a[[1]]
      cl$cl=a[[2]]
      sil$sil=a[[3]]
      guete$guete_global=a[[4]]
      guete$guete_lokal=a[[5]]
      length_variable$dat2=a[[6]]
      daten_val$dat=a[[7]] #dataframe validity measures
      d_matrix$mat=a[[8]]
      method_out$method=a[[9]]
    }#if not exist daten2$dat
    
    else if (length(daten2$dat)>1){

      if (any((sort(list.append(input$selectize,input$selectize_comp))==sort(
        used_variables$list))==FALSE) || length(
          list.append(input$selectize,input$selectize_comp))!=
        length(used_variables$list) || adist_used$list==TRUE & 
        !(input$methode %in% c("hclust","diana","pam","agnes" ))
      )
      { 
        used_variables$list=list.append(input$selectize,input$selectize_comp)
        ilr_names$name=""
        eliminated_dimension$list=""
        names_transformed_attribute$list=""
        adist_names$list=""
        adist_used$list=FALSE
        #if variables are deleted in the sidebar we have to update them in the 
        #selecticecom. If we are not update them values are usable which are not 
        #valid
        updateSelectizeInput(session,"selectvariable1_num1",
                             label="Composition 1",choices=input$selectize_comp)
        updateSelectizeInput(session,"selectvariable1_num2",
                             label="Composition 1",choices=input$selectize_comp)
        updateSelectizeInput(session,"selectvariable2_num2",
                             label="Composition 2",choices=input$selectize_comp)
        updateSelectizeInput(session,"selectvariable1_num3",
                             label="Composition 1",choices=input$selectize_comp)
        updateSelectizeInput(session,"selectvariable2_num3",
                             label="Composition 2",choices=input$selectize_comp)
        updateSelectizeInput(session,"selectvariable3_num3",
                             label="Composition 3",choices=input$selectize_comp)
        updateSelectizeInput(session,"selectvariable1_num4",
                             label="Composition 1",choices=input$selectize_comp)
        updateSelectizeInput(session,"selectvariable2_num4",
                             label="Composition 2",choices=input$selectize_comp)
        updateSelectizeInput(session,"selectvariable3_num4",
                             label="Composition 3",choices=input$selectize_comp)
        updateSelectizeInput(session,"selectvariable4_num4",
                             label="Composition 4",choices=input$selectize_comp)
        
        updateCheckboxInput(session,inputId = "check_box_adist",
                            label="Apply Aitchison distance measurement
                            to compositional data?",value = FALSE)
        enable("check_box_adist")
        enable("button_adist")
        a=datensatz2(select(daten$dat,input$lon),select(daten$dat,input$lat),
                     select(daten$dat,input$id), 
                     select(daten$dat,list.append(
                       input$selectize,input$selectize_comp)),
                     input$methode, input$distance,input$num, 
                     non_numeric_list$nonnumericlist,input$agglomeration_method)
        daten2$dat=a[[1]]
        cl$cl=a[[2]]
        sil$sil=a[[3]]
        guete$guete_global=a[[4]]
        guete$guete_lokal=a[[5]]      
        length_variable$dat2=a[[6]]
        daten_val$dat=a[[7]] #dataframe validity measures
        d_matrix$mat=a[[8]]
        method_out$method=a[[9]] 
        
        
      } #if exist and the same variable choosen
      
      # if not new variable was append in the selectize input 
      # he append all selected data from data set 2 
      else{
        a=datensatz2(select(daten$dat,input$lon),select(daten$dat,input$lat),
                     select(daten$dat,input$id), select(daten2$dat,setdiff(
                       list.append(input$selectize,input$selectize_comp,ilr_names$name)
                       ,
                       eliminated_dimension$list)),
                     input$methode, input$distance,input$num, 
                     non_numeric_list$nonnumericlist,input$agglomeration_method,
                     distance_all_comp$list,adist_used$list,weight_count_fact$value,
                     select(daten2$dat,adist_names$list))
        daten2$dat=a[[1]]
        cl$cl=a[[2]]
        sil$sil=a[[3]]
        guete$guete_global=a[[4]]
        guete$guete_lokal=a[[5]]
        length_variable$dat2=a[[6]]
        daten_val$dat=a[[7]] #dataframe validity measures
        d_matrix$mat=a[[8]]
        method_out$method=a[[9]]
      }#if new variable are choosen they must first be appand in the dataset2 
    }#if exist daten2$dat
  })
  
  
  #update validity measures ("header in the plotmeans" to change values )
  observeEvent(input$lonlatok,{
    
    updateSelectInput(session,
                      "select_validity_measures",label="Choose A Validity Measure:",
                      choices=names(as.data.frame(t(guete$guete_lokal)))
    ) #updateSelectInput
    
    updateSelectInput(session,
                      "select_validity_measures2",label="Choose A Validity Measure:",
                      choices=names(as.data.frame(t(guete$guete_lokal)))
    ) #updateSelectInput
  } #lonlatok
  ) #observeEvent
  
  
  
  # wenn nicht numerische Variablen ausgesucht wurden, muss hclust ausgew?hlt werden
  observeEvent(input$selectize,{
    updateSelectInput(session,
                      "methode", label = "Cluster Method",
                      choices = if (length(non_numeric_list$nonnumericlist)==0){list(
                        "k-means"="kmeans", #only numeric
                        "Mclust"="Mclust", #only numeric
                        "hclust"="hclust", #numeric and factor
                        "CLARA"="clara",   #only numeric
                        "PAM"="pam",
                        "cmeans"="cmeans",
                        "Diana"="diana",
                        "Agnes"="agnes")}      #only numeric
                      
                      else
                        list(
                          "Agnes"="agnes",
                          "hclust"="hclust",
                          "PAM"="pam",
                          "Diana"="diana"
                        ) #list
    )#updatevarselectinput
  }#input$selectize
  )#obsereve event
  
  # je nach geweahlter Methode und geweahlter VAlues(type),
  # koennen bedingte Distanzmasse ausgeweahlt werden
  observeEvent(input$methode,{
    if(input$methode=="diana" || input$methode=="pam" || input$methode=="hclust" || 
       input$methode=="agnes"){
      # enable("check_box_adist")
      # enable("button_adist")
    }
    else{disable("check_box_adist")
      disable("button_adist")
    }
    
    updateSelectInput(session,"distance",choices = 
                        if (input$methode=="kmeans"){
                          list(
                            "Euclidean"="euclidean"
                          )
                        }
                      else if (input$methode=="Mclust"){
                        list("Euclidean"="euclidean"
                        )
                      }
                      else if (input$methode=="hclust" & 
                               length(non_numeric_list$nonnumericlist)==0 ){
                        list("Euclidean"="euclidean",
                             "Manhattan"="manhattan",
                             "Maximum"="maximum",
                             "Manhattan"="manhattan",
                             "Canberra"= "canberra",
                             "Binary"="binary",
                             "Minkowski"= "minkowski",
                             "Pearson"="pearson",
                             "Spearman"= "spearman",
                             "Kendall"="kendall"
                        )}
                      else if (input$methode=="hclust" & 
                               length(non_numeric_list$nonnumericlist)>0){
                        list("Gower"="gower")
                      }
                      else if (input$methode=="pam" & 
                               length(non_numeric_list$nonnumericlist)==0){
                        list("Euclidean"="euclidean",
                             "Manhattan"="manhattan")
                      }
                      else if (input$methode=="pam" & 
                               length(non_numeric_list$nonnumericlist)>0){
                        list("Gower"="gower")
                      }
                      else if (input$methode=="diana" & 
                               length(non_numeric_list$nonnumericlist)==0  ){
                        list("Euclidean"="euclidean",
                             "Manhattan"="manhattan",
                             "Maximum"="maximum",
                             "Manhattan"="manhattan",
                             "Canberra"= "canberra",
                             "Binary"="binary",
                             "Minkowski"= "minkowski",
                             "Pearson"="pearson",
                             "Spearman"= "spearman",
                             "Kendall"="kendall")
                      }
                      else if (input$methode=="diana" & 
                               length(non_numeric_list$nonnumericlist)>0){
                        list("Gower"="gower")
                      }
                      else if (input$methode=="agnes" & 
                               length(non_numeric_list$nonnumericlist)==0  ){
                        list("Euclidean"="euclidean",
                             "Manhattan"="manhattan",
                             "Maximum"="maximum",
                             "Manhattan"="manhattan",
                             "Canberra"= "canberra",
                             "Binary"="binary",
                             "Minkowski"= "minkowski",
                             "Pearson"="pearson",
                             "Spearman"= "spearman",
                             "Kendall"="kendall")
                      }
                      else if (input$methode=="agnes" & 
                               length(non_numeric_list$nonnumericlist)>0){
                        list("Gower"="gower")
                      }
                      else if (input$methode=="clara"){
                        list("Euclidean"="euclidean",
                             "Manhattan"="manhattan",
                             "Jaccard"="jaccard")
                      }
                      else if (input$methode=="cmeans"){
                        list("Euclidean"="euclidean",
                             "Manhattan"="manhattan")
                      }
    ) #update selectizeinput
  } #input$methode
  ) #obsereevent
  
  
  # je nach gewaehlter Methode und gewaehlter VAlues(type), 
  # koennen bedingte Distanzmasse ausgew?¤hlt werden
  # diese wiederholung ist von  n?ten update bei ver?nderung der Methode oder der VAlues
  observeEvent(input$selectize,{
    updateSelectInput(session,"distance",choices = 
                        if (input$methode=="kmeans"){
                          list(
                            "Euclidean"="euclidean"
                          )
                        }
                      else if (input$methode=="Mclust"){
                        list("Euclidean"="euclidean"
                        )
                      }
                      else if (input$methode=="hclust" & 
                               length(non_numeric_list$nonnumericlist)==0 ){
                        list("Euclidean"="euclidean",
                             "Manhattan"="manhattan",
                             "Maximum"="maximum",
                             "Manhattan"="manhattan",
                             "Canberra"= "Canberra",
                             "Binary"="binary",
                             "Minkowski"= "minkowski",
                             "Pearson"="pearson",
                             "Spearman"= "spearman",
                             "Kendall"="kendall"
                        )}
                      else if (input$methode=="hclust" & 
                               length(non_numeric_list$nonnumericlist)>0){
                        list("Gower"="gower")
                      }
                      else if (input$methode=="pam" & 
                               length(non_numeric_list$nonnumericlist)==0){
                        list("Euclidean"="euclidean",
                             "Manhattan"="manhattan")
                      }
                      else if (input$methode=="pam" & 
                               length(non_numeric_list$nonnumericlist)>0){
                        list("Gower"="gower")
                      }
                      else if (input$methode=="diana" & 
                               length(non_numeric_list$nonnumericlist)==0  ){
                        list("Euclidean"="euclidean",
                             "Manhattan"="manhattan",
                             "Maximum"="maximum",
                             "Manhattan"="manhattan",
                             "Canberra"= "Canberra",
                             "Binary"="binary",
                             "Minkowski"= "minkowski",
                             "Pearson"="pearson",
                             "Spearman"= "spearman",
                             "Kendall"="kendall")
                      }
                      else if (input$methode=="diana" & 
                               length(non_numeric_list$nonnumericlist)>0){
                        list("Gower"="gower")
                      }
                      else if (input$methode=="agnes" & 
                               length(non_numeric_list$nonnumericlist)==0  ){
                        list("Euclidean"="euclidean",
                             "Manhattan"="manhattan",
                             "Maximum"="maximum",
                             "Manhattan"="manhattan",
                             "Canberra"= "Canberra",
                             "Binary"="binary",
                             "Minkowski"= "minkowski",
                             "Pearson"="pearson",
                             "Spearman"= "spearman",
                             "Kendall"="kendall")
                      }
                      else if (input$methode=="agnes" & 
                               length(non_numeric_list$nonnumericlist)>0){
                        list("Gower"="gower")
                      }
                      else if (input$methode=="clara"){
                        list("Euclidean"="euclidean",
                             "Manhattan"="manhattan",
                             "Jaccard"="jaccard")
                      }
                      else if (input$methode=="cmeans"){
                        list("Euclidean"="euclidean",
                             "Manhattan"="manhattan")
                      }
                      
    ) #update selectizeinput
  } #input$methode
  ) #obsereevent
  
  #datensatz als reaktive value abspeichern
  observeEvent(input$button,{
    if(input$choosedatatyp=="From CSV")
    {
      dataset_with_na$dat=read.csv(input$file1$datapath,
                                   header = input$header,
                                   sep = input$sep,
                                   quote = input$quote)
      dat=na.omit(dataset_with_na$dat)
      dat=dat %>% mutate_if(is.character, as.factor) 
      daten$dat=dat[is.finite(rowSums(dat[sapply(dat,is.numeric)])),]
    }
    if(input$choosedatatyp=="From Excel"){
      dataset_with_na$dat=as.data.frame( read_xlsx(input$file1$datapath))
      dat <- na.omit(dataset_with_na$dat)
      dat=dat %>% mutate_if(is.character, as.factor) 
      daten$dat=dat[is.finite(rowSums(dat[sapply(dat,is.numeric)])),]
    }
    if (input$choosedatatyp=="Global Environment" ){
      dataset_with_na$dat=get(input$globalvar)
      dat=na.omit(dataset_with_na$dat)
      dat=dat %>% mutate_if(is.character, as.factor) 
      daten$dat=dat[is.finite(rowSums(dat[sapply(dat,is.numeric)])),]
    }
  }) #observeEvent input$button
  
  
  
  
  #datensatz 2 wird nochmals ohne Transformationen berechnet (Ausgangslage)
  observeEvent({input$buttondatareset}
               ,                                       
               { 
                 updateCheckboxInput(session,inputId = "check_box_adist",
                                     label="Apply Aitchison distance measurement
                                     to compositional data?",value = FALSE)
                 adist_names$list=NULL
                 dim_count$count=0
                 ilr_names$name=""
                 eliminated_dimension$list=""
                 names_transformed_attribute$list=""
                 adist_used$list=FALSE
                 a=datensatz2(select(daten$dat,input$lon),select(daten$dat,input$lat),
                              select(daten$dat,input$id), 
                              select(daten$dat,list.append(
                                input$selectize,input$selectize_comp)),
                              input$methode, input$distance,input$num, 
                              non_numeric_list$nonnumericlist,
                              input$agglomeration_method,"null",
                              adist_used$list,"null","null")
                 
                 
                 daten2$dat=a[[1]]
                 cl$cl=a[[2]]
                 sil$sil=a[[3]]
                 guete$guete_global=a[[4]]
                 guete$guete_lokal=a[[5]]
                 length_variable$dat2=a[[6]]
                 daten_val$dat=a[[7]] #dataframe validity measures
                 d_matrix$mat=a[[8]]
                 method_out$method=a[[9]]
                 
                 not_negative_values$list=c(names(
                   select(daten2$dat,
                          setdiff(
                            setdiff(
                              list.append(
                                input$selectize,input$selectize_comp),
                              non_numeric_list$nonnumericlist),  
                            names(which(
                              apply(
                                select(daten2$dat,
                                       setdiff( 
                                         list.append(
                                           input$selectize,input$selectize_comp),
                                         non_numeric_list$nonnumericlist)),
                                MARGIN=2,function(x){any (is.negative(x)==TRUE)})==TRUE)))
                   )))
                 updateSelectizeInput(session,"selectvariableselecttrans",
                                      label="Variable Selection For Transformation And Scaling",
                                      choices=
                                        if(input$check_transformation=="Prepare Compositional Data" & 
                                           adist_used$list==FALSE)
                                        {setdiff(comp$list,names_transformed_attribute$list)}
                                      else if(input$check_transformation=="Prepare Compositional Data" 
                                              & adist_used$list==TRUE)
                                      {""}
                                      #{setdiff(comp$list,names_transformed_attribute$list)} 
                                      else if(length(non_numeric_list$nonnumericlist)!=0 ){
                                        setdiff(input$selectize,non_numeric_list$nonnumericlist)}
                                      else{
                                        setdiff(list.append(
                                          input$selectize,input$selectize_comp),comp$list)}
                 )#updateselectizeInput
                 
                 if(input$methode=="diana" || input$methode=="pam" ||
                    input$methode=="hclust" || input$methode=="agnes"){
                   enable("check_box_adist")
                   enable("button_adist")}
               }) 
  
  
  
  #datensatz 2 wird nochmals ohne Transformationen berechnet (Ausgangslage)
  observeEvent({input$button_adist_reset},{
    updateCheckboxInput(session,inputId = "check_box_adist",
                        label="Apply Aitchison distance measurement
                        to compositional data?",value = FALSE)
    adist_names$list=NULL
    dim_count$count=0
    ilr_names$name=""
    eliminated_dimension$list=""
    names_transformed_attribute$list=""
    adist_used$list=FALSE
    a=datensatz2(select(daten$dat,input$lon),select(daten$dat,input$lat),
                 select(daten$dat,input$id), 
                 select(daten$dat,list.append(input$selectize,input$selectize_comp)),
                 input$methode, input$distance,input$num, 
                 non_numeric_list$nonnumericlist,input$agglomeration_method)
    daten2$dat=a[[1]]
    cl$cl=a[[2]]
    sil$sil=a[[3]]
    guete$guete_global=a[[4]]
    guete$guete_lokal=a[[5]]
    length_variable$dat2=a[[6]]
    daten_val$dat=a[[7]] #dataframe validity measures
    d_matrix$mat=a[[8]]
    method_out$method=a[[9]]
    
    
    not_negative_values$list=c(names(select(daten2$dat,setdiff(
      setdiff(list.append(input$selectize,input$selectize_comp),
              non_numeric_list$nonnumericlist),  
      names(which(apply(select(daten2$dat,setdiff( 
        list.append(input$selectize,input$selectize_comp),
        non_numeric_list$nonnumericlist)),MARGIN=2,function(x){
          any (is.negative(x)==TRUE)})==TRUE)))
    )))
    updateSelectizeInput(session,"selectvariableselecttrans",
                         label="Variable Selection For Transformation And Scaling",
                         choices=if(input$check_transformation=="Prepare Compositional Data" & 
                                    adist_used$list==FALSE)
                         {setdiff(comp$list,names_transformed_attribute$list)}
                         else if(input$check_transformation=="Prepare Compositional Data" & 
                                 adist_used$list==TRUE)
                           #{setdiff(comp$list,names_transformed_attribute$list)} 
                         {""}
                         else if(length(non_numeric_list$nonnumericlist)!=0){
                           setdiff(input$selectize,non_numeric_list$nonnumericlist)}
                         else{
                           setdiff(list.append(input$selectize,input$selectize_comp),comp$list)}
    )#updateselectizeInput
    
    if(input$methode=="diana" || input$methode=="pam" || input$methode=="hclust" || 
       input$methode=="agnes"){
      enable("check_box_adist")
      enable("button_adist")} 
    
    names_transformed_attribute$list=""
    
  }) 
  
  
  
  
  #update id selectinput 
  observeEvent(input$button,
               {updateSelectInput(session,"id",label="Probe ID: (optional)",
                                  choices = names(daten$dat))})
  #update lon selectinput 
  observeEvent(input$button,
               {updateSelectInput(session,"lon",label="Longitude",
                                  choices = names(daten$dat))})
  #update lat selectinput
  observeEvent(input$button,
               {updateSelectInput(session,"lat",label="Latitude",
                                  choices = names(daten$dat))})
  #update variable selectizeinput
  observeEvent(input$button,
               {updateSelectizeInput(session, 'selectize',
                                     label="Variables Of Non-Compositional Data:",
                                     choices = names(daten$dat), server = TRUE)})
  
  #with html non numeric sidebar
  observeEvent(input$button,{
    list.out=list(names(daten$dat)[which(sapply(daten$dat,is.numeric)==FALSE)])
    list.out=ifelse(identical(list.out[[1]], character(0)),"non",list.out)
    if(list.out!="non"){
      list.out=paste( " ",list.out[[1]],collapse = ",")
      shiny::showNotification(paste("Variables containing non numerical values:",
                                    list.out
      ),closeButton = TRUE,duration=NULL,type="message")
    }
  })
  
  #with html containig values of 0
  observeEvent(input$button,{
    list.out=list(names(
      select(daten$dat,which(sapply(daten$dat,is.numeric)==TRUE)))[which(
        apply(select(daten$dat,which(sapply(daten$dat,is.numeric)==TRUE)),
              MARGIN=2,function(x){prod(x)==0}))])
    
    list.out=ifelse(identical(list.out[[1]], character(0)),"non",list.out)
    if(list.out!="non"){
      list.out=paste(" ",list.out[[1]],collapse = ",")
      shiny::showNotification(paste("Variables containing values of 0:",
                                    list.out
      ),closeButton = TRUE,duration=NULL,type="warning")#shownotification
    }#if
  }#observevent
  )#observevent
  
  observeEvent(input$methode,{
    updateSelectInput(session, "agglomeration_method",
                      label = "Agglomeration Method", choices = 
                        if (input$methode =="hclust" ) 
                        {list( "ward.D", "ward.D2", "single", "complete", "average" ,
                               "mcquitty" , "median" , "centroid")}
                      else if(input$methode=="agnes"){
                        list("average","single","complete",
                             "ward.D","ward.D2")}
    )#updateSelectinput
  }#observevent
  )#observevent
  
  
  observeEvent(input$lonlatok,{
    
    if(input$methode=="diana" || input$methode=="pam" || input$methode=="hclust" || 
       input$methode=="agnes"){
      if(length(setdiff(comp$list,names_transformed_attribute$list))==length(comp$list)){
        enable("check_box_adist")
        enable("button_adist")
      }}
    else{disable("check_box_adist")
      disable("button_adist")}
    
    
    not_negative_values$list=c(names(
      select(daten2$dat,setdiff(setdiff(setdiff(
        list.append(input$selectize,input$selectize_comp,ilr_names$name),
        eliminated_dimension$list),non_numeric_list$nonnumericlist),  
        names(which(apply(select(daten2$dat,setdiff(setdiff(list.append(
          input$selectize,input$selectize_comp,ilr_names$name),
          eliminated_dimension$list),non_numeric_list$nonnumericlist)),
          MARGIN=2,function(x){any (is.negative(x)==TRUE)})==TRUE)))
      )))
    # we didn't fill up the reaktivevalue if i choose one of this options
    # so we have to fill it up with a empty string
    if (input$check_composition=="No"){
      comp$list<-""
    }
    updateSelectizeInput(session,"selectvariableselecttrans",
                         label="Variable Selection For Transformation And Scaling",
                         choices=if(input$check_transformation==
                                    "Prepare Compositional Data" & adist_used$list==FALSE)
                         {setdiff(comp$list,names_transformed_attribute$list)}
                         else if(input$check_transformation==
                                 "Prepare Compositional Data" & adist_used$list==TRUE)
                           #{setdiff(comp$list,names_transformed_attribute$list)} 
                         {""}
                         else if(length(non_numeric_list$nonnumericlist)!=0){setdiff(
                           setdiff(input$selectize,non_numeric_list$nonnumericlist),
                           names_transformed_attribute$list)}
                         else{
                           setdiff(input$selectize,names_transformed_attribute$list)}
    )#updateselectizeInput
  }#observevent
  )#observevent
  
  observeEvent(input$selectvariableselecttrans,{
    updateSelectInput(session,"selecttrans",label="Transformation",
                      choices= 
                        if (length(setdiff(
                          input$selectvariableselecttrans,not_negative_values$list))>0 || 
                          input$selectvariableselecttrans %in% 
                          adist_names$list){list("Select_Transformation")}
                      else if (input$check_transformation==
                               "Prepare Non-Compositional Data"){
                        list("Select_Transformation","Logarithm","Root_Transformation",
                             "Logcentered", "BoxCox")}
                      else if (input$check_transformation==
                               "Prepare Compositional Data"){list(
                                 "Select_Transformation","Additive_Log_Ratio",
                                 "Centred_Log_Ratio", "Isometric_Pivot_Log_Ratio"
                               )}
    )#uppdateSlectinput
  }#observeevent
  )#observeEvent
  
  observeEvent(input$check_transformation,{
    if(!(is.null(daten$dat))){
    updateSelectInput(session,"selecttrans",label="Transformation",
                      choices= 
                      if (input$check_transformation==
                               "Prepare Non-Compositional Data"){
                        list("Select_Transformation","Logarithm","Root_Transformation",
                             "Logcentered", "BoxCox")}
                      else if (input$check_transformation==
                               "Prepare Compositional Data"){list(
                                 "Select_Transformation","Additive_Log_Ratio",
                                 "Centred_Log_Ratio", "Isometric_Pivot_Log_Ratio"
                               )}
    )#uppdateSlectinput
    }
  }#observeevent
  )#observeEvent
  
  observeEvent(input$check_transformation,{
    #checkbox will be checked in the beginning also before we have inserted a dataset
    #and then it ends in an errror
    if(!(is.null(daten$dat))){
      updateSelectizeInput(session,"selectvariableselecttrans",
                           label="Variable Selection For Transformation And Scaling",
                           choices=  
                             # if checkbox active and then if they are transformed or not 
                             if(input$check_transformation=="Prepare Compositional Data" & 
                                adist_used$list==FALSE)
                             {setdiff(comp$list,names_transformed_attribute$list)}
                           else if(input$check_transformation=="Prepare Compositional Data" &
                                   adist_used$list==TRUE)
                           {""}
                           #{setdiff(comp$list,names_transformed_attribute$list)} 
                           #with nonumeric values
                           else if(length(non_numeric_list$nonnumericlist)!=0)
                           {setdiff(setdiff(input$selectize,non_numeric_list$nonnumericlist),
                                    names_transformed_attribute$list)}
                           else{
                             setdiff(input$selectize,names_transformed_attribute$list)
                           }
      )
    }
  })
  
  observeEvent(input$check_box_adist,{
    #checkbox will be checked in the beginning also before we have inserted 
    #a dataset and then it ends in an errror
    if(!(is.null(daten$dat))){
      updateSelectizeInput(session,"selectvariableselecttrans",
                           label="Variable Selection For Transformation And Scaling",
                           choices=  
                             # if checkbox active and then if they are transformed or not 
                             if(input$check_transformation=="Prepare Compositional Data" & 
                                adist_used$list==FALSE)
                             {setdiff(comp$list,names_transformed_attribute$list)}
                           else if(input$check_transformation=="Prepare Compositional Data" & 
                                   adist_used$list==TRUE)
                           {""}
                           #{setdiff(comp$list,names_transformed_attribute$list)} 
                           #with nonumeric values
                           else if(length(non_numeric_list$nonnumericlist)!=0)
                           {setdiff(setdiff(input$selectize,non_numeric_list$nonnumericlist),
                                    names_transformed_attribute$list)}
                           else{
                             setdiff(input$selectize,names_transformed_attribute$list)
                           }
      )
    }
  })
  
  observeEvent(input$buttontrans,{ 
    
    if (any(sort(input$selectvariableselecttrans)==sort(
      input$selectize_comp))){
      disable("check_box_adist")
      disable("button_adist")
    }
    newdat=replace(select(daten2$dat,input$selectvariableselecttrans),
                   select(daten2$dat,
                          input$selectvariableselecttrans)==0,0.1)
    newdat=as.data.frame(newdat)
    # Transformationsfunktionsaufruf
    a=prepare(newdat,scaling=input$selectscaling, 
              transformation=input$selecttrans, powers="none")  
    a=as.data.frame(a)
    
    if (input$selecttrans=="Isometric_Pivot_Log_Ratio" || 
        input$selecttrans=="Additive_Log_Ratio"){
      eliminated_dimension$values=setdiff(
        input$selectvariableselecttrans,colnames(a))
      daten2$dat[[eliminated_dimension$values]]<-NULL
      count=0
      dim_count$count=dim_count$count+1
      if (input$selecttrans=="Isometric_Pivot_Log_Ratio"){
        for (i in setdiff(
          input$selectvariableselecttrans,eliminated_dimension$values)){
          count=count+1
          daten2$dat[i]=a[which(i==names(a))]
          ilr_names$name=list.append(ilr_names$name,paste0(
            "ilr",dim_count$count,"ilr_",paste(count)))
          names(daten2$dat)[names(daten2$dat)==i]<-paste0(
            "ilr",dim_count$count,"ilr_",paste(count))
        }}
      if(input$selecttrans=="Additive_Log_Ratio"){
        for (i in setdiff(
          input$selectvariableselecttrans,eliminated_dimension$values)){
          count=count+1
          daten2$dat[i]=a[which(i==names(a))]
          ilr_names$name=list.append(ilr_names$name,paste0(
            "alr",dim_count$count,"alr_",paste(count)))
          names(daten2$dat)[names(daten2$dat)==i]<-paste0(
            "alr",dim_count$count,"alr_",paste(count))
        }}
      eliminated=input$selectvariableselecttrans
      eliminated_dimension$list=list.append(
        eliminated_dimension$list,eliminated)
    }
    else{
      #ueberschreiben der neuen transformierten Values
      for (i in input$selectvariableselecttrans){
        daten2$dat[i]=a[which(i==names(a))]
      }}
    liste_transformed=input$selectvariableselecttrans
    # wenn eine Spalte transformiert wird,
    # darf sie nicht ein zweites mal transformiert werden
    if(input$selecttrans!="Select_Transformation"){
      names_transformed_attribute$list=list.append(
        liste_transformed,
        names_transformed_attribute$list)}
    #-Calling the function data set2
    #-With allready tranformed data
    #ilr name names of vaiables used ilr
    #ilr not used ilr=""
    b=datensatz2(select(daten$dat,input$lon),select(daten$dat,input$lat),
                 select(daten$dat,input$id), 
                 select(daten2$dat,setdiff(
                   list.append(input$selectize,input$selectize_comp,
                               ilr_names$name),
                   eliminated_dimension$list)),
                 input$methode, input$distance,input$num, 
                 non_numeric_list$nonnumericlist,
                 input$agglomeration_method,distance_all_comp$list,
                 adist_used$list,weight_count_fact$value,
                 select(daten2$dat,adist_names$list)
    )
    daten2$dat=b[[1]]
    cl$cl=b[[2]]
    sil$sil=b[[3]]
    guete$guete_global=b[[4]]
    guete$guete_lokal=b[[5]]
    length_variable$dat2=b[[6]]
    daten_val$dat=b[[7]] #dataframe validity measures
    d_matrix$mat=b[[8]]
    method_out$method=b[[9]]
    
    
    #all positive values which also are not factors. 
    #Values which are transfomable
    not_negative_values$list=c(names(
      select(daten2$dat,
             setdiff(
               setdiff(
                 setdiff(
                   list.append(input$selectize,input$selectize_comp,
                               ilr_names$name),eliminated_dimension$list),
                 non_numeric_list$nonnumericlist),  
               names(which(apply(select(daten2$dat,
                                        setdiff(
                                          setdiff(list.append(
                                            input$selectize,input$selectize_comp,
                                            ilr_names$name),eliminated_dimension$list)
                                          ,non_numeric_list$nonnumericlist)),
                                 MARGIN=2,function(x){
                                   any (is.negative(x)==TRUE)})==TRUE)))
      )))
    
  }#observeevent
  )#observeevent
  
  #after transformation the dataset 2 has to be updated in the table output
  observeEvent(input$buttontrans,
               { output$datas2=renderDataTable(daten2$dat,options =list(scrollX = TRUE))
               })
  
  
  # reactive names transformed attribut update variables which are transformed
  # Values with 0 will changed to 0.1
  observeEvent(input$buttontrans,{
    updateSelectizeInput(session,"selectvariableselecttrans",
                         label="Variable Selection For Transformation And Scaling",
                         choices=  
                           if(input$check_transformation=="Prepare Compositional Data" & 
                              adist_used$list==FALSE)
                           {setdiff(comp$list,names_transformed_attribute$list)}
                         else if(input$check_transformation=="Prepare Compositional Data" & 
                                 adist_used$list==TRUE)
                           #{setdiff(comp$list,names_transformed_attribute$list)} 
                         {""}
                         else if(length(non_numeric_list$nonnumericlist)!=0){
                           setdiff(setdiff(input$selectize,non_numeric_list$nonnumericlist),
                                   names_transformed_attribute$list)}
                         else{
                           setdiff(input$selectize,names_transformed_attribute$list)}
    )})
  
  
  observeEvent(input$button,{
    updateTabItems(session,"sidebarmenu",selected="dataset")
  })
  
  observeEvent(input$lonlatok,{
    updateTabItems(session,"sidebarmenu",selected="dashboard")
  })
  
  observeEvent(input$button_adist,{
    updateTabItems(session,"sidebarmenu",selected="dashboard")
  })
  
  
  observeEvent(input$sidebarmenu ,
               {
                 #as soon as you go to Calculate best number of cluster in the sidebar 
                 # best number of k will be calculated automatcally
                 if (input$sidebarmenu=="calc"){
               
                   if (input$methode=="kmeans"|| input$methode=="diana" || 
                       input$methode=="agnes" || 
                       input$methode=="hclust"|| input$methode=="pam" ){
                     
                     plot$plot<-NULL
                     nblist$list<-NULL
                     
                     nblist$list<- nb(
                       daten=if(is.null(
                         select(
                           daten2$dat,
                           setdiff(
                             list.append(input$selectize,input$selectize_comp,
                                         ilr_names$name),
                             eliminated_dimension$list)))==FALSE){
                         select(
                           daten2$dat,setdiff(
                             list.append(
                               input$selectize,input$selectize_comp,ilr_names$name),
                             eliminated_dimension$list))},
                       methode=input$methode,
                       dist=input$distance,non_numeric_list$nonnumericlist,
                       input$agglomeration_method,d_matrix$mat)
                     if(length(as.numeric(as.character(nblist$list[[1]])))==1){
                       update_num_reaktiv$num=as.numeric(as.character(nblist$list[[1]]))
                       updateNumericInput(session,"num",label = "Number of Cluster",
                                          value = if(as.numeric(as.character(nblist$list[[1]]))>1)
                                          {as.numeric(as.character(nblist$list[[1]]))}else{2},
                                          min=2)}
                     else{
                       update_num_reaktiv$num=max(as.numeric(as.character(nblist$list[[1]])))
                       updateNumericInput(session,"num",label = "Number of Cluster",
                                          value = if(as.numeric(as.character(nblist$list[[1]][1]))>1)
                                          {max(as.numeric(as.character(nblist$list[[1]])))}else{2},
                                          min=2)}
                     a=datensatz2(select(daten$dat,input$lon),select(daten$dat,input$lat),
                                  select(daten$dat,input$id), select(daten2$dat,setdiff(
                                    list.append(input$selectize,input$selectize_comp,
                                                ilr_names$name),eliminated_dimension$list)),
                                  input$methode, input$distance,update_num_reaktiv$num,
                                  non_numeric_list$nonnumericlist,
                                  input$agglomeration_method,distance_all_comp$list,
                                  adist_used$list,weight_count_fact$value,
                                  select(daten2$dat,adist_names$list))
                     daten2$dat=a[[1]]
                     cl$cl=a[[2]]
                     sil$sil=a[[3]]
                     guete$guete_global=a[[4]]
                     guete$guete_lokal=a[[5]]
                     length_variable$dat2=a[[6]]
                     daten_val$dat=a[[7]] #dataframe validity measures
                     d_matrix$mat=a[[8]]
                     method_out$method=a[[9]]
                     
                   }#kmeans
                   
                   #mclust brauch kein aufruf von der funktion nb
                   if (input$methode=="Mclust"){
 
                     nblist$list<-NULL
                     plot$plot=NULL
                     a=datensatz2(select(daten$dat,input$lon),select(daten$dat,input$lat),
                                  select(daten$dat,input$id), 
                                  select(daten2$dat,
                                         setdiff(
                                           list.append(input$selectize,input$selectize_comp,
                                                       ilr_names$name),
                                           eliminated_dimension$list)),
                                  input$methode, 
                                  input$distance,amount_k = c(2:20),
                                  non_numeric_list$nonnumericlist,
                                  input$agglomeration_method)
                     daten2$dat=a[[1]]
                     cl$cl=a[[2]]
                     sil$sil=a[[3]]
                     guete$guete_global=a[[4]]
                     guete$guete_lokal=a[[5]]
                     length_variable$dat2=a[[6]]
                     daten_val$dat=a[[7]] #dataframe validity measures
                     d_matrix$mat=a[[8]]
                     method_out$method=a[[9]]
                     
                     
                     updateNumericInput(session,"num",
                                        label = "Number Of Cluster",
                                        value = max(daten2$dat$Cluster),min=2)
                     
                   }#Mclust
                   if (input$methode=="clara" || input$methode=="cmeans" ){
                     plot$plot<-NULL
                     nblist$list<-NULL
                     
                     nblist$list<- nb(
                       daten=if(is.null(
                         select(daten2$dat,
                                setdiff(
                                  list.append(
                                    input$selectize,input$selectize_comp,ilr_names$name),
                                  eliminated_dimension$list)))==FALSE){
                         select(daten2$dat,
                                setdiff(
                                  list.append(
                                    input$selectize,input$selectize_comp,ilr_names$name),
                                  eliminated_dimension$list))},
                       methode=input$methode,
                       dist=input$distance,
                       non_numeric_list$nonnumericlist,"null","null")
                     update_num_reaktiv$num=as.numeric(as.character(nblist$list[[1]]))
                     updateNumericInput(session,"num",label = "Number Of Cluster",
                                        value = as.numeric(as.character(nblist$list[[1]])),min=2)
                     a=datensatz2(select(daten$dat,input$lon),
                                  select(daten$dat,input$lat),
                                  select(daten$dat,input$id), 
                                  select(daten2$dat,
                                         setdiff(
                                           list.append(
                                             input$selectize,input$selectize_comp,
                                             ilr_names$name),eliminated_dimension$list)),
                                  input$methode, input$distance,update_num_reaktiv$num,
                                  non_numeric_list$nonnumericlist,
                                  input$agglomeration_method)
                     
                     daten2$dat=a[[1]]
                     cl$cl=a[[2]]
                     sil$sil=a[[3]]
                     guete$guete_global=a[[4]]
                     guete$guete_lokal=a[[5]]
                     length_variable$dat2=a[[6]]
                     daten_val$dat=a[[7]] #dataframe validity measures
                     d_matrix$mat=a[[8]]
                     method_out$method=a[[9]]
                     
                   }#clara
                 }#calc
               }#choseok
  )#observeEvent
  
  
  
  #best number of cluster
  observeEvent(input$sidebarmenu ,{
    
    if (input$sidebarmenu=="calc"){
      
      if (input$methode=="kmeans" || input$methode=="hclust" 
          || input$methode=="agnes"){
        if (length(non_numeric_list$nonnumericlist)==0){
          output$nbplot=renderPlot(
            nblist$list[[3]])}
        if ( length(non_numeric_list$nonnumericlist)>0){
          data=data.frame(
            "Number_of_cluster"=1:15,"Average_Silhouette_Width"=nblist$list[[2]])
          output$nbplot=renderPlot(
            ggplot(data,aes(Number_of_cluster,Average_Silhouette_Width,label=
                              round(Average_Silhouette_Width,3)))
            +geom_point()+geom_line()
            +geom_text(colour="blue" ,position = position_stack(vjust = 1.1))
          )}
      }#kmeans, hclust,diana, agnes
      
      if (input$methode=="Mclust"){
        plot$plot=fviz_mclust(cl$cl, "BIC", palette = "jco")
        # BIC values used for choosing the number of clusters
        output$nbplot=renderPlot(
        plot$plot)
      }#mclust
      
      if ( input$methode=="clara" ){
        output$nbplot=renderPlot(
          nblist$list[[2]])
      }
      if (input$methode=="diana" || input$methode=="pam" || input$methode=="cmeans"  ){
        data=data.frame(
          "Number_of_cluster"=1:15,"Average_Silhouette_Width"=nblist$list[[2]])
        output$nbplot=renderPlot(
          ggplot(data,aes(
            Number_of_cluster,Average_Silhouette_Width,
            label=round(Average_Silhouette_Width,3)))
          +geom_point()+geom_line()
          +geom_text(colour="blue" ,position = position_stack(vjust = 1.1))
        )
      }
    }#pam,clara,cmeans
  }#observeevent
  )#observeevent
  
  
  output$mymap <- renderLeaflet({
    
    if(daten2$dat$lon[1]=="Na" ) 
      stop("Geographical output requires geospatial data. 
           Data set must include longitude and latitude to produce visuals.")
    
    #ICONS GENERIEREN fuer den Plot auf der MAP
    pchIcons = function(pch = 1, width = 10, height = 10, 
                        bg = "transparent", col = NULL, ...) {
      n = length(pch)
      files = character(n)
      # create a sequence of png images
      for (i in seq_len(n)) {
        f = tempfile(fileext = '.png')
        png(f, width = width, height = height, bg = bg)
        par(mar = c(0, 0, 0, 0))
        plot.new()
        points(.5, .5, pch = pch[i], col = col[i], cex = min(width, height) / 8, ...)
        dev.off()
        files[i] = f
      }
      files
    }
    cl = daten2$dat$Cluster
    
    leaflet(daten$dat) %>%
      addTiles() %>%
      addMarkers(
        data = daten$dat,
        lat = ~select(daten$dat,input$lat)[,1],
        lng = ~select(daten$dat,input$lon)[,1],
        popup = ~paste(
          "<strong>Probe ID:</strong>",daten2$dat$id,
          "<br>",
          "<strong>Longitude:</strong>",select(daten$dat,input$lon)[,1],
          "<br>",
          "<strong>Latitude:</strong>",select(daten$dat,input$lat)[,1],
          "<br>",
          "<strong>Silhouette:</strong>",round(daten2$dat$sil,digits=3),
          "<br>",
          "<strong>Cluster:</strong>",daten2$dat$Cluster,
          "<br>"
        ),
        icon = ~ icons(
          iconUrl = pchIcons(pch = cl,width = 10,height = 10,col = cl,lwd = 4), 
          popupAnchorX = 20, popupAnchorY = 0
        ))
  })
  
  # PLOTMEANS in dashboard
  output$plotmeans=renderPlot({
    
    find_Center_2 <- function(x, clustering){
      k <- length(unique(clustering))
      a1 <- matrix(nrow=k, ncol=ncol(x))
      colnames(a1) <- colnames(x)
      for( i in 1:k){
        a1[i,] <- apply(x[ which( clustering == i ), , drop=FALSE ], 2, mean)
      }
      a1
    }
    
    plotmeans_2 <- function(x, cl ){
      #x11() #
      cen1 <- find_Center_2(x, cl)
      df <- t(cen1)
      colnames(df) <- paste("Cluster", 1:ncol(df),"_",
                            names(select(as.data.frame(t(guete$guete_lokal)),
                                         input$select_validity_measures)#select
                            )#names
      )#paste
      df <- data.frame(df)
      df$variable <- rownames(df)
      df <- reshape2::melt(df)
      colnames(df) <- c("variable", "Cluster", "mean")
      df$id <- as.integer(as.factor(df$variable))
      mati_guete=t(guete$guete_lokal)
      mati_guete=as.data.frame(mati_guete)
      df$guete=rep(NA,length(df[,1]))
      # um df$guete mit dem gew?hltem Guetemass zu f?llen
      for (i in 1:input$num){
        df_guete=as.numeric(as.factor(df[,2]))
        df_guete=as.data.frame(df_guete)
        df$guete[which(df_guete==i)]=
          rep(select(mati_guete,input$select_validity_measures)[i,],
              length(list.append(input$selectize,input$selectize_comp)))
      }
      
      daten2$dat$variable_name_plotmeans[1:length(df[,1])]=df[,1]
      daten2$dat$cluster_number_plotmeans[1:length(df[,1])]=df[,2]
      daten2$dat$cluster_center_plotmeans[1:length(df[,1])]=df[,3]
      daten2$dat$cluster_id_plotmeans[1:length(df[,1])]=df[,4]
      
      ggplot(df, aes(x = id, y = mean, label = variable)) +
        geom_text() +
        facet_wrap(~Cluster+guete, ncol = 6)+
        geom_hline(yintercept = 0)+xlab("") +
        ylab("Cluster-means") +
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
    }
    #plotmeans im Datensatz auff?llen
    plotmeans_2(
      select(
        daten2$dat,
        setdiff(
          list.append(
            input$selectize,input$selectize_comp,ilr_names$name),
          eliminated_dimension$list))[which(
            sapply(
              select(daten2$dat,setdiff(
                list.append(input$selectize,input$selectize_comp,ilr_names$name),
                eliminated_dimension$list)),
              is.numeric)==TRUE)],
      cl = daten2$dat$Cluster)
    
  })
  
  
  
  # PLOTMEANS in prepare
  #we calculated plotmeans above and saved the results in plot_mean$plot
  #so it is not necessary to calculate twice 
  output$plotmeans2=renderPlot({
    
    find_Center_2 <- function(x, clustering){
      k <- length(unique(clustering))
      a1 <- matrix(nrow=k, ncol=ncol(x))
      colnames(a1) <- colnames(x)
      for( i in 1:k){
        a1[i,] <- apply(x[ which( clustering == i ), , drop=FALSE ], 2, mean)
      }
      a1
    }
    
    plotmeans_2 <- function(x, cl ){
      #x11() #
      cen1 <- find_Center_2(x, cl)
      df <- t(cen1)
      colnames(df) <- paste("Cluster", 1:ncol(df),"_",
                            names(select(as.data.frame(t(guete$guete_lokal)),
                                         input$select_validity_measures2)#select
                            )#names
      )#paste
      df <- data.frame(df)
      df$variable <- rownames(df)
      df <- reshape2::melt(df)
      colnames(df) <- c("variable", "Cluster", "mean")
      df$id <- as.integer(as.factor(df$variable))
      mati_guete=t(guete$guete_lokal)
      mati_guete=as.data.frame(mati_guete)
      df$guete=rep(NA,length(df[,1]))
      # um df$guete mit dem gew?hltem Guetemass zu f?llen
      for (i in 1:input$num){
        df_guete=as.numeric(as.factor(df[,2]))
        df_guete=as.data.frame(df_guete)
        df$guete[which(df_guete==i)]=
          rep(select(mati_guete,input$select_validity_measures2)[i,],
              length(list.append(input$selectize,input$selectize_comp)))
      }
      
      daten2$dat$variable_name_plotmeans[1:length(df[,1])]=df[,1]
      daten2$dat$cluster_number_plotmeans[1:length(df[,1])]=df[,2]
      daten2$dat$cluster_center_plotmeans[1:length(df[,1])]=df[,3]
      daten2$dat$cluster_id_plotmeans[1:length(df[,1])]=df[,4]
      
      ggplot(df, aes(x = id, y = mean, label = variable)) +
        geom_text() +
        facet_wrap(~Cluster+guete, ncol = 6)+
        geom_hline(yintercept = 0)+xlab("") +
        ylab("Cluster-means") +
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
    }
    #plotmeans im Datensatz auff?llen
    plotmeans_2(
      select(
        daten2$dat,
        setdiff(
          list.append(
            input$selectize,input$selectize_comp,ilr_names$name),
          eliminated_dimension$list))[which(
            sapply(
              select(daten2$dat,setdiff(
                list.append(input$selectize,input$selectize_comp,ilr_names$name),
                eliminated_dimension$list)),
              is.numeric)==TRUE)],
      cl = daten2$dat$Cluster)
    
  })
  
  output$heatdist=renderPlotly(
    if( length(non_numeric_list$nonnumericlist)==0 & adist_used$list==FALSE){
      dist=get_dist(
        select(
          daten2$dat,
          setdiff(
            list.append(
              input$selectize,input$selectize_comp,ilr_names$name),
            eliminated_dimension$list)),
        method=input$distance)
      
      res.hc <- stats::hclust(dist, method = "ward.D2")
      
      heatmaply::heatmaply(
        x=as.matrix(dist)[res.hc$order,res.hc$order], 
        scale_fill_gradient_fun = 
          ggplot2::scale_fill_gradient2(
            low = "red", mid="white",high = "blue", midpoint =mean(dist) 
                                                  ))}
    #if adist==TRUE we habe d_matrix. If nonumericlist>0 we have d_matrix
    # we have to say else if for each method which use d_matrix. If we dont do that it
    # would no update heatmap by changing method.
    else if(input$methode=="hclust"){
      res.hc <- stats::hclust(d_matrix$mat, method = "ward.D2")
      heatmaply::heatmaply(
        x=as.matrix(
          d_matrix$mat )[res.hc$order,res.hc$order],
        scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
            low = "red", mid="white",high = "blue", midpoint =mean(d_matrix$mat)
                                       ))}
    else if(input$methode=="pam"){
      res.hc <- stats::hclust(d_matrix$mat, method = "ward.D2")
      heatmaply::heatmaply(
        x=as.matrix(
          d_matrix$mat )[res.hc$order,res.hc$order],
          scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
            low = "red", mid="white",high = "blue", midpoint =mean(d_matrix$mat) 
                                       ))}
    else if(input$methode=="diana"){
      res.hc <- stats::hclust(d_matrix$mat, method = "ward.D2")
      heatmaply::heatmaply(
        x=as.matrix(
          d_matrix$mat )[res.hc$order,res.hc$order], 
          scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
            low = "red", mid="white",high = "blue", midpoint =mean(d_matrix$mat) 
                                       ))}
    else{
      res.hc <- stats::hclust(d_matrix$mat, method = "ward.D2")
      heatmaply::heatmaply(
        x=as.matrix(
          d_matrix$mat )[res.hc$order,res.hc$order], 
          scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
            low = "red", mid="white",high = "blue", midpoint =mean(d_matrix$mat) 
                                       ))}
  ) #fviz_dist
  
  
  output$heatdist2=renderPlot(
    if(input$methode=="pam" || input$methode=="diana" || input$methode=="hclust"
       || input$methode=="agnes"){
      fviz_dist(d_matrix$mat)}
    else{fviz_dist(get_dist(select(
      daten2$dat,
      setdiff(
        list.append(
          input$selectize,input$selectize_comp,ilr_names$name),
        eliminated_dimension$list)),
      method=input$distance)
    )
    }
  )
  
  
  # show variable with na
  observeEvent(input$button,{
    count_names_na=sapply(dataset_with_na$dat, function(x) sum(is.na(x)))
    count_names_na=count_names_na[which(count_names_na>0)]
    count_names_na=if(length(count_names_na)==0){"non"}else(count_names_na)
    if(count_names_na[1]!="non"){
      count=as.list(count_names_na)
      list=rep(NA,length(count))
      for(i in 1:length(count)){
        list[i]=paste(" ",names(count[i]),"=",count[i],"NA,","")
      }
      output=paste(list,collapse = "")
      shiny::showNotification(paste("Variables containing NA:",
                                    output,"Rows with Na will be deleted"),
                              type="error",closeButton=TRUE,duration=NULL)
    }
  })    
  
  observeEvent(input$button,{
    count_names_infinite=sapply(dataset_with_na$dat, function(x) sum(is.infinite(x)))
    count_names_infinite=count_names_infinite[which(count_names_infinite>0)]
    count_names_infinite=if(
      length(count_names_infinite)==0){"non"}else(count_names_infinite)
    if(count_names_infinite[1]!="non"){
      count=as.list(count_names_infinite)
      list=rep(NA,length(count))
      for(i in 1:length(count)){
        list[i]=paste(" ",names(count[i]),"=",count[i],"Inf/-Inf","",",")
      }
      output=paste(list,collapse = "")
      shiny::showNotification(paste("Variables containing Inf/-Inf:",
                                    output,"Rows with Inf/-Inf will be deleted"),
                              type="error",closeButton=TRUE,duration=NULL)
    }
  })  
  
  
  
  
  output$comparison_local=renderText({
    paste("Local Validity Measures For Comparison:",input$Secondary_validity)
  })
  output$comparison_global=renderText({
    paste("Global Validity Measures For Comparison:",input$Secondary_validity)
  })
  
  
  output$trans=renderText({
    list.out=list(names(daten$dat)[which(sapply(daten$dat,is.numeric)==FALSE)])
    list.out=ifelse(identical(list.out[[1]], character(0)),"",list.out)
    list.out=paste(list.out[[1]],collapse = ",")
    paste("The following variables have been identified containing values that are not numeric. Transformation is therefore prohibited for variables:",
              list.out,sep="<br/>")
          #list(names(daten$dat)[which(sapply(daten$dat,is.numeric)==FALSE)]))
  })
  
  output$trans2=renderText({
    list.out=list(
      names(
        select(daten$dat,
               which(sapply(daten$dat,is.numeric)==TRUE)))[
                 which(
                   apply(select(daten$dat,
                                which(sapply(daten$dat,is.numeric)==TRUE)),
                         MARGIN=2,function(x){prod(x)==0})#apply
                 )#which
                 ]#which
    )#list
    
    list.out=ifelse(identical(list.out[[1]], character(0)),"",list.out)
    list.out=paste(list.out[[1]],collapse = ",")
    paste("WARNING: The transformation of variables containing values of 0 is prohibited. Therefore, values of 0 are changed to 0.1 to successfully apply transformation to the selected variables:",
          list.out,sep="<br/>"
    )#paste
  })#renderText
  
  output$trans4=renderText({
    list.out=list(
      setdiff(
        list.append(input$selectize_comp,input$selectize),not_negative_values$list))
    list.out=ifelse(is_empty(list.out) ,"",list.out)
    list.out=paste(list.out[[1]],collapse = ",")
    paste("WARNING: The transformation of variables containing neagativ values
          in results is prohibited:",
          list.out,sep="<br/>"
    )#paste
  })#renderText
  
  #output dataset 1
  output$datas=renderDataTable(daten$dat,options = list(scrollX = TRUE))
  
  # with color but if we append color scrollbar wouldnt work both togeather are not
  #compatible!!!!
  # # output dataset 2
  # output$datas2=DT::renderDataTable(datatable(daten2$dat)%>% formatStyle(
  #   c('variable_name_plotmeans',"cluster_number_plotmeans","cluster_center_plotmeans",
  #     "cluster_id_plotmeans"),
  #   backgroundColor = 'yellow')
  #   ,options = list(scrollX = TRUE))
  
  # output dataset 2
  output$datas2=DT::renderDataTable(daten2$dat
                                    ,options = list(scrollX = TRUE))
  
  output$guetemasse=renderDT(guete$guete_lokal,options = list(scrollX = TRUE))
  output$guetemasse2=renderDT(guete$guete_global,options = list(scrollX = TRUE))
  
  output$guetemasse_comparative=renderDT(
    validitymeasures[[input$Secondary_validity]][1:8,1:(
      amount_k_validity[[input$Secondary_validity]]+1)]  ,
    options = list(scrollX = TRUE) 
  )
  
  output$guetemasse2_comparative=renderDT(
    validitymeasures[[input$Secondary_validity]][1:10,(
      amount_k_validity[[input$Secondary_validity]]+2):(
        amount_k_validity[[input$Secondary_validity]]+3)] , 
    options = list(scrollX = TRUE) 
  )
  
  # Einzelne Clusterplot auf je einer separierten Maps
  output$subplot1=renderPlot({
    if(daten2$dat$lon[1]=="Na" ) 
      stop(print("Geographical output requires geospatial data. 
                 Data set must include longitude and latitude to produce visuals."))
    
    satmap <- get_stamenmap(bbox = c(left = min(daten2$dat$lon)*(0.98),
                                     bottom = min(daten2$dat$lat)*(0.98),
                                     right = max(daten2$dat$lon)*1.02,
                                     top = max(daten2$dat$lat)*1.02),
                            zoom= 7, maptype = "toner-background") #terrain-background
    
    ggmap(satmap)+geom_point(data=daten2$dat,aes(x=lon,y=lat,color=sil))+scale_color_gradientn(colours=c(low = "red",high = "blue"))+
      facet_wrap(~Cluster+Average_Silhouette_Width,labeller = label_context
      )
  })
  
  
  output$subplot2=renderPlot({
    if (input$methode=="kmeans" || input$methode=="Mclust" || input$methode=="clara" ){
      #cl$cl==method out bei diesen methoden
      cl$cl$cluster=as.factor(cl$cl$cluster)
      fviz_cluster(cl$cl,select(daten2$dat,
                                setdiff(
                                  list.append(
                                    input$selectize,input$selectize_comp,ilr_names$name),
                                  eliminated_dimension$list)),
                   ellipse.type = "norm")}
    # muss separiert werden da nicht mit fviz_cluster kompatibel
    else if (input$methode=="pam" & length(non_numeric_list$nonnumericlist)==0){
      pam=method_out$method
      fviz_cluster(
        pam,
        select(
          daten2$dat,
          setdiff(
            list.append(
              input$selectize,input$selectize_comp,ilr_names$name),
            eliminated_dimension$list)), ellipse.type = "norm")
    }
    else if (input$methode=="pam" & length(non_numeric_list$nonnumericlist)>0 ){
      #gower_dist=daisy(select(daten2$dat,setdiff(list.append(
      #input$selectize,input$selectize_comp,ilr_names$name),eliminated_dimension$list)),
      #metric = "gower")
      gower_dist=d_matrix$mat
      tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
      tsne_data <- tsne_obj$Y %>%
        data.frame() %>%
        setNames(c("Dim1", "Dim2")) %>%
        mutate(Cluster = factor(daten2$dat$Cluster)
        )
      ggplot(aes(x = Dim1, y = Dim2), data = tsne_data) +
        geom_point(aes(color = Cluster))+
        ggtitle("Multi-Dimensional Reduction and Visualisation with t-SNE")
    }
    else if (input$methode=="cmeans"){
      fviz_cluster(list(
        data=select(
          daten2$dat,
          setdiff(
            list.append(
              input$selectize,input$selectize_comp,ilr_names$name),
            eliminated_dimension$list)), cluster=cl$cl$cluster),
        ellipse.type = "norm",
        ggtheme = theme_minimal())
    }
    
    else if (input$methode=="diana"){
      if(length(non_numeric_list$nonnumericlist)==0){
        # Visualize cluster
        diana=method_out$method
        fviz_cluster(diana, ellipse.type = "norm")
      }
      else if(length(non_numeric_list$nonnumericlist)>0){
        #gower_dist=daisy(select(daten2$dat,setdiff(list.append(input$selectize,
        #input$selectize_comp,ilr_names$name),eliminated_dimension$list)),
        #metric = "gower")
        gower_dist=d_matrix$mat
        tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
        tsne_data <- tsne_obj$Y %>%
          data.frame() %>%
          setNames(c("Dim1", "Dim2")) %>%
          mutate(Cluster = factor(daten2$dat$Cluster)
          )
        ggplot(aes(x = Dim1, y = Dim2), data = tsne_data) +
          geom_point(aes(color = Cluster))+
          ggtitle("Multi-Dimensional Reduction and Visualisation with t-SNE")
      }#elseif
    }
    
    else if (input$methode=="agnes"){
      if(length(non_numeric_list$nonnumericlist)==0){
        # Visualize cluster
        agnes=method_out$method
        fviz_cluster(agnes, ellipse.type = "norm")
      }
      else if(length(non_numeric_list$nonnumericlist)>0){
        gower_dist=d_matrix$mat
        #tsne because factor variable
        tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
        
        tsne_data <- tsne_obj$Y %>%
          data.frame() %>%
          setNames(c("Dim1", "Dim2")) %>%
          mutate(Cluster = factor(daten2$dat$Cluster)
          )
        ggplot(aes(x = Dim1, y = Dim2), data = tsne_data) +
          geom_point(aes(color = Cluster))+
          ggtitle("Multi-Dimensional Reduction and Visualisation with t-SNE")
      }#elseif
    }#agnes
    
    else if (input$methode=="hclust") {
      if(length(non_numeric_list$nonnumericlist)==0){
        # Visualize cluster
        hclust=method_out$method
        fviz_cluster(hclust, ellipse.type = "norm")
      }#if
      
      # wenn nicht nummerische Daten ausgew?hrtet werden,
      # muss eine Dimensionsreduktion mittels Tsne durchgef?hrt werden.
      else if(length(non_numeric_list$nonnumericlist)>0){

        #gower_dist=daisy(select(daten2$dat,setdiff(list.append(input$selectize,
        #input$selectize_comp,ilr_names$name),eliminated_dimension$list)),
        #metric = "gower")
        gower_dist=d_matrix$mat
        tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
        
        tsne_data <- tsne_obj$Y %>%
          data.frame() %>%
          setNames(c("Dim1", "Dim2")) %>%
          mutate(Cluster = factor(daten2$dat$Cluster)
          )
        ggplot(aes(x = Dim1, y = Dim2), data = tsne_data) +
          geom_point(aes(color = Cluster))+
          ggtitle("Multi-Dimensional Reduction and Visualisation with t-SNE")
      }#elseif
    }#hclust
  })#renderplot
  
  output$subplot3=renderPlot({
    # Silhouetteplot strukturierung stark: 0.75<s(o)<
    # mittel: 0.5<s(o)<0.75
    # schwach 0.25<s(o)<0.5 
    # keine struktur 0<s(o)<0.25
    fviz_silhouette(sil$sil,print.summary = FALSE)                                          
  }) 
  
  
}