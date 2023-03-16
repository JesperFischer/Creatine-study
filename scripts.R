##scripts to analysis:

pacman::p_load(tidyverse,MANOVA.RM, janitor)

get_data = function(){
  
  data = read.csv("final_data1.csv")
  data$tid = as.factor(data$tid)
  data$gruppe = as.factor(data$gruppe)
  data$id = as.factor(data$id)
  return(data)
  
}


get_data1 = function(){
  #function to get data from raw-files
  setwd("C:/Users/au645332/Documents/creatine-raw/realdeal")
  videoer = readxl::read_xlsx("Videotider.xlsx")
  dag1 = readxl::read_xlsx("data forsøgs dag 1real.xlsx")
  dag2 = readxl::read_xlsx("Data forsøgsdag 2real.xlsx")
  
  dag1$tid = 1
  dag2$tid = 2
  
  datax = rbind(dag1,dag2)
  datax = inner_join(datax,videoer, by = c("id","tid"))
  
  datax$pullups_r = as.numeric(datax$pullups_r)
  datax$pullupstid = as.numeric(datax$pullupstid)
  datax$lattice_r = as.numeric(datax$lattice_r)
  datax$ll_r = as.numeric(datax$ll_r)
  datax$lr_r = as.numeric(datax$lr_r)
  
  
  datax$HC1 = as.numeric(datax$HC1)
  datax$HC2 = as.numeric(datax$HC2)
  datax$VC1 = as.numeric(datax$VC1)
  datax$VC2 = as.numeric(datax$VC2)
  datax$`pull ups` = as.numeric(datax$`pull ups`)
  datax$lattice = as.numeric(datax$lattice)
  datax$LL = as.numeric(datax$LL)
  datax$LR = as.numeric(datax$LR)
  datax$id = as.factor(datax$id)
  datax$gruppe = as.factor(datax$gruppe)
  datax$tid = as.factor(datax$tid)
  datax$BW = as.numeric(datax$BW)
  datax$`moon-tal` = as.numeric(datax$`moon-tal`)
  datax$`moon-sess` = as.numeric(datax$`moon-sess`)
  datax$VP1 = as.numeric(datax$VP1)
  datax$VP2 = as.numeric(datax$VP2)
  
  setwd("C:/Users/au645332/Documents/creatine-raw/realdeal")
  aaa = read.csv("ROFD_max.csv")
  aaa$X = NULL
  aaa$id = as.factor(aaa$id)
  aaa$tid = as.factor(aaa$tid)
  
  
  
  datax = inner_join(aaa,datax, by=c("id","tid"))
  
  
  datax$crimphøjremax = pmax.int(datax$ch_1, datax$ch_2, na.rm = T)
  
  datax$crimph1 = datax$crimphøjremax == datax$ch_1
  datax$crimph2 = datax$crimphøjremax == datax$ch_2
  
  datax$crimph1 = ifelse(datax$crimph1 == TRUE, 1,0)
  datax$crimph2 = ifelse(datax$crimph2 == TRUE, 2,0)
  
  datax$testnumberhøjrecrimp = pmax.int(datax$crimph1, datax$crimph2, na.rm = T)
  
  datax$crimpvenstremax = pmax.int(datax$cv_1, datax$cv_2, na.rm = T)
  
  datax$crimph1 = datax$crimpvenstremax == datax$cv_1
  datax$crimph2 = datax$crimpvenstremax == datax$cv_2
  
  datax$crimph1 = ifelse(datax$crimph1 == TRUE, 1,0)
  datax$crimph2 = ifelse(datax$crimph2 == TRUE, 2,0)
  
  datax$testnumbervenstrecrimp = pmax.int(datax$crimph1, datax$crimph2, na.rm = T)
  
  datax$pinchhøjremax = pmax.int(datax$ph_1, datax$ph_2, na.rm = T)
  
  datax$crimph1 = datax$pinchhøjremax == datax$ph_1
  datax$crimph2 = datax$pinchhøjremax == datax$ph_2
  
  datax$crimph1 = ifelse(datax$crimph1 == TRUE, 1,0)
  datax$crimph2 = ifelse(datax$crimph2 == TRUE, 2,0)
  
  datax$testnumberpinchhøjre = pmax.int(datax$crimph1, datax$crimph2, na.rm = T)
  
  datax$pinchvenstremax = pmax.int(datax$pv_1, datax$pv_2, na.rm = T)
  
  datax$crimph1 = datax$pinchvenstremax == datax$pv_1
  datax$crimph2 = datax$pinchvenstremax == datax$pv_2
  
  datax$crimph1 = ifelse(datax$crimph1 == TRUE, 1,0)
  datax$crimph2 = ifelse(datax$crimph2 == TRUE, 2,0)
  
  datax$testnumberpinchvenstre = pmax.int(datax$crimph1, datax$crimph2, na.rm = T)
  
  
  datax = datax %>% 
    mutate(pullups_r = coalesce(pullups_r,`pull ups`),lattice_r = coalesce(lattice_r,lattice),ll_r = coalesce(ll_r,LL),lr_r = coalesce(lr_r,LR))
  
  cols = c(1:11,13,14,29,31,32,33,35,41,42,43,44,45,46,47,48,52,54,56)
  
  data = datax[,cols]
  
  data$hcbw = data$crimphøjremax/data$BW
  data$vcbw = data$crimpvenstremax/data$BW
  
  data$hpbw = data$pinchhøjremax/data$BW
  
  data$vpbw = datax$pinchvenstremax/data$BW
  
  data$llbw = data$ll_r/data$BW
  
  data$lrbw = data$lr_r/data$BW
  
  data$temp = as.numeric(data$temp)
  data$humidity = as.numeric(data$humidity)
  
  data$kod = as.factor(data$kod)
  
  #data = data %>% 
  #  group_by(id) %>% 
    #fill(color, age, gender) %>% #default direction down
  #  tidyr::fill(højde, span, .direction = "downup")
  
  #data$ape = data$span- data$højde
  return(data)
  
}


get_summary = function(data){
  table = data %>% 
    filter(gruppe == 1) %>% 
    group_by(tid) %>% 
    summarize(n = n(),
              crimpright = round(mean(crimphøjremax),0), 
              ch_sd = round(sd(crimphøjremax)/sqrt(n),1), 
              crimpleft = round(mean(crimpvenstremax, na.rm =T),0), 
              cv_sd = round(sd(crimpvenstremax, na.rm =T)/sqrt(n),1),
              lattice = round(mean(lattice_r, na.rm = T),0),
              lattice_sd = round(sd(lattice_r, na.rm = T)/sqrt(n),1),
              lockoffleft = round(mean(ll_r, na.rm = T),0),
              ll_sd = round(sd(ll_r, na.rm = T)/sqrt(n),1),
              lockoffright = round(mean(lr_r, na.rm = T),0),
              lr_sd = round(sd(lr_r, na.rm = T)/sqrt(n),1),
              moontal = round(mean(moon.tal, na.rm = T),0),
              moontal_sd = round(sd(moon.tal, na.rm = T)/sqrt(n),1),
              Bw = round(mean(BW, na.rm = T),0),
              BW_sd = round(sd(BW, na.rm = T)/sqrt(n),1),
              pullups = round(mean(pullups_r, na.rm = T),0),
              pullups_sd = round(sd(pullups_r, na.rm = T)/sqrt(n),1),
              pinchhøjre = round(mean(pinchhøjremax, na.rm = T),0),
              pinchhøjre_sd = round(sd(pinchhøjremax, na.rm = T)/sqrt(n),1),
              pinchvenstre = round(mean(pinchvenstremax, na.rm = T),0),
              pinchvenstre_sd = round(sd(pinchvenstremax, na.rm = T)/sqrt(n),1)
              
              )
  

  ncol = ncol(table)
  for (i in 1:as.integer((ncol-2)/2)){
    for (ii in 1:nrow(table)){
      table[ii,ncol+i] = paste(table[ii,i*2+1],"\u00B1",table[ii,i*2+2])
    }}
  names = names(table)
  realnames = names[seq(3,ncol, by = 2)]
  names(table)[(ncol+1):ncol(table)] = realnames
  table = table[,c(1,(ncol+1):ncol(table))]
  
  
  
  table2 = data %>% 
    filter(gruppe == 2) %>% 
    group_by(tid) %>% 
    summarize(n = n(),
              crimpright = round(mean(crimphøjremax),0), 
              ch_sd = round(sd(crimphøjremax)/sqrt(n),1), 
              crimpleft = round(mean(crimpvenstremax, na.rm =T),0), 
              cv_sd = round(sd(crimpvenstremax, na.rm =T)/sqrt(n),1),
              lattice = round(mean(lattice_r, na.rm = T),0),
              lattice_sd = round(sd(lattice_r, na.rm = T)/sqrt(n),1),
              lockoffleft = round(mean(ll_r, na.rm = T),0),
              ll_sd = round(sd(ll_r, na.rm = T)/sqrt(n),1),
              lockoffright = round(mean(lr_r, na.rm = T),0),
              lr_sd = round(sd(lr_r, na.rm = T)/sqrt(n),1),
              moontal = round(mean(moon.tal, na.rm = T),0),
              moontal_sd = round(sd(moon.tal, na.rm = T)/sqrt(n),1),
              Bw = round(mean(BW, na.rm = T),0),
              BW_sd = round(sd(BW, na.rm = T)/sqrt(n),1),
              pullups = round(mean(pullups_r, na.rm = T),0),
              pullups_sd = round(sd(pullups_r, na.rm = T)/sqrt(n),1),
              pinchhøjre = round(mean(pinchhøjremax, na.rm = T),0),
              pinchhøjre_sd = round(sd(pinchhøjremax, na.rm = T)/sqrt(n),1),
              pinchvenstre = round(mean(pinchvenstremax, na.rm = T),0),
              pinchvenstre_sd = round(sd(pinchvenstremax, na.rm = T)/sqrt(n),1))
  
  
  ncol = ncol(table2)
  for (i in 1:as.integer((ncol-2)/2)){
    for (ii in 1:nrow(table2)){
      table2[ii,ncol+i] = paste(table2[ii,i*2+1],"\u00B1",table2[ii,i*2+2])
    }}
  names = names(table2)
  realnames = names[seq(3,ncol, by = 2)]
  names(table2)[(ncol+1):ncol(table2)] = realnames
  table2 = table2[,c(1,(ncol+1):ncol(table2))]
 
  
  table = rbind(table,table2)
  
  table$tid = as.factor(table$tid)
  levels(table$tid) = c("pre","post")
  
  table = as.data.frame(t(table))
  
     
  
  table = table %>%
    row_to_names(row_number = 1)
    
  return(table)
}


get_summary_general = function(data){
  table = data %>% 
    filter(gruppe == 1) %>% 
    summarize(n = n(),
              age = round(mean(alder2,na.rm =T),0), 
              age_sd = round(sd(alder2,na.rm =T),1),
              experience = round(mean(klatre_r2,na.rm =T),0),
              experience_sd = round(sd(klatre_r2,na.rm =T),1),
              self_rep_pull = round(mean(self_report_pullup,na.rm =T),0),
              self_rep_pull_sd = round(sd(self_report_pullup,na.rm =T),1),
              sport_grad1 = round(mean(sport_grad,na.rm =T),0),
              sport_grad_sd = round(sd(sport_grad,na.rm =T),1),
              boulder_grad = round(mean(bouldering_grad,na.rm =T),0),
              boulder_grad_sd = round(sd(bouldering_grad,na.rm =T),1),
              height = round(mean(højde,na.rm =T),0),
              height_sd = round(sd(højde,na.rm =T),1),
              ape_index = round(mean(span-højde,na.rm =T),0),
              ape_index_sd = round(sd(span-højde,na.rm =T),1)
              
              
    )
  
  
  ncol = ncol(table)
  for (i in 1:as.integer((ncol-1)/2)){
    for (ii in 1:nrow(table)){
      table[ii,ncol+i] = paste(table[ii,i*2],"\u00B1",table[ii,i*2+1])
    }}
  names = names(table)
  realnames = names[seq(2,ncol, by = 2)]
  names(table)[(ncol+1):ncol(table)] = realnames
  table = table[,c(1,(ncol+1):ncol(table))]
  
  
  
  table2 = data %>% 
    filter(gruppe == 2) %>% 
    summarize(n = n(),
              age = round(mean(alder2,na.rm =T),0), 
              age_sd = round(sd(alder2,na.rm =T),1),
              experience = round(mean(klatre_r2,na.rm =T),0),
              experience_sd = round(sd(klatre_r2,na.rm =T),1),
              self_rep_pull = round(mean(self_report_pullup,na.rm =T),0),
              self_rep_pull_sd = round(sd(self_report_pullup,na.rm =T),1),
              sport_grad1 = round(mean(sport_grad,na.rm =T),0),
              sport_grad_sd = round(sd(sport_grad,na.rm =T),1),
              boulder_grad = round(mean(bouldering_grad,na.rm =T),0),
              boulder_grad_sd = round(sd(bouldering_grad,na.rm =T),1),
              height = round(mean(højde,na.rm =T),0),
              height_sd = round(sd(højde,na.rm =T),1),
              ape_index = round(mean(span-højde,na.rm =T),0),
              ape_index_sd = round(sd(span-højde,na.rm =T),1))
  
  
  ncol = ncol(table2)
  for (i in 1:as.integer((ncol-1)/2)){
    for (ii in 1:nrow(table2)){
      table2[ii,ncol+i] = paste(table2[ii,i*2],"\u00B1",table2[ii,i*2+1])
    }}
  names = names(table2)
  realnames = names[seq(2,ncol, by = 2)]
  names(table2)[(ncol+1):ncol(table2)] = realnames
  table2 = table2[,c(1,(ncol+1):ncol(table2))]
  
  table = rbind(table,table2)
  
  table$n = NULL
  
  table = as.data.frame(t(table))
  
  names(table) <- c("placebo","Creatine")
  
  
  return(table)
}
