##scripts to analysis:

pacman::p_load(tidyverse,MANOVA.RM)

get_data = function(){
  
  data = read.csv("final_data.csv")
  data$tid = as.factor(data$tid)
  data$gruppe = as.factor(data$gruppe)
  data$id = as.factor(data$id)
  return(data)
  
}


get_data1 = function(){
  #function to get data from raw-files
  setwd("C:/Users/au645332/Documents/Creatine-study/realdeal")
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
  
  setwd("C:/Users/au645332/Documents/Creatine-study/realdeal")
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
  
  cols = c(1:11,13,14,29,31,32,33,41,42,43,44,45,46,47,48,52,54,56)
  
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