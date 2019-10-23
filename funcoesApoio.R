# Converter datas de apac para data
convert_apac = function(dataset,colunas){
  for(j in 1:length(colunas)){
    if (nchar(dataset[1,colunas[j]])==6){
      for (i in 1:nrow(dataset)){
        if(!is.na(dataset[i,colunas[j]])){
          dataset[i,colunas[j]] = paste(substr(dataset[i,colunas[j]],1,6),"01",sep="")
        }
      }
    }
  }
  return(dataset)
}

# Converter datas de mortalidade para data
convert_mort = function(dataset,colunas){
  for (i in 1:nrow(dataset)){
    for(j in 1:length(colunas)){
      if(!is.na(dataset[i,colunas[j]])){
        if (nchar(dataset[i,colunas[j]])==7){
          dataset[i,colunas[j]] = paste(substr(dataset[i,colunas[j]],4,7),substr(dataset[i,colunas[j]],2,3),"0",substr(dataset[i,colunas[j]],1,1),sep="")
        }
        else if(nchar(dataset[i,colunas[j]])==8){
          dataset[i,colunas[j]] = paste(substr(dataset[i,colunas[j]],5,8),substr(dataset[i,colunas[j]],3,4),substr(dataset[i,colunas[j]],1,2),sep="")
        }
      }
    }
  }
  return(dataset)
}

#Cria a base de dados referente às primeiras APACs 
cria_df_primeira_apac = function(df){
  hashNumbers = df$AP_CNSPCN
  hashNumbersUnique = unique(hashNumbers)
  
  for(i in 1:length(hashNumbersUnique)){
    cat(i)
    cat(", ")
    id = hashNumbersUnique[i]
    df_id = df[df$AP_CNSPCN == id,]
    min_date = min(df_id$AP_CMP)
    line_to_add = df_id[df_id$AP_CMP == min_date,][1,]
    line_to_add$MIN_VAL = min(df_id$AP_VL_AP)
    line_to_add$MAX_VAL = max(df_id$AP_VL_AP)
    line_to_add$MEAN_VAL = mean(df_id$AP_VL_AP)
    line_to_add$NUM_APACS = length(df_id$AP_CMP)
    line_to_add$QTD_LEITOS = mean(df_id$qtd_leitos)
    line_to_add$QTD_MEDICOS = mean(df_id$qtd_medicos)
    
    if(i==1){
      df_final = line_to_add
    }else{
      df_final = rbind(df_final,line_to_add)
    }
  }
  return(df_final)
}
