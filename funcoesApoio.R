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