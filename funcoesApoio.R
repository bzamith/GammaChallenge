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
get_month_dif_mean = function(df_id){
  dif_meses = 0
  for(j in 1:nrow(df_id)){
    #Processamento
    ano_proc = as.numeric(substr(df_id[j,'AP_MVM'], 1,4))
    mes_proc = as.numeric(substr(df_id[j,'AP_MVM'], 5,6))
    #Atendimento
    ano_atend = as.numeric(substr(df_id[j,'AP_CMP'], 1,4))
    mes_atend = as.numeric(substr(df_id[j,'AP_CMP'], 5,6))
    
    dif_meses = dif_meses + (ano_proc - ano_atend)*12 + (mes_proc - mes_atend)
  }
  #cat(dif_meses)
  return(dif_meses/nrow(df_id))
}

#Cria a base de dados referente às primeiras APACs 
cria_df_primeira_apac = function(df){
  df = na.omit(df, cols="AP_CNSPCN")
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
    line_to_add$QTD_LEITOS = mean(df_id$QTD_LEITOS)
    line_to_add$QTD_MEDICOS = mean(df_id$QTD_MEDICOS)
    line_to_add$QTD_HESP = mean(df_id$QTD_HESP)
    line_to_add$QTD_HGERAL = mean(df_id$QTD_HGERAL)
    line_to_add$QTD_CESP = mean(df_id$QTD_CESP)
    line_to_add$QTD_UBS = mean(df_id$QTD_UBS)
    line_to_add$QTD_UADT = mean(df_id$QTD_UADT)
    
    #LO_P LA_P LO_H LA_H DISTANCIA
    #Distancias
    line_to_add$MIN_DIST = min(df_id$DISTANCIA)
    line_to_add$MEAN_DIST = mean(df_id$DISTANCIA)
    line_to_add$MAX_DIST = max(df_id$DISTANCIA)
    
    #AP_MVM = data do processamento
    #AP_CMP = data do atendimento
    #print(df_id[,c('AP_MVM', 'AP_CMP', 'AP_AUTORIZ')], digits = 12)
    line_to_add$MEDIA_MES_ATEND = get_month_dif_mean(df_id)
    
    line_to_add$NRO_DIF_HOSP = length(unique(df_id$AP_CODUNI))
    
    if(i==1){
      df_final = line_to_add
    }else{
      df_final = rbind(df_final,line_to_add)
    }
  }
  return(df_final)
}

# Converte nome das colunas para data
cols_data = function(dataset){
  colunas = c('Município')
  for(i in 2:ncol(dataset)){
    atual = colnames(dataset)[i]
    year = substr(atual,3,6)
    month = substr(atual,8,10)
    nome_correto = paste(year,str_pad(match(month,month.abb),2,pad="0"),"01",sep="")
    colunas = c(colunas,nome_correto)
  }
  colnames(dataset) = colunas
  dataset$AP_UFMUN = substr(dataset$Município,1,6)
  return(dataset)
}

# Cruza tabelas
cruza = function(apac_linfoma){
  for(i in 1:nrow(apac_linfoma)){
    municipio_apac = apac_linfoma[i,'AP_MUNPCN']
    i_municipio_dataset = grep(municipio_apac,medicos$Município)
    if(length(i_municipio_dataset)!=0){
      data_apac = apac_linfoma[i,'AP_CMP']
      apac_linfoma[i,'QTD_MEDICOS'] = medicos[i_municipio_dataset,data_apac]
      apac_linfoma[i,'QTD_LEITOS'] = leitos[i_municipio_dataset,data_apac]
      apac_linfoma[i,'QTD_HESP'] = hesp[i_municipio_dataset,data_apac]
      apac_linfoma[i,'QTD_HGERAL'] = hgeral[i_municipio_dataset,data_apac]
      apac_linfoma[i,'QTD_CESP'] = cesp[i_municipio_dataset,data_apac]
      apac_linfoma[i,'QTD_UBS'] = ubs[i_municipio_dataset,data_apac]
      apac_linfoma[i,'QTD_UADT'] = uadt[i_municipio_dataset,data_apac]
    }
    else{
        apac_linfoma[i,'QTD_MEDICOS'] = -1
        apac_linfoma[i,'QTD_LEITOS'] = -1
        apac_linfoma[i,'QTD_HESP'] = -1
        apac_linfoma[i,'QTD_HGERAL'] = -1
        apac_linfoma[i,'QTD_CESP'] = -1
        apac_linfoma[i,'QTD_UBS'] = -1
        apac_linfoma[i,'QTD_UADT'] = -1
    }
    cat(i)
    cat(" ")
  }
  return(apac_linfoma)
}

# Retorna UF
retorna_UF = function(dataset){
  for(i in 1:nrow(dataset)){
    cod_uf = substr(dataset[i,'AP_MUNPCN'],1,2)
    if(cod_uf=='11')
      dataset[i,'UF']='RO'
    else if(cod_uf=='12')
      dataset[i,'UF']='AC'
    else if(cod_uf=='13')
      dataset[i,'UF']='AM'
    else if(cod_uf=='14')
      dataset[i,'UF']='RR'
    else if(cod_uf=='15')
      dataset[i,'UF']='PA'
    else if(cod_uf=='16')
      dataset[i,'UF']='AP'
    else if(cod_uf=='17')
      dataset[i,'UF']='TO'
    else if(cod_uf=='21')
      dataset[i,'UF']='MA'
    else if(cod_uf=='22')
      dataset[i,'UF']='PI'
    else if(cod_uf=='23')
      dataset[i,'UF']='CE'
    else if(cod_uf=='24')
      dataset[i,'UF']='RN'
    else if(cod_uf=='25')
      dataset[i,'UF']='PB'
    else if(cod_uf=='26')
      dataset[i,'UF']='PE'
    else if(cod_uf=='27')
      dataset[i,'UF']='AL'
    else if(cod_uf=='28')
      dataset[i,'UF']='SE'
    else if(cod_uf=='29')
      dataset[i,'UF']='BA'
    else if(cod_uf=='31')
      dataset[i,'UF']='MG'
    else if(cod_uf=='32')
      dataset[i,'UF']='ES'
    else if(cod_uf=='33')
      dataset[i,'UF']='RJ'
    else if(cod_uf=='35')
      dataset[i,'UF']='SP'
    else if(cod_uf=='41')
      dataset[i,'UF']='PR'
    else if(cod_uf=='42')
      dataset[i,'UF']='SC'
    else if(cod_uf=='43')
      dataset[i,'UF']='RS'
    else if(cod_uf=='50')
      dataset[i,'UF']='MS'
    else if(cod_uf=='51')
      dataset[i,'UF']='MT'
    else if(cod_uf=='52')
      dataset[i,'UF']='GO'
    cat(i)
    cat(" ")
  }
  return(dataset)
}

pega_distancias = function(dataset,municipios){
  for(i in 1:nrow(dataset)){
    pessoa = grep(dataset[i,'AP_MUNPCN'],municipios$codigo_ibge)
    hospital = grep(dataset[i,'AP_UFMUN'],municipios$codigo_ibge)
    dataset[i,'LO_P'] = -1
    dataset[i,'LA_P'] = -1
    dataset[i,'LO_H'] = -1
    dataset[i,'LA_H'] = -1
    if(length(pessoa)!=0){
      dataset[i,'LO_P'] = municipios[pessoa,'longitude']
      dataset[i,'LA_P'] = municipios[pessoa,'latitude']
    }
    if(length(hospital)!=0){
      dataset[i,'LO_H'] = municipios[hospital,'longitude']
      dataset[i,'LA_H'] = municipios[hospital,'latitude']
    }
    dataset[i,'DISTANCIA'] = -1
    if(length(pessoa)!=0 && length(hospital)){
      if(pessoa == hospital){
        dataset[i,'DISTANCIA'] = 0.0
      }
      else{
        dataset[i,'DISTANCIA'] = distm(c(dataset[i,'LO_P'],dataset[i,'LA_P']),c(dataset[i,'LO_H'],dataset[i,'LA_H']),fun = distHaversine)[1]/1000
      }
    }
    cat(i)
    cat(" ")
  }
  return(dataset)
}
