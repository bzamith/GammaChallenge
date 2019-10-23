library("stringr")

# Le base de medicos
# Tem que ter excluido o comecinho
# Tem que ter colocado os meses em ingles
# Tem que trocar ; por ,
medicos = read.csv("RH- Médicos.csv",stringsAsFactors = FALSE)

# Corrige nome colunas
colunas = c('Município')

for(i in 2:ncol(medicos)){
  atual = colnames(medicos)[i]
  year = substr(atual,2,5)
  month = substr(atual,7,9)
  nome_correto = paste(year,str_pad(match(month,month.abb),2,pad="0"),"01",sep="")
  colunas = c(colunas,nome_correto)
}

colnames(medicos) = colunas

# Combina com a base de apac_linfoma
medicos$AP_UFMUN = substr(medicos$Município,1,6)
apac_linfoma$qtd_medicos = NA

for(i in 1:nrow(apac_linfoma)){
  atual = apac_linfoma[i,'AP_UFMUN']
  atual2 = apac_linfoma[i,'AP_CMP']
  atual = grep(atual,medicos$Município)
  apac_linfoma[i,'qtd_medicos'] = medicos[atual,atual2] 
}

# Le base de leitos
# Tem que ter excluido o comecinho
# Tem que ter colocado os meses em ingles
# Tem que trocar ; por ,
leitos = read.csv("RF- Leitos de Internação.csv",stringsAsFactors = FALSE,quote="")

# Corrige nome colunas
colunas = c('Município')

for(i in 2:ncol(leitos)){
  atual = colnames(leitos)[i]
  year = substr(atual,3,6)
  month = substr(atual,8,10)
  nome_correto = paste(year,str_pad(match(month,month.abb),2,pad="0"),"01",sep="")
  colunas = c(colunas,nome_correto)
}

colnames(leitos) = colunas

# Combina com a base de apac_linfoma
leitos$AP_UFMUN = substr(leitos$Município,3,7)
apac_linfoma$qtd_leitos = NA

for(i in 1:nrow(apac_linfoma)){
  atual = apac_linfoma[i,'AP_UFMUN']
  atual2 = apac_linfoma[i,'AP_CMP']
  atual = grep(atual,leitos$Município)
  apac_linfoma[i,'qtd_leitos'] = leitos[atual,atual2] 
}

# Limpa
rm(atual)
rm(atual2)
rm(colunas)
rm(month)
rm(nome_correto)
rm(year)
rm(i)

#Escreve CSVs
write.csv(apac_linfoma,"apac_linfoma_unificado2.csv")