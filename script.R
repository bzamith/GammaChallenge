#=======================================================================================
# CRIA TABELA UNIFICADA
#=======================================================================================

# Le o Mortalidade Hodgkin
mort_hodgkin = read.csv("Mortalidade Linfoma de Hodgkin SIM-SUS.csv")
mort_naohodgkin = read.csv("Mortalidade Linfoma não Hodgkin SIM-SUS.csv")

mort_linfoma = rbind(mort_hodgkin,mort_naohodgkin)
mort_linfoma = convert_mort(mort_linfoma,c('DTOBITO','DTNASC'))

# Le Radio e Quimio Linfoma
# Tem que ter salvo os dados como UTF-8 no SUBLIME!
radio_linfoma = read.csv("Linfomas Radioterapia SIA-SUS.csv", stringsAsFactors = FALSE)
quimio_linfoma = read.csv("Linfomas Quimioterapia SIA-SUS.csv", stringsAsFactors = FALSE)

radio_linfoma_reduz = radio_linfoma[c('AP_MVM','AP_CONDIC','AP_GESTAO','AP_CODUNI','AP_AUTORIZ','AP_CMP',
                                      'AP_PRIPAL','AP_VL_AP','AP_UFMUN','AP_TPUPS','AP_TIPPRE','AP_CNPJCPF',
                                      'AP_CNPJMNT','AP_CNSPCN','AP_COIDADE','AP_NUIDADE','AP_SEXO','AP_RACACOR',
                                      'AP_MUNPCN','AP_UFNACIO','AP_CEPPCN','AP_UFDIF','AP_MNDIF','AP_DTINIC',
                                      'AP_DTFIM','AP_TPATEN','AP_TPAPAC','AP_MOTSAI','AP_OBITO','AP_APACANT',
                                      'AP_UNISOL','AP_DTSOLIC','AP_DTAUT','AP_CIDCAS','AP_CIDPRI','AP_CIDSEC',
                                      'AR_ESTADI','AR_DTIDEN','AR_TRANTE','AR_DTINTR')]
colnames(radio_linfoma_reduz) = c('AP_MVM','AP_CONDIC','AP_GESTAO','AP_CODUNI','AP_AUTORIZ','AP_CMP',
                                  'AP_PRIPAL','AP_VL_AP','AP_UFMUN','AP_TPUPS','AP_TIPPRE','AP_CNPJCPF',
                                  'AP_CNPJMNT','AP_CNSPCN','AP_COIDADE','AP_NUIDADE','AP_SEXO','AP_RACACOR',
                                  'AP_MUNPCN','AP_UFNACIO','AP_CEPPCN','AP_UFDIF','AP_MNDIF','AP_DTINIC',
                                  'AP_DTFIM','AP_TPATEN','AP_TPAPAC','AP_MOTSAI','AP_OBITO','AP_APACANT',
                                  'AP_UNISOL','AP_DTSOLIC','AP_DTAUT','AP_CIDCAS','AP_CIDPRI','AP_CIDSEC',
                                  'AP_ESTADI','AP_DTIDEN','AP_TRANTE','AP_DTINTR')

quimio_linfoma_reduz = quimio_linfoma[c('AP_MVM','AP_CONDIC','AP_GESTAO','AP_CODUNI','AP_AUTORIZ','AP_CMP',
                                        'AP_PRIPAL','AP_VL_AP','AP_UFMUN','AP_TPUPS','AP_TIPPRE','AP_CNPJCPF',
                                        'AP_CNPJMNT','AP_CNSPCN','AP_COIDADE','AP_NUIDADE','AP_SEXO','AP_RACACOR',
                                        'AP_MUNPCN','AP_UFNACIO','AP_CEPPCN','AP_UFDIF','AP_MNDIF','AP_DTINIC',
                                        'AP_DTFIM','AP_TPATEN','AP_TPAPAC','AP_MOTSAI','AP_OBITO','AP_APACANT',
                                        'AP_UNISOL','AP_DTSOLIC','AP_DTAUT','AP_CIDCAS','AP_CIDPRI','AP_CIDSEC',
                                        'AQ_ESTADI','AQ_DTIDEN','AQ_TRANTE','AQ_DTINTR')]
colnames(quimio_linfoma_reduz) = c('AP_MVM','AP_CONDIC','AP_GESTAO','AP_CODUNI','AP_AUTORIZ','AP_CMP',
                                   'AP_PRIPAL','AP_VL_AP','AP_UFMUN','AP_TPUPS','AP_TIPPRE','AP_CNPJCPF',
                                   'AP_CNPJMNT','AP_CNSPCN','AP_COIDADE','AP_NUIDADE','AP_SEXO','AP_RACACOR',
                                   'AP_MUNPCN','AP_UFNACIO','AP_CEPPCN','AP_UFDIF','AP_MNDIF','AP_DTINIC',
                                   'AP_DTFIM','AP_TPATEN','AP_TPAPAC','AP_MOTSAI','AP_OBITO','AP_APACANT',
                                   'AP_UNISOL','AP_DTSOLIC','AP_DTAUT','AP_CIDCAS','AP_CIDPRI','AP_CIDSEC',
                                   'AP_ESTADI','AP_DTIDEN','AP_TRANTE','AP_DTINTR')

apac_linfoma = rbind(radio_linfoma_reduz,quimio_linfoma_reduz)
apac_linfoma = convert_apac(apac_linfoma,c('AP_CMP'))

# Limpa
rm(mort_hodgkin)
rm(mort_naohodgkin)
rm(quimio_linfoma_reduz)
rm(quimio_linfoma)
rm(radio_linfoma_reduz)
rm(radio_linfoma)

#Escreve CSVs
write.csv(apac_linfoma,"apac_linfoma_unificado.csv")
write.csv(mort_linfoma,"mort_linfoma_unificado.csv")

#=======================================================================================
# CRUZA COM OUTRAS TABELAS
#=======================================================================================

library("stringr")
library("geosphere")
apac_linfoma$AP_CMP = as.character(apac_linfoma$AP_CMP)

# Le base
# Tem que ter excluido o comecinho
# Tem que ter colocado os meses em ingles
# Tem que trocar ; por ,
medicos = read.csv("RH- Médicos.csv",stringsAsFactors = FALSE,quote="")
medicos = cols_data(medicos)
medicos = medicos[order(medicos$Município),]

leitos = read.csv("RF- Leitos de Internação.csv",stringsAsFactors = FALSE,quote="")
leitos = cols_data(leitos)
leitos = leitos[order(leitos$Município),]

hesp = read.csv("Estabelecimentos- Hospital Especializado.csv",stringsAsFactors = FALSE,quote="")
hesp = cols_data(hesp)
hesp = hesp[order(hesp$Município),]

hgeral = read.csv("Estabelecimentos- Hospital Geral.csv",stringsAsFactors = FALSE,quote="")
hgeral = cols_data(hgeral)
hgeral = hgeral[order(hgeral$Município),]

cesp = read.csv("Estabelecimentos- Clínicas-Ambulatórios Especializados.csv",stringsAsFactors = FALSE,quote="")
cesp = cols_data(cesp)
cesp = cesp[order(cesp$Município),]

ubs = read.csv("Estabelecimentos- Unidade Básica de Saúde.csv",stringsAsFactors = FALSE,quote="")
ubs = cols_data(ubs)
ubs = ubs[order(ubs$Município),]

uadt = read.csv("Estabelecimentos- Unidade de Serviço de Apoio ao Diagnose e Terapia.csv",stringsAsFactors = FALSE,quote="")
uadt = cols_data(uadt)
uadt = uadt[order(uadt$Município),]

# Combina com a base de apac_linfoma
apac_linfoma = cruza(apac_linfoma)

apac_linfoma$QTD_MEDICOS[apac_linfoma$QTD_MEDICOS=='-'] = '0'
apac_linfoma$QTD_LEITOS[apac_linfoma$QTD_LEITOS=='-'] = '0'
apac_linfoma$QTD_HESP[apac_linfoma$QTD_HESP=='-'] = '0'
apac_linfoma$QTD_HGERAL[apac_linfoma$QTD_HGERAL=='-'] = '0'
apac_linfoma$QTD_CESP[apac_linfoma$QTD_CESP=='-'] = '0'
apac_linfoma$QTD_UBS[apac_linfoma$QTD_UBS=='-'] = '0'
apac_linfoma$QTD_UADT[apac_linfoma$QTD_UADT=='-'] = '0'
apac_linfoma$QTD_MEDICOS[apac_linfoma$QTD_MEDICOS=='-1'] = '0'
apac_linfoma$QTD_LEITOS[apac_linfoma$QTD_LEITOS=='-1'] = '0'
apac_linfoma$QTD_HESP[apac_linfoma$QTD_HESP=='-1'] = '0'
apac_linfoma$QTD_HGERAL[apac_linfoma$QTD_HGERAL=='-1'] = '0'
apac_linfoma$QTD_CESP[apac_linfoma$QTD_CESP=='-1'] = '0'
apac_linfoma$QTD_UBS[apac_linfoma$QTD_UBS=='-1'] = '0'
apac_linfoma$QTD_UADT[apac_linfoma$QTD_UADT=='-1'] = '0'

# Pega UF
apac_linfoma = retorna_UF(apac_linfoma)
apac_linfoma = apac_linfoma[,2:ncol(apac_linfoma)]

# Calcula distancias
# Fonte: https://github.com/kelvins/Municipios-Brasileiros
municipios = read.csv("municipios.csv")
municipios$codigo_ibge = substr(municipios$codigo_ibge,1,6)
apac_linfoma = pega_distancias(apac_linfoma,municipios)

#Escreve CSVs 
write.csv(apac_linfoma,"apac_linfoma_unificado2.csv")

#=======================================================================================
# PEGA APACS PRIMEIRAS
#=======================================================================================

apac_linfoma_unica = cria_df_primeira_apac(apac_linfoma)
