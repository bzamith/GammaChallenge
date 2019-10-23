setwd("~/Documents/Faculdade/10o_Semestre/BCG Datathon/Banco Datathon/Banco Datathon")

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
