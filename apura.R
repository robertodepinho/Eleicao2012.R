library(XML)

munic = read.table("lista_municipios_justica_eleitoral.txt", head=T, stringsAsFactors=F, encoding="latin1", sep=";", quote="")

getMUNIC<-function(nome="SALVADOR", listaMu=munic){
  MUNIC = listaMu[nome==listaMu$NOME,"CÓDIGO"]
  return(MUNIC)
}


apuraMU <- function(
  UF="ba"
                    , 
  MUNIC="38490"
  ,
  PASTA= "~/Documents/source_what/eleicao/2012/divulgacao/oficial/48/distribuicao/"
  ) {
  #<UF><MUNIC>-<CCCC>-e<ELEICA>-v.xml
  arquivo = paste(UF,MUNIC,"-0011-e000483-v",sep="")
  print(arquivo)
  arquivoZip = paste(arquivo, ".zip", sep="")
  arquivoXml = paste(arquivo, ".xml", sep="")
  arquivoFull = paste(PASTA, tolower(UF),"/", arquivoZip, sep="")
  unzip(zipfile=arquivoFull)
  return(apuraPorZona(arquivoXml))
  
}


apuraPorZona <- function(
  resultFile = "sp71072-0011-e000473-v.xml"
  
  ) {
  
  
  
  
  
  #Carrega apuracao da zona
  result = xmlTreeParse(resultFile, useInternalNodes=T)
  r <- xmlRoot(result)
  #sapply(xmlChildren(r), xmlName)
  
  x = getNodeSet(doc=result,path="/Resultado/Abrangencia[@tipoAbrangencia='ZONA']")
  zona.result  = as.data.frame(t(sapply(x, xmlAttrs)), stringsAsFactors=F)
  
  zona.result$eleitoradoNaoApurado = as.numeric(zona.result$eleitoradoNaoApurado)
  zona.result$eleitoradoApurado = as.numeric(zona.result$eleitoradoApurado)
  
  zona.result$eleitorado = zona.result$eleitoradoApurado + zona.result$eleitoradoNaoApurado
  
  #Carrega resultados por zona
  #td for each zona
  zona.voto = data.frame()
  for(zonaCod in unique(zona.result$codigoAbrangencia)){
    pafi = paste("/Resultado/Abrangencia[@codigoAbrangencia='",zonaCod, "']/VotoCandidato", sep="")
    x = getNodeSet(doc=result,path=pafi)
    zv  = as.data.frame(t(sapply(x, xmlAttrs)), stringsAsFactors=F)
    zv$zonaCod = zonaCod
    zv$totalVotos = as.numeric(zv$totalVotos)
    zona.voto= rbind(zona.voto,zv)
  }
  
  #aggregate(totalVotos ~ numeroCandidato, zona.voto, sum)
  
  
  x = getNodeSet(doc=result,path="/Resultado/Abrangencia[@tipoAbrangencia='MU']/VotoCandidato")
  mu = as.data.frame(t(sapply(x, xmlAttrs)), stringsAsFactors=F)
  
  
  #estimativa
  zona.result$fator = zona.result$eleitorado / zona.result$eleitoradoApurado
  zona.voto = merge(zona.voto,zona.result[, c("codigoAbrangencia","fator")],by.x="zonaCod", by.y="codigoAbrangencia", all=T)
  zona.voto$totalVotosCorr = zona.voto$totalVotos * zona.voto$fator
  
  return(aggregate(cbind(totalVotos,totalVotosCorr) ~ numeroCandidato, zona.voto, sum))
  
}

apuraMU(UF="ba",MUNIC=getMUNIC("SALVADOR"))

apuraMU(UF="sp",MUNIC=getMUNIC("SÃO PAULO"))
