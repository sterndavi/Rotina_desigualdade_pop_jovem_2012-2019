
{library(PNADcIBGE)
  library(survey)
  library(convey)
  library(here)
  }


for (x in 2012:2019){
  
  y = "ginirendamaioque0"
  z = "ginirenda0"
  t = "rendamediadefl"
  q = "qsr"
  out = paste(here(),"/Output/", sep = "")
  data = paste(here(),"/input_PNADc_2012-2019/","PNADC_",x,"_visita1.txt", sep = "")
  input = paste(here(),"/input_PNADc_2012-2019/","input_PNADC_",x,"_visita1.txt", sep = "")
  dicionario = paste(here(),"/input_PNADc_2012-2019/","dicionario_PNADC_microdados_", x, "_visita1.xls", sep = "")
  
  
 
  
  dadosx = read_pnadc(data,input,
                     vars = c("V2009","V2010",
                              "VD4020","VD3004",
                              "VD4001","VD4002",
                              "VD4009","VD4017"))
  
  pnadc_labeller(dadosx, dicionario)
  
  pnadc_deflator(dadosx, paste(here(),"/input_PNADc_2012-2019/","deflator_PNADC_2019.xls",
                               sep = ""))
  
  dadosx <- pnadc_design(dadosx)
  
  dadosx <- convey_prep(dadosx)
  
###SUBSET#######################################################################

  #15-29 
  {  
  #x renda 15-29
  subxj15_29 <-
    subset(
      dadosx,
      !is.na( VD4020 ) & VD4020 >=0 & V2009 >=15 & V2009 <=29
    )
  
  #x desocup 15-29
  subdesocupxj15_29 <-
    subset(
      dadosx,
      !is.na( VD4002 ) & V2009 >=15 & V2009 <=29
    )
  
  #x pea 15-29
  subpeaxj15_29 <-
    subset(
      dadosx,
      !is.na( VD4001 ) & V2009 >=15 & V2009 <=29
    )
 
   #x informalidade
  
  subinfxj15_29 <-
    subset(
      dadosx,
      !is.na( VD4009 ) & V2009 >=15 & V2009 <=29
    )
  
  subcrenda015_29 <-
    subset(
      dadosx,
    !is.na(VD4020) & VD4020 >=0 & V2009 >=15 & V2009 <=29 & VD4001 == 1
    )
}
  
  #15-17
  {
    #x 15-17
    subxj15_17 <-
      subset(
        dadosx,
        !is.na( VD4020 ) & VD4020 >=0 & V2009 >=15 & V2009 <=17
      )
    
    #x desocup 15-17
    subdesocupxj15_17 <-
      subset(
        dadosx,
        !is.na( VD4002 ) & V2009 >=15 & V2009 <=17
      )
    
    #x pea 15-17
    subpeaxj15_17 <-
      subset(
        dadosx,
        !is.na( VD4001 ) & V2009 >=15 & V2009 <=17
      )
    
    #x informalidade
    
    subinfxj15_17 <-
      subset(
        dadosx,
        !is.na( VD4009 ) & V2009 >=15 & V2009 <=17
      )
    
    subcrenda015_17 <-
      subset(
        dadosx,
        !is.na(VD4020) & VD4020 >=0 & V2009 >=15 & V2009 <=17 & VD4001 == 1
      )
  }
  
  #18-24
  {
    #x 18-24
    subxj18_24 <-
      subset(
        dadosx,
        !is.na( VD4020 ) & VD4020 >=0 & V2009 >=18 & V2009 <=24
      )
    
    #x desocup 18-24
    subdesocupxj18_24 <-
      subset(
        dadosx,
        !is.na( VD4002 ) & V2009 >=18 & V2009 <=24
      )
    
    #x pea 18-24
    subpeaxj18_24 <-
      subset(
        dadosx,
        !is.na( VD4001 ) & V2009 >=18 & V2009 <=24
      )
  
    #informalidade x 18-24  
    subinfxj18_24 <-
      subset(
        dadosx,
        !is.na( VD4009 ) & V2009 >=18 & V2009 <=24
      )
    
    subcrenda018_24 <-
      subset(
        dadosx,
        !is.na(VD4020) & VD4020 >=0 & V2009 >=18 & V2009 <=24 & VD4001 == 1
      )
  }
  
  #25-29
  {
    #x 25-29
    subxj25_29 <-
      subset(
        dadosx,
        !is.na( VD4020 ) & VD4020 >=0 & V2009 >=25 & V2009 <=29
      )
    
    #x desocup 25-29
    subdesocupxj25_29 <-
      subset(
        dadosx,
        !is.na( VD4002 ) & V2009 >=25 & V2009 <=29
      )
    
    #x pea 25-29
    subpeaxj25_29 <-
      subset(
        dadosx,
        !is.na( VD4001 ) & V2009 >=25 & V2009 <=29
      )
   
    #informalidade x 25-29
     subinfxj25_29 <-
      subset(
        dadosx,
        !is.na( VD4009 ) & V2009 >=25 & V2009 <=29
      )
     subcrenda025_29 <-
       subset(
         dadosx,
         !is.na(VD4020) & VD4020 >=0 & V2009 >=25 & V2009 <=29 & VD4001 == 1
       )
     
  }
  
###GINI#########################################################################
  {
    ginixj15_29 <-
      svygini(~ VD4020,
              subxj15_29, na.rm = TRUE)
    
    ginixj15_17 <-
      svygini(~ VD4020,
              subxj15_17, na.rm = TRUE)
    
    ginixj18_24 <-
      svygini(~ VD4020,
              subxj18_24, na.rm = TRUE)
    
    ginixj25_29 <-
      svygini(~ VD4020,
              subxj25_29, na.rm = TRUE)
    
    ginir015_29 <-
      svygini(~VD4020,
              subcrenda015_29, na.rm = TRUE)
    
    ginir015_17 <-
      svygini(~VD4020,
              subcrenda015_17, na.rm = TRUE)
    
    ginir018_24 <-
      svygini(~VD4020,
              subcrenda018_24, na.rm = TRUE)
    
    ginir025_29 <-
      svygini(~VD4020,
              subcrenda025_29, na.rm = TRUE)
    
    renda15_29 <-
      svymean(~VD4020,
            subxj15_29, na.rm = TRUE)
    
    renda15_17 <-
      svymean(~VD4020,
             subxj15_17, na.rm = TRUE)
  
    renda18_24 <-
      svymean(~VD4020,
             subxj18_24, na.rm = TRUE)
    
    renda25_29 <-
      svymean(~VD4020,
             subxj25_29, na.rm = TRUE)
    
###QSR##########################################################################
    
    quintil15_29 <-
      svyqsr(~VD4020,subcrenda015_29, na.rm = TRUE, alpha1 = 0.1, alpha2 = 0.4)
    
    quintil15_17 <-
      svyqsr(~VD4020,subcrenda015_17, na.rm = TRUE, alpha1 = 0.1, alpha2 = 0.4)
    
    quintil18_24 <-
      svyqsr(~VD4020,subcrenda018_24, na.rm = TRUE, alpha1 = 0.1, alpha2 = 0.4)
    
    quintil25_29 <-
      svyqsr(~VD4020,subcrenda025_29, na.rm = TRUE, alpha1 = 0.1, alpha2 = 0.4)
    
###OUTPUT############################################################################    
    
    write.table(
      cbind(
        ginixj15_29, ginixj15_17,
        ginixj18_24, ginixj25_29),
      sep = ",",
      file = paste(
        out,y,x,".csv",
        sep = ""
      ),
      row.names = F,
      col.names = c('gini 15-29','gini 15-17',
                    'gini 18-24','gini 25-29'
      )
    )
    
    write.table(
      cbind(
        ginir015_29, ginir015_17,
        ginir018_24, ginir025_29
      ),
      sep = ",",
      file = paste(
        out,z,x,".csv",
        sep = ""
      ),
      row.names = F,
      col.names = c(
        'gini r0 15-29','gini r0 15-17',
        'gini r0 18-24','gini r0 25-29'
      )
    )
    
    
    write.table(
      cbind(
        renda15_29,renda15_17,
        renda18_24,renda25_29),
      sep = ",",
      file = paste(
        out,t,x,".csv",
        sep = ""
      ),
      row.names= F,
      col.names = c('renda 15-29','renda 15-17',
                    'renda 18-24','renda 25-29'
      )
    )
    
    write.table(
      cbind(
        quintil15_29, quintil15_17,
        quintil18_24, quintil25_29
      ),
      sep = ",",
      file = paste(
        out,q,x,".csv",
        sep = ""
      ),
      row.names = F,
      col.names = c(
        'q15-29','q15-17',
        'q 18-24','q25-29'
      )
    )

  }

}

