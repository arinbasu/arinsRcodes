
R version 2.10.1 (2009-12-14)
Copyright (C) 2009 The R Foundation for Statistical Computing
ISBN 3-900051-07-0

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Previously saved workspace restored]

> options(chmhelp=FALSE)
> options(htmlhelp=TRUE)
> options(chmhelp = FALSE, help_type = "text")
> .help.ESS <- help
> options(STERM='iESS', editor='winclient.exe')
> 
> x = c(1,2,3,4,5)
> y = c(2,4,6,8, 10)
> 
> plot(x,y)
> list.files()
 [1] "131109"                                   
 [2] "452 conversionsetc.jpg"                   
 [3] "AbstractTables260709ABRev08090909.doc"    
 [4] "AbstractTables260709ABRev08090909_mod.doc"
 [5] "AbstractTables260709ABRev08100909_mod.doc"
 [6] "AbstractTablesABRev08100909_mod1004.doc"  
 [7] "AbstractTablesABRev101009_mod1442.doc"    
 [8] "AbstractTablesABRev191009_mod2125.doc"    
 [9] "AbstractTablesABRev201009_mod2121.doc"    
[10] "AbstractTablesABRev241009_mod9737.doc"    
[11] "AbstractTablesABRev251009_mod0823.doc"    
[12] "AbstractTablesABRev291009_mod0942.doc"    
[13] "abtabmethpaper_AB.doc"                    
[14] "AllanResponsesSep0609.doc"                
[15] "analysis291009.R"                         
[16] "basu table 4 nov 21 AB.xls"               
[17] "Basu Tables 1-4.doc"                      
[18] "Basu Tables 1-4_AB_131109.doc"            
[19] "bigdata2a.csv"                            
[20] "dmapctmultivar.csv"                       
[21] "dmapctmultivar.ods"                       
[22] "highBMIsubset.csv"                        
[23] "highBMIsubset.xls"                        
[24] "highBMIsubset291009.xls"                  
[25] "inaspctmultivar.csv"                      
[26] "inaspctmultivar.ods"                      
[27] "JGRError.log"                             
[28] "lists.txt"                                
[29] "matrixdma.txt"                            
[30] "matrixinas.txt"                           
[31] "matrixmma.txt"                            
[32] "meansdvar.txt"                            
[33] "meansdvar1.csv"                           
[34] "meanSE081009.csv"                         
[35] "meanSE081009.ods"                         
[36] "mmamatrix.csv"                            
[37] "mmapctmultivar-inputtable.ods"            
[38] "mmapctmultivar.csv"                       
[39] "mmapctmultivar.ods"                       
[40] "models.ods"                               
[41] "newWrkspsav.sav"                          
[42] "outputfrom9sepwork"                       
[43] "reanalysis28Aug09AB.r"                    
[44] "responsestoahsmethpaper.doc"              
[45] "results131109.txt"                        
[46] "ribofl.txt"                               
[47] "session.R"                                
[48] "session.R~"                               
[49] "table8rawformat.odt"                      
[50] "trying.R"                                 
[51] "trying.R~"                                
[52] "varnames.doc"                             
[53] "work_2010092122"                          
[54] "work2510090825.RData"                     
[55] "workable.csv"                             
[56] "workable1.xls"                            
[57] "workdata.csv"                             
[58] "workon241009"                             
[59] "workspace_201009"                         
> 
> newdata <- read.csv("workdata.csv")
> summar(newdata)
Error: could not find function "summar"
> summary(newdata)
       X            Subject       moisture        Proteinan     
 Min.   :  1.0   AA001  :  1   Min.   : 38.96   Min.   : 0.000  
 1st Qu.:102.2   AB002  :  1   1st Qu.:128.81   1st Qu.: 2.149  
 Median :203.5   AC003  :  1   Median :167.01   Median : 3.746  
 Mean   :203.5   AD004  :  1   Mean   :183.87   Mean   : 4.591  
 3rd Qu.:304.8   AE005  :  1   3rd Qu.:220.21   3rd Qu.: 5.821  
 Max.   :406.0   AF006  :  1   Max.   :547.27   Max.   :21.019  
                 (Other):400                                    
    Protnveg       Fatanimal           Fatveg         carbohydr    
 Min.   :11.67   Min.   : 0.0000   Min.   : 1.051   Min.   :123.2  
 1st Qu.:17.79   1st Qu.: 0.4405   1st Qu.: 6.002   1st Qu.:190.6  
 Median :19.44   Median : 1.0948   Median : 8.421   Median :202.4  
 Mean   :19.97   Mean   : 1.8925   Mean   : 9.864   Mean   :198.5  
 3rd Qu.:21.63   3rd Qu.: 2.6084   3rd Qu.:12.455   3rd Qu.:210.6  
 Max.   :30.77   Max.   :14.2555   Max.   :41.301   Max.   :224.8  
                                                                   
     fibre            calcium            phosph           iron       
 Min.   : 0.7129   Min.   :  39.90   Min.   :319.5   Min.   : 2.855  
 1st Qu.: 1.4357   1st Qu.: 127.15   1st Qu.:432.1   1st Qu.: 4.556  
 Median : 2.0016   Median : 190.51   Median :471.1   Median : 5.790  
 Mean   : 2.3946   Mean   : 224.89   Mean   :505.9   Mean   : 6.298  
 3rd Qu.: 3.0216   3rd Qu.: 281.59   3rd Qu.:554.6   3rd Qu.: 7.457  
 Max.   :10.3246   Max.   :1095.66   Max.   :853.2   Max.   :21.319  
                                                                     
      zinc          carotene              VitA            thyamine     
 Min.   :2.315   Min.   :    6.877   Min.   :  0.000   Min.   :0.4280  
 1st Qu.:3.764   1st Qu.:   92.248   1st Qu.:  1.897   1st Qu.:0.5906  
 Median :4.041   Median :  304.022   Median : 13.367   Median :0.6296  
 Mean   :4.085   Mean   : 1570.607   Mean   : 25.060   Mean   :0.6655  
 3rd Qu.:4.449   3rd Qu.: 1279.832   3rd Qu.: 36.915   3rd Qu.:0.7050  
 Max.   :5.921   Max.   :20608.802   Max.   :251.293   Max.   :1.1003  
                                                                       
   riboflavin         niacin           VitB6          Folicfree    
 Min.   :0.1173   Min.   : 6.186   Min.   :0.2179   Min.   :13.02  
 1st Qu.:0.1915   1st Qu.: 9.205   1st Qu.:0.4872   1st Qu.:23.87  
 Median :0.2568   Median : 9.967   Median :0.5699   Median :27.87  
 Mean   :0.2873   Mean   : 9.731   Mean   :0.5607   Mean   :30.43  
 3rd Qu.:0.3434   3rd Qu.:10.428   3rd Qu.:0.6286   3rd Qu.:33.81  
 Max.   :0.8086   Max.   :12.938   Max.   :1.0879   Max.   :87.91  
                  NA's   : 1.000                                   
   Folictotal          VitC           Totalnitr        arginine     
 Min.   : 26.77   Min.   :  1.421   Min.   :0.393   Min.   : 361.6  
 1st Qu.: 46.13   1st Qu.: 19.697   1st Qu.:2.231   1st Qu.:1213.7  
 Median : 63.44   Median : 33.193   Median :2.652   Median :1521.9  
 Mean   : 74.35   Mean   : 51.434   Mean   :2.605   Mean   :1464.2  
 3rd Qu.: 91.67   3rd Qu.: 59.626   3rd Qu.:2.965   3rd Qu.:1744.2  
 Max.   :302.07   Max.   :400.198   Max.   :4.952   Max.   :2498.0  
                                                                    
   histidine         lysine         tryptophan       phenylana     
 Min.   :132.5   Min.   : 175.1   Min.   : 55.11   Min.   : 247.5  
 1st Qu.:389.3   1st Qu.: 584.4   1st Qu.:164.13   1st Qu.: 735.8  
 Median :473.2   Median : 709.8   Median :192.18   Median : 894.9  
 Mean   :465.9   Mean   : 745.7   Mean   :193.52   Mean   : 866.0  
 3rd Qu.:541.3   3rd Qu.: 875.9   3rd Qu.:217.89   3rd Qu.:1000.7  
 Max.   :806.6   Max.   :1753.1   Max.   :510.86   Max.   :1631.2  
                                                   NA's   :   1.0  
    tyrosine        methionin        cystine         threonine     
 Min.   : 203.8   Min.   :144.8   Min.   : 59.38   Min.   : 208.8  
 1st Qu.: 596.6   1st Qu.:397.8   1st Qu.:201.92   1st Qu.: 605.2  
 Median : 722.6   Median :492.3   Median :247.21   Median : 735.6  
 Mean   : 704.0   Mean   :475.9   Mean   :240.32   Mean   : 719.6  
 3rd Qu.: 829.9   3rd Qu.:568.2   3rd Qu.:277.88   3rd Qu.: 832.0  
 Max.   :1297.3   Max.   :769.8   Max.   :629.55   Max.   :1339.0  
                                                                   
    leucine         isoleucine         valine         Subject.1  
 Min.   : 393.6   Min.   : 225.4   Min.   : 321.8   AA001  :  1  
 1st Qu.:1171.5   1st Qu.: 683.8   1st Qu.: 925.4   AB002  :  1  
 Median :1419.4   Median : 815.6   Median :1129.8   AC003  :  1  
 Mean   :1396.9   Mean   : 807.7   Mean   :1100.0   AD004  :  1  
 3rd Qu.:1607.3   3rd Qu.: 929.4   3rd Qu.:1292.5   AE005  :  1  
 Max.   :2807.9   Max.   :1511.9   Max.   :1821.2   AF006  :  1  
                                                    (Other):400  
    Glucose           Chol           Trans          Vit.B12      
 Min.   :  2.0   Min.   : 79.0   Min.   : 75.0   Min.   :  78.0  
 1st Qu.: 67.0   1st Qu.:129.0   1st Qu.:204.0   1st Qu.: 281.0  
 Median : 81.0   Median :151.0   Median :238.0   Median : 382.0  
 Mean   : 85.4   Mean   :155.1   Mean   :236.9   Mean   : 434.9  
 3rd Qu.: 96.0   3rd Qu.:175.0   3rd Qu.:269.0   3rd Qu.: 509.0  
 Max.   :315.0   Max.   :270.0   Max.   :393.0   Max.   :4987.0  
 NA's   : 29.0   NA's   : 29.0   NA's   : 29.0   NA's   :  33.0  
     Folate           Selen           Cysteine        GlutGSSG     
 Min.   : 0.800   Min.   : 0.350   Min.   : 73.0   Min.   : 0.420  
 1st Qu.: 2.000   1st Qu.: 0.860   1st Qu.:193.0   1st Qu.: 1.710  
 Median : 2.700   Median : 1.150   Median :214.0   Median : 2.620  
 Mean   : 3.376   Mean   : 1.173   Mean   :215.0   Mean   : 3.155  
 3rd Qu.: 3.700   3rd Qu.: 1.430   3rd Qu.:241.0   3rd Qu.: 3.780  
 Max.   :33.300   Max.   : 2.750   Max.   :330.0   Max.   :31.780  
 NA's   :35.000   NA's   :29.000   NA's   : 33.0   NA's   :33.000  
    GlutGSH         Homocyst        Retinol           Atoc       
 Min.   : 0.84   Min.   : 4.80   Min.   : 3.80   Min.   : 141.0  
 1st Qu.: 3.42   1st Qu.:10.20   1st Qu.:26.60   1st Qu.: 492.0  
 Median : 5.24   Median :12.90   Median :33.30   Median : 596.0  
 Mean   : 6.31   Mean   :14.78   Mean   :34.07   Mean   : 635.6  
 3rd Qu.: 7.56   3rd Qu.:16.60   3rd Qu.:40.80   3rd Qu.: 742.0  
 Max.   :63.56   Max.   :74.60   Max.   :68.10   Max.   :1579.0  
 NA's   :33.00   NA's   :33.00   NA's   :33.00   NA's   :  33.0  
     LutZea           Bcrypt          Lycopene         BetaCar      
 Min.   : 11.00   Min.   : 0.420   Min.   : 0.000   Min.   :  4.00  
 1st Qu.: 47.00   1st Qu.: 2.440   1st Qu.: 1.002   1st Qu.: 31.20  
 Median : 61.00   Median : 3.970   Median : 1.900   Median : 51.00  
 Mean   : 66.51   Mean   : 5.738   Mean   : 3.339   Mean   : 83.63  
 3rd Qu.: 81.00   3rd Qu.: 7.000   3rd Qu.: 3.500   3rd Qu.: 99.50  
 Max.   :215.00   Max.   :50.930   Max.   :53.800   Max.   :706.50  
 NA's   : 33.00   NA's   :33.000   NA's   :36.000   NA's   : 33.00  
     Vit.B6          Methion          SelenF           Status.1  
 Min.   : 10.80   Min.   : 5.00   Min.   :  0.460   case   :192  
 1st Qu.: 27.30   1st Qu.:15.60   1st Qu.:  1.035   control:213  
 Median : 34.70   Median :19.00   Median :  1.365   NA's   :  1  
 Mean   : 40.82   Mean   :19.41   Mean   :  1.359                
 3rd Qu.: 44.60   3rd Qu.:23.10   3rd Qu.:  1.640                
 Max.   :805.00   Max.   :40.70   Max.   :  2.750                
 NA's   : 53.00   NA's   :42.00   NA's   :186.000                
   Statrec.1       Matchnumber        Fromset           Age        Sex    
 Min.   :0.0000   Min.   :  2.00   Min.   :  1.0   Min.   : 0.00    :  1  
 1st Qu.:0.0000   1st Qu.: 71.75   1st Qu.: 41.5   1st Qu.:26.00   F:149  
 Median :0.0000   Median :139.50   Median : 99.0   Median :37.00   M:256  
 Mean   :0.4741   Mean   :137.48   Mean   :111.1   Mean   :37.06          
 3rd Qu.:1.0000   3rd Qu.:204.25   3rd Qu.:178.5   3rd Qu.:50.00          
 Max.   :1.0000   Max.   :266.00   Max.   :255.0   Max.   :82.00          
 NA's   :1.0000   NA's   : 22.00   NA's   :327.0   NA's   : 1.00          
        house                school        creat          in.as       
 kacha     :210                 :  5   Min.   :  40   Min.   :  0.40  
 puca      : 58   College       : 19   1st Qu.: 220   1st Qu.:  4.20  
 semi-pucca:138   No formal educ:113   Median : 460   Median :  9.85  
                  Primary       :200   Mean   : 621   Mean   : 19.38  
                  Secondary     : 68   3rd Qu.: 870   3rd Qu.: 19.50  
                  University    :  1   Max.   :2840   Max.   :260.40  
                                       NA's   :   3   NA's   :  4.00  
      mma              dma              totas            totasafs  
 Min.   :  0.15   Min.   :   1.80   Min.   :   2.60   .      : 10  
 1st Qu.:  1.20   1st Qu.:  14.25   1st Qu.:  24.32   15.4   :  4  
 Median :  3.45   Median :  33.00   Median :  53.50   26     :  4  
 Mean   : 10.39   Mean   :  75.95   Mean   : 114.64   27     :  3  
 3rd Qu.:  9.65   3rd Qu.:  69.95   3rd Qu.: 113.60   28     :  3  
 Max.   :237.20   Max.   :1291.90   Max.   :1564.90   34.2   :  3  
 NA's   :  4.00   NA's   :   3.00   NA's   :  70.00   (Other):379  
     total           mma.prop          dma.prop         in.prop      
 Min.   :   0.0   Min.   :0.00000   Min.   :0.1625   Min.   :0.0000  
 1st Qu.:  22.3   1st Qu.:0.05112   1st Qu.:0.6258   1st Qu.:0.1252  
 Median :  47.1   Median :0.07511   Median :0.7255   Median :0.1765  
 Mean   : 105.1   Mean   :0.08192   Mean   :0.6876   Mean   :0.2305  
 3rd Qu.: 101.1   3rd Qu.:0.10383   3rd Qu.:0.7879   3rd Qu.:0.2805  
 Max.   :1564.9   Max.   :0.24041   Max.   :1.0000   Max.   :0.8280  
 NA's   :   1.0   NA's   :4.00000   NA's   :4.0000   NA's   :4.0000  
     ratio            agerec       gender                  educ    
 Min.   :0.00000   15--29:111   female:150   high school plus: 20  
 1st Qu.:0.07523   30--44:135   male  :256   no formal       :118  
 Median :0.10994   45--59: 82                primary         :200  
 Mean   :0.12494   gte 60: 34                secondary       : 68  
 3rd Qu.:0.15206   lt 15 : 42                                      
 Max.   :0.54369   NA's  :  2                                      
 NA's   :4.00000                                                   
      inas           newmma          newdma          totalas      
 Min.   :  1.0   Min.   :  1.0   Min.   :  1.00   Min.   :  2.00  
 1st Qu.: 56.5   1st Qu.: 14.5   1st Qu.: 88.75   1st Qu.: 79.75  
 Median :108.0   Median : 66.0   Median :167.50   Median :158.50  
 Mean   :113.4   Mean   : 66.2   Mean   :170.16   Mean   :157.67  
 3rd Qu.:174.5   3rd Qu.:108.0   3rd Qu.:253.25   3rd Qu.:236.00  
 Max.   :231.0   Max.   :171.0   Max.   :341.00   Max.   :312.00  
 NA's   :  3.0   NA's   :  3.0   NA's   :  2.00   NA's   : 70.00  
    inaspct          mmapct           dmapct            bmi1      
 Min.   : 0.00   Min.   : 0.000   Min.   : 16.25   Min.   :11.36  
 1st Qu.:12.52   1st Qu.: 5.112   1st Qu.: 62.58   1st Qu.:16.66  
 Median :17.65   Median : 7.511   Median : 72.55   Median :18.37  
 Mean   :23.05   Mean   : 8.192   Mean   : 68.76   Mean   :18.90  
 3rd Qu.:28.05   3rd Qu.:10.383   3rd Qu.: 78.79   3rd Qu.:20.83  
 Max.   :82.80   Max.   :24.041   Max.   :100.00   Max.   :34.17  
 NA's   : 4.00   NA's   : 4.000   NA's   :  4.00   NA's   :37.00  
    totaltertiles
 (188,282] :100  
 (2,95.5]  :100  
 (95.5,188]:101  
 NA's      :105  
                 
                 
                 
> plot(bmi1)
Error in plot(bmi1) : object 'bmi1' not found
> plot(newdata$bmi1)
> names(newdata)
 [1] "X"             "Subject"       "moisture"      "Proteinan"    
 [5] "Protnveg"      "Fatanimal"     "Fatveg"        "carbohydr"    
 [9] "fibre"         "calcium"       "phosph"        "iron"         
[13] "zinc"          "carotene"      "VitA"          "thyamine"     
[17] "riboflavin"    "niacin"        "VitB6"         "Folicfree"    
[21] "Folictotal"    "VitC"          "Totalnitr"     "arginine"     
[25] "histidine"     "lysine"        "tryptophan"    "phenylana"    
[29] "tyrosine"      "methionin"     "cystine"       "threonine"    
[33] "leucine"       "isoleucine"    "valine"        "Subject.1"    
[37] "Glucose"       "Chol"          "Trans"         "Vit.B12"      
[41] "Folate"        "Selen"         "Cysteine"      "GlutGSSG"     
[45] "GlutGSH"       "Homocyst"      "Retinol"       "Atoc"         
[49] "LutZea"        "Bcrypt"        "Lycopene"      "BetaCar"      
[53] "Vit.B6"        "Methion"       "SelenF"        "Status.1"     
[57] "Statrec.1"     "Matchnumber"   "Fromset"       "Age"          
[61] "Sex"           "house"         "school"        "creat"        
[65] "in.as"         "mma"           "dma"           "totas"        
[69] "totasafs"      "total"         "mma.prop"      "dma.prop"     
[73] "in.prop"       "ratio"         "agerec"        "gender"       
[77] "educ"          "inas"          "newmma"        "newdma"       
[81] "totalas"       "inaspct"       "mmapct"        "dmapct"       
[85] "bmi1"          "totaltertiles"
> plot(newdata$bmi1, newdata$mmapct)
> summary(newdata$methion)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  144.8   397.8   492.3   475.9   568.2   769.8 
> plot(newdata$methion, newdata$bmi1, xlab = "methionine", ylab = "mma%")
> plot(newdata$methion, newdata$Homocyst)
> 