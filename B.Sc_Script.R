

install.packages("psych") 
install.packages("GPArotation")
install.packages("ggplot2")
library(psych)


#########Descriptive#########

    print(bfi)
    print(bfi.dictionary) 
    
    #Data Cleaning
    bfi = bfi[!(bfi$age == 3),] #Alter mit Bildung nicht koherent
    
    
    #Descripitve Statistics
    sapply(bfi, mean, na.rm = TRUE) #mean of every variable
    nrow(bfi)
    
    #Geschlecht der Beobachtungsobjekte
    table(bfi$gender)
    x = prop.table(table(bfi$gender))
    plot1 = barplot(x, horiz = FALSE, names.arg = c("male", "female"),
                    col =  c("steelblue3","grey90"), las = 1, ylim=c(0.00, 0.80))
    
    #Bildungsstand
    summary(bfi$education)
    edprop = prop.table(table(bfi$gender,bfi$education))
    barplot(edprop, beside = TRUE,
            names.arg = c("in HS", "fin HS", "coll", "coll grad", "grad deg"), 
            col = c("steelblue3","grey90"), ylim=c(0.00, 0.4),
            legend.text = c("male","female"), las = 1)
    
    #Alter
    summary(bfi$age)
    boxplot(bfi$age, las =1, ylim=c(0.00, 100), horizontal = FALSE, ylab ="Age")
    barplot(table(bfi$gender, bfi$age), xlab="Age", col = c("steelblue3","grey90"), 
            beside = FALSE, legend.text = c("male","female"), las = 1, ylim=c(0.00, 250))


  
    
    
#########Korrelationsmatrix#########

    #Subset and remove Gender, Education and Age
    bfi1 = subset(bfi, select = -c(gender, education, age))
    
    #z-Standartisierung 
    bfi_z = scale(bfi1) 
    
    #Korrelationsmatrix und Heatplot
    R = cor(x = bfi_z, y = NULL, use = "complete.obs")
    corPlot(R, numbers = TRUE, show.legend=TRUE)
    
    #Eignung der Korrelationsmatrix f??r Faktorenanalyse
    cortest.bartlett(R, n = 280) # Bartlett-Test auf Sph??rizi??t (10% Sample)
    KMO(R) #Kaiser-Meyer-Olkin Kriterium


    

    
    
    
#########Reduzierte Korrelationsmatrix mit Squared Multiple Korrelations#########

    
    
    #Erstellung reduzierte Korrelationsmatrix
      R_invert = solve(R) #invertierte Korrelation
      smc = diag(1-(1/R_invert)) #SMCs
      R_reduced = R
      diag(R_reduced) = smc #Diagonale ersetzen
      R_reduced 
      smc == diag(R_reduced)
      smc
      smc.sum = sum(smc) 
      smc.sum  #Summe der Kommunalit??ten
    
   
  

      v = 25 #Summe der auf 1 normierten Varianz
      smc.sum/v #Kommunalitat / Gesamtvarianz
      
      #Plot von Reduzierter Korrelationsmatrix
      corPlot(R_reduced, numbers = TRUE, main = "Korrelationsplot", show.legend=TRUE)
      
    
    
    
    
    
    
#########Eigenwertberechnungen#########
 
    
    #Eigenvalues reduzierte Korrelationsmatrix
    Eigen.Rh = eigen(R_reduced)
      ew.Rh = Eigen.Rh$values
      ew.Rh
      
      sum(smc)
      sum(ew.Rh) #Summe der SMCs entspricht Summe d. Eigenwerte
    
    #Eigenvalues der empirischen Korrelationsmatrix
    Eigen = eigen(R)
      ew = Eigen$values
      ew
      ew_sum = sum(ew)
      ew_sum  # = 25, Anzahl d. Variablen, da normiert auf 1)
 
      
      
      
      
#########Scree-Plot und Parallelanalyse#########
    
      
    plot((eigen(R)$values), type ="b", main = "Scree Plot", ylab ="Eigenvalue", col ="red")   
  lines((eigen(R_reduced)$values), type ="b", main = "Scree Plot", ylab ="Eigenvalue") 
      
    #Eigenwertverlauf von empirischer und reduzierter Korrelationsmatrix
    fa.parallel(R, fm="pa", fa="both", main="Scree Plot & Parallelanalyse", SMC = TRUE, 
                ylabel = "Eigenvalue", cor="cor", use = "complete.obs", n.obs = 200)



    
    
    
    
#########UnrotierteFaktorladungen#########

    #Principal Axis
    unrotiert = fa(bfi1, nfactors=5, fm="pa", rotate="none", SMC = TRUE,
                   min.err=0.000001, max.iter=50, warnings = TRUE)
    
      sum(unrotiert$values)
      sum(unrotiert$communality)
      sum(unrotiert$e.values)
    
      #Ladungsdiagram_PA
      fa.diagram(unrotiert, Phi=NULL, fe.results=NULL, sort=TRUE, labels=NULL,cut=.3,
                 simple=TRUE, errors=TRUE, g=FALSE, main ="Faktorladungen")
      
    
    #Maximum-Likelihood
    unrotiert2 = fa(bfi1, nfactors = 5, rotate = "none", residuals =TRUE, SMC = TRUE, 
                    max.iter=50, fm = "ml", warnings = TRUE) 
    
      #Ladungsdiagram_ML
      fa.diagram(unrotiert2, Phi=NULL, fe.results=NULL, sort=TRUE, labels=NULL,cut=.3,
                 simple=TRUE, errors=TRUE, g=FALSE, main ="Faktorladungen")


      
      
      
      
      
#########Varimax#########
      varimax = fa(bfi1, nfactors=5, fm="pa", rotate="varimax", SMC = TRUE,
                   min.err=0.000001, max.iter=50)
      
        print(varimax)
        
        varimax$communality
        sum(varimax$communality)
        varimax$values
        sum(varimax$values)
        varimax$e.values
        sum(varimax$e.values)

        sum(varimax$communality)/sum(varimax$e.values)
        
        
        
        
        
        
#########Promax#########
    
    #Haupachsen-Extraktion
    promax = fa(bfi1, nfactors=5, fm="pa", rotate="promax", SMC = TRUE,
                min.err=0.000001, max.iter=50)
    
      print(promax)
      promaxstructure = promax$Structure
      print(promaxstructure, cutoff = 0.00)
    
      sum(promax$communality)/sum(promax$e.values) #Wie bei Varimax
    
      
    #Maximum-Likelihood
    promaxml = fa(bfi1, nfactors=5, fm="ml", rotate="promax", SMC = TRUE,
                  min.err=0.000001, max.iter=50)
      
    
    sum(promaxml$communality)/sum(promax$e.values)
    
      print(promaxml)
    
    
      
      
      
      
      
#########Direct Oblimin#########  
      
    oblimin = fa(bfi1, nfactors=5, fm="ml", rotate="oblimin", SMC = TRUE,
                 min.err=0.000001, max.iter=50)
    
      print(oblimin)
      
      
  
#########Kongruenzmatritzen und -Plots######### 
      
      #Kongruenz von Varimax und Promax
      cong = factor.congruence(varimax, promax ,digits=2, use="complete" ,structure=FALSE)
        print(cong)
        cor.plot(cong)
        
        acos(0.96) / 0.0174533
        acos(0.99) / 0.0174533
      
      #Kongruenz von Promax und Oblimin
      cong2 = factor.congruence(promaxml, oblimin ,digits=2, use="complete" ,structure=FALSE)
        print(cong2)
        cor.plot(cong2)
         
        acos(-0.96) / 0.0174533
        acos(0.97) / 0.0174533
        acos(0.99) / 0.0174533
      
