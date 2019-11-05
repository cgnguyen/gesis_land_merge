  ####Libraries####
  # install.packages("needs");
  library(needs)
  
  needs(tidyverse)
  needs(readstata13)
  needs(car)
  needs(rio)
  needs(readxl)
  needs(data.table)
  needs(stringr )
  needs(ggplot2)
  needs(lme4)
  
  
  
  ####Merge state level datasets####
  #car::recode File#
  TRANSLATE<-read_excel("D://Dropbox/Datasets/Gesis-Land/recodes.xlsx")
  
  DATA.vec<-names(TRANSLATE)[-c(1:2)]
  

  #Merge data set loop 
  
  for (i in 1:length(DATA.vec)){
    #generate names and paths
    temp.name<-DATA.vec[i]
    temp.path<-paste("D:/Dropbox/Datasets/Gesis-Land/",temp.name,".dta",sep="")
    print(temp.name)
  
    #Read in data 
    temp.data<-read.dta13(temp.path,nonint.factors = T)
  
    #Rename data 
    temp.variables<-as.vector(TRANSLATE[,temp.name])
    names(temp.variables)<-"old"
    
      #identify missing variables 
      temp.missing<-TRANSLATE$Code[temp.variables$old=="missing"]
      temp.newcode<-TRANSLATE$Code[temp.variables$old!="missing"]
      temp.variables<-temp.variables$old[temp.variables$old!="missing"]
      
      temp.merge<-as.data.frame(cbind(temp.newcode,temp.variables))
      names(temp.merge)<-c("new","old")
      temp.merge$old<-as.character(temp.merge$old)
      temp.merge$new<-as.character(temp.merge$new)
    
      
    #rename variables
    setnames(temp.data, old=temp.merge$old, new=temp.merge$new)
    
    #Add missing variables back in as missing
      if (length(temp.missing)>0){
        
        for (j in 1:length(temp.missing)){
        temp.data[,temp.missing[j]]<-NA
        }
      }
    temp.data<-temp.data[,TRANSLATE$Code]
    temp.data$survey<-temp.name
    temp.data<-as.data.frame(temp.data)
    if (i==1){
      DATA<-temp.data
    } else{
    DATA<-rbind(DATA,temp.data)
    }
  }
  
  DATA$state<-as.factor(substr(DATA$survey,1,2))
  DATA$year<-as.factor(substr(DATA$survey,3,7))
  
  DATA.backup<-DATA
  
####Data Cleaning####
  
DATA<-DATA.backup
library(car)
DATA<-droplevels(DATA)


  #Gender
  levels(DATA$gender)<-c("male","female","female","male","male")
  
  #Education 
  
  levels(DATA$education)<-c("No Degree",            
               "No Degree",
               "Hauptschule",
               "HS-Ausbildung",
               "Realschule",             
               "RS-Ausbildung",
               "Fachhochschulreife",
               "Abitur",
               "Still Student",
               "Realschule",           
               "RS-Ausbildung",
               "Hauptschule",
               "HS-Ausbildung",
               "Realschule",
               "RS-Ausbildung",        
               "Realschule",
               "RS-Ausbildung",
               "Hauptschule",
               "Still Student",
               "No Degree",            
               "Hauptschule",
               "Realschule",
               "Fachhochschulreife",
               "Abitur",
               "Still Student",        
               "Realschule",
               "Still Student",
               "Still Student"
  )
  
  
  

  
  #Political Interest 
  levels(DATA$pol.int)[1]<-NA
  levels(DATA$pol.int)[c(6:7)]<-"mittelmaessig"
  levels(DATA$pol.int)[6]<-"ueberhaupt nicht"
  
  #Pol Interest Bund 

  levels(DATA$pol.int.fed)[1]<-NA
  levels(DATA$pol.int.fed)[c(6:7)]<-"mittelmaessig"
  levels(DATA$pol.int.fed)[6]<-"ueberhaupt nicht"
  
  
  #Pol Interest Land
  levels(DATA$pol.int.state)[1]<-NA
  levels(DATA$pol.int.state)[c(6:7)]<-"mittelmaessig"
  levels(DATA$pol.int.state)[6]<-"ueberhaupt nicht"
  
  #Pol Interest EU
  levels(DATA$pol.int.eur)[1]<-NA
  levels(DATA$pol.int.eur)[c(6:7)]<-"mittelmaessig"
  levels(DATA$pol.int.eur)[6]<-"ueberhaupt nicht"
  
  #Voting 
  levels(DATA$vote)[c(1:2,8,11)]<-NA
  levels(DATA$vote)[6:7]<-"bestimmt zur Wahl gehen" #Assume Postal Vote is actual vote
  
  
  #Pocketbook Economic Evaluations
  DATA$econ.pocket.now<-car::recode(DATA$econ.pocket.now,"'keine Angabe'=NA")
  DATA$econ.pocket.now<-factor(DATA$econ.pocket.now, levels = 
                               c("sehr schlecht","schlecht","teils gut, teils schlecht","gut", "sehr gut"), 
                               ordered = T)

  DATA$econ.pocket.prosp<-car::recode(DATA$econ.pocket.prosp,"
    'keine Angabe_(-99)'=NA;
    'weiss nicht' = NA;
    'keine Angabe' = NA")

  DATA$econ.pocket.prosp<-factor(DATA$econ.pocket.prosp, levels =
                                c("wesentlich schlechter","etwas schlechter", "gleich geblieben", "etwas besser", "wesentlich besser"),
                                ordered=T)


  DATA$econ.pocket.retro<-car::recode(DATA$econ.pocket.retro,"
                               'keine Angabe' = NA")
  DATA$econ.pocket.retro<-factor(DATA$econ.pocket.retro, levels =
                                   c("wesentlich besser geworden","etwas besser geworden", "gleich geblieben", "etwas schlechter geworden", "wesentlich schlechter geworden"),
                                 ordered=T)



  #Sociotropic Economic Evaluations
  DATA$econ.socio.now<-car::recode(DATA$econ.socio.now,"
    'keine Angabe'=NA;
    'keine Angabe_(-99)'=NA;
    'keine Angabe_(99)'=NA;
    'keine Angabe_(7)' =NA;")
  DATA$econ.socio.now<-factor(DATA$econ.socio.now, levels = 
                                c("sehr schlecht","schlecht","teils gut, teils schlecht","gut", "sehr gut"), ordered = T)


  DATA$econ.socio.retro<-car::recode(DATA$econ.socio.retro,"
                                 'keine Angabe' = NA")
  DATA$econ.socio.retro<-factor(DATA$econ.socio.retro, levels =
                                  c("wesentlich schlechter geworden","etwas schlechter geworden", "gleich geblieben", "etwas besser geworden", "wesentlich besser geworden"),
                                ordered=T)


#Responsibility Attributions
  
  
  #  resp.socio.nat
  DATA$resp.socio.nat<-car::recode(DATA$resp.socio.nat,"
    'keine Angabe' = NA;
    'trifft nicht zu'= NA;
    'ziemlich stark' = 'stark';
    'mittelmae?ig'= 'mittelmaessig';  
    'mittelm??ig' = 'mittelmaessig';
    '?berhaupt nicht'= '?berhaupt nicht';
    'ueberhaupt nicht' ='?berhaupt nicht'")
                                   
                                   
  
  levels(DATA$resp.socio.nat)[4]<-NA #Needed because category messes up in recode
  
  DATA$resp.socio.nat<-factor(DATA$resp.socio.nat, levels =
                                  c("?berhaupt nicht","weniger stark", "mittelmaessig", "stark","sehr stark"),
                                  ordered=T)
  
  DATA$resp.socio.nat.simple<-factor(DATA$resp.socio.nat, levels =
                                c("?berhaupt nicht","weniger stark", "mittelmaessig", "stark","sehr stark"),
                              ordered=T)
  
  
  
  #resp.socio.state
  DATA$resp.socio.state<-car::recode(DATA$resp.socio.state,"
    'keine Angabe' = NA;
    'trifft nicht zu'= NA;
    'ziemlich stark' = 'stark';
    'mittelmae?ig'= 'mittelmaessig';  
    'mittelm??ig' = 'mittelmaessig';
    '?berhaupt nicht'= '?berhaupt nicht';
  'ueberhaupt nicht' ='?berhaupt nicht'")
  
    levels(DATA$resp.socio.state)[4]<-NA

  DATA$resp.socio.state<-factor(DATA$resp.socio.state, levels =
                                c("?berhaupt nicht","weniger stark", "mittelmaessig", "stark","sehr stark"),
                              ordered=T)
  
  #resp.socio.eu
  DATA$resp.socio.eu<-car::recode(DATA$resp.socio.eu,"
    'keine Angabe' = NA;
                                'trifft nicht zu'= NA;
                                'ziemlich stark' = 'stark';
                                'mittelmae?ig'= 'mittelmaessig';  
                                'mittelm??ig' = 'mittelmaessig';
                                '?berhaupt nicht'= '?berhaupt nicht';
  'ueberhaupt nicht' ='?berhaupt nicht'")
  levels(DATA$resp.socio.eu)[4]<-NA
  
  DATA$resp.socio.eu<-factor(DATA$resp.socio.eu, levels =
                                  c("?berhaupt nicht","weniger stark", "mittelmaessig", "stark","sehr stark"),
                                ordered=T)
  
  #resp.pocket.nat
  DATA$resp.pocket.nat<-car::recode(DATA$resp.pocket.nat,"
    'keine Angabe' = NA;
                             'trifft nicht zu'= NA;
                             'ziemlich stark' = 'stark';
                             'mittelmae?ig'= 'mittelmaessig';  
                             'mittelm??ig' = 'mittelmaessig';
                             '?berhaupt nicht'= '?berhaupt nicht';
  'ueberhaupt nicht' ='?berhaupt nicht'")
  levels(DATA$resp.pocket.nat)[4]<-NA

  DATA$resp.pocket.nat<-factor(DATA$resp.pocket.nat, levels =
                                c("?berhaupt nicht","weniger stark", "mittelmaessig","stark","sehr stark"),
                              ordered=T)
  #resp.pocket.state
  DATA$resp.pocket.state<-car::recode(DATA$resp.pocket.state,"
                                'keine Angabe' = NA;
                               'trifft nicht zu'= NA;
                               'ziemlich stark' = 'stark';
                               'mittelmae?ig'= 'mittelmaessig';  
                               'mittelm??ig' = 'mittelmaessig';
                               '?berhaupt nicht'= '?berhaupt nicht';
  'ueberhaupt nicht' ='?berhaupt nicht'")

  levels(DATA$resp.pocket.state)[4]<-NA
  
  DATA$resp.pocket.state<-factor(DATA$resp.pocket.state, levels =
                                 c("?berhaupt nicht","weniger stark", "mittelmaessig","stark","sehr stark"),
                               ordered=T)
  
  #resp.pocket.eu
  DATA$resp.pocket.eu<-car::recode(DATA$resp.pocket.eu,"
                                'keine Angabe' = NA;
                                 'trifft nicht zu'= NA;
                                 'ziemlich stark' = 'stark';
                                 'mittelmae?ig'= 'mittelmaessig';  
                                 'mittelm??ig' = 'mittelmaessig';
                                 '?berhaupt nicht'= '?berhaupt nicht';
  'ueberhaupt nicht' ='?berhaupt nicht'")
  

  levels(DATA$resp.pocket.eu)[4]<-NA
  
  DATA$resp.pocket.eu<-factor(DATA$resp.pocket.eu, levels =
                                 c("?berhaupt nicht","weniger stark", "mittelmaessig","stark","sehr stark"),
                               ordered=T)

  
  #Difference in Governments 
  #diff.gov
  levels(DATA$diff.gov)[1]<-NA 
  levels(DATA$diff.gov)[c(6,7)]<-c("ueberhaupt keinen Unterschied","sehr grossen Unterschied")
  levels(DATA$diff.gov)[c(6,7,8)]<-c("sehr grossen Unterschied","ueberhaupt keinen Unterschied","sehr grossen Unterschied")
  
  #diff.part
  levels(DATA$diff.part)[1]<-NA 
  levels(DATA$diff.part)[c(6,7)]<-c("ueberhaupt keinen Unterschied","sehr grossen Unterschied")
  levels(DATA$diff.part)[c(6,7,8)]<-c("sehr grossen Unterschied","ueberhaupt keinen Unterschied","sehr grossen Unterschied")
  
  
  #diff.part.state
  levels(DATA$diff.part.state)[1]<-NA 
  levels(DATA$diff.part.state)[c(6,7)]<-c("ueberhaupt keine Unterschiede","sehr grosse Unterschiede")
  levels(DATA$diff.part.state)[c(6,7,8)]<-c("sehr grossen Unterschied","ueberhaupt keinen Unterschied","sehr grossen Unterschied")
  
  #Belonging
 # belong.community
  DATA$belong.community<-car::recode(DATA$belong.community,"
    'ziemlich verbunden'= 'verbunden';
    'ueberhaupt nicht verbunden' = 'gar nicht verbunden';
    'keine Angabe' =NA")
  DATA$belong.community<-factor(DATA$belong.community, levels= 
    c("gar nicht verbunden","wenig verbunden","verbunden","stark verbunden"),
                                   ordered=T)
  
                                
  #belong.region
  DATA$belong.region<-car::recode(DATA$belong.region,"
                                'ziemlich verbunden'= 'verbunden';
                                'ueberhaupt nicht verbunden' = 'gar nicht verbunden';
                                'keine Angabe' =NA")
  DATA$belong.region<-factor(DATA$belong.region, levels= 
                                  c("gar nicht verbunden","wenig verbunden","verbunden","stark verbunden"),
                                ordered=T)
  
            
  #belong.state
  DATA$belong.state<-car::recode(DATA$belong.state,"
                                'ziemlich verbunden'= 'verbunden';
                             'ueberhaupt nicht verbunden' = 'gar nicht verbunden';
                             'keine Angabe' =NA")
  DATA$belong.state<-factor(DATA$belong.state, levels= 
                               c("gar nicht verbunden","wenig verbunden","verbunden","stark verbunden"),
                             ordered=T)
  
  #Belong West
  DATA$belong.west<-car::recode(DATA$belong.west,"
                                'ziemlich verbunden'= 'verbunden';
                            'ueberhaupt nicht verbunden' = 'gar nicht verbunden';
                            'keine Angabe' =NA")
  DATA$belong.west<-factor(DATA$belong.west, levels= 
                              c("gar nicht verbunden","wenig verbunden","verbunden","stark verbunden"),
                            ordered=T)
  
  
  #belong.east
  DATA$belong.east<-car::recode(DATA$belong.east,"
                                'ziemlich verbunden'= 'verbunden';
                           'ueberhaupt nicht verbunden' = 'gar nicht verbunden';
                           'keine Angabe' =NA")
  DATA$belong.east<-factor(DATA$belong.east, levels= 
                             c("gar nicht verbunden","wenig verbunden","verbunden","stark verbunden"),
                           ordered=T)
  

  #belong.ger
  DATA$belong.ger<-car::recode(DATA$belong.ger,"
                           'ziemlich verbunden'= 'verbunden';
                           'ueberhaupt nicht verbunden' = 'gar nicht verbunden';
                           'keine Angabe' =NA")
  DATA$belong.ger<-factor(DATA$belong.ger, levels= 
                             c("gar nicht verbunden","wenig verbunden","verbunden","stark verbunden"),
                           ordered=T)

  #belong.eu
  DATA$belong.eu<-car::recode(DATA$belong.eu,"
                           'ziemlich verbunden'= 'verbunden';
                          'ueberhaupt nicht verbunden' = 'gar nicht verbunden';
                          'keine Angabe' =NA")
  DATA$belong.eu<-factor(DATA$belong.eu, levels= 
                            c("gar nicht verbunden","wenig verbunden","verbunden","stark verbunden"),
                          ordered=T)
  
  #belong.europe
  DATA$belong.europe<-car::recode(DATA$belong.europe,"
                           'ziemlich verbunden'= 'verbunden';
                         'ueberhaupt nicht verbunden' = 'gar nicht verbunden';
                         'keine Angabe' =NA")
  DATA$belong.europe<-factor(DATA$belong.europe, levels= 
                           c("gar nicht verbunden","wenig verbunden","verbunden","stark verbunden"),
                         ordered=T)
  
  #Life Satisfaction 
  DATA$satisfaction<-car::recode(DATA$satisfaction," 
    'keine Angabe'= NA") 
    
  levels(DATA$satisfaction)<-str_replace_all(levels(DATA$satisfaction), fixed(" "), "") #Clear Whitespace
  levels(DATA$satisfaction)<-gsub("[^0-9]", "", levels(DATA$satisfaction)) 
  DATA$satisfaction.cont<-as.numeric(as.character(DATA$satisfaction))-1
  
  
  
  #Family
  levels(DATA$family)<- c(NA, 
      "married", "married", "married","married", 
      "single","divorced", "widow",
      "married","married","married","married","married","married")
                          
                          
                          
  #activity
  levels(DATA$activity)
  temp.string<-c(NA, "Full Time Work","Part Time Work",
                           "Student","Student","Student","Student",
                           "Unemployed","Part Time Work","Civil or National Service",
                           "Retired",
                           "Parental Leave","Economically Inactive", 
                           "Student","Student","Student", "Civil or National Service","Retired",
                           "Economically Inactive", "Economically Inactive",
                            "Full Time Work","Part Time Work","Student","Retired",
                           "Economically Inactive","Part Time Work","Civil or National Service",
                           "Part Time Work", "Retired")
  
  cbind(temp.string, levels(DATA$activity))
  
  levels(DATA$activity)<-temp.string
                           
  
  #Class - Check
  levels(DATA$class)
  #Code lower middle class into working class because otherwise it is just all middle class
  
  temp.string<-c(NA,"Lower/Working Class", "Lower/Working Class","Lower/Working Class","Middle Class","Upper Class","Upper Class","Middle Class",NA)
  cbind(levels(DATA$class),temp.string)
  
  levels(DATA$class)<-temp.string
  DATA$class<-relevel(DATA$class,ref="Middle Class")
  
  
  #Income - Code by low, middle, high - base of income distribution  low <1500 , middle 1500 to 3500 , high 
  levels(DATA$income) 
  temp.string<-c(NA, "low","low","low","low","low",
                 "middle","middle","middle","middle",
                 "high","high","high","high",
                 "low","low","low",
                 "middle","middle",
                 "high","high","high","high","low")
  cbind(levels(DATA$income),temp.string)
  levels(DATA$income)<-temp.string
  DATA$income<-relevel(DATA$income, ref="middle")
  
  #Party ID
  levels(DATA$partyid) 
  temp.string<-c(NA,NA,"CDU/CSU","CDU/CSU","SPD","FDP","GREEN","DIE LINKE","NPD","PIRATEN","AFD","Sonstige","Sonstige",
                 "None","Sonstige","None","Sonstige")

  cbind(levels(DATA$partyid),temp.string)
  levels(DATA$partyid)<-temp.string
  
  
  
####Code Problemlösungsebene####
  
  
  
  ####Write Data####
  names(DATA)<-gsub("\\.","_",names(DATA),)
  write_dta(DATA, "combined-gesis.dta")
  
  
  