### LCVP-Based Species Harmonizing function (LCVP-Bash) ###
species.unifier<- function(names,mdist=2){
  
  ### require packages
  require(stringr)
  require(lcvplants)
  require(LCVP)
  
  #simple operator
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  ### save original names ###
  nam<-names
  
  ### creating a diagnostics file to store the extend of altered information
  diagnostics<-matrix(0,1,6)
  colnames(diagnostics)<-c("Varieties","Subspecies","Hybrids","No_Match","Unprecise","Hybrids_Aggregated")
  
  nam<-gsub("'.*","var.",nam)#replaces breeds with var.
  
  diagnostics[1,1]<-sum(str_count(nam, " var\\.+"))+sum(str_count(nam," varietas "))+sum(str_count(nam, "'.*"))
  diagnostics[1,2]<-sum(str_count(nam, " subsp\\.+"))+sum(str_count(nam, " sp\\.+"))+sum(str_count(nam, " spp\\.+")) # this will not work if names include incomplete species names like "Acacia sp."

  nam_subs<-c(unique(grep(" subsp\\.+", nam, value=TRUE)),unique(grep(" sp\\.+", nam, value=TRUE),unique(grep(" spp\\.+", nam, value=TRUE))))
  nam_var<-c(unique(grep(" var\\.+", nam, value=TRUE)),unique(grep(" varietas ", nam, value=TRUE)),unique(grep("'.*", nam, value=TRUE)))
  
  nam<-gsub("'.*","",nam)#for breeds
  nam<-gsub(" subsp\\..+","",nam)
  nam<-gsub(" var\\..+","",nam)
  nam<-gsub(" varitas ","",nam) # There are some species legitimately
  nam<-gsub(" spp\\.+","",nam)
  nam<-gsub(" sp\\.+","",nam)
  

  countSpaces <- function(s) { sapply(gregexpr(" ", s), function(p) { sum(p>=0) } ) }
  diagnostics[5]<-table(countSpaces(nam)==0)[2] # how many species are only available on family level i.e. not precise
  nam_unpr<-unique(names[which(countSpaces(nam)==0)]) # use names so original input is saved
  
  
  ### standardize hybrids ###
  nam<-gsub(" .. "," x ",nam)
  nam<-gsub(pattern=" . ",replacement = " x ", nam)
  nam<-gsub("Ã-","x",nam)# as the step before does not work for two symbol things
  nam<-gsub(pattern="x",replacement = "x", nam) # the first x is a special character
  hyb<-rep(0,length(nam))
  hyb<-str_count(nam, " x ") # this counts how often the expression is found, because hybrid names are of different length and extend. So, different amount of words must be dropped and hybrid indicator added at diferent points in this process
  
  
  diagnostics[,3]<-length(hyb[which(hyb!=0)])
  nam_hybrids<-unique(nam[which(hyb>0,)])
  
  
  ##for simple hybrids
  for (i in which(hyb==1)){
    nam[i]<-str_replace(nam[i], c("x "), "")
    nam[i]<-paste(nam[i],"_x",collapse = "\n",sep = "")
  }
  
  ##for complex hybrids
  for (i in which(hyb>1)){
    nam[i]<-str_replace(nam[i], c("x "), "")
    nam[i]<-sub("^(\\S*\\s+\\S+).*", "\\1", nam[i])
    nam[i]<-paste(nam[i],"_x",collapse = "\n",sep = "")
  }
  
  ## for hybrids with missing indicator 
  spacecount<-rep(0,length(nam))
  for ( i in 1:length(nam)) {
    spacecount[i]<-length(gregexpr(" ", nam[i])[[1]])
  }
  nam<-sub("^(\\S*\\s+\\S+).*", "\\1", nam)
  for (i in which(spacecount==2)){
    nam[i]<-paste(nam[i],"_x",collapse = "\n",sep = "")
  } 
  
  hyb<-str_count(nam, "_x")
  
  nam2<-nam[which(str_count(nam,pattern=" ")>0)]#removes resulting "only family" entries. Will be assigned NA later
  

  ## Pre-matching with LCVP
  
  ifelse("lcvplants" %in% rownames(installed.packages()),NA,
  ((devtools::install_github("idiv-biodiversity/lcvplants"))))
  ifelse("LCVP" %in% rownames(installed.packages()),NA,
         ((devtools::install_github("idiv-biodiversity/LCVP"))))
  library(lcvplants)
  library(LCVP) # checks if LCVP is installed and, if not, installs it
  
  tab_lcvp$Usage<-sub("^(\\S*\\s+\\S+).*", "\\1", tab_lcvp$Output.Taxon) #shortens output to binomial style
  tab_lcvp$match<-sub("^(\\S*\\s+\\S+).*", "\\1", tab_lcvp$Input.Taxon)  #enables pre-lcvp run perfect match application
  tab_lcvp2<-tab_lcvp[which(tab_lcvp$Status=="valid"),]
  tab_lcvp2<-tab_lcvp2[which(!duplicated(tab_lcvp2[c("match", "Usage")])),]
  tab_lcvp3<-tab_lcvp[which(tab_lcvp$Status=="synonym"),]
  tab_lcvp3<-tab_lcvp3[which(!duplicated(tab_lcvp3[c("match", "Usage")])),]

  nam_natch<-unique(nam2[which(nam2%!in%tab_lcvp$match)])#run
  
  tab_lcvp2b<-tab_lcvp2[which(tab_lcvp2$match%in%nam),c("match","Usage","Family","Order")]
  tab_lcvp3b<-tab_lcvp3[which(tab_lcvp3$match%in%nam),c("match","Usage","Family","Order")]
  tab_lcvpb<-rbind(tab_lcvp2b,tab_lcvp3b)
  tab_lcvpb<-tab_lcvpb[which(!duplicated(tab_lcvpb[c("match", "Usage")])),]

  ex_nam<-numeric(0)    # a dummy to make it simpler to run the code if everything matches
  LCVP_nam<-numeric(0)  # a dummy to make it simpler to run the code if everything matches
  LCVP_ex<-numeric(0)  # a dummy to make it simpler to run the code if everything matches
  
  
  if(length(nam_natch)>0){
  LCVP_nam<-LCVP(nam_natch,max.distance = mdist,max.cores = 8,genus_search = FALSE)
  doublesz<-LCVP_nam$Submitted_Name[which(duplicated(LCVP_nam$Submitted_Name))]
  nam_double<-LCVP_nam[which(LCVP_nam$Submitted_Name%in%doublesz),]
  LCVP_nam<-LCVP_nam[which(LCVP_nam$Submitted_Name%!in%doublesz),]#sorts out uncertain cases
  ex_nam<-LCVP_nam[which(LCVP_nam$Score%in%c("Genus name not found","Epithet name not found")),]#not found species
  LCVP_nam<-LCVP_nam[which(LCVP_nam$Score%!in%c("Genus name not found","Epithet name not found")),]
  LCVP_nam$Usage<-sub("^(\\S*\\s+\\S+).*", "\\1", LCVP_nam$LCVP_Accepted_Taxon) #shortens output to binomial style
  LCVP_sortout<-LCVP_nam
  nam_na<-nam_natch[which(nam_natch%!in%LCVP_nam$Submitted_Name)]#not found names
  }
  
  #adding to big list
  LCVP_nam<-LCVP_nam[,c("Submitted_Name","Usage","Family","Order")]
  colnames(tab_lcvpb)<-c("Submitted_Name","Usage","Family","Order")
  tab_lcvpc<-rbind(tab_lcvpb,LCVP_nam)
  
  notfound<-nam[which(nam%!in%tab_lcvpc$Submitted_Name)]
  not_lcvp<-matrix("NA",length(notfound),4)
  not_lcvp[,1]<-notfound
  colnames(not_lcvp)<-c("Submitted_Name","Usage","Family","Order")
  tab_lcvpd<-rbind(tab_lcvpc,not_lcvp)
  colnames(tab_lcvpd)<-c("Submitted_Name","Assigned_Name","Family","Order")
  
  #restore & rematch (assigning original name provided by user for matching)
  output<-tab_lcvpd
  hyb_loss<-length(grep("_x",output$Submitted_Name))-length(grep("_x",output$Assigned.Name)) #number of hybrid species pooled. Has to be done before name readjusting
  
  for (i in 1:nrow(output)){
  output$Submitted_Name[i]<-unique(names[which(nam==tab_lcvpd$Submitted_Name[i])])
  }
  
  doubles_rec<-unique(output$Submitted_Name[which(duplicated(output$Submitted_Name))]) # Which name had more than one hit. Has to be done before sorting them out
  
  output<-output[which(!duplicated(output$Submitted_Name)),]# as everything is done in order, valid is chosen over synonym over error search and always the first record is chosen for reproducibility
  output<-output[match(names,output$Submitted_Name),] #reorder to input order

  #### Output and diagnostics generation 2 ####
  
  list_output<- vector(mode = "list", length = 7)
  names(list_output)<-c("Hybrids","Subspecies","Varieties","Doubles","Not_Found","Invalid","Hybrids_Aggregated")
  list_output$Hybrids<-names[which(hyb>0)]
  list_output$Subspecies<-nam_subs
  list_output$Varieties<-nam_var
  list_output$Invalid<-nam_unpr
  list_output$Doubles<-doubles_rec
  list_output$Not_Found<-output$Submitted.Name[which(output$Assigned_Name%in%c("NA","unresolved",""))]
  list_output$Hybrids_Aggregated<-hyb_loss
  diagnostics[,4]<-length(unique(list_output$Not_Found))
  diagnostics_output<<-diagnostics
  list_output<<-list_output
  diagnostics[,6]<-hyb_loss
  diagnostics[,3]<-length(nam[which(hyb>0)])
  
  ## Return
  return(output)
}

