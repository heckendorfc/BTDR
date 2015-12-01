.get_measureid <- function(id){
	paste("MID",id,sep="_")
}

.get_sid <- function(scan){
	paste("SID",scan,sep="_")
}

.get_prot_accession <- function(name){
	sub("[|].*","",name)
}

.get_pep_evidenceid <- function(prot){
	paste("PE",paste("PEP",get_unique_prot_id(prot$peakid,prot$id),sep="_"),sep="_")
}

.get_protid <- function(prot){
	paste("PROT",paste("PEP",get_unique_prot_id(prot$peakid,prot$id),sep="_"),sep="_")
}

.get_param_ms_max <- function(par){
	par$msmass+par$mstol
}

.get_param_ms_min <- function(par){
	par$msmass-par$mstol
}

.get_res <- function(site){
	if(site>=0 && site<22)
		substr("ACDEFGHIKLMNPQRSTVWY..",site+1,site+1)
	else
		"."
}

.get_unimod_accession <- function(name){
	name <- sub(" (.+)$","",name)
	unimododo$id[which(unimododo$name==name)]
}

.gen_mods <- function(xml,par){
	for(mi in 1:nrow(par)){
		xml$addNode("SearchModification",attrs=c(fixedMod=par$fixed[mi],massDelta=par$mass[mi],residues=.get_res(par$site[mi])),close=F)
			xml$addNode("cvParam",attrs=c(accession=.get_unimod_accession(par$name[mi]),name=par$name[mi],cvRef="UNIMOD"))
		xml$closeTag() #SearchModification
	}

	xml
}

.gen_frags <- function(xml,dsf){
	#TODO: internal and intact
	frag_param <- data.frame(lu=c("a","b","c","x","y","z"),accession=c("MS:1001229","MS:1001224","MS:1001231","MS:1001228","MS:1001220","MS:1001230"),stringsAsFactors=F)

	fragtype <- substr(dsf$frag,1,1)
	for(fi in unique(fragtype)){
		fpi <- which(frag_param$lu==fi)
		if(length(fpi)>0){
			#fg <- which(sapply(1:length(dsf$results),FUN=function(dsfi)if(substr(dsf$results[dsfi]]$frag,1,1)==fi)dsfi else 0)>0)
			fg <- which(fragtype==fi)
			#zlist <- sapply(fg,FUN=function(fgi) dsf$prot$param$peaks$z[dsf$results[[fgi]]$peak+1])
			#mlist <- sapply(fg,FUN=function(fgi) dsf$results[[fgi]]$ion$mass)
			#ilist <- sapply(fg,FUN=function(fgi) dsf$prot$param$peaks$intensity[dsf$results[[fgi]]$peak+1])
			#errlist <- sapply(fg,FUN=function(fgi) dsf$results[[fgi]]$err)
			xml$addNode("IonType",attrs=c(charge=0),close=F)
				xml$addNode("FragmentArray",attrs=c(measure_ref=.get_measureid("m"),values=paste0(dsf[fg,"peak.mass"],collapse=" ")))
				xml$addNode("FragmentArray",attrs=c(measure_ref=.get_measureid("i"),values=paste0(dsf[fg,"peak.intensity"],collapse=" ")))
				xml$addNode("FragmentArray",attrs=c(measure_ref=.get_measureid("err"),values=paste0(dsf[fg,"error"],collapse=" ")))
				xml$addNode("cvParam",attrs=c(accession=frag_param$accession[fpi],name=paste(frag_param$lu[fpi],"ion",sep=" "),cvRef="PSI-MS"))
			xml$closeTag() #IonType
		}
	}

	xml
}

.gen_data_results_xml <- function(xml, data){
	fragl <- data.frame(sn=c("m","i","err"),accession=c("MS:1001225","MS:1001226","MS:1001227"),name=c("product ion m/z","product ion intensity","product ion m/z error"),stringsAsFactors=F)
	xml$addNode("FragmentationTable",close=F)
	for(i in 1:nrow(fragl)){
		xml$addNode("Measure",attrs=c(id=.get_measureid(fragl$sn[i])),close=F)
			xml$addNode("cvParam",attrs=c(cvRef="PSI-MS",accession=fragl$accession[i],name=fragl$name[i]))
		xml$closeTag() #Measure
	}
	xml$closeTag() #FragmentationTable

	plids <- unique(data@decon$id)
	for(plid in plids){
		#sids <- which(sapply(1:length(data$search),FUN=function(dsi) if(data$search[[dsi]]$prot$param$peaks$id==plid) dsi else 0 )>0)
		sids <- subset(data@prot,plid==plid)
		ds <- data@search
		xml$addNode("SpectrumIdentificationResult",attrs=c(id=paste("SIR",plid,sep="_"),spectrumID=paste("index",plid,sep="="),spectraData_ref=.get_sid(plid)),close=F)
		for(dsi in 1:nrow(sids)){
			protind <- which(get_unique_prot_id(data@prot$peakid,data@prot$protid)==data@search[dsi,"searchid"])
			paramind <- which(data@param$peakid==data@search[dsi,"peakid"])
			xml$addNode("SpectrumIdentificationItem",attrs=c(peptide_ref=.get_protid(data@prot[protind,]),chargeState=0,experimentalMassToCharge=data@param$msmass[paramind],id=paste("SII",paste(plid,dsi,sep="_"),sep="_"),passThreshold=1,rank=dsi),close=F)
				xml$addNode("PeptideEvidenceRef",attrs=c(peptideEvidence_ref=.get_pep_evidenceid(data@prot[protind,])))
				dsfi <- subset(data@fit,peak.id==plid & protid==data@prot[protind,"protid"])
				#dsfi <- which(sapply(1:length(data$fit),FUN=function(i){ if(data$fit[[i]]$prot$param$peaks$id==plid && data$fit[[i]]$prot$param$msmass==ds[[dsi]]$prot$param$msmass) i else 0})>0)
				if(nrow(dsfi)>0){
					xml$addNode("Fragmentation",close=F)
					xml <- .gen_frags(xml,dsfi)
					#xml <- .gen_frags(xml,data@fit[dsfi,])
					xml$closeTag() #Fragmentation
				}
			xml$closeTag() #SpectrumIdentificationItem
		}
		xml$closeTag() #SpectrumIdentificationResult
	}

	xml
}

.gen_data_prot_xml <- function(xml, data){
	for(i in 1:nrow(data@prot)){
		xml$addNode("DBSequence",attrs=c(accession=.get_prot_accession(data@prot[i,"name"]),id=paste("DBS",i,sep="_"),searchDatabase_ref="SDB_1"))
	}

	for(i in 1:nrow(data@prot)){
		#TOOD: Check sequence uniqueness
		xml$addNode("Peptide",attrs=c(id=.get_protid(data@prot[i,])),close=F)
			xml$addNode("PeptideSequence",data@prot[i,"seq"])
		xml$closeTag() #Peptide
	}

	for(i in 1:nrow(data@prot)){
		xml$addNode("PeptideEvidence",attrs=c(dBSequence_ref=paste("DBS",i,sep="_"),id=.get_pep_evidenceid(data@prot[i,]),peptide_ref=.get_protid(data@prot[i,])))
	}

	xml
}



#' Write results as mzIdentML.
#' 
#' Convert an object created with read.bupid to mzIdentML.
#' 
#' @param data
#' output from read.bupid
#' @param file
#' the name of the file you want to save the mzIdentML results as.
#' @param inputfile
#' the name of file passed to read.bupid.
#' 
#' @return Returns the name of the file generated.
#' @seealso \code{\link{read.bupid}}
#' @examples
#' server <- "http://bupid.bumc.bu.edu/BUPID_TD/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' write.mzid(data,"data.mzid",infile)
#' 
#' @export write.mzid
write.mzid <- function(data,file,inputfile=NULL){
	options(suppressXMLNamespaceWarning=T)
	datetime <- format(Sys.time(),"%Y-%m-%dT%H:%M:%S")

	# data independent header information
	xml <- xmlTree("MzIdentML",attrs=c(id="", version="1.1.0", creationDate=datetime, "xsi:schemaLocation"="http://psidev.info/psi/pi/mzIdentML/1.1 mzIdentML1.1.0.xsd"), namespaces=c("http://psidev.info/psi/pi/mzIdentML/1.1",xsi="http://www.w3.org/2001/XMLSchema-instance"), dtd="mzidentml")

	xml$addNode("cvList",close=F)
		xml$addNode("cv", attrs=c(id="UNIMOD",fullName="UNIMOD",uri="http://www.unimod.org/obo/unimod.obo"))
		xml$addNode("cv", attrs=c(id="PSI-MS",fullName="Proteomics Standards Initiative Mass Spectrometry Vocabularies",uri="http://psidev.cvs.sourceforge.net/viewvc/*checkout*/psidev/psi/psi-ms/mzML/controlledVocabulary/psi-ms.obo",version="2.32.0"))
		xml$addNode("cv", attrs=c(id="UO",fullName="UNIT-ONTOLOGY",uri="http://obo.cvs.sourceforge.net/*checkout*/obo/obo/ontology/phenotype/unit.obo"))
	xml$closeTag() #cvList

	# TODO: list each module ?
	xml$addNode("AnalysisSoftwareList",close=F)
		xml$addNode("AnalysisSoftware",attrs=c(id="AS_bupid_topdown",name="BUPID Top-Down",version="1.0.0",uri="http://bupid.bumc.bu.edu/BUPID_TD/"),close=F)
			xml$addNode("SoftwareName",close=F)
				xml$addNode("userParam",attrs=c(name="BUPID Top-Down"))
			xml$closeTag() #SoftwareName
		xml$closeTag() #AnalysisSoftware
		
	xml$closeTag() #AnalysisSoftwareList

	xml$addNode("SequenceCollection",close=F)
		xml <- .gen_data_prot_xml(xml,data)
	xml$closeTag() #SequenceCollection

	xml$addNode("AnalysisCollection",close=F)
		xml$addNode("SpectrumIdentification",attrs=c(id="SPEC_ID_1",spectrumIdentificationList_ref="SIL_LIST_1",spectrumIdentificationProtocol_ref="SIP_1"),close=F)
			for(si in unique(data@decon$id)){
				xml$addNode("InputSpectra",attrs=c(spectraData_ref=.get_sid(si)))
			}
			xml$addNode("SearchDatabaseRef",attrs=c(searchDatabase_ref="SDB_1"))
		xml$closeTag() #SpectrumIdentification
	xml$closeTag() #AnalysisCollection

	xml$addNode("AnalysisProtocolCollection",close=F)
		xml$addNode("SpectrumIdentificationProtocol",attrs=c(analysisSoftware_ref="AS_bupid_topdown",id="SIP_1"),close=F)
			xml$addNode("SearchType",close=F)
				xml$addNode("userParam",attrs=c(name="top-down tag or ms/ms search"))
			xml$closeTag() #SearchType
			if(nrow(data@mod)>0){
				xml$addNode("ModificationParams",close=F)
					xml <- .gen_mods(xml,unique(data@mod[2:ncol(data@mod),]))
				xml$closeTag() #ModificationParams
			}
			xml$addNode("FragmentTolerance",close=F)
				xml$addNode("cvParam",attrs=c(accession="MS:1001412",cvRef="PSI-MS",name="search tolerance plus value",value=data@param$msmstol[1],unitAccession="UO:0000169",unitName="parts per million",unitCvRef="UO"))
				xml$addNode("cvParam",attrs=c(accession="MS:1001413",cvRef="PSI-MS",name="search tolerance minus value",value=data@param$msmstol[1],unitAccession="UO:0000169",unitName="parts per million",unitCvRef="UO"))
			xml$closeTag() #FragmentTolerance
			xml$addNode("ParentTolerance",close=F)
				xml$addNode("cvParam",attrs=c(accession="MS:1001412",cvRef="PSI-MS",name="search tolerance plus value",value=data@param$mstol[1],unitAccession="UO:0000221",unitName="dalton",unitCvRef="UO"))
				xml$addNode("cvParam",attrs=c(accession="MS:1001413",cvRef="PSI-MS",name="search tolerance minus value",value=data@param$mstol[1],unitAccession="UO:0000221",unitName="dalton",unitCvRef="UO"))
			xml$closeTag() #ParentTolerance
			xml$addNode("Threshold",close=F)
				xml$addNode("cvParam",attrs=c(accession="MS:1001494",cvRef="PSI-MS",name="no threshold"))
			xml$closeTag() #Threshold
			xml$addNode("DatabaseFilters",close=F)
				xml$addNode("Filter",close=F)
					xml$addNode("FilterType",close=F)
						xml$addNode("cvParam",attrs=c(accession="MS:1001020",cvRef="PSI-MS",name="DB filter taxonomy"))
					xml$closeTag() #FilterType
					xml$addNode("Include",close=F)
						xml$addNode("cvParam",attrs=c(accession="MS:1001469",cvRef="PSI-MS",name="taxonomy: scientific name",value=data@param$taxonomy[1]))
					xml$closeTag() #FilterType
				xml$closeTag() #Filter
				xml$addNode("Filter",close=F)
					xml$addNode("FilterType",close=F)
						xml$addNode("cvParam",attrs=c(accession="MS:1001022",cvRef="PSI-MS",name="DB MW filter"))
					xml$closeTag() #FilterType
					xml$addNode("Include",close=F)
						xml$addNode("cvParam",attrs=c(accession="MS:1001201",cvRef="PSI-MS",name="DB MW filter maximum",value=.get_param_ms_max(data@param[1,])))
						xml$addNode("cvParam",attrs=c(accession="MS:1001202",cvRef="PSI-MS",name="DB MW filter minimum",value=.get_param_ms_min(data@param[1,])))
					xml$closeTag() #FilterType
				xml$closeTag() #Filter
			xml$closeTag() #DatabaseFilters
		xml$closeTag() #SpectrumIdentification
	xml$closeTag() #AnalysisProtocolCollection

	xml$addNode("DataCollection",close=F)#
		xml$addNode("Inputs",close=F)
			xml$addNode("SearchDatabase",attrs=c(id="SDB_1",location="/dev/zero"),close=F)
				xml$addNode("DatabaseName",close=F)
					xml$addNode("userParam",attrs=c(name="BUPID Top-Down Database - Swiss-Prot or manual input"))
				xml$closeTag() #SearchDatabase
			xml$closeTag() #SearchDatabase
			for(si in unique(data@decon$id)){
				xml$addNode("SpectraData",attrs=c(id=.get_sid(si),location=inputfile),close=F)
					xml$addNode("SpectrumIDFormat",close=F)
						xml$addNode("cvParam",attrs=c(accession="MS:1000774",cvRef="PSI-MS",name="multiple peak list nativeID format"))
					xml$closeTag() #SpectrumDFormat
				xml$closeTag() #SpectraData
			}
		xml$closeTag() #Inputs
		xml$addNode("AnalysisData",close=F)
			xml$addNode("SpectrumIdentificationList",attrs=c(id="SIL_LIST_1"),close=F)
				xml <- .gen_data_results_xml(xml,data)
			xml$closeTag() #SpectrumIdentificationList
		xml$closeTag() #AnalysisData
	xml$closeTag() #DataCollection

	xml$closeTag() #MZIdentML

	saveXML(xml,prefix = '<?xml version="1.0" encoding="ISO-8859-1"?>\n',file,indent=F)

	file
}
