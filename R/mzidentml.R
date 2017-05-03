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
	paste("PE",paste("PEP",get_unique_prot_id(prot$peakid,prot$protid),sep="_"),sep="_")
}

.get_protid <- function(prot){
	paste("PROT",paste("PEP",get_unique_prot_id(prot$peakid,prot$protid),sep="_"),sep="_")
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
		txml <- xml_add_child(xml,.where=0L,"SearchModification",fixedMod=par$fixed[mi],massDelta=par$mass[mi],residues=.get_res(par$site[mi]))
			xml_add_child(txml,"cvParam",accession=.get_unimod_accession(par$name[mi]),name=par$name[mi],cvRef="UNIMOD")
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
			txml <- xml_add_child(xml,.where=0L,"IonType",charge=0)
				xml_add_child(txml,"FragmentArray",measure_ref=.get_measureid("m"),values=paste0(dsf[fg,"peak.mass"],collapse=" "))
				xml_add_child(txml,"FragmentArray",measure_ref=.get_measureid("i"),values=paste0(dsf[fg,"peak.intensity"],collapse=" "))
				xml_add_child(txml,"FragmentArray",measure_ref=.get_measureid("err"),values=paste0(dsf[fg,"error"],collapse=" "))
				xml_add_child(txml,"cvParam",accession=frag_param$accession[fpi],name=paste(frag_param$lu[fpi],"ion",sep=" "),cvRef="PSI-MS")
		}
	}

	xml
}

.gen_data_results_xml <- function(xml, data){
	plids <- unique(data@scan$plid)
	for(plid in plids){
		scanid <- which(data@scan$plid == plid)[1]
		sids <- which(data@prot$peakid==plid)
		sirxml <- xml_add_child(xml,.where=0L,"SpectrumIdentificationResult",id=paste("SIR",plid,sep="_"),spectrumID=paste("index",plid,sep="="),spectraData_ref=.get_sid(plid))
		for(dsi in sids){
			protind <- which(get_unique_prot_id(data@prot$peakid,data@prot$protid)==data@search[dsi,"searchid"])
			siixml <- xml_add_child(sirxml,.where=0L,"SpectrumIdentificationItem",peptide_ref=.get_protid(data@prot[protind,]),chargeState=data@scan[scanid,"z"],experimentalMassToCharge=data@scan[scanid,"mz"],id=paste("SII",paste(plid,dsi,sep="_"),sep="_"),passThreshold=1,rank=dsi)
				xml_add_child(siixml,"PeptideEvidenceRef",peptideEvidence_ref=.get_pep_evidenceid(data@prot[protind,]))
				if(length(data@fit) > 0){
					dsfi <- subset(data@fit,peak.id==plid & protid==data@prot[protind,"protid"])
					if(nrow(dsfi)>0){
						xml_add_child(siixml,"Fragmentation") %>%
							.gen_frags(dsfi)
					}
				}
		}
	}

	fragl <- data.frame(sn=c("m","i","err"),accession=c("MS:1001225","MS:1001226","MS:1001227"),name=c("product ion m/z","product ion intensity","product ion m/z error"),stringsAsFactors=F)
	ftxml <- xml_add_child(xml,.where=0L,"FragmentationTable")
	for(i in 1:nrow(fragl)){
		xml_add_child(ftxml,.where=0L,"Measure",id=.get_measureid(fragl$sn[i])) %>%
			xml_add_child("cvParam",cvRef="PSI-MS",accession=fragl$accession[i],name=fragl$name[i])
	}

	xml
}

.gen_data_prot_xml <- function(xml, data){
	pid <- .get_protid(data@prot)
	peid <- .get_pep_evidenceid(data@prot)
	for(i in 1:nrow(data@prot)){
		xml_add_child(xml,.where=0L,"PeptideEvidence",dBSequence_ref=paste("DBS",i,sep="_"),id=peid[i],peptide_ref=pid[i])
	}

	safeprot <- gsub("[^A-Z]","X",data@prot[,"seq"])
	for(i in 1:nrow(data@prot)){
		#TOOD: Check sequence uniqueness
		xml_add_child(xml,.where=0L,"Peptide",id=pid[i]) %>%
			xml_add_child("PeptideSequence",safeprot[i])
	}

	acc <- .get_prot_accession(data@prot[,"name"])
	for(i in 1:nrow(data@prot)){
		xml_add_child(xml,.where=0L,"DBSequence",accession=acc[i],id=paste("DBS",i,sep="_"),searchDatabase_ref="SDB_1")
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
#' \dontrun{
#' server <- "http://bupid.bumc.bu.edu/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' write.mzid(data,"data.mzid",infile)
#' }
#' 
#' @export write.mzid
write.mzid <- function(data,file,inputfile=NULL){
	options(suppressXMLNamespaceWarning=T)
	datetime <- format(Sys.time(),"%Y-%m-%dT%H:%M:%S")

	if(is.null(inputfile)) inputfile <- "yaml.results"

	# data independent header information
	xml <- xml_new_root("MzIdentML",id="1234", version="1.1.1", creationDate=datetime, "xsi:schemaLocation"="http://psidev.info/psi/pi/mzIdentML/1.1.1 mzIdentML1.1.1.xsd",
				   xmlns="http://psidev.info/psi/pi/mzIdentML/1.1.1","xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance")

	txml <- xml_add_child(xml,"cvList")
		xml_add_child(txml,"cv", id="UNIMOD",fullName="UNIMOD",uri="http://www.unimod.org/obo/unimod.obo")
		xml_add_child(txml,"cv", id="PSI-MS",fullName="Proteomics Standards Initiative Mass Spectrometry Vocabularies",uri="http://psidev.cvs.sourceforge.net/viewvc/*checkout*/psidev/psi/psi-ms/mzML/controlledVocabulary/psi-ms.obo",version="2.32.0")
		xml_add_child(txml,"cv", id="UO",fullName="UNIT-ONTOLOGY",uri="http://obo.cvs.sourceforge.net/*checkout*/obo/obo/ontology/phenotype/unit.obo")

	# TODO: list each module ?
	xml_add_child(xml,"AnalysisSoftwareList") %>%
		xml_add_child("AnalysisSoftware",id="AS_bupid_topdown",name="BUPID Top-Down",version="1.0.0",uri="http://bupid.bumc.bu.edu/") %>%
			xml_add_child("SoftwareName") %>%
				xml_add_child("userParam",name="BUPID Top-Down")

	xml_add_child(xml,"SequenceCollection") %>%
		.gen_data_prot_xml(data)

	sixml <- xml_add_child(xml,"AnalysisCollection") %>%
		xml_add_child("SpectrumIdentification",id="SPEC_ID_1",spectrumIdentificationList_ref="SIL_LIST_1",spectrumIdentificationProtocol_ref="SIP_1")
			for(si in unique(data@decon$id)){
				xml_add_child(sixml,.where=0L,"InputSpectra",spectraData_ref=.get_sid(si))
			}
			xml_add_child(sixml,"SearchDatabaseRef",searchDatabase_ref="SDB_1")

	sipxml <- xml_add_child(xml,"AnalysisProtocolCollection") %>%
		xml_add_child("SpectrumIdentificationProtocol",analysisSoftware_ref="AS_bupid_topdown",id="SIP_1")
			xml_add_child(sipxml,"SearchType") %>%
				xml_add_child("userParam",name="top-down tag or ms/ms search")
			if(nrow(data@mod)>0){
				xml_add_child(sipxml,"ModificationParams") %>%
					.gen_mods(unique(data@mod[,2:ncol(data@mod)]))
			}
			txml <- xml_add_child(sipxml,"FragmentTolerance")
				xml_add_child(txml,"cvParam",accession="MS:1001412",cvRef="PSI-MS",name="search tolerance plus value",value=data@param$msmstol[1],unitAccession="UO:0000169",unitName="parts per million",unitCvRef="UO")
				xml_add_child(txml,"cvParam",accession="MS:1001413",cvRef="PSI-MS",name="search tolerance minus value",value=data@param$msmstol[1],unitAccession="UO:0000169",unitName="parts per million",unitCvRef="UO")
			txml <- xml_add_child(sipxml,"ParentTolerance")
				xml_add_child(txml,"cvParam",accession="MS:1001412",cvRef="PSI-MS",name="search tolerance plus value",value=data@param$mstol[1],unitAccession="UO:0000221",unitName="dalton",unitCvRef="UO")
				xml_add_child(txml,"cvParam",accession="MS:1001413",cvRef="PSI-MS",name="search tolerance minus value",value=data@param$mstol[1],unitAccession="UO:0000221",unitName="dalton",unitCvRef="UO")
			xml_add_child(sipxml,"Threshold") %>%
				xml_add_child("cvParam",accession="MS:1001494",cvRef="PSI-MS",name="no threshold")
			dfxml <- xml_add_child(sipxml,"DatabaseFilters")
				fxml <- xml_add_child(dfxml,"Filter")
					xml_add_child(fxml,"FilterType") %>%
						xml_add_child("cvParam",accession="MS:1001020",cvRef="PSI-MS",name="DB filter taxonomy")
					xml_add_child(fxml,"Include") %>%
						xml_add_child("cvParam",accession="MS:1001469",cvRef="PSI-MS",name="taxonomy: scientific name",value=data@param$taxonomy[1])
				fxml <- xml_add_child(dfxml,"Filter")
					xml_add_child(fxml,"FilterType") %>%
						xml_add_child("cvParam",accession="MS:1001022",cvRef="PSI-MS",name="DB MW filter")
					txml <- xml_add_child(fxml,"Include")
						xml_add_child(txml,"cvParam",accession="MS:1001201",cvRef="PSI-MS",name="DB MW filter maximum",value=.get_param_ms_max(data@param[1,]))
						xml_add_child(txml,"cvParam",accession="MS:1001202",cvRef="PSI-MS",name="DB MW filter minimum",value=.get_param_ms_min(data@param[1,]))

	dcxml <- xml_add_child(xml,"DataCollection")
		ixml <- xml_add_child(dcxml,"Inputs")
			for(si in unique(data@decon$id)){
				xml_add_child(ixml,.where=0L,"SpectraData",id=.get_sid(si),location=inputfile) %>%
					xml_add_child("SpectrumIDFormat") %>%
						xml_add_child("cvParam",accession="MS:1000774",cvRef="PSI-MS",name="multiple peak list nativeID format")
			}
			xml_add_child(ixml,.where=0L,"SearchDatabase",id="SDB_1",location="/dev/zero") %>%
				xml_add_child("DatabaseName") %>%
					xml_add_child("userParam",name="BUPID Top-Down Database - Swiss-Prot or manual input")
		xml_add_child(dcxml,"AnalysisData") %>%
			xml_add_child("SpectrumIdentificationList",id="SIL_LIST_1") %>%
				.gen_data_results_xml(data)

	write_xml(xml,file,options="as_xml")

	file
}
