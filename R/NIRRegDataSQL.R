#' Henter data registrert for Hjerneslag
#'
#' Henter data for Hjerneslagregisteret fra "staging"
#'
#' @inheritParams FigAndeler
#'
#' @return Henter dataramma RegData for Hjerneslag
#' @export
#'
#'
NIRRegDataSQL <- function(datoFra = '2011-01-01', datoTil = '2099-01-01') {
  
  registryName <- "intensiv"
  dbType <- "mysql"
  
  query <- paste0('SELECT
	AgeAdmitted,
	Bilirubin,
	BrainDamage,
	CerebralCirculationAbolished, 
	CerebralCirculationAbolishedReasonForNo,
	ChronicDiseases,
	DateAdmittedIntensive,
	DateDischargedIntensive,
	DaysAdmittedIntensiv,
	DeadPatientDuring24Hours,
	AgeAdmitted AS decimalAge,
	DischargedHospitalStatus,
	DischargedIntensiveStatus,
	Glasgow,
	Hco3,
	HeartRate,
	Leukocytes,
	MechanicalRespirator,
	MoreThan24Hours,
	MovedPatientToAnotherIntensivDuring24Hours,
	MvOrCpap,
	Nas,
	Nems,
	OrganDonationCompletedReasonForNoStatus,
	OrganDonationCompletedStatus,
	PatientGender,
	PatientTransferredFromHospital,
	PatientTransferredToHospital,
	Potassium,
	ReAdmitted,
	Respirator,
	ReshId,
	Saps2Score,
	Saps2ScoreNumber,
	SerumUreaOrBun,
	ShNavn,
	ShType,
	Sodium,
	SystolicBloodPressure,
	Temperature,
	TransferredStatus,
	TypeOfAdmission,
	UrineOutput
FROM
	Main
WHERE DateAdmittedIntensive >= \'', datoFra, '\' AND DateAdmittedIntensive <= \'', datoTil, '\'')
  
  
  RegData <- rapbase::LoadRegData(registryName, query, dbType)
  
  return(RegData)
}