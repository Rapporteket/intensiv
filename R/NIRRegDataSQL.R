#' Henter data registrert for Intensiv
#'
#' Henter data for Intensivregisteret fra "staging"
#'
#' @inheritParams NIRFigAndeler
#'
#' @return Henter dataramma RegData for Intensivregisteret
#' @export
#'
#'
NIRRegDataSQL <- function(datoFra = '2011-01-01', datoTil = '2099-01-01') {
  
  registryName <- "nir"
  dbType <- "mysql"

#	DischargedHospitalStatus,
  
  query <- paste0('SELECT
	Bilirubin,
	BrainDamage,
	CerebralCirculationAbolished, 
	CerebralCirculationAbolishedReasonForNo,
	ChronicDiseases,
	DateAdmittedIntensive,
	DateDischargedIntensive,
	DaysAdmittedIntensiv,
	DeadPatientDuring24Hours,
	DischargedIntensiveStatus,
	Glasgow,
	Hco3,
	HeartRate,
	Leukocytes,
	MechanicalRespirator,
	MoreThan24Hours,
	Morsdato,
	MovedPatientToAnotherIntensivDuring24Hours,
	MvOrCpap,
	Nas,
	Nems,
	OrganDonationCompletedReasonForNoStatus,
	OrganDonationCompletedStatus,
	PatientAge,
	PatientGender,
    PasientGUID,
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
WHERE cast(DateAdmittedIntensive as date) BETWEEN \'', datoFra, '\' \'', datoTil, '\'')
#WHERE cast(DateAdmittedIntensive as date) >= \'', datoFra, '\' AND DateAdmittedIntensive <= \'', datoTil, '\'')
  
  RegData <- rapbase::LoadRegData(registryName, query, dbType)
  
  return(RegData)
}