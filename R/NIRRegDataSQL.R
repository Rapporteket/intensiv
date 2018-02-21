#' Henter data registrert for Intensiv
#'
#' Henter data for Intensivregisteret fra "staging"
#'
#' @inheritParams NIRAndeler
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
      Eeg,
      EcmoEcla,
      ExtendedHemodynamicMonitoring,
      Glasgow,
      Hco3,
      HeartRate,
      Hyperbar,
      Iabp,
      Icp, 
      Impella,
      Intermitterende,
      IntermitterendeDays,
      InvasivVentilation,
      Isolation,
      IsolationDaysTotal,
      KidneyReplacingTreatment,
      Kontinuerlig,
      KontinuerligDays,
      Leukocytes,
      Leverdialyse,
      MechanicalRespirator,
      MoreThan24Hours,
      Morsdato,
      MovedPatientToAnotherIntensivDuring24Hours,
      MvOrCpap,
      Nas,
      Nems,
      No,
      NonInvasivVentilation,
      OrganDonationCompletedReasonForNoStatus,
      OrganDonationCompletedStatus,
      Oscillator,
      PatientInRegistryGUID,
      PatientAge,
      PatientGender,
      PatientTransferredFromHospital,
      PatientTransferredToHospital,
      Potassium,
      PrimaryReasonAdmitted,
      ReAdmitted,
      ReshID,
      Respirator,
      Saps2Score,
      Saps2ScoreNumber,
      SerumUreaOrBun,
      ShNavn,
      ShType,
      Sodium,
      SystolicBloodPressure,
      Temperature,
      TerapetiskHypotermi,
      Trakeostomi,
      TransferredStatus,
      TypeOfAdmission,
      UnitId,
      UrineOutput,
      VasoactiveInfusion
FROM
	MainFormDataContract
WHERE cast(DateAdmittedIntensive as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')
  #WHERE cast(DateAdmittedIntensive as date) >= \'', datoFra, '\' AND DateAdmittedIntensive <= \'', datoTil, '\'')  

  RegData <- rapbase::LoadRegData(registryName, query, dbType)
  
  return(RegData)
}