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
  #, session='ingen Rsesjon'
  
#	DischargedHospitalStatus,
  
  query <- paste0('SELECT
	Bilirubin,
      BrainDamage,
      Bukleie,
      CerebralCirculationAbolished,
      CerebralCirculationAbolishedReasonForNo,
      ChronicDiseases,
      cast(DateAdmittedIntensive AS char(20)) AS DateAdmittedIntensive,
      cast(DateDischargedIntensive AS char(20)) AS DateDischargedIntensive,
      DaysAdmittedIntensiv,
      DeadPatientDuring24Hours,
      DischargedIntensiveStatus,
      Eeg,
      EcmoEcla,
      ExtendedHemodynamicMonitoring,
      FormStatus,
      Glasgow,
      Hco3,
      HeartRate,
HF,
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
      cast(Morsdato AS char(20)) AS Morsdato,
      MovedPatientToAnotherIntensivDuring24Hours,
      MvOrCpap,
      Nas,
      Nems,
      No,
      NonInvasivVentilation,
      OrganDonationCompletedReasonForNoStatus,
      OrganDonationCompletedStatus,
      Oscillator,
      -- PasientGUID,
      PatientInRegistryGuid,
      -- PatientInRegistryGUID,
      PatientAge,
      PatientGender,
      PatientTransferredFromHospital,
      PatientTransferredToHospital,
      Potassium,
      PrimaryReasonAdmitted,
      ReAdmitted,
      ReshID,
      Respirator,
RHF,
      Saps2Score,
      Saps2ScoreNumber,
      SerumUreaOrBun,
      ShNavn,
      ShType,
      SkjemaGUID,
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

  RegData <- rapbase::LoadRegData(registryName= "nir", query=query, dbType="mysql")
  
  raplog::repLogger(session = session, 'Hentet alle data fra intensivregisteret')
  
    return(RegData)
}