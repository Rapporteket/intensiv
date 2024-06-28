#' Henter data registrert for Intensiv
#'
#' Henter data for Intensivregisteret fra "staging"
#'
#' @inheritParams NIRFigAndeler
#' @inheritParams NIRUtvalgEnh
#'
#' @return Henter dataramma RegData for Intensivregisteret
#' @export
#'
#'
NIRRegDataSQL <- function(datoFra = '2019-01-01', datoTil = '2099-01-01') { #,session='') {


  #	DischargedHospitalStatus,

  query <- paste0('SELECT
  Age,
  AgeAdmitted,
	Bilirubin,
      BrainDamage,
      Bukleie,
      CerebralCirculationAbolished,
      CerebralCirculationAbolishedReasonForNo,
      ChronicDiseases,
      CreationDate,
      cast(DateAdmittedIntensive AS char(20)) AS DateAdmittedIntensive,
      cast(DateDischargedIntensive AS char(20)) AS DateDischargedIntensive,
      DaysAdmittedIntensiv,
      DeadPatientDuring24Hours,
      DischargedIntensiveStatus,
      Eeg,
      EcmoEcla,
      ExtendedHemodynamicMonitoring,
      FirstTimeClosed,
      FormStatus,
      FrailtyIndex,
      Glasgow,
      Hco3,
      HeartRate,
      -- HelseenhetKortnavn,
      HealthUnitShortName AS HelseenhetKortnavn,
      HF,
      Hyperbar,
      Iabp,
      ICD10_1,
      ICD10_2,
      ICD10_3,
      ICD10_4,
      ICD10_5,
      Icp,
      Impella,
      Intermitterende,
      IntermitterendeDays,
      InvasivVentilation,
      Isolation,
      IsolationDaysTotal,
      KidneyReplacingTreatment,
      KompHypoglykemi,
      KompPneumotoraks,
      KompLuftveisproblem,
      KompDekubitus,
      KomIngen,
      KompIkkeUtfylt,
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
      -- PatientAge,
      PatientGender,
      PatientTransferredFromHospital,
      PatientTransferredFromHospitalName,
      PatientTransferredFromHospitalText,
      PatientTransferredToHospital,
      PatientTransferredToHospitalName,
      PatientTransferredToHospitalText,
      PIM_Probability,
      PIM_Score,
      Potassium,
      PrimaryReasonAdmitted,
      -- ReAdmitted,
      ReshId,
      Respirator,
      RHF,
      Saps2Score,
      Saps2ScoreNumber,
      SecondaryReasonAdmitted,
      SerumUreaOrBun,
      ShNavn,
      ShType,
      SkjemaGUID,
      Sodium,
      Sofa,
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

   # query <- paste0('select * from MainFormDataContract
   # WHERE cast(DateAdmittedIntensive as date) BETWEEN \'', '2020-01-01', '\' AND \'', Sys.Date(), '\'')

  RegData <- rapbase::loadRegData(registryName= "nir", query=query, dbType="mysql")

  #rapbase::repLogger(session = session, 'Hentet alle data fra intensivregisteret')



    return(RegData)
}
