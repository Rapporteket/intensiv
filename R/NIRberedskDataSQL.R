#' Henter data fra pårørendeskjema registrert for Intensiv og kobler til 
#' noen variabler fra hovedskjema.
#'
#' Henter data for Intensivregisterets database
#'
#' @inheritParams NIRFigAndeler
#' @param medH kobler på variabler fra hovedskjema
#'
#' @return Henter dataramma RegData for Intensivregisteret
#' @export
#'
#'
NIRberedskDataSQL <- function(datoFra = '2020-03-01', datoTil = Sys.Date(), medH=0) {
      
      
  varBeredsk <- c("UPPER(M.SkjemaGUID) AS SkjemaGUID
              ,PasientGUID
              ,Skjematype
              ,SkjemaGUID
              ,HovedskjemaGUID
              ,UnitId
              ,FormTypeId
              ,FormStatus
              ,FormDate
              ,LastUpdate
              ,RHF
              ,HF
              ,HealthUnitShortName
              ,HealthUnitId
              ,MigrationInformation
              ,PatientAge
              ,PatientGender
              ,CurrentMunicipalNumber
              ,MunicipalNumber
              ,Municipal
              ,PostalCode
              ,DistrictCode
              ,MoreThan24Hours
              ,MechanicalRespirator
              ,MechanicalRespiratorStart
              ,MechanicalRespiratorEnd
              ,EcmoStart
              ,EcmoEnd
              ,IsEcmoTreatmentAdministered
              ,DeadPatientDuring24Hours
              ,MovedPatientToAnotherIntensivDuring24Hours
              ,Morsdato
              ,VasoactiveInfusion
              ,DateAdmittedIntensive
              ,DateDischargedIntensive
              ,DaysAdmittedIntensiv
              ,AgeAdmitted
              ,Fødselsdato
              ,TransferredStatus
              ,Diagnosis
              ,DischargedIntensiveStatus
              ,IsHighNasalOxyenFlow
              ,IsRiskFactor
              ,IsCancerPatient
              ,IsImpairedImmuneSystemIncludingHivPatient
              ,IsDiabeticPatient
              ,IsHeartDiseaseIncludingHypertensionPatient
              ,IsObesePatient
              ,IsAsthmaticPatient
              ,IsChronicLungDiseasePatient
              ,IsKidneyDiseaseIncludingFailurePatient
              ,IsLiverDiseaseIncludingFailurePatient
              ,IsChronicNeurologicNeuromuscularPatient
              ,IsPregnant
              ,IsActiveSmoker
              ,EcmoDurationInHours
              ,MechanicalRespiratorDurationInHour")
              
      
      queryP <- paste0('SELECT ',
                      varBeredsk,
                      ' FROM BeredskFormDataContract Q
                      WHERE cast(DateAdmittedIntensive as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')

      query <- switch(as.character(medH),
                      '0' = queryP,
                      '1' = queryH)
      
      RegData <- rapbase::LoadRegData(registryName="nir", query=query, dbType="mysql")
      return(RegData)
}