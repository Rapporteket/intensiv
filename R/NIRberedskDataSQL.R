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
NIRberedskDataSQL <- function(datoFra = '2020-03-01', datoTil = Sys.Date()) {
      
      
  varBeredsk <- c("UPPER(SkjemaGUID) AS SkjemaGUID
,AddressQuality
,AgeAdmitted
,Astma
-- ,Birthdate
,CurrentMunicipalNumber
,DateAdmittedIntensive
,DateDischargedIntensive
,DaysAdmittedIntensiv
,DeadPatientDuring24Hours
,Diabetes
,Diagnosis
,DischargedIntensivStatus
,DistrictCode
,EcmoEnd
,EcmoStart
,FormDate
,FormStatus
,FormTypeId
,Graviditet
,Helseenhet
,HelseenhetID
,HelseenhetKortnavn
,HF
,HovedskjemaGUID
,IsActivSmoker
,IsChronicLungDiseasePatient
,IsChronicNeurologicNeuromuscularPatient
,IsEcmoTreatmentAdministered
,IsHeartDiseaseIncludingHypertensionPatient
,IsImpairedImmuneSystemIncludingHivPatient
,IsKidneyDiseaseIncludingFailurePatient
,IsLiverDiseaseIncludingFailurePatient
,IsObesePatient
,IsRiskFactor
,Kreft
,LastUpdate
,MajorVersion
,MechanicalRespirator
,MechanicalRespiratorEnd
,MechanicalRespiratorStart
,MinorVersion
,MoreThan24Hours
,Morsdato
,MorsdatoOppdatert
,MovedPatientToAnotherIntensivDuring24Hours
,Municipal
,MunicipalNumber
,PatientAge
,PatientGender
,PatientInRegistryGuid
-- ,PostalCode
,RHF
,ShNavn
,SkjemaGUID
,Sykehus
,TransferredStatus
,UnitId")
              
      
      query <- paste0('SELECT ',
                      varBeredsk,
                      ' FROM ReadinessFormDataContract Q
                      WHERE cast(DateAdmittedIntensive as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')

      
      RegData <- rapbase::LoadRegData(registryName="nir", query=query, dbType="mysql")
      return(RegData)
}