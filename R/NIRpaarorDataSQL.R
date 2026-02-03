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
NIRpaarorDataSQL <- function(datoFra = '2023-10-01', datoTil = Sys.Date(), medH=0) {

  varPaaror <- 'PasientGUID,
SkjemaGUID,
HovedskjemaGUID,
UnitId,
FormVersionNumber,
FormStatus,
CreationDate,
FormDate,
LastUpdate,
LastUpdateBy,
RHF,
CreatedBy,
Owner,
HF,
Hospital,
HealthUnitName,
HealthUnitShortName,
HealthUnitId,
PatientAge,
PatientGender,
MunicipalNumber,
CurrentMunicipalNumber,
Municipal,
PostalCode,
DistrictCode,
AddressQuality,
FirstTimeClosed,
PasientHash,
DateDischargedIntensive,
DateAdmittedIntensive,
Svardato_paarorendeskjema,
Kjoenn,
Alder,
PasientRelasjon,
PasientRelasjonAnnet,
IntensivAvdelingInvolvertFoer,
BorMedPasienten,
HvorOfteSerDuPasienten,
HvorBorDu,
HoeyesteFullfoerteUtdannelse,
BehandlingHoeflighetRespektMedfoelelse_2,
BehandlingHoeflighetRespektMedfoelelseSkaaring_2,
SymptomSmerte_2,
SymptomSmerteSkaaring_2,
SymptomPustebesvaer_2,
SymptomPustebesvaerSkaaring_2,
SymptomUro_2,
SymptomUroSkaaring_2,
BehandlingBesvarerBeho_2v,
BehandlingBesvarerBehovSkaaring_2,
BehandlingBesvarerStoette_2,
BehandlingBesvarerStoetteSkaaring_2,
BehandlingSamarbeid_2,
BehandlingSamarbeidSkaaring_2,
BehandlingBesvarerHoeflighetRespektMedfoelelseSkaaring_2,
BehandlingBesvarerHoeflighetRespektMedfoelelse_2,
SykepleierOmsorg_2,
SykepleierOmsorgSkaaring_2,
SykepleierKommunikasjon_2,
SykepleierKommunikasjonSkaaring_2,
LegeBehandling_2,
LegeBehandlingSkaaring_2,
AtmosfaerenIntensivAvd_2,
AtmosfaerenIntensivAvdSkaaring_2,
AtmosfaerenPaaroerenderom_2,
AtmosfaerenPaaroerenderomSkaaring_2,
OmfangetAvBehandlingen_2,
OmfangetAvBehandlingenSkaaring_2,
DeltagelseIOmsorg,
DeltagelseIOmsorgSkaaring,
MengdenAvHelsetjenester,
ReshId,
MengdenAvHelsetjenesterSkaaring,
SumScoreSatisfactionCare_2,
LegeInformasjonFrekvens_2,
LegeInformasjonFrekvensSkaaring_2,
SvarPaaSpoersmaal_2,
SvarPaaSpoersmaalSkaaring_2,
ForklaringForstaaelse_2,
ForklaringForstaaelseSkaaring_2,
InformasjonsAerlighet_2,
InformasjonsAerlighetSkaaring_2,
InformasjonOmForloep_2,
InformasjonOmForloepSkaaring_2,
InformasjonsOverensstemmelse_2,
InformasjonsOverensstemmelseSkaaring_2,
BeslutningsInvolvering_2,
BeslutningsInvolveringSkaaring_2,
BeslutningsStoette_2,
BeslutningsStoetteSkaaring_2,
BeslutningsKontroll_2,
BeslutningsKontrollSkaaring_2,
BeslutningsTid_2,
BeslutningsTidSkaaring_2,
SumScoreSatisfactionDecision_2,
LivsLengde_2,
LivsLengdeSkaaring_2,
LivssluttKomfor_2,
LivssluttKomforSkaaring_2,
LivssluttStoette_2,
LivssluttStoetteSkaaring_2,
SumScoreAllQuestions_2'



  #varPaaror <- '*'
  datoFraP <- '2024-11-01'
  datoTilP <- Sys.Date()
  queryP <- paste0('SELECT ',
                   varPaaror,
                   ' FROM sporreskjema_om_paarorendes_tilfredshet_med_behandlingen
                   WHERE FormVersionNumber > 13')
  # WHERE cast(FormDate as date) BETWEEN \'', datoFraP, '\' AND \'', datoTil, '\'')

PaarorData <- rapbase::loadRegData(
    registryName="data",
    query= queryP)

  # query <- switch(as.character(medH),
  #                 '0' = queryP,
  #                 '1' = queryH)

if (medH == 1){


  varHoved <- "SkjemaGUID
                ,DateAdmittedIntensive
                ,DaysAdmittedIntensiv
                ,Respirator
                ,TransferredStatus
                ,Saps2Score
                ,Saps2ScoreNumber
                ,TypeOfAdmission
                ,Nems
                ,Morsdato
                ,PatientTransferredFromHospital
                ,PatientTransferredToHospital
                ,ShNavn
                ,DateDischargedIntensive "

  # varHoved <- '*'
  queryH <- paste0(
    'SELECT ', varHoved,
    'FROM intensivopphold
     WHERE cast(DateAdmittedIntensive as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')

  HovedData <- rapbase::loadRegData(registryName="data", query=queryH)

  PaarorData <- merge(PaarorData, HovedData,
        by.x='HovedskjemaGUID', by.y = 'SkjemaGUID',
        incomparables = NA)

}



  #Variabler i gammelt skjema
  varPaaror1 <- 'Q.SkjemaGUID
  , Q.Kjoenn
  , Q.Alder AS AlderPaaror
  , Q.PasientRelasjon
  , Q.PasientRelasjonAnnet
  , Q.IntensivAvdelingInvolvertFoer
  , Q.BorMedPasienten
  , Q.HvorOfteSerDuPasienten
  , Q.HvorBorDu
  , Q.FormStatus
  , Q.KontaktMedInstensivavdelingen
  , Q.BehandlingHoeflighetRespektMedfoelelse
  , Q.BehandlingHoeflighetRespektMedfoelelseSkaaring
  , Q.SymptomSmerte
  , Q.SymptomSmerteSkaaring
  , Q.SymptomPustebesvaer
  , Q.SymptomPustebesvaerSkaaring
  , Q.SymptomUro
  , Q.SymptomUroSkaaring
  , Q.BehandlingBesvarerBehov
  , Q.BehandlingBesvarerBehovSkaaring
  , Q.BehandlingBesvarerStoette
  , Q.BehandlingBesvarerStoetteSkaaring
  , Q.BehandlingSamarbeid
  , Q.BehandlingSamarbeidSkaaring
  , Q.BehandlingBesvarerHoeflighetRespektMedfoelelse
  , Q.BehandlingBesvarerHoeflighetRespektMedfoelelseSkaaring
  , Q.SykepleierOmsorg
  , Q.SykepleierOmsorgSkaaring
  , Q.SykepleierKommunikasjon
  , Q.SykepleierKommunikasjonSkaaring
  , Q.LegeBehandling
  , Q.LegeBehandlingSkaaring
  , Q.AtmosfaerenIntensivAvd
  , Q.AtmosfaerenIntensivAvdSkaaring
  , Q.AtmosfaerenPaaroerenderom
  , Q.AtmosfaerenPaaroerenderomSkaaring
  , Q.OmfangetAvBehandlingen
  , Q.OmfangetAvBehandlingenSkaaring
  , Q.SumScoreSatisfactionCare
  , Q.LegeInformasjonFrekvens
  , Q.LegeInformasjonFrekvensSkaaring
  , Q.SvarPaaSpoersmaal
  , Q.SvarPaaSpoersmaalSkaaring
  , Q.ForklaringForstaaelse
  , Q.ForklaringForstaaelseSkaaring
  , Q.InformasjonsAerlighet
  , Q.InformasjonsAerlighetSkaaring
  , Q.InformasjonOmForloep
  , Q.InformasjonOmForloepSkaaring
  , Q.InformasjonsOverensstemmelse
  , Q.InformasjonsOverensstemmelseSkaaring
  , Q.BeslutningsInvolvering
  , Q.BeslutningsInvolveringSkaaring
  , Q.BeslutningsStoette
  , Q.BeslutningsStoetteSkaaring
  , Q.BeslutningsKontroll
  , Q.BeslutningsKontrollSkaaring
  , Q.BeslutningsTid
  , Q.BeslutningsTidSkaaring
  , Q.SumScoreSatisfactionDecision
  , Q.LivsLengde
  , Q.LivsLengdeSkaaring
  , Q.LivssluttKomfor
  , Q.LivssluttKomforSkaaring
  , Q.LivssluttStoette
  , Q.LivssluttStoetteSkaaring
  , Q.SumScoreAllQuestions
  --     , Q.Forslag
  --       , Q.Kommentar
  --       , Q.Personalet
  -- , Q.ReshId
  , UPPER(Q.HovedskjemaGUID) AS HovedskjemaGUID
  , Q.FormTypeId
  , Q.UnitId AS ReshId
  , Q.RHF
  , Q.HF
  -- , Q.Sykehus
  -- , Q.Helseenhet
  , Q.HealthUnitShortName
  -- , Q.HelseenhetID
  --  , Q.LastUpdate
  --   , Q.FormStatus
  , Q.PatientAge
  , Q.PatientGender
  --      , Q.MunicipalNumber
  --      , Q.CurrentMunicipalNumber
  --  , Q.Municipal
  --  , Q.PostalCode
  --  , Q.DistrictCode
  --  , Q.AddressQuality
  --  , Q.FormDate
  --  , Q.MajorVersion
  --  , Q.MinorVersion
  , Q.PasientGUID AS PasientID'



    return(PaarorData)
}
