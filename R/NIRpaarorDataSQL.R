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
NIRpaarorDataSQL <- function(datoFra = '2015-12-01', datoTil = Sys.Date(), medH=0) {
  
  
  varHoved <- c("UPPER(M.SkjemaGUID) AS SkjemaGUID
                , M.DateAdmittedIntensive 
                , M.DaysAdmittedIntensiv
                , M.Respirator
                , M.TransferredStatus
                , M.Saps2Score 
                , M.Saps2ScoreNumber 
                , M.TypeOfAdmission 
                , M.Nems 
                , M.Morsdato
                , M.PatientTransferredFromHospital 
                , M.PatientTransferredToHospital 
                , M.ShNavn 
                , M.ShType
                , M.DateDischargedIntensive,")
  varPaaror <- 'Q.SkjemaGUID
  , Q.Kjoenn
  , Q.Alder
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
  , Q.ReshId
  , UPPER(Q.HovedskjemaGUID) AS HovedskjemaGUID
  , Q.FormTypeId
  , Q.UnitId
  , Q.RHF
  , Q.HF
  , Q.Sykehus
  , Q.Helseenhet
  , Q.HelseenhetKortNavn
  , Q.HelseenhetID
  --      , Q.LastUpdate
  --      , Q.FormStatus
  , Q.PatientAge
  , Q.PatientGender
  --      , Q.MunicipalNumber
  --       , Q.CurrentMunicipalNumber
  --  , Q.Municipal
  --  , Q.PostalCode
  --  , Q.DistrictCode
  --  , Q.AddressQuality
  --  , Q.FormDate
  --  , Q.MajorVersion
  --  , Q.MinorVersion
  , Q.PatientInRegistryGuid'
  
  queryH <- paste0('SELECT ',
                   varHoved,
                   varPaaror,
                   ' FROM QuestionaryFormDataContract Q
                   INNER JOIN  MainFormDataContract M
                   ON Q.HovedskjemaGUID = M.SkjemaGUID
                   WHERE cast(DateAdmittedIntensive as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')
  #UPPER(Q.HovedskjemaGUID) = UPPER(M.SkjemaGUID)
  
  queryP <- paste0('SELECT ',
                   varPaaror,
                   ' FROM QuestionaryFormDataContract Q ')
                   #WHERE cast(DateAdmittedIntensive as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')
  
  query <- switch(as.character(medH),
                  '0' = queryP,
                  '1' = queryH)
  
  RegData <- rapbase::LoadRegData(registryName="nir", query=query, dbType="mysql")
  return(RegData)
}