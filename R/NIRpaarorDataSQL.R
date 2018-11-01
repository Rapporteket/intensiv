#' Henter data fra pårørendeskjema registrert for Intensiv
#'
#' Henter data for Intensivregisteret fra "staging"
#'
#' @inheritParams NIRFigAndeler
#'
#' @return Henter dataramma RegData for Intensivregisteret
#' @export
#'
#'
NIRpaarorDataSQL <- function(datoFra = '2011-01-01', datoTil = '2099-01-01') {
      
      registryName <- "nir"
      dbType <- "mysql"
      
varHoved <- c("SkjemaGUID 
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
      , Q.Forslag
      , Q.Kommentar
      , Q.Personalet
      , Q.ReshId
      , Q.HovedskjemaGUID
      , Q.FormTypeId
      , Q.UnitId
      , Q.RHF
      , Q.HF
      , Q.Sykehus
      , Q.Helseenhet
      , Q.HelseenhetKortNavn
      , Q.HelseenhetID
      , -- Q.LastUpdate
      , --  Q.FormStatus
      , Q.PatientAge
      , Q.PatientGender
      , --  Q.MunicipalNumber, Q.CurrentMunicipalNumber, Q.Municipal, Q.PostalCode, Q.DistrictCode, Q.AddressQuality, Q.FormDate, Q.MajorVersion, Q.MinorVersion
      , Q.PatientInRegistryGuid'


      query <- paste0('SELECT ',
                       varHoved,
                      varPaaror,
                      ' FROM QuestionaryFormDataContract Q
INNER JOIN  MainFormDataContract M
ON UPPER(Q.HovedskjemaGUID) = UPPER(M.SkjemaGUID)' )
#WHERE cast(DateAdmittedIntensive as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')

      RegData <- rapbase::LoadRegData(registryName, query, dbType)
      
      return(RegData)
}