#' Extract calibration results from .txt or .xml files.
#' @description Extract calibration results from ER60 (.txt) or EK80 (.xml) software.
#' @param filename An string containing the name of the calibration file.
#' @param vessel.name A string containing the vessel name.
#' @param survey.name A string containing the survey name.
#' @param cal.group A string containing the name of the calibration group.
#' @return A list containing calibration information, results, and ping data.
#' @export
extract_cal <- function(filename, vessel.name = NA_character_, survey.name = NA_character_,
                        cal.group = NA_character_) {
  if (stringr::str_detect(filename, ".xml")) {
    # Read cal files
    cal <- xml2::read_xml(filename)

    # Extract group data
    Application          <- xtrct_df(cal, "Application")
    EnvironmentData      <- xtrct_df(cal, "EnvironmentData")
    Transducer           <- xtrct_df(cal, "Transducer")
    Transceiver          <- xtrct_df(cal, "Transceiver")
    TransceiverSettings  <- xtrct_df(cal, "TransceiverSetting")
    TargetReference      <- xtrct_df(cal, "TargetReference")
    SingleTargetSettings <- xtrct_df(cal, "SingleTargetDetectorSetting")
    PreviousModelParams  <- xtrct_df(cal, "PreviousModelParameters")
    CalibrationResults   <- xtrct_df(cal, "CalibrationResults")
    Hits                 <- xtrct_df(cal, "HitData")

    # Get calibration info ----------------------------------------------------
    cal.ver              <- Application$SoftwareVersion
    cal.date             <- lubridate::ymd_hms(xtrct_df(cal, "Common")$TimeOfFileCreation)
    comments             <- as.character(NA)

    # Extract sounder info ------------------------------------------
    sounder.type         <- as.character(Transceiver$SoftwareVersion)

    # Extract reference target info -------------------------------------------
    target.type          <- TargetReference$Name
    # For the response and frequency, take mid value of 1000 values
    target.response      <- as.numeric(unlist(stringr::str_split(TargetReference$Response,";")))
    target.frequency     <- as.numeric(unlist(stringr::str_split(TargetReference$Frequency,";")))
    target.speed.long    <- TargetReference$LongitudinalSoundSpeed
    target.speed.trans   <- TargetReference$TransversalSoundSpeed
    target.ts            <- target.response[length(target.response)/2]
    target.dev           <- SingleTargetSettings$TsDeviation
    target.mind          <- min(SingleTargetSettings$Range)
    target.maxd          <- max(SingleTargetSettings$Range)

    # Extract transducer (txdr) info ------------------------------------------
    txdr.type            <- Transducer$Name
    txdr.sn              <- Transducer$SerialNumber
    txdr.freq            <- CalibrationResults$Frequency/1000
    txdr.beam.type       <- stringr::str_replace(TransceiverSettings$BeamType, "BeamType","")
    txdr.gain            <- PreviousModelParams$Gain
    txdr.2way.ba         <- PreviousModelParams$EquivalentBeamAngle
    txdr.athw.ang.sens   <- PreviousModelParams$AngleSensitivityAthwartship
    txdr.alon.ang.sens   <- PreviousModelParams$AngleSensitivityAlongship
    txdr.athw.ba         <- PreviousModelParams$BeamWidthAthwartship
    txdr.alon.ba         <- PreviousModelParams$BeamWidthAlongship
    txdr.athw.oa         <- PreviousModelParams$AngleOffsetAthwartship
    txdr.alon.oa         <- PreviousModelParams$AngleOffsetAlongship
    txdr.sa.corr         <- PreviousModelParams$SaCorrection
    txdr.z               <- Transducer$TransducerDepth

    # Extract transceiver (gpt) info ------------------------------------------
    gpt.type             <- stringr::str_replace(Transceiver$Type, "TransceiverType","")
    gpt.pd               <- TransceiverSettings$PulseLength * 1000
    gpt.si               <- TransceiverSettings$SampleInterval * 1000
    gpt.power            <- TransceiverSettings$TransmitPower
    gpt.pulse.form       <- TransceiverSettings$PulseForm
    gpt.freq.start       <- TransceiverSettings$FrequencyStart
    gpt.freq.end         <- TransceiverSettings$FrequencyEnd
    gpt.rcr.bw           <- NA

    # Extract TS detection info ------------------------------------------
    ts.min.val           <- SingleTargetSettings$MinTSValue
    ts.min.spacing       <- SingleTargetSettings$MinSpacing
    ts.max.beam.comp     <- SingleTargetSettings$MaxGainCompensation
    ts.min.echo.l        <- SingleTargetSettings$MinEchoLength*100
    ts.max.echo.l        <- SingleTargetSettings$MaxEchoLength*100
    ts.max.phase.dev     <- SingleTargetSettings$MaxPhaseDeviation

    # Extract environment info ------------------------------------------
    env.c                <- EnvironmentData$SoundVelocity
    env.alpha            <- EnvironmentData$AbsorptionCoefficient * 1000

    # Extract beam model results ----------------------------------------------
    bm.txdr.gain         <- CalibrationResults$Gain
    bm.sa.corr           <- CalibrationResults$SaCorrection
    bm.athw.ba           <- CalibrationResults$BeamWidthAthwartship
    bm.alon.ba           <- CalibrationResults$BeamWidthAlongship
    bm.athw.oa           <- CalibrationResults$AngleOffsetAthwartship
    bm.alon.oa           <- CalibrationResults$AngleOffsetAlongship

    # Extract data deviation from beam model results --------------------------
    dev.bm.rms           <- CalibrationResults$TsRmsError
    dev.bm.max           <- NA # "Max\\s*=\\s*\\S+\\s+dB"
    dev.bm.max.no        <- NA # "No.\\s*=\\s*\\S+\\s+"
    dev.bm.max.athw      <- NA # "Athw.\\s*=\\s*\\S+\\s+"
    dev.bm.max.alon      <- NA # "Along.\\s*=\\s*\\S+\\s+"
    dev.bm.min           <- NA # "Min\\s*=\\s*\\S+\\s+dB"
    dev.bm.min.no        <- NA # "No.\\s*=\\s*\\S+\\s+"
    dev.bm.min.athw      <- NA # "Athw.\\s*=\\s*\\S+\\s+"
    dev.bm.min.alon      <- NA # "Along.\\s*=\\s*\\S+\\s+"

    # Extract data deviation from polynomial model results --------------------
    dev.poly.rms         <- NA # "RMS\\s*=\\s*\\S+\\s+dB"
    dev.poly.max         <- NA # "Max\\s*=\\s*\\S+\\s+dB"
    dev.poly.max.no      <- NA # "No.\\s*=\\s*\\S+\\s+"
    dev.poly.max.athw    <- NA # "Athw.\\s*=\\s*\\S+\\s+"
    dev.poly.max.alon    <- NA # "Along.\\s*=\\s*\\S+\\s+"
    dev.poly.min         <- NA # "Min\\s*=\\s*\\S+\\s+dB"
    dev.poly.min.no      <- NA # "No.\\s*=\\s*\\S+\\s+"
    dev.poly.min.athw    <- NA # "Athw.\\s*=\\s*\\S+\\s+"
    dev.poly.min.alon    <- NA # "Along.\\s*=\\s*\\S+\\s+"

    # create a id for merging data tables
    id <- paste(format(cal.date, "%Y%m%d"),"_",
                format(Hits$Time[1], format = "%HH%MM%SS"),"_",
                vessel.name, "_", txdr.freq, "kHz_", cal.group, sep = "")

    # extract ping data from end of file
    # extract all rows below header (do not start with #)
    cal.pings <- Hits %>%
      dplyr::select(ping_num = Number,
                    date_time = Time,
                    distance = Range,
                    TS_c = TsComp,
                    TS_u = TsUncomp,
                    athw = Athwart,
                    along = Along,
                    sA = SaValue,
                    outlier = IsSuspended) %>%
      dplyr::mutate(
        txdr_freq = txdr.freq,
        outlier = dplyr::case_when(
          outlier == FALSE ~ 0,
          outlier == TRUE  ~ 1),
        `id` = id)

  } else {
    # Read calibration file
    cal     <- readLines(filename)
    cal.ver <- stringr::str_extract(stringr::str_subset(cal, pattern = 'Calibration  Version\\s+\\S+\\s*'),
                                    "[0-9].*[0-9]")
    # extract calibration date
    cal.date    <- lubridate::mdy(
      stringr::str_extract(stringr::str_subset(cal, pattern = 'Date:\\s+\\S+'), "[0-9].*[0-9]"))
    if (is.na(cal.date) == TRUE) { #use alternate date format
      cal.date  <- lubridate::ymd(stringr::str_extract(
        stringr::str_subset(cal, pattern = 'Date:\\s+\\S+'),"[0-9].*[0-9]"))
    }
    # extract comments; sub semi colons for commas
    comments <- gsub(",", ";", glue::trim(stringr::str_extract(cal[which(cal == "#  Comments:") + 1],"[^#]+")))

    # extract reference target info
    target.ts          <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'TS\\s+\\S+\\sdB'))[1],"[-0-9].*[0-9]"))
    target.dev         <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'TS Deviation\\s+\\S+\\sdB'))[1],"[-0-9].*[0-9]"))
    target.mind        <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Min. Distance\\s+\\S+\\sm')),"[-0-9].*[0-9]"))
    target.maxd        <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Max. Distance\\s+\\S+\\sm')),"[-0-9].*[0-9]"))
    # extract transducer (txdr) info
    txdr.type          <- stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Transducer:\\s+\\S+')),"[A-Z]{2}[0-9]{2,3}.*")
    txdr.sn            <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Serial No.\\s+\\S+')),"[0-9]{1,6}"))
    # often, SN is left blank; this replaces with NA for missing values
    if (length(txdr.sn) == 0) txdr.sn <- NA
    txdr.freq          <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Frequency\\s+\\S+\\sHz')),"[0-9]{1,}"))/1000
    txdr.beam.type     <- stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Beamtype\\s+\\S+')),"(Split)")
    txdr.gain          <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Gain\\s+\\S+\\sdB')),"[0-9]{1,}\\.[0-9]{1,}"))
    txdr.2way.ba       <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Two Way Beam Angle\\s+\\S+\\s+dB')),"[-0-9]{1,}\\.[0-9]{1,}"))
    txdr.athw.ang.sens <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Athw. Angle Sens.\\s+\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
    txdr.alon.ang.sens <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Along. Angle Sens.\\s+\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
    txdr.athw.ba       <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Athw. Beam Angle\\s+\\d+.\\d+\\s?deg+')),"[-0-9]{1,}\\.[0-9]{1,}"))
    txdr.alon.ba       <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Along. Beam Angle\\s+\\d+.\\d+\\s?deg+')),"[-0-9]{1,}\\.[0-9]{1,}"))
    txdr.athw.oa       <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Athw. Offset Angle\\s+[^=]\\S+\\s+deg')),"[-0-9]{1,}\\.[0-9]{1,}"))
    txdr.alon.oa       <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Along. Offset Angle\\s+\\S+\\s+deg')),"[-0-9]{1,}\\.[0-9]{1,}"))
    txdr.sa.corr       <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'SaCorrection\\s+\\S+\\s+dB')),"[-0-9]{1,}\\.[0-9]{1,}"))
    txdr.z             <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Depth\\s+\\S+\\s+m')),"[-0-9]{1,}\\.[0-9]{1,}"))
    # extract transceiver (gpt) info
    gpt.type           <- glue::trim(paste(stringr::str_extract_all(unlist(
      stringr::str_extract_all(cal, pattern = 'Transceiver:[^#]+')),"\\s+[a-zA-Z0-9_-]+")[[1]], collapse = ""))
    gpt.pd             <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Pulse Duration\\s+\\S+\\s+ms')),"[-0-9]{1,}\\.[0-9]{1,}"))
    gpt.si             <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Sample Interval\\s+\\S+\\s+m')),"[-0-9]{1,}\\.[0-9]{1,}")) # in meters
    gpt.power          <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Power\\s+\\S+\\s+W')),"[-0-9]{1,}"))
    gpt.rcr.bw         <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Receiver Bandwidth\\s+\\S+\\s+kHz')),"[-0-9]{1,}\\.[0-9]{1,}"))*1000
    # extract sounder info
    sounder.type       <- as.character(glue::trim(stringr::str_extract(cal[which(cal == "#  Sounder Type:") + 1],"[^#]+")))

    # extract TS detection info
    ts.min.val         <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Min. Value\\s+\\S+\\s+dB')),
      "[-0-9]{1,}\\.[0-9]{1,}"))
    ts.min.spacing     <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Min. Spacing\\s+\\S+\\s+%')),
      "[-0-9]{1,}"))
    # Replace missing spacing data
    ifelse(length(ts.min.spacing) == 0, ts.min.spacing <- NA, ts.min.spacing <- NA)

    ts.max.beam.comp   <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Max. Beam Comp.\\s+\\S+\\s+dB')),
      "[-0-9]{1,}\\.[0-9]{1,}"))
    ts.min.echo.l      <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Min. Echolength\\s+\\S+\\s+%')),
      "[-0-9]{1,}"))
    ts.max.phase.dev   <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Max. Phase Dev.\\s+\\S+')),
      "[0-9]{1,6}"))
    ts.max.echo.l      <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Max. Echolength\\s+\\S+\\s+%')),
      "[-0-9]{1,}"))
    # extract environment info
    env.c              <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Sound Velocity\\s+\\S+\\s+m')),
      "[-0-9]{1,}\\.[0-9]{1,}"))
    env.alpha          <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Absorption Coeff.\\s+\\S+\\s+dB')),
      "[-0-9]{1,}\\.[0-9]{1,}"))
    # extract beam model results
    bm.txdr.gain       <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Transducer Gain\\s*=\\s+\\S+\\s+dB')),
      "[-0-9]{1,}\\.[0-9]{1,}"))
    bm.sa.corr         <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'SaCorrection\\s*=\\s+\\S+\\s+dB')),
      "[-0-9]{1,}\\.[0-9]{1,}"))
    bm.athw.ba         <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Athw. Beam Angle\\s*=\\s?\\S+\\s?deg')),
      "[-0-9]{1,}\\.[0-9]{1,}"))
    bm.alon.ba         <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Along. Beam Angle\\s*=\\s?\\S+\\s?deg')),
      "[-0-9]{1,}\\.[0-9]{1,}"))
    bm.athw.oa         <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Athw. Offset Angle\\s*=\\s*\\S+\\s+deg')),
      "[-0-9]{1,}\\.[0-9]{1,}"))
    bm.alon.oa         <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Along. Offset Angle\\s*=\\s*\\S+\\s+deg')),
      "[-0-9]{1,}\\.[0-9]{1,}"))
    # extract data deviation from beam model results
    dev.bm.rms         <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'RMS\\s*=\\s*\\S+\\s+dB')),
      "[-0-9]{1,}\\.[0-9]{1,}"))[1]
    dev.bm.max         <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Max\\s*=\\s*\\S+\\s+dB')),
      "[-0-9]{1,}\\.[0-9]{1,}"))[1]
    dev.bm.max.no      <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'No.\\s*=\\s*\\S+\\s+')),
      "[-0-9]{1,}"))[1]
    dev.bm.max.athw    <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Athw.\\s*=\\s*\\S+\\s+')),
      "[-0-9]{1,}\\.[0-9]{1,}"))[1]
    dev.bm.max.alon    <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Along.\\s*=\\s*\\S+\\s+')),
      "[-0-9]{1,}\\.[0-9]{1,}"))[1]
    dev.bm.min         <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Min\\s*=\\s*\\S+\\s+dB')),
      "[-0-9]{1,}\\.[0-9]{1,}"))[1]
    dev.bm.min.no      <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'No.\\s*=\\s*\\S+\\s+')),
      "[-0-9]{1,}"))[2]
    dev.bm.min.athw    <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Athw.\\s*=\\s*\\S+\\s+')),
      "[-0-9]{1,}\\.[0-9]{1,}"))[2]
    dev.bm.min.alon    <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Along.\\s*=\\s*\\S+\\s+')),
      "[-0-9]{1,}\\.[0-9]{1,}"))[2]
    # extract data deviation from polynomial model results
    dev.poly.rms       <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'RMS\\s*=\\s*\\S+\\s+dB')),
      "[-0-9]{1,}\\.[0-9]{1,}"))[2]
    dev.poly.max       <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Max\\s*=\\s*\\S+\\s+dB')),
      "[-0-9]{1,}\\.[0-9]{1,}"))[2]
    dev.poly.max.no    <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'No.\\s*=\\s*\\S+\\s+')),
      "[-0-9]{1,}"))[3]
    dev.poly.max.athw  <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Athw.\\s*=\\s*\\S+\\s+')),
      "[-0-9]{1,}\\.[0-9]{1,}"))[3]
    dev.poly.max.alon  <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Along.\\s*=\\s*\\S+\\s+')),
      "[-0-9]{1,}\\.[0-9]{1,}"))[3]
    dev.poly.min       <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Min\\s*=\\s*\\S+\\s+dB')),
      "[-0-9]{1,}\\.[0-9]{1,}"))[2]
    dev.poly.min.no    <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'No.\\s*=\\s*\\S+\\s+')),
      "[-0-9]{1,}"))[4]
    dev.poly.min.athw  <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Athw.\\s*=\\s*\\S+\\s+')),
      "[-0-9]{1,}\\.[0-9]{1,}"))[4]
    dev.poly.min.alon  <- as.numeric(stringr::str_extract(unlist(
      stringr::str_extract_all(cal, pattern = 'Along.\\s*=\\s*\\S+\\s+')),
      "[-0-9]{1,}\\.[0-9]{1,}"))[4]

    # extract ping data from end of file
    # extract all rows below header (do not start with #)
    ping.all <- unlist(stringr::str_extract_all(cal, pattern = '^[^#]+'))
    # identify pings flagged as outliers (start with asterisk)
    outliers <- grep("[\\*]{1}", ping.all)
    # trim whitespace from strings and write to text file
    ping.strings  <- glue::trim(stringr::str_extract(unlist(
      stringr::str_extract_all(ping.all, pattern = '^[^#]+')),
      "[^\\*]+"))

    pings.tmpfile <- file.path(tempdir(), "ping_strings.txt")

    write.table(ping.strings, file = pings.tmpfile,
                quote = F, row.names = F, col.names = F)

    # read text file and add column names
    cal.pings <- read.delim(pings.tmpfile,
                            header = F, sep = "", as.is = TRUE, fill = TRUE) %>%
      tibble::as_tibble()

    # remove temporary text file
    invisible(file.remove(pings.tmpfile))

    names(cal.pings) <- c("ping_num","date_time","distance","TS_c",
                          "TS_u","athw","along","sA")

    # paste date and time and convert time to dttm
    cal.pings <- cal.pings %>%
      dplyr::mutate(date_time = lubridate::ymd_hms(paste(cal.date, .$date_time)))

    # Get index of ping that crosses midnight
    date.id <- which(c(0, diff(cal.pings$date_time)) < 0)

    if (length(date.id) > 0) {
      # Add a day when time goes past midnight
      # Rename time column and remove diff.time
      cal.pings <- cal.pings %>%
        dplyr::mutate(
          date_time = dplyr::case_when(
            ping_num < date.id ~ date_time,
            TRUE ~ date_time + lubridate::days(1)
          ))
    }

    # create outliers vector and set outliers to T [1] for those with asterisks
    cal.pings <- cal.pings %>%
      dplyr::mutate(
        outlier = dplyr::case_when(
          ping_num %in% outliers ~ 1,
          TRUE ~ 0
        ),
        txdr_freq = txdr.freq)

    # create a id for merging data tables
    id <- as.factor(paste(format(cal.date, "%Y%m%d"),"_",
                          format(cal.pings$date_time[1], format = "%HH%MM%SS"),"_",
                          vessel.name, "_", txdr.freq, "kHz_", cal.group, sep = ""))

    # Add id to ping data
    cal.pings$id <- id
  }

  # create a data frame for calibration results
  # create a cal_results.txt table, to hold summary data about transducer calibrations from cal/LOBE files
  cal.res <- tibble::tibble(id,cal.ver,cal.date,comments,target.ts,target.dev,target.mind,target.maxd,
                            txdr.type,txdr.sn,txdr.freq,txdr.beam.type,txdr.gain,txdr.2way.ba,
                            txdr.athw.ang.sens,txdr.alon.ang.sens,
                            txdr.athw.ba,txdr.alon.ba,txdr.athw.oa,txdr.alon.oa,txdr.sa.corr,txdr.z,
                            gpt.type,gpt.pd,gpt.si,gpt.power,gpt.rcr.bw,sounder.type,
                            ts.min.val,ts.min.spacing,ts.max.beam.comp,ts.min.echo.l,ts.max.phase.dev,ts.max.echo.l,
                            env.c,env.alpha,
                            bm.txdr.gain,bm.sa.corr,bm.athw.ba,bm.alon.ba,bm.athw.oa,bm.alon.oa,
                            dev.bm.rms,dev.bm.max,dev.bm.max.no,dev.bm.max.athw,dev.bm.max.alon,dev.bm.min,dev.bm.min.no,
                            dev.bm.min.athw,dev.bm.min.alon,
                            dev.poly.rms,dev.poly.max,dev.poly.max.no,dev.poly.max.athw,dev.poly.max.alon,
                            dev.poly.min,dev.poly.min.no,dev.poly.min.athw,dev.poly.min.alon)

  names(cal.res) <- c("id","cal_ver","cal_date","comments","target_ts","target_dev","target_mind","target_maxd",
                      "txdr_type","txdr_sn","txdr_freq","txdr_beam_type","txdr_gain","txdr_2way_ba",
                      "txdr_athw_ang_sens","txdr_alon_ang_sens",
                      "txdr_athw_ba","txdr_alon_ba","txdr_athw_oa","txdr_alon_oa","txdr_sa_corr","txdr_z",
                      "gpt_type","gpt_pd","gpt_si","gpt_power","gpt_rcr_bw","sounder_type",
                      "ts_min_val","ts_min_spacing","ts_max_beam_comp","ts_min_echo_l",
                      "ts_max_phase_dev","ts_max_echo_l", "env_c","env_alpha",
                      "bm_txdr_gain","bm_sa_corr","bm_athw_ba","bm_alon_ba","bm_athw_oa","bm_alon_oa",
                      "dev_bm_rms","dev_bm_max","dev_bm_max_no","dev_bm_max_athw","dev_bm_max_alon",
                      "dev_bm_min","dev_bm_min_no","dev_bm_min_athw","dev_bm_min_alon","dev_poly_rms",
                      "dev_poly_max","dev_poly_max_no","dev_poly_max_athw","dev_poly_max_alon",
                      "dev_poly_min","dev_poly_min_no","dev_poly_min_athw","dev_poly_min_alon")

  # reformat cal.res$cal.date
  cal.res$cal_date <- format(cal.res$cal_date, format = "%m/%d/%Y")

  # Create a cal_info.txt table, to hold info about calibration files,
  # including vessel, date, freq, file name,file path, etc.
  cal.info <- tibble::tibble(id, vessel_name = vessel.name, survey_name = survey.name,
                             group_name = cal.group,
                             date_time = format(cal.pings$date_time[1], format = "%m/%d/%Y %H:%M:%S"),
                             freq = txdr.freq,
                             filepath = "\\\\swc-storage1.nmfs.local\\AST1\\CALIBRATIONS",
                             filename = filename)

  # Return calibration information as a list of tibbles
  list(cal.res = cal.res, cal.info = cal.info, cal.pings = cal.pings)
}

#' Extract calibration results from calibrations in FM-mode.
#' @description Extract calibration results from ER60 (.txt) or EK80 (.xml) software.
#' @param filename An string containing the name of the calibration file.
#' @param vessel.name A string containing the vessel name.
#' @param survey.name A string containing the survey name.
#' @param cal.group A string containing the name of the calibration group.
#' @return A data frame containing frequency, gain, beam angles, and beam offsets.
#' @export
extract_cal_fm <- function(filename, vessel.name = NA_character_, survey.name = NA_character_,
                           cal.group = NA_character_) {
  if (stringr::str_detect(filename, ".xml")) {
    # Read cal files
    cal <- xml2::read_xml(filename)

    # Extract group data
    Application          <- xtrct_df(cal, "Application")
    EnvironmentData      <- xtrct_df(cal, "EnvironmentData")
    Transducer           <- xtrct_df(cal, "Transducer")
    Transceiver          <- xtrct_df(cal, "Transceiver")
    TransceiverSettings  <- xtrct_df(cal, "TransceiverSetting")
    TargetReference      <- xtrct_df(cal, "TargetReference")
    SingleTargetSettings <- xtrct_df(cal, "SingleTargetDetectorSetting")
    PreviousModelParams  <- xtrct_df(cal, "PreviousModelParameters")
    CalibrationResults   <- xtrct_df(cal, "CalibrationResults")
    Hits                 <- xtrct_df(cal, "HitData")

    # Get calibration info ----------------------------------------------------
    cal.ver              <- Application$SoftwareVersion
    cal.date             <- lubridate::ymd_hms(xtrct_df(cal, "Common")$TimeOfFileCreation)
    comments             <- as.character(NA)

    # Extract sounder info ------------------------------------------
    sounder.type         <- as.character(Transceiver$SoftwareVersion)

    # Extract reference target info -------------------------------------------
    target.type          <- TargetReference$Name
    # For the response and frequency, take mid value of 1000 values
    target.response      <- as.numeric(unlist(stringr::str_split(TargetReference$Response,";")))
    target.frequency     <- as.numeric(unlist(stringr::str_split(TargetReference$Frequency,";")))
    target.speed.long    <- TargetReference$LongitudinalSoundSpeed
    target.speed.trans   <- TargetReference$TransversalSoundSpeed
    target.ts            <- target.response[length(target.response)/2]
    target.dev           <- SingleTargetSettings$TsDeviation
    target.mind          <- min(SingleTargetSettings$Range)
    target.maxd          <- max(SingleTargetSettings$Range)

    # Extract transducer (txdr) info ------------------------------------------
    txdr.type            <- Transducer$Name
    txdr.sn              <- Transducer$SerialNumber
    # txdr.freq            <- CalibrationResults$Frequency/1000
    txdr.freq            <- as.numeric(unlist(stringr::str_split(CalibrationResults$Frequency,";")))/1000
    txdr.beam.type       <- stringr::str_replace(TransceiverSettings$BeamType, "BeamType","")
    txdr.gain            <- PreviousModelParams$Gain
    txdr.2way.ba         <- PreviousModelParams$EquivalentBeamAngle
    txdr.athw.ang.sens   <- PreviousModelParams$AngleSensitivityAthwartship
    txdr.alon.ang.sens   <- PreviousModelParams$AngleSensitivityAlongship
    txdr.athw.ba         <- PreviousModelParams$BeamWidthAthwartship
    txdr.alon.ba         <- PreviousModelParams$BeamWidthAlongship
    txdr.athw.oa         <- PreviousModelParams$AngleOffsetAthwartship
    txdr.alon.oa         <- PreviousModelParams$AngleOffsetAlongship
    txdr.sa.corr         <- PreviousModelParams$SaCorrection
    txdr.z               <- Transducer$TransducerDepth

    # Extract transceiver (gpt) info ------------------------------------------
    gpt.type             <- stringr::str_replace(Transceiver$Type, "TransceiverType","")
    gpt.pd               <- TransceiverSettings$PulseLength * 1000
    gpt.si               <- TransceiverSettings$SampleInterval * 1000
    gpt.power            <- TransceiverSettings$TransmitPower
    gpt.pulse.form       <- TransceiverSettings$PulseForm
    gpt.freq.start       <- TransceiverSettings$FrequencyStart
    gpt.freq.end         <- TransceiverSettings$FrequencyEnd
    gpt.rcr.bw           <- NA

    # Extract TS detection info ------------------------------------------
    ts.min.val           <- SingleTargetSettings$MinTSValue
    ts.min.spacing       <- SingleTargetSettings$MinSpacing
    ts.max.beam.comp     <- SingleTargetSettings$MaxGainCompensation
    ts.min.echo.l        <- SingleTargetSettings$MinEchoLength*100
    ts.max.echo.l        <- SingleTargetSettings$MaxEchoLength*100
    ts.max.phase.dev     <- SingleTargetSettings$MaxPhaseDeviation

    # Extract environment info ------------------------------------------
    env.c                <- EnvironmentData$SoundVelocity
    env.alpha            <- EnvironmentData$AbsorptionCoefficient * 1000

    # Extract beam model results ----------------------------------------------
    bm.txdr.gain         <- as.numeric(unlist(stringr::str_split(CalibrationResults$Gain,";")))
    bm.sa.corr           <- as.numeric(unlist(stringr::str_split(CalibrationResults$SaCorrection,";")))
    bm.athw.ba           <- as.numeric(unlist(stringr::str_split(CalibrationResults$BeamWidthAthwartship,";")))
    bm.alon.ba           <- as.numeric(unlist(stringr::str_split(CalibrationResults$BeamWidthAlongship,";")))
    bm.athw.oa           <- as.numeric(unlist(stringr::str_split(CalibrationResults$AngleOffsetAthwartship,";")))
    bm.alon.oa           <- as.numeric(unlist(stringr::str_split(CalibrationResults$AngleOffsetAlongship,";")))

    # Extract data deviation from beam model results --------------------------
    dev.bm.rms           <- CalibrationResults$TsRmsError
    dev.bm.max           <- NA # "Max\\s*=\\s*\\S+\\s+dB"
    dev.bm.max.no        <- NA # "No.\\s*=\\s*\\S+\\s+"
    dev.bm.max.athw      <- NA # "Athw.\\s*=\\s*\\S+\\s+"
    dev.bm.max.alon      <- NA # "Along.\\s*=\\s*\\S+\\s+"
    dev.bm.min           <- NA # "Min\\s*=\\s*\\S+\\s+dB"
    dev.bm.min.no        <- NA # "No.\\s*=\\s*\\S+\\s+"
    dev.bm.min.athw      <- NA # "Athw.\\s*=\\s*\\S+\\s+"
    dev.bm.min.alon      <- NA # "Along.\\s*=\\s*\\S+\\s+"

    # Extract data deviation from polynomial model results --------------------
    dev.poly.rms         <- NA # "RMS\\s*=\\s*\\S+\\s+dB"
    dev.poly.max         <- NA # "Max\\s*=\\s*\\S+\\s+dB"
    dev.poly.max.no      <- NA # "No.\\s*=\\s*\\S+\\s+"
    dev.poly.max.athw    <- NA # "Athw.\\s*=\\s*\\S+\\s+"
    dev.poly.max.alon    <- NA # "Along.\\s*=\\s*\\S+\\s+"
    dev.poly.min         <- NA # "Min\\s*=\\s*\\S+\\s+dB"
    dev.poly.min.no      <- NA # "No.\\s*=\\s*\\S+\\s+"
    dev.poly.min.athw    <- NA # "Athw.\\s*=\\s*\\S+\\s+"
    dev.poly.min.alon    <- NA # "Along.\\s*=\\s*\\S+\\s+"

    # create a id for merging data tables
    id <- paste(format(cal.date, "%Y%m%d"),"_",
                format(Hits$Time[1], format = "%HH%MM%SS"),"_",
                vessel.name, "_", min(txdr.freq), "-", max(txdr.freq), "kHz_", cal.group, sep = "")

    # extract ping data from end of file
    # extract all rows below header (do not start with #)
    cal.pings <- Hits %>%
      dplyr::select(ping_num = Number,
                    date_time = Time,
                    distance = Range,
                    TS_c = TsComp,
                    TS_u = TsUncomp,
                    athw = Athwart,
                    along = Along,
                    sA = SaValue,
                    outlier = IsSuspended) %>%
      dplyr::mutate(
        txdr_freq = paste0(min(txdr.freq), "-", max(txdr.freq)),
        outlier = dplyr::case_when(
          outlier == FALSE ~ 0,
          outlier == TRUE  ~ 1),
        `id` = id)

  }

  # create a data frame for calibration results
  # create a cal_results.txt table, to hold summary data about transducer calibrations from cal/LOBE files
  cal.res <- tibble::tibble(freq = txdr.freq, gain = bm.txdr.gain,
                            sa.corr = bm.sa.corr, ba.athw = bm.athw.ba,
                            ba.alon = bm.alon.ba, oa.athw = bm.athw.oa,
                            oa.alon = bm.alon.oa)

  # Return calibration information as a list of tibbles
  list(cal.res = cal.res)
}

#' Extract calibration results from Echoview calibration supplement (.ecs) files.
#' @description Extract calibration results from Echoview calibration supplement (.ecs) files.
#' @param filename An string containing the name of the calibration file.
#' @return A data frame containing calibration results.
#' @export
extract_cal_ecs <- function(filename) {

  # Read calibration file
  cal     <- readLines(filename)

  # extract calibration environment
  Temperature   <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '^[^#]*\\s*Temperature\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))[1]
  Salinity      <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '^[^#]*\\s*Salinity\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))[1]
  SoundSpeed    <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '^[^#]*\\s*SoundSpeed\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))[1]

  # extract calibration results
  Freq          <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '\\s+Frequency\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
  BW.athwt      <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '^[^#]*\\s*MajorAxis3dbBeamAngle\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
  BW.along      <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '^[^#]*\\s*MinorAxis3dbBeamAngle\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
  Offset.athwt  <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '^[^#]*\\s*MajorAxisAngleOffset\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
  Offset.along  <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '^[^#]*\\s*MinorAxisAngleOffset\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
  Sa.corr       <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '^[^#]*\\s*SaCorrectionFactor\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
  Gain          <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '^[^#]*\\s*TransducerGain\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
  EBA           <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '^[^#]*\\s*TwoWayBeamAngle\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))

  # create a tibble of calibration results
  cal.res <- tibble::tibble(Temperature,
                            Salinity,
                            SoundSpeed,
                            Freq,
                            BW.athwt,
                            BW.along,
                            Offset.athwt,
                            Offset.along,
                            Sa.corr,
                            Gain,
                            EBA)

  # Define column names
  names(cal.res) <- c("Temperature",
                      "Salinity",
                      "SoundSpeed",
                      "Frequency",
                      "Beamwidth_athwartship",
                      "Beamwidth_alongship",
                      "OffsetAngle_athwartship",
                      "OffsetAngle_alongship",
                      "Sa_correction",
                      "Gain",
                      "TwoWayBeamAngle")

  # Return the cal.res variable
  return(cal.res)
}
