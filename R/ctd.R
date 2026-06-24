#' Extract header information from CTD and UCTD casts.
#'
#' @param header.filename Name of header file.
#' @param type Cast type (CTD or UCTD).
#' @return A data frame containing header information.
#' @export
extract_ctd_header <- function(header.filename, type) {
  if (type == "UCTD") {
    # Process UCTD header file -----------------------------------------------------
    # Read header text
    header.txt <- readLines(header.filename)

    # If Oceansciences UCTD
    if(stringr::str_detect(header.filename, ".asc")) {
      # Extract cast date as dttm
      cast.date  <- lubridate::dmy_hms(
        stringr::str_extract(
          unlist(
            stringr::str_extract_all(header.txt,
                                     pattern = '\\*Cast[\\s\\S]*\\d{2}\\s\\w{3}\\s\\d{4}\\s\\d{2}:\\d{2}:\\d{2}')),
          '\\d{2}\\s\\w{3}\\s\\d{4}\\s\\d{2}:\\d{2}:\\d{2}'))

      # Extract probe serial number
      sn <- as.numeric(stringr::str_extract(
        unlist(stringr::str_extract_all(header.txt,
                                        pattern = '\\*SerialNumber=\\d{8}'))[1],"\\d{8}"))

      # Extract cast name from file name
      cast <- tail(stringr::str_split(header.filename, "/")[[1]], n = 1) %>%
        stringr::str_replace(".asc", "")

      # If Valeport UCTD
    } else if (stringr::str_detect(header.filename, ".vp2")) {
      # Extract cast date as dttm
      cast.date  <- lubridate::ymd_hms(
        stringr::str_extract(
          unlist(
            stringr::str_extract_all(header.txt,
                                     pattern = 'DataStartTime=\\d{4}/\\d{2}/\\d{2} \\d{2}:\\d{2}:\\d{2}')),
          '\\d{4}/\\d{2}/\\d{2} \\d{2}:\\d{2}:\\d{2}'))

      # Extract probe serial number
      sn <- as.numeric(stringr::str_extract(
        unlist(
          stringr::str_extract_all(header.txt,
                                   pattern = 'SerialNumber=\\d{5}'))[1],
        '\\d{5}'))

      # Extract cast name from file name
      cast <- tail(stringr::str_split(header.filename, "/")[[1]], n = 1) %>%
        stringr::str_replace(".vp2", "")

      # If MVP200 UCTD
    } else if (stringr::str_detect(header.filename, ".m1")) {
      # Extract cast date as dttm
      cast.date  <- lubridate::dmy_hms(
        paste(
          # Extract the date
          stringr::str_extract(
            unlist(
              stringr::str_extract_all(header.txt,
                                       pattern = 'Date \\(dd/mm/yyyy\\): \\d{2}/\\d{2}/\\d{4}')),
            '\\d{2}/\\d{2}/\\d{4}'),

          # Extract the time
          stringr::str_extract(
            unlist(
              stringr::str_extract_all(header.txt,
                                       pattern = 'Time \\(hh\\|mm\\|ss.s\\): \\d{2}:\\d{2}:\\d{2}')),
            '\\d{2}:\\d{2}:\\d{2}')))

      # Extract probe serial number
      sn <- as.numeric(stringr::str_extract(
        unlist(
          stringr::str_extract_all(header.txt,
                                   pattern = 'SER1 Serial Number: \\d{1,5}'))[1],
        ' \\d{1,5}'))

      # Extract cast name from file name
      cast <- tail(stringr::str_split(header.filename, "/")[[1]], n = 1) %>%
        stringr::str_replace(".m1", "")
    }
  } else if (type == "CTD") {
    # Process CTD header file -----------------------------------------------------
    # Read header text
    header.txt <- readLines(header.filename)

    # Extract cast date as dttm
    cast.date  <- lubridate::mdy_hms(
      stringr::str_extract(
        unlist(stringr::str_extract_all(header.txt, pattern = '\\* NMEA UTC \\(Time\\).*')),
        "\\w{3}\\s\\d{2}\\s\\d{4}\\s*\\d{2}:\\d{2}:\\d{2}"))

    if (length(cast.date) == 0) {
      cast.date  <- lubridate::mdy_hms(
        stringr::str_extract(
          unlist(stringr::str_extract_all(
            header.txt, pattern = '\\*Cast[\\s\\S]*\\d{2}\\s\\w{3}\\s\\d{4}\\s\\d{2}:\\d{2}:\\d{2}')),
          "\\d{2}\\s\\w{3}\\s\\d{4}\\s\\d{2}:\\d{2}:\\d{2}"))
    }
    # Set serial number to NA
    sn <- NA

    # Extract cast name from file name
    cast <- tail(stringr::str_split(header.filename, "/")[[1]], n = 1) %>%
      stringr::str_replace(".hdr", "")
  }

  # If all date parsing fails, return NA
  if (length(cast.date) == 0) cast.date <- NA

  # Combine data frames for export
  data.frame(cast = cast, path = header.filename, cast.date = cast.date, sn = sn)
}

#' Extract cast data from CTD and UCTD casts.
#'
#' @param cast.filename Name of cast file.
#' @param type Cast type (CTD or UCTD).
#' @param skip Number of lines to skip (default = 79 for vp2 files and 62 for m1 files). Else, may be defined in the survey-specific settings file.
#' @param min.Z Minimum depth (Z, m) for identifying downcast data (default = 2)
#' @param col.names Vector containing column names. May be defined in the survey-specific settings file.
#' @return A data frame containing cast data.
#' @export
extract_ctd_cast <- function(cast.filename, type, skip = NULL, min.Z = 2, col.names = NULL) {

  # Extract cast name from file path
  if(stringr::str_detect(cast.filename, ".asc")) {
    # If Oceansciences UCTD
    cast <- tail(stringr::str_split(cast.filename, "/")[[1]], n = 1) %>%
      stringr::str_replace("_processed.asc", "")

  } else if (stringr::str_detect(cast.filename, ".vp2")) {
    # If Valeport UCTD
    cast <- tail(stringr::str_split(cast.filename, "/")[[1]], n = 1) %>%
      stringr::str_replace(".vp2", "")
  } else if (stringr::str_detect(cast.filename, ".m1")) {
    # If MVP200 UCTD
    cast <- tail(stringr::str_split(cast.filename, "/")[[1]], n = 1) %>%
      stringr::str_replace(".m1", "")
  }

  if (type == "UCTD") {
    # Process UCTD cast -------------------------------------------------------
    # If Oceansciences UCTD
    if(stringr::str_detect(cast.filename, ".asc")) {
      # Read cast data and rename columns
      read.table(cast.filename, header = TRUE) %>%
        dplyr::rename(scan = Scan, C = C0S.m, 'T' = Tnc90C, P = PrM,
                      Z = DepSM, S = Sal00, Sv = SvCM, avgsVCM = AvgsvCM,
                      Dens = Density00, Flag = Flag) %>%
        dplyr::mutate(
          scan = scan - min(scan),
          s    = scan/16 - min(scan/16), # Calculate time (s) from scan (scan rate is 16 Hz)
          dt   = c(0, diff(s)), # Calculate time interval
          dZ   = c(1, diff(Z)), # Calculate change in depth (dZ, m)
          dZt  = as.numeric(forecast::ma(dZ/dt, order = 5)),
          dZt  = na_if(dZt, Inf),
          Z    = -Z,
          cast = cast,
          path = cast.filename)

      # If Valeport UCTD
    } else if (stringr::str_detect(cast.filename, ".vp2")) {
      # Define skip if not included in function call
      if (is.null(skip)) uctd.skip <- c("vp2" = 79)

      # Read cast data and rename columns
      read.table(cast.filename, skip = uctd.skip["vp2"],
                 col.names = unlist(uctd.col.names["vp2"])) %>%
        # Include only downcast data
        dplyr::slice(which(Z > min.Z)[1]:which.max(Z)) %>%
        dplyr::mutate(
          scan = seq_along(date),
          t    = lubridate::ymd_hms(paste(date, time)),
          # Calculate time interval
          dt   = as.numeric(difftime(t, dplyr::lag(t, 1, default = t[1]), units = "secs")),
          s    = cumsum(dt),
          # Calculate change in depth (dZ, m)
          dZ   = c(1, diff(Z)),
          dZt  = as.numeric(forecast::ma(dZ/dt, order = 5)),
          dZt  = dplyr::na_if(dZt, Inf),
          Z    = -Z,
          cast = cast,
          path = cast.filename)

      # If MVP200 UCTD
    } else if (stringr::str_detect(cast.filename, ".m1")) {
      # Define skip if not included in function call
      if (is.null(skip)) uctd.skip <- c("m1" = 62)

      # Read header text
      header.txt <- readLines(cast.filename)

      # Extract cast date as dttm
      cast.date  <- lubridate::dmy_hms(
        paste(
          # Extract the date
          stringr::str_extract(
            unlist(
              stringr::str_extract_all(header.txt,
                                       pattern = 'Date \\(dd/mm/yyyy\\): \\d{2}/\\d{2}/\\d{4}')),
            '\\d{2}/\\d{2}/\\d{4}'),

          # Extract the time
          stringr::str_extract(
            unlist(
              stringr::str_extract_all(header.txt,
                                       pattern = 'Time \\(hh\\|mm\\|ss.s\\): \\d{2}:\\d{2}:\\d{2}')),
            '\\d{2}:\\d{2}:\\d{2}')))

      # Read cast data and rename columns
      read.table(cast.filename, skip = uctd.skip["m1"], sep = ",",
                 col.names = unlist(uctd.col.names["m1"])) %>%
        # Include only downcast data
        dplyr::slice(which(Z > min.Z)[1]:which.max(Z)) %>%
        dplyr::mutate(
          scan = seq_along(P),
          # t    = lubridate::ymd_hms(paste(date, time)),
          # Calculate time interval (assuming 6 Hz)
          dt     = 1/6,
          # dt   = as.numeric(difftime(t, dplyr::lag(t, 1, default = t[1]), units = "secs")),
          s    = cumsum(dt),
          # Compute date from cast start date and cumulative time (s)
          t    = cast.date + seconds(s),
          # Calculate change in depth (dZ, m)
          dZ   = c(1, diff(Z)),
          dZt  = as.numeric(forecast::ma(dZ/dt, order = 5)),
          dZt  = dplyr::na_if(dZt, Inf),
          Z    = -Z,
          cast = cast,
          path = cast.filename)

    }

  } else if (type == "CTD") {
    # Process CTD cast --------------------------------------------------------
    # Read cast data and rename columns
    read.table(cast.filename, header = TRUE) %>%
      dplyr::rename(P = PrDM, 'T' = T090C, C = C0S.m, Z = DepSM, S = Sal00,
                    Sv = SvCM, avgsvCM = AvgsvCM, Dens = Density00, Flag = Flag) %>%
      dplyr::mutate(
        Z    = -Z, # Make depth negative
        cast = cast,
        path = cast.filename)
  }
}

#' Calculate FG absorption
#'
#' @param T Temperature value(s).
#' @param S Salinity value(s).
#' @param D Density value(s).
#' @param f Some variable I don't know what it is.
#' @return A value for absorption (alpha).
#' @export
calculate_fg_absorption <- function(T, S, D, f) {
  T_K <- T + 273.15
  c <- 1412.0 + 3.21 * T + 1.19 * S + 0.0167 * D

  f1 <- 2.8 * sqrt (S/ 35.0) * 10^(4.0-1245.0 / T_K)
  A1 <- (8.86 / c) * 10^(0.78* 8.0 - 5.0)

  f2 <- (21.9 * 10^(6.0 - 1960.0 / T_K)) / (1.0 + 10^(3.0 - 0.03 * T))
  A2 <- 21.4 * (S / 35.0) * (1.0 + 0.025 * T)/c
  P2 <- 1.0 - 1.37e-4 * D + 6.2e-9 * D^2

  A3_low_temp <- 4.937e-4 - 2.59e-5 * T + 9.11e-7 * T^2 - 1.5e-8 * T^3
  A3_high_temp <- 1.809e-4 - 5.64e-6 * T + 6.88e-8 * T^2
  A3 <- ifelse(T <= 20, A3_low_temp, A3_high_temp)
  P3 <- 1.0 - 3.83e-5 * D + 4.9e-9 * D^2

  alpha <- ((A1 * f1 * f^2) / (f^2 + f1^2)) + ((A2 * P2 * f2 * f^2) / (f^2 + f2^2)) + (A3 * P3 * f^2)
  return(alpha)
}

#' Calculate depth-integrated average values
#'
#' @param df Data frame containing data.
#' @param value_col Input value column.
#' @param depth_col Depth column (default = "Depth").
#' @return Depth-integrated average value(s).
#' @export
calculate_depth_integrated_average <- function(df, value_col, depth_col = 'Depth') {
  # Trapezoidal Integration Engine
  df_clean <- df[!is.na(df[[value_col]]) & !is.na(df[[depth_col]]), ]
  df_sorted <- df_clean[order(df_clean[[depth_col]]), ]

  if (nrow(df_sorted) == 0) return(0.0)

  z <- df_sorted[[depth_col]]
  val <- df_sorted[[value_col]]

  dz <- diff(z)
  if (length(dz) == 0) return(val[1])

  mid_vals <- (val[-length(val)] + val[-1]) / 2.0
  total_integral <- sum(mid_vals * dz)
  total_depth_range <- z[length(z)] - z[1]

  if (total_depth_range > 0) {
    return(total_integral / total_depth_range)
  } else {
    return(val[1])
  }
}

#' Extract coordinates from MVP200 cast file
#'
#' @param coord_str String containing lat/long value(s).
#' @return Coordinate value(s) in decimal degrees.
#' @export
mvp_parse_coordinate <- function(coord_str) {
  # Decodes custom geographic tracking strings from MVP file headers.
  if (is.null(coord_str) || is.na(coord_str) || coord_str == "") return(NA)

  clean_str <- trimws(coord_str)
  match <- stringr::str_match(clean_str, "([0-9.]+),([NSEW])")
  if (is.na(match[1,1])) return(NA)

  val <- as.numeric(match[1, 2])
  direction <- match[1, 3]

  degrees <- floor(val / 100)
  minutes <- val - (degrees * 100)
  decimal_degrees <- degrees + (minutes / 60.0)

  if (direction %in% c('S', 'W')) decimal_degrees <- -decimal_degrees
  return(decimal_degrees)
}

#' Calculate depth-integrated average values
#'
#' @param file_path Path to MVP200 cast file.
#' @param sampling_rate_hz Sampling rate (Hz, default = 25.0).
#' @return A list containing the downcast values, binned values, and metadata.
#' @export
mvp_process_file <- function(file_path, sampling_rate_hz = 25.0) {
  # 1. Parse header and isolate data payload
  lines <- readLines(file_path, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[lines != ""]

  header_end_idx <- which(grepl("<END_OF_HEADER>", lines))
  metadata <- list()

  if (length(header_end_idx) > 0 && header_end_idx[1] > 1) {
    header_lines <- lines[1:(header_end_idx[1] - 1)]
    for (line in header_lines) {
      if (grepl(":", line)) {
        parts <- stringr::str_split_fixed(line, ":", 2)
        metadata[[trimws(parts[1])]] <- trimws(parts[2])
      }
    }

    data_lines <- lines[(header_end_idx[1] + 1):length(lines)]

    # Safely remove the units row by checking if the second line contains 'dbar' or 'm'
    if (length(data_lines) > 1 && grepl("dbar|m/s", data_lines[2])) {
      data_to_read <- c(data_lines[1], data_lines[3:length(data_lines)])
    } else {
      data_to_read <- data_lines
    }

    df <- read.csv(text = paste(data_to_read, collapse = "\n"), header = TRUE, check.names = FALSE, strip.white = TRUE)
    names(df) <- trimws(names(df))
  } else {
    cat("   -> Failed: Could not locate <END_OF_HEADER> tag.\n")
    return(NULL)
  }

  # Safety Check: Did the columns parse correctly?
  if (nrow(df) == 0 || !"Depth" %in% names(df)) {
    cat("   -> Failed: 'Depth' column missing or file is empty.\n")
    return(NULL)
  }

  # 2. Isolate free-fall descent over 0.5 m/s
  dt <- 1.0 / sampling_rate_hz
  max_depth_idx <- which.max(df$Depth)

  # EXPLICITLY CREATE descent_df
  descent_df <- df[1:max_depth_idx, ]

  if (nrow(descent_df) == 0) {
    cat("   -> Failed: Descent data array is empty.\n")
    return(NULL)
  }

  descent_df$Descent_Speed_m_s <- c(NA, diff(descent_df$Depth)) / dt
  if (nrow(descent_df) > 1) descent_df$Descent_Speed_m_s[1] <- descent_df$Descent_Speed_m_s[2]

  cleaned_df <- descent_df[!is.na(descent_df$Descent_Speed_m_s) & descent_df$Descent_Speed_m_s > 0.5, ]
  if (nrow(cleaned_df) == 0) {
    cat("   -> Failed: No data left after filtering for 0.5 m/s descent speed.\n")
    return(NULL)
  }

  # 3. Compute TEOS-10 properties and Multi-Frequency Arrays
  lat_raw <- metadata[["LAT ( ddmm.mmmmmmm,N)"]]
  lon_raw <- metadata[["LON (dddmm.mmmmmmm,E)"]]
  lat <- ifelse(is.na(mvp_parse_coordinate(lat_raw)), 32.7221, mvp_parse_coordinate(lat_raw))
  lon <- ifelse(is.na(mvp_parse_coordinate(lon_raw)), -117.4006, mvp_parse_coordinate(lon_raw))

  cleaned_df$Absolute_Salinity <- gsw::gsw_SA_from_SP(cleaned_df$Sal, cleaned_df$Press, lon, lat)
  cleaned_df$Conservative_Temperature <- gsw::gsw_CT_from_t(cleaned_df$Absolute_Salinity, cleaned_df$Temp, cleaned_df$Press)
  cleaned_df$Potential_Density_Anomaly <- gsw::gsw_sigma0(cleaned_df$Absolute_Salinity, cleaned_df$Conservative_Temperature)
  cleaned_df$Sound_Speed_m_s <- gsw::gsw_sound_speed(cleaned_df$Absolute_Salinity, cleaned_df$Conservative_Temperature, cleaned_df$Press)

  cleaned_df$Alpha_18kHz  <- calculate_fg_absorption(cleaned_df$Temp, cleaned_df$Sal, cleaned_df$Depth, 18.0)
  cleaned_df$Alpha_38kHz  <- calculate_fg_absorption(cleaned_df$Temp, cleaned_df$Sal, cleaned_df$Depth, 38.0)
  cleaned_df$Alpha_70kHz  <- calculate_fg_absorption(cleaned_df$Temp, cleaned_df$Sal, cleaned_df$Depth, 70.0)
  cleaned_df$Alpha_120kHz <- calculate_fg_absorption(cleaned_df$Temp, cleaned_df$Sal, cleaned_df$Depth, 120.0)
  cleaned_df$Alpha_200kHz <- calculate_fg_absorption(cleaned_df$Temp, cleaned_df$Sal, cleaned_df$Depth, 200.0)
  cleaned_df$Alpha_333kHz <- calculate_fg_absorption(cleaned_df$Temp, cleaned_df$Sal, cleaned_df$Depth, 333.0)

  # 4. Bin into uniform 1-meter layers
  min_b <- floor(min(cleaned_df$Depth, na.rm = TRUE))
  max_b <- ceiling(max(cleaned_df$Depth, na.rm = TRUE))
  bin_edges <- seq(min_b, max_b + 1.0, by = 1.0)
  bin_labels <- bin_edges[-length(bin_edges)] + 0.5

  cleaned_df$Depth_Bin <- cut(cleaned_df$Depth, breaks = bin_edges, labels = bin_labels, include.lowest = TRUE)

  binned <- cleaned_df %>%
    dplyr::group_by(Depth_Bin) %>%
    dplyr::summarise(
      Temp_Conservative_C = mean(Conservative_Temperature, na.rm = TRUE),
      Salinity_Absolute_g_kg = mean(Absolute_Salinity, na.rm = TRUE),
      Density_SigmaTheta_kg_m3 = mean(Potential_Density_Anomaly, na.rm = TRUE),
      Sound_Speed_m_s = mean(Sound_Speed_m_s, na.rm = TRUE),
      Absorption_18kHz_dB_km = mean(Alpha_18kHz, na.rm = TRUE),
      Absorption_38kHz_dB_km = mean(Alpha_38kHz, na.rm = TRUE),
      Absorption_70kHz_dB_km = mean(Alpha_70kHz, na.rm = TRUE),
      Absorption_120kHz_dB_km = mean(Alpha_120kHz, na.rm = TRUE),
      Absorption_200kHz_dB_km = mean(Alpha_200kHz, na.rm = TRUE),
      Absorption_333kHz_dB_km = mean(Alpha_333kHz, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    dplyr::filter(!is.na(Sound_Speed_m_s))

  binned$Depth_m <- as.numeric(as.character(binned$Depth_Bin))

  return(list(raw_descent = cleaned_df, binned = binned, meta = metadata))
}

#' Export template ECS file for MVP200 casts
#'
#' @param raw_descent_df Data frame containing raw descent data.
#' @param binned_df Data frame containing binned descent data.
#' @param cast_id Data frame containing binned descent data.
#' @param output_dir Data frame containing binned descent data.
#' @param template_path Path to ECS template file.
#' @param tx_depth Depth of probe during transits (meters, default = 2).
#' @return Exports a new ECS file in the specified directory.
#' @export
mvp_export_template_ecs <- function(raw_descent_df, binned_df, cast_id, output_dir,
                                    template_path, tx_depth = 2) {

  # 1. Calculate integrated volumetric averages for the file
  spec_temp <- calculate_depth_integrated_average(raw_descent_df, 'Conservative_Temperature')
  spec_sal  <- calculate_depth_integrated_average(raw_descent_df, 'Absolute_Salinity')
  spec_dens <- calculate_depth_integrated_average(raw_descent_df, 'Potential_Density_Anomaly')
  spec_sv   <- calculate_depth_integrated_average(raw_descent_df, 'Sound_Speed_m_s')
  spec_depth <- mean(raw_descent_df$Depth, na.rm = TRUE)

  spec_a18  <- calculate_depth_integrated_average(raw_descent_df, 'Alpha_18kHz')
  spec_a38  <- calculate_depth_integrated_average(raw_descent_df, 'Alpha_38kHz')
  spec_a70  <- calculate_depth_integrated_average(raw_descent_df, 'Alpha_70kHz')
  spec_a120 <- calculate_depth_integrated_average(raw_descent_df, 'Alpha_120kHz')
  spec_a200 <- calculate_depth_integrated_average(raw_descent_df, 'Alpha_200kHz')
  spec_a333 <- calculate_depth_integrated_average(raw_descent_df, 'Alpha_333kHz')

  # 2. Read Base Template
  ECS <- readr::read_file(template_path)

  # 3. Extract nominal parameters from template to serve as baseline
  c_0 <- as.numeric(stringr::str_match(ECS, "\\bSoundSpeed\\s*=\\s*([^\\s]+)")[,2])
  g_0 <- as.numeric(stringr::str_match_all(ECS, "\\bTransducerGain\\s*=\\s*(-?\\d*\\.?\\d+)")[[1]][,2])
  EBA_0 <- as.numeric(stringr::str_match_all(ECS, "\\bTwoWayBeamAngle\\s*=\\s*(-?\\d*\\.?\\d+)")[[1]][,2])
  BW_minor_0 <- as.numeric(stringr::str_match_all(ECS, "\\bMinorAxis3dbBeamAngle\\s*=\\s*(-?\\d*\\.?\\d+)")[[1]][,2])
  BW_major_0 <- as.numeric(stringr::str_match_all(ECS, "\\bMajorAxis3dbBeamAngle\\s*=\\s*(-?\\d*\\.?\\d+)")[[1]][,2])

  # 4. Find Transducer Sound Speed for Parameter Compensation
  idx <- which.min(abs(raw_descent_df$Depth - tx_depth))
  txdcr_c <- raw_descent_df$Sound_Speed_m_s[idx]
  if(is.na(c_0) || length(c_0) == 0) c_0 <- txdcr_c

  ECS_new <- ECS

  # 5. Compensate Gain and Beam Angles
  if (length(g_0) > 0) {
    for (j in 1:length(g_0)) {

      pattern <- paste("(?s)SourceCal T", j,
                       ".*?TransducerGain\\s*=\\s*(\\d*\\.*\\d*)", sep = '')

      # Gain Compensation
      pattern <- paste("(?s)SourceCal T", j, ".*?TransducerGain\\s*=\\s*(\\d*\\.*\\d*)", sep = '')
      temp <- regexec(pattern, ECS_new, perl = TRUE)
      if (temp[[1]][1] != -1) {
        ECS_new <- paste0(stringr::str_sub(ECS_new, 1, temp[[1]][2]-1),
                          sprintf('%#.4f', g_0[j] + 20*log10(c_0 / txdcr_c)),
                          stringr::str_sub(ECS_new, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
      }

      # EBA Compensation
      pattern <- paste0("(?s)SourceCal T", j, "*?TwoWayBeamAngle\\s*=\\s*(\\d*\\.*\\d*)")
      temp <- regexec(pattern, ECS_new, perl = TRUE)
      if (temp[[1]][1] != -1) {
        ECS_new <- paste0(stringr::str_sub(ECS_new, 1, temp[[1]][2]-1),
                          sprintf('%#.4f', EBA_0[j] + 20*log10(txdcr_c / c_0)),
                          stringr::str_sub(ECS_new, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
      }

      # Minor BW Compensation
      pattern <- paste0("(?s)SourceCal T", j, ".*?MinorAxis3dbBeamAngle\\s*=\\s*(\\d*\\.*\\d*)")
      temp <- regexec(pattern, ECS_new, perl = TRUE)
      if (temp[[1]][1] != -1) {
        ECS_new <- paste0(stringr::str_sub(ECS_new, 1, temp[[1]][2]-1),
                          sprintf('%#.4f', BW_minor_0[j] * (txdcr_c / c_0)),
                          stringr::str_sub(ECS_new, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
      }

      # Major BW Compensation
      pattern <- paste0("(?s)SourceCal T", j, ".*?MajorAxis3dbBeamAngle\\s*=\\s*(\\d*\\.*\\d*)")
      temp <- regexec(pattern, ECS_new, perl = TRUE)
      if (temp[[1]][1] != -1) {
        ECS_new <- paste0(stringr::str_sub(ECS_new, 1, temp[[1]][2]-1),
                          sprintf('%#.4f', BW_major_0[j] * (txdcr_c / c_0)),
                          stringr::str_sub(ECS_new, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
      }
    }
  }

  # 6. Inject FILESET profiles and averages
  ECS_new <- gsub('\\bCtdDepthProfile\\s*=\\s*[^#\n]*', paste('CtdDepthProfile = ', paste(sprintf("%.2f", binned_df$Depth_m), collapse = ';'), ' '), ECS_new)
  ECS_new <- gsub('\\bSoundSpeedProfile\\s*=\\s*[^#\n]*', paste('SoundSpeedProfile = ', paste(sprintf("%.2f", binned_df$Sound_Speed_m_s), collapse = ';'), ' '), ECS_new)

  ECS_new <- gsub('\\bTemperature\\s*=\\s*[^#\n]*', sprintf('Temperature = %.2f ', spec_temp), ECS_new)
  ECS_new <- gsub('\\bSalinity\\s*=\\s*[^#\n]*', sprintf('Salinity = %.2f ', spec_sal), ECS_new)
  ECS_new <- gsub('\\bAbsorptionDepth\\s*=\\s*[^#\n]*', sprintf('AbsorptionDepth = %.2f ', spec_depth), ECS_new)
  ECS_new <- gsub('\\bSoundSpeed\\s*=\\s*[^#\n]*', sprintf('SoundSpeed = %.2f ', spec_sv), ECS_new)

  # 7. Create Comment Header Block for Acoustic Variables
  summary_txt <- paste0(
    "# =====================================================================#\n",
    "# COMPENSATED WATER COLUMN VOLUMETRIC AVERAGES (TRAPEZOIDAL INTEGRATION):\n",
    sprintf("# Avg Temperature (CT):            %.4f C\n", spec_temp),
    sprintf("# Avg Salinity (SA):               %.4f g/kg\n", spec_sal),
    sprintf("# Avg Density Anomaly (sigma0):    %.4f kg/m3\n", spec_dens),
    sprintf("# Avg Sound Speed (TEOS10):        %.4f m/s\n", spec_sv),
    sprintf("# Avg Acoustic Absorption 18kHz:   %.4f dB/km\n", spec_a18),
    sprintf("# Avg Acoustic Absorption 38kHz:   %.4f dB/km\n", spec_a38),
    sprintf("# Avg Acoustic Absorption 70kHz:   %.4f dB/km\n", spec_a70),
    sprintf("# Avg Acoustic Absorption 120kHz:  %.4f dB/km\n", spec_a120),
    sprintf("# Avg Acoustic Absorption 200kHz:  %.4f dB/km\n", spec_a200),
    sprintf("# Avg Acoustic Absorption 333kHz:  %.4f dB/km\n", spec_a333),
    "# =====================================================================#\n\n",
    "#========================================================================================#\n",
    "#                                    FILESET SETTINGS                                    #\n"
  )

  # Inject the summary block directly above the FILESET SETTINGS banner
  target_anchor <- "#========================================================================================#\r?\n#                                    FILESET SETTINGS                                    #\r?\n"
  ECS_final <- sub(target_anchor, summary_txt, ECS_new)

  # 8. Write the final ECS file
  ecs_filename <- paste0(cast_id, ".ecs")
  readr::write_file(ECS_final, file.path(output_dir, ecs_filename))
  cat(sprintf("     -> Created Template .ecs: '%s'\n", ecs_filename))
}
