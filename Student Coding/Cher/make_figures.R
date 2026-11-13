options(warn = -1)

required <- c("ggplot2", "dplyr", "readr", "tidyr", "scales")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install packages first: install.packages(c(",
       paste(sprintf('"%s"', missing), collapse = ", "), "))")
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(scales)
})

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
project_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]])))
} else getwd()

data_dir <- file.path(project_dir, "data")
fig_dir <- file.path(project_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

input_file <- function(filename) {
  path <- file.path(data_dir, filename)
  if (!file.exists(path)) stop("Missing CSV: ", path)
  path
}

font_family <- "sans"
if (requireNamespace("systemfonts", quietly = TRUE)) {
  available <- systemfonts::system_fonts()$family
  if ("Raleway" %in% available) font_family <- "Raleway"
}

base_theme <- theme_minimal(base_size = 12, base_family = font_family) +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E4E8EB", linewidth = 0.35),
    axis.text = element_text(color = "#657581"),
    axis.title = element_text(color = "#344652"),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = margin(12, 14, 12, 10)
  )

save_figure <- function(p, name, width = 7.2, height = 4.2) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p,
         width = width, height = height, units = "in", dpi = 300, bg = "white")
}

read_noaa <- function(filename, precipitation = FALSE) {
  df <- read_csv(input_file(filename), comment = "#", show_col_types = FALSE) |>
    transmute(year = as.integer(Date %/% 100), value = as.numeric(Value)) |>
    filter(!is.na(year), !is.na(value)) |>
    arrange(year)
  if (precipitation) df <- mutate(df, value = value / 5)
  df
}

precip <- read_noaa("NOAA Precipitation September 60 month 1900-2026.csv", TRUE)
p_precip <- ggplot(precip, aes(year, value)) +
  geom_line(color = "#52718B", linewidth = 0.85) +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(x = "Year", y = "Average annual precipitation (inches)") +
  base_theme
save_figure(p_precip, "precipitation")

temp <- read_noaa("NOAA Max Temp September 60 month 1900-2026.csv")
p_temp <- ggplot(temp, aes(year, value)) +
  geom_line(color = "#70452F", linewidth = 0.85) +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(x = "Year", y = "Average daily high (°F)") +
  base_theme
save_figure(p_temp, "temperature")

co2 <- read_csv(input_file("Norfolk_Vulcan_2010-2022.csv"), show_col_types = FALSE) |>
  transmute(year = as.integer(Year), value = as.numeric(Total_FFCO2_tC)) |>
  filter(!is.na(year), !is.na(value)) |>
  arrange(year)
baseline <- co2$value[co2$year == 2010]
if (length(baseline) != 1L || baseline <= 0) stop("Expected one valid 2010 CO2 baseline")
co2 <- mutate(co2, pct_change = (value / baseline - 1) * 100)

yale_fields <- c(
  "happening", "human", "affectweather", "attrsealevels", "attrflood",
  "exp", "transitioneconomy", "regulate"
)

yale_labels <- c(
  "Global warming is happening",
  "Global warming caused mostly by human activities",
  "Global warming affecting the weather",
  "Global warming affecting rising sea levels",
  "Global warming affecting flooding",
  "Personally affected",
  "Transition to 100% clean energy by 2050",
  "Regulate CO2 as a pollutant"
)

yale <- read_csv(
  input_file("Yale_2025_Norfolk_MA_US.csv"),
  show_col_types = FALSE
) |>
  mutate(
    geography = case_when(
      GeoType == "county" ~ "Norfolk County",
      GeoType == "state" ~ "Massachusetts",
      GeoType == "national" ~ "United States",
      TRUE ~ NA_character_
    ),
    federal = (president + congress) / 2
  ) |>
  select(geography, all_of(yale_fields)) |>
  pivot_longer(
    cols = all_of(yale_fields),
    names_to = "metric",
    values_to = "percent"
  ) |>
  mutate(
    metric = factor(
      metric,
      levels = rev(yale_fields),
      labels = rev(yale_labels)
    ),
    geography = factor(
      geography,
      levels = c(
        "Norfolk County",
        "Massachusetts",
        "United States"
      )
    )
  )

regional_colors <- c(
  "Norfolk County" = "#714B39",
  "Massachusetts" = "#426783",
  "United States" = "#A8C7E8"
)

p_yale <- ggplot(
  yale,
  aes(x = metric, y = percent, fill = geography)
) +
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.65
  ) +
  coord_flip() +
  scale_fill_manual(values = regional_colors) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = label_number(suffix = "%", accuracy = 1)
  ) +
  labs(
    x = NULL,
    y = "Respondents (%)",
    fill = NULL
  ) +
  base_theme +
  theme(panel.grid.major.y = element_blank())

save_figure(
  p_yale,
  "yale_regional",
  width = 10.5,
  height = 7.8
)


methane_filename <- function(year) sprintf(
  "Express_Extension_Gridded_GHGI_Methane_v2_%d.nc", year
)
methane_years <- 2012:2020
methane_dirs <- c(file.path(data_dir, "methane"), data_dir)
methane_paths <- vapply(methane_years, function(year) {
  candidates <- file.path(methane_dirs, methane_filename(year))
  available <- candidates[file.exists(candidates)]
  if (length(available)) available[[1L]] else NA_character_
}, character(1))

if (all(is.na(methane_paths))) {
} else if (anyNA(methane_paths)) {
  missing_years <- methane_years[is.na(methane_paths)]
  stop("Methane input incomplete. Missing years: ", paste(missing_years, collapse = ", "),
       ". Download every Express Extension annual file for 2012-2020.")
} else {
  methane_packages <- c("ncdf4", "sf", "tigris")
  missing_methane_packages <- methane_packages[
    !vapply(methane_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_methane_packages)) {
    stop("Install methane mapping dependencies: install.packages(c(",
         paste(sprintf('"%s"', missing_methane_packages), collapse = ", "), "))")
  }

  county_cache <- file.path(data_dir, "norfolk_county_census_2024.gpkg")
  if (file.exists(county_cache)) {
    norfolk <- sf::st_read(county_cache, quiet = TRUE)
  } else {
    ma_counties <- tigris::counties(state = "MA", year = 2024, cb = FALSE,
                                   class = "sf")
    norfolk <- ma_counties[ma_counties$GEOID == "25021", ]
    if (nrow(norfolk) != 1L) stop("Cannot identify Norfolk County (GEOID 25021).")
    sf::st_write(norfolk, county_cache, quiet = TRUE)
  }
  norfolk <- sf::st_transform(sf::st_make_valid(norfolk), 4326)
  county_bbox <- sf::st_bbox(norfolk)

  locate_dim <- function(nc, candidates) {
    nms <- names(nc$dim)
    found <- which(tolower(nms) %in% candidates)
    if (length(found) != 1L) {
      stop("Expected one ", paste(candidates, collapse = "/"),
           " dimension. Available: ", paste(nms, collapse = ", "))
    }
    nms[found]
  }

  read_window <- function(nc, variable, lon_dim, lat_dim, lon_idx, lat_idx) {
    var <- nc$var[[variable]]
    dims <- vapply(var$dim, function(d) d$name, character(1))
    lon_axis <- match(lon_dim, dims)
    lat_axis <- match(lat_dim, dims)
    if (anyNA(c(lon_axis, lat_axis))) stop("Missing lon/lat axis in ", variable)
    starts <- rep(1L, length(dims))
    counts <- vapply(var$dim, function(d) as.integer(d$len), integer(1))
    starts[lon_axis] <- min(lon_idx)
    counts[lon_axis] <- length(lon_idx)
    starts[lat_axis] <- min(lat_idx)
    counts[lat_axis] <- length(lat_idx)

    other_axes <- setdiff(seq_along(dims), c(lon_axis, lat_axis))
    if (length(other_axes)) counts[other_axes] <- 1L
    vals <- ncdf4::ncvar_get(nc, variable, start = starts, count = counts,
                             collapse_degen = FALSE)
    oriented <- aperm(vals, c(lon_axis, lat_axis, other_axes))
    matrix(as.numeric(oriented), nrow = length(lon_idx), ncol = length(lat_idx))
  }

  make_overlap_weights <- function(lon, lat, lon_spacing, lat_spacing, county) {
    indices <- expand.grid(lon_i = seq_along(lon), lat_i = seq_along(lat))
    polygons <- lapply(seq_len(nrow(indices)), function(i) {
      x <- lon[indices$lon_i[i]]
      y <- lat[indices$lat_i[i]]
      half_x <- lon_spacing / 2
      half_y <- lat_spacing / 2
      sf::st_polygon(list(matrix(c(
        x - half_x, y - half_y,
        x + half_x, y - half_y,
        x + half_x, y + half_y,
        x - half_x, y + half_y,
        x - half_x, y - half_y
      ), byrow = TRUE, ncol = 2)))
    })
    cells <- sf::st_sf(
      cell_id = seq_len(nrow(indices)),
      geometry = sf::st_sfc(polygons, crs = 4326)
    )
    cells <- sf::st_transform(cells, 5070)
    full_area <- as.numeric(sf::st_area(cells))
    county_area <- sf::st_sf(
      geometry = sf::st_union(sf::st_geometry(
        sf::st_transform(sf::st_make_valid(county), 5070)
      ))
    )
    clipped <- suppressWarnings(sf::st_intersection(cells, county_area))
    fractions <- numeric(nrow(cells))
    if (nrow(clipped)) {
      parts <- tapply(as.numeric(sf::st_area(clipped)), clipped$cell_id, sum)
      ids <- as.integer(names(parts))
      fractions[ids] <- pmin(1, as.numeric(parts) / full_area[ids])
    }

    matrix(fractions, nrow = length(lon), ncol = length(lat))
  }

  read_county_methane <- function(filepath, year, county) {
    nc <- ncdf4::nc_open(filepath)
    on.exit(ncdf4::nc_close(nc), add = TRUE)

    lon_dim <- locate_dim(nc, c("lon", "longitude"))
    lat_dim <- locate_dim(nc, c("lat", "latitude"))
    all_lon_raw <- nc$dim[[lon_dim]]$vals
    all_lat <- nc$dim[[lat_dim]]$vals
    all_lon <- ifelse(all_lon_raw > 180, all_lon_raw - 360, all_lon_raw)
    dx <- median(diff(sort(unique(all_lon_raw))))
    dy <- median(diff(sort(unique(all_lat))))
    if (!is.finite(dx) || !is.finite(dy) || dx <= 0 || dy <= 0) {
      stop("Invalid grid spacing in ", basename(filepath))
    }

    lon_idx <- which(all_lon + dx/2 >= county_bbox[["xmin"]] &
                     all_lon - dx/2 <= county_bbox[["xmax"]])
    lat_idx <- which(all_lat + dy/2 >= county_bbox[["ymin"]] &
                     all_lat - dy/2 <= county_bbox[["ymax"]])
    if (!length(lon_idx) || !length(lat_idx)) stop("No Norfolk cells in ", filepath)
    lon_idx <- seq(min(lon_idx), max(lon_idx))
    lat_idx <- seq(min(lat_idx), max(lat_idx))

    all_vars <- names(nc$var)
    area_name <- all_vars[tolower(all_vars) == "grid_cell_area"]
    flux_vars <- all_vars[grepl("^emi", all_vars, ignore.case = TRUE)]
    if (length(area_name) != 1L || length(flux_vars) < 20L) {
      stop("Unexpected EPA NetCDF variable names in ", basename(filepath),
           ". Found: ", paste(all_vars, collapse = ", "))
    }
    area <- read_window(nc, area_name, lon_dim, lat_dim, lon_idx, lat_idx)
    flux_total <- matrix(0, nrow = length(lon_idx), ncol = length(lat_idx))
    for (source in flux_vars) {
      values <- read_window(nc, source, lon_dim, lat_dim, lon_idx, lat_idx)
      flux_total <- flux_total + replace(values, is.na(values), 0)
    }
    weights <- make_overlap_weights(all_lon[lon_idx], all_lat[lat_idx],
                                    dx, dy, county)
    if (!any(weights > 0)) stop("No grid cells overlap Norfolk County")

    days <- as.integer(as.Date(sprintf("%d-01-01", year + 1L)) -
                         as.Date(sprintf("%d-01-01", year)))
    seconds <- days * 86400
    avogadro <- 6.02214076e23
    total_tonnes <- sum(flux_total * area * weights, na.rm = TRUE) *
      seconds * 16.04 / (avogadro * 1e6)
    if (!is.finite(total_tonnes) || total_tonnes <= 0) {
      stop("Invalid methane total for ", year, ": ", total_tonnes)
    }
    data.frame(year = year, methane_metric_tonnes = total_tonnes)
  }

  methane <- dplyr::bind_rows(lapply(seq_along(methane_years), function(i) {
    read_county_methane(methane_paths[[i]], methane_years[[i]], norfolk)
  }))
  baseline_methane <- methane$methane_metric_tonnes[methane$year == 2012]
  methane <- methane |>
    mutate(pct_change = 100 * (methane_metric_tonnes / baseline_methane - 1))
  readr::write_csv(methane, file.path(data_dir, "Norfolk_Methane_2012-2020_derived.csv"))
}


methane_cache <- file.path(data_dir, "Norfolk_Methane_2012-2020_derived.csv")
if (!exists("methane") && file.exists(methane_cache)) {
  methane <- read_csv(methane_cache, show_col_types = FALSE)
}

if (exists("methane")) {
  co2_baseline <- co2$value[co2$year == 2010]
  methane_baseline <- methane$methane_metric_tonnes[methane$year == 2012]

  if (length(co2_baseline) != 1L || !is.finite(co2_baseline) || co2_baseline <= 0) {
    stop("Expected one valid 2010 CO2 baseline")
  }
  if (length(methane_baseline) != 1L || !is.finite(methane_baseline) || methane_baseline <= 0) {
    stop("Expected one valid 2012 methane baseline")
  }

  emissions <- bind_rows(
    co2 |>
      transmute(
        year,
        gas = "Carbon dioxide",
        pct_change = 100 * (value / co2_baseline - 1)
      ),
    methane |>
      transmute(
        year = as.integer(year),
        gas = "Methane",
        pct_change = 100 * (methane_metric_tonnes / methane_baseline - 1)
      )
  ) |>
    mutate(gas = factor(gas, levels = c("Carbon dioxide", "Methane"))) |>
    arrange(gas, year)

  p_emissions <- ggplot(emissions, aes(year, pct_change, color = gas)) +
    geom_hline(yintercept = 0, color = "#29343B", linewidth = 0.5) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.6) +
    scale_color_manual(values = c(
      "Carbon dioxide" = "#426783",
      "Methane" = "#714B39"
    )) +
    scale_x_continuous(breaks = c(2010, 2015, 2020), limits = c(2010, 2022)) +
    scale_y_continuous(
      labels = label_number(suffix = "%", accuracy = 1),
      breaks = breaks_pretty(n = 6)
    ) +
    labs(x = "Year", y = "Change from baseline (%)", color = NULL) +
    base_theme

  save_figure(p_emissions, "co2_methane_percent_change")
}

make_coastal_map <- function() {
  needed <- c("sf", "tigris", "httr2", "jsonlite")
  absent <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(absent)) stop("Install required packages: ", paste(absent, collapse = ", "))

  cache <- file.path(data_dir, "coastal")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  old_cache <- file.path(project_dir, "norfolk_climate_options", "coast_cache")
  suppressMessages(sf::sf_use_s2(FALSE))

  flood_2030_url <- paste0(
    "https://services1.arcgis.com/7iJyYTjCtKsZS1LR/ArcGIS/rest/services/",
    "MA_2030_1Perc_v11/FeatureServer/2"
  )
  flood_2050_url <- paste0(
    "https://services1.arcgis.com/7iJyYTjCtKsZS1LR/ArcGIS/rest/services/",
    "MA_2050_1Perc_v11/FeatureServer/1"
  )

  fetch_layer <- function(name, url, where, bbox, gpkg = NULL) {
    candidates <- c(file.path(cache, paste0(name, ".rds")),
                    file.path(old_cache, paste0(name, ".rds")))
    found <- candidates[file.exists(candidates)]
    if (length(found)) return(sf::st_transform(readRDS(found[[1]]), 26986))
    if (!is.null(gpkg) && file.exists(gpkg)) {
      return(sf::st_transform(sf::st_read(gpkg, quiet = TRUE), 26986))
    }

    pages <- list()
    offset <- 0L
    page_size <- 100L
    repeat {
      response <- httr2::request(paste0(url, "/query")) |>
        httr2::req_url_query(
          f = "geojson", where = where, outFields = "*",
          returnGeometry = "true", geometry = paste(bbox, collapse = ","),
          geometryType = "esriGeometryEnvelope", inSR = 4326, outSR = 4326,
          spatialRel = "esriSpatialRelIntersects",
          resultOffset = offset, resultRecordCount = page_size
        ) |>
        httr2::req_timeout(120) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_perform()
      body <- httr2::resp_body_string(response)
      metadata <- jsonlite::fromJSON(body, simplifyVector = FALSE)
      if (!is.null(metadata$error)) stop(name, ": ", metadata$error$message)
      count <- length(metadata$features)
      if (!count) break
      pages[[length(pages) + 1L]] <- sf::st_read(body, quiet = TRUE)
      if (count < page_size) break
      offset <- offset + count
      if (offset > 25000L) stop("Unexpected GIS response size: ", name)
    }
    if (!length(pages)) stop("No GIS features found for: ", name)
    out <- sf::st_transform(sf::st_make_valid(do.call(rbind, pages)), 26986)
    saveRDS(out, file.path(cache, paste0(name, ".rds")))
    out
  }

  north_box <- c(-71.09, 42.20, -70.85, 42.37)
  south_box <- c(-70.86, 42.16, -70.72, 42.29)
  whole_box <- c(-71.09, 42.16, -70.72, 42.37)
  view_box <- c(-71.09, 42.19, -70.77, 42.32)

  frame <- function(bbox) {
    sf::st_transform(sf::st_as_sfc(sf::st_bbox(
      c(xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4]),
      crs = 4326
    )), 26986)
  }
  clip <- function(x, area) {
    out <- suppressWarnings(sf::st_intersection(sf::st_make_valid(x), area))
    out[!sf::st_is_empty(out), , drop = FALSE]
  }

  mass_towns <- tigris::county_subdivisions(
    state = "MA", year = 2024, cb = TRUE, class = "sf"
  )
  mass_towns <- sf::st_transform(sf::st_make_valid(mass_towns), 26986)
  county_codes <- substr(as.character(mass_towns$GEOID), 1, 5)
  norfolk_land <- mass_towns[county_codes == "25021", , drop = FALSE]
  surrounding_land <- mass_towns[county_codes != "25021", , drop = FALSE]
  if (!nrow(norfolk_land)) stop("Norfolk County not found in Census subdivisions")
  towns <- norfolk_land
  towns$municipality <- sub(
    "\\s+(?:town|city)(?:\\s+city)?$", "", as.character(towns$NAME),
    ignore.case = TRUE, perl = TRUE
  )
  towns <- towns[tolower(towns$municipality) %in% c(
    "quincy", "braintree", "weymouth", "milton", "cohasset"
  ), , drop = FALSE]
  if (!nrow(towns)) stop("No coastal municipalities matched Census names")

  flood2030 <- fetch_layer(
    "coastal_flood_2030_1pct", flood_2030_url, "1=1", whole_box,
    gpkg = file.path(cache, "mcfrm_1pct_2030_norfolk.gpkg")
  )
  flood2050 <- fetch_layer(
    "coastal_flood_2050_1pct", flood_2050_url, "1=1", whole_box,
    gpkg = file.path(cache, "mcfrm_1pct_2050_norfolk.gpkg")
  )

  bounds <- frame(view_box)
  bb <- sf::st_bbox(bounds)
  plot_bb <- c(
    xmin = bb[["xmin"]] + (88 / 1186) * (bb[["xmax"]] - bb[["xmin"]]),
    ymin = bb[["ymin"]] + (90 / 662) * (bb[["ymax"]] - bb[["ymin"]]),
    xmax = bb[["xmax"]] - (40 / 1186) * (bb[["xmax"]] - bb[["xmin"]]),
    ymax = bb[["ymax"]] - (58 / 662) * (bb[["ymax"]] - bb[["ymin"]])
  )
  background_land <- clip(surrounding_land, bounds)
  highlighted_land <- clip(norfolk_land, bounds)
  town_panel <- clip(towns, bounds)
  if (!nrow(town_panel)) stop("No Norfolk coastal municipalities in map extent")

  context_hatching <- NULL
  if (nrow(background_land)) {
    rise <- as.numeric(bb[["ymax"]] - bb[["ymin"]])
    intercepts <- seq(as.numeric(bb[["xmin"]]) - rise,
                      as.numeric(bb[["xmax"]]) + rise, by = 1150)
    diagonals <- sf::st_sfc(lapply(intercepts, function(x0) {
      sf::st_linestring(rbind(
        c(x0, as.numeric(bb[["ymin"]])),
        c(x0 + rise, as.numeric(bb[["ymax"]]))
      ))
    }), crs = 26986)
    context_hatching <- tryCatch({
      x <- suppressWarnings(sf::st_intersection(
        sf::st_sf(geometry = diagonals),
        sf::st_union(sf::st_geometry(background_land))
      ))
      x <- x[!sf::st_is_empty(x), , drop = FALSE]
      if (nrow(x)) x else NULL
    }, error = function(e) NULL)
  }

  north_towns <- clip(
    town_panel[tolower(town_panel$municipality) %in%
                 c("quincy", "braintree", "weymouth", "milton"), , drop = FALSE],
    frame(north_box)
  )
  south_towns <- clip(
    town_panel[tolower(town_panel$municipality) == "cohasset", , drop = FALSE],
    frame(south_box)
  )
  if (!nrow(north_towns) || !nrow(south_towns)) {
    stop("Missing Quincy or Cohasset municipal geometry")
  }

  north_coast <- sf::st_union(sf::st_geometry(north_towns))
  south_coast <- sf::st_union(sf::st_geometry(south_towns))
  
  county_edge <- sf::st_boundary(
    sf::st_union(sf::st_geometry(highlighted_land))
  )

  coastal_land <- sf::st_union(sf::st_geometry(town_panel))
  f30 <- clip(flood2030, sf::st_intersection(bounds, coastal_land))
  f50 <- clip(flood2050, sf::st_intersection(bounds, coastal_land))
  if (!nrow(f30) || !nrow(f50)) stop("Missing modeled coastal flood coverage")
  g30 <- sf::st_make_valid(sf::st_union(sf::st_geometry(f30)))
  g50 <- sf::st_make_valid(sf::st_union(sf::st_geometry(f50)))
  extra2050 <- sf::st_make_valid(sf::st_difference(g50, g30))
  extra2050 <- sf::st_collection_extract(extra2050, "POLYGON", warn = FALSE)
  extra2050 <- sf::st_sf(geometry = extra2050)
  zone2030 <- sf::st_sf(geometry = g30)

  municipal_dividers <- list()
  if (nrow(town_panel) > 1L) {
    for (i in seq_len(nrow(town_panel) - 1L)) {
      for (j in seq.int(i + 1L, nrow(town_panel))) {
        name_i <- tolower(town_panel$municipality[i])
        name_j <- tolower(town_panel$municipality[j])
        edge_i <- sf::st_boundary(sf::st_geometry(town_panel[i, , drop = FALSE]))
        edge_j <- sf::st_boundary(sf::st_geometry(town_panel[j, , drop = FALSE]))
        if (setequal(c(name_i, name_j), c("quincy", "braintree"))) {
          braintree_edge <- if (name_i == "braintree") edge_i else edge_j
          quincy_polygon <- if (name_i == "quincy") {
            sf::st_geometry(town_panel[i, , drop = FALSE])
          } else {
            sf::st_geometry(town_panel[j, , drop = FALSE])
          }
          shared <- suppressWarnings(sf::st_intersection(
            braintree_edge, sf::st_buffer(quincy_polygon, 150)
          ))
        } else {
          shared <- suppressWarnings(sf::st_intersection(edge_i, edge_j))
        }
        shared <- suppressWarnings(sf::st_collection_extract(
          shared, "LINESTRING", warn = FALSE
        ))
        if (length(shared) && any(!sf::st_is_empty(shared))) {
          municipal_dividers[[length(municipal_dividers) + 1L]] <-
            shared[!sf::st_is_empty(shared)]
        }
      }
    }
  }
  municipal_dividers <- if (length(municipal_dividers)) {
    sf::st_sf(geometry = do.call(c, municipal_dividers))
  } else {
    NULL
  }

  town_labels <- sf::st_point_on_surface(town_panel)
  label_xy <- sf::st_coordinates(town_labels)
  town_label_data <- data.frame(
    municipality = town_labels$municipality,
    label_x = label_xy[, "X"],
    label_y = label_xy[, "Y"]
  )
  town_label_data$label_y[
    tolower(town_label_data$municipality) == "quincy"
  ] <- town_label_data$label_y[
    tolower(town_label_data$municipality) == "quincy"
  ] - 950
  town_label_data$label_y[
    tolower(town_label_data$municipality) == "weymouth"
  ] <- town_label_data$label_y[
    tolower(town_label_data$municipality) == "weymouth"
  ] - 750

  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = background_land, fill = "#F1F2F2", color = NA)
  if (!is.null(context_hatching)) {
    map <- map + ggplot2::geom_sf(
      data = context_hatching, color = "#E4E7E8",
      linewidth = 0.055, show.legend = FALSE
    )
  }
  map <- map +
    ggplot2::geom_sf(data = highlighted_land, fill = "#F4EEDF", color = NA) +
    ggplot2::geom_sf(
      data = zone2030, ggplot2::aes(fill = "2030"),
      color = "#A9C4D4", linewidth = 0.12, linetype = "22"
    )
  if (nrow(extra2050) && !all(sf::st_is_empty(extra2050))) {
    map <- map + ggplot2::geom_sf(
      data = extra2050, ggplot2::aes(fill = "2050"),
      color = "#7FA5BD", linewidth = 0.12, linetype = "22"
    )
  }
  if (!is.null(municipal_dividers)) {
    map <- map + ggplot2::geom_sf(
      data = municipal_dividers,
      color = "#D6D1C7", linewidth = 0.14,
      show.legend = FALSE
    )
  }
  map <- map +
    ggplot2::geom_text(data = town_label_data,
                       ggplot2::aes(x = label_x, y = label_y,
                                    label = municipality),
                       size = 2.8, color = "black", check_overlap = TRUE,
                       family = font_family) +
    ggplot2::scale_fill_manual(
      values = c("2030" = "#DCEBF3", "2050" = "#AFC9D9"),
      breaks = c("2030", "2050"), drop = FALSE
    ) +
    ggplot2::coord_sf(
      crs = sf::st_crs(26986),
      xlim = c(plot_bb[["xmin"]], plot_bb[["xmax"]]),
      ylim = c(plot_bb[["ymin"]], plot_bb[["ymax"]]),
      expand = FALSE, datum = NA
    ) +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL, color = NULL) +
    base_theme +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#EAF3F8", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key = ggplot2::element_rect(fill = "white", color = NA),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 9, color = "black"),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )

  save_figure(map, "coastal_change", width = 8.5, height = 4.25)
}

make_coastal_map()
