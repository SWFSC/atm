#' Create basemap
#'
#' @param nav Data frame containing navigation data, used to set map boundaries.
#' @param states Simple feature with state boundaries.
#' @param countries Simple feature with country boundaries.
#' @param labels A data frame with names and locations of landmark labels.
#' @param bathy Simple feature with bathymetr contours.
#' @param crs Coordinate reference system (CRS) for map projection.
#' @return A ggplot2 map object.
#' @importFrom magrittr "%>%"
#' @examples
#' get_basemap(nav, states, countries, landmarks, bathy, crs = 3310)
#' @export
get_basemap <- function(nav, states, countries, labels, bathy, crs) {
  # Set padding around data
  map.bounds <- nav %>%
    sf::st_transform(crs = crs) %>%
    sf::st_bbox()

  # Create map object
  ggplot2::ggplot() +
    # Plot bathymetry contours
    ggplot2::geom_sf(data = bathy, colour = "gray90", alpha = 0.5) +
    # # Plot high-res land polygons
    ggplot2::geom_sf(data = countries, fill = "gray90", colour = "gray50") +
    ggplot2::geom_sf(data = states, fill = "gray90", colour = "gray50") +
    # Plot landmarks
    ggplot2::geom_point(data = labels, aes(X, Y), size = 2, colour = 'gray50') +
    shadowtext::geom_shadowtext(data  = labels, aes(X, Y, label = name),
                                colour = 'gray20', size = 2, fontface = 'bold',
                                hjust = 0, nudge_x = 0.2, nudge_y = 0.05, angle = 25,
                                bg.colour = "white") +
    # Format axes and titles
    xlab("Longitude") + ylab("Latitude") +
    ggplot2::coord_sf(crs = crs,
             xlim = c(map.bounds["xmin"], map.bounds["xmax"]),
             ylim = c(map.bounds["ymin"], map.bounds["ymax"])) +
    theme_bw() +
    theme(axis.text.y          = element_text(angle = 90, hjust = 0.5),
          legend.position      = c(0,0),
          legend.justification = c(0,0),
          legend.background    = element_blank(),
          legend.key           = element_blank(),
          plot.title           = element_text(hjust = 0.5),
          panel.grid.major     = element_line(color = "gray90"))
}
