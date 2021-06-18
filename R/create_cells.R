#' @title create_cells
#'
#' @description Create a specific tower layer with attached directional cells
#'
#' @param area.sf sf dataframe, focus area with geometry column (tile polygons)
#' @param tower.dist numeric value, distance between towers (e.g., in meters, however in general dimensionless)
#' @param rotation.deg numeric value, layer rotation of directional cells, sectorized, in reference to the northern direction
#' @param jitter numeric value, amount of jitter (e.g., in meters, however in general dimensionless)
#' @param small logical value, TRUE for omnidirectional cell and FALSE for directional cell
#' @param subscript character value, abbreviation for layer id
#' @param seed numeric value, seed for reproducibility
#'
#'
#' @return A data frame with antennas of the specfic layer in the rows and the
#'        the following columns: point location (x, y), tower.id, cell.id, cell.kind
#'        (layer), intra.cell.number (in case of directional cells, three antennas per tower,
#'        sectorized), kind of cell (directional or omnidirectional), and rotation degree.
#'        This data frame corresponds to an unvalidated cell plan that can be validated
#'        with `create_cellplan()`.
#'
#' @export
#' @importFrom dplyr "%>%"
#'
#'


create_cells <- function(area.sf,
                         tower.dist,
                         rotation.deg,
                         jitter,
                         small = FALSE,
                         subscript,
                         seed) {


  set.seed = seed

  rotation = function(a){
    r = a * pi / 180 #degrees to radians
    matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
  }

  layer_network_generate = function(x, tower.dist, rotation.deg){
    layer.geo <- x %>%
      sf::st_make_grid(cellsize = tower.dist,
                   square = F, # hexagon
                   flat_topped = T) %>%  # different cell size (qm)
      sf::st_geometry()

    layer.centroid <- sf::st_centroid(layer.geo)
    layer <- (layer.geo - layer.centroid) * rotation(rotation.deg) + layer.centroid # rotate by 35 degrees
    return(layer)

  }

  # create layer object, placing towers
  layer <- layer_network_generate(x = area.sf, tower.dist = tower.dist, rotation.deg = rotation.deg)

  # specify exact location of towers and labelling
  towers <- layer %>%
    sf::st_centroid() %>%
    sf::st_jitter(jitter) %>%
    sf::st_coordinates() %>%
    tibble::as_tibble() %>%
    dplyr::select(X.tow = X, Y.tow = Y) %>%
    dplyr::mutate(tower.id = paste0(subscript, ".", 1:dplyr::n()))

  # create 3 cells per tower and labelling
  cells.unparam <- towers %>%
    dplyr::slice(rep(1:dplyr::n(), each = 3)) %>%
    dplyr::group_by(tower.id) %>%
    dplyr::mutate(cell = paste(tower.id, "C", 1:3, sep = ".")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cell.kind = subscript) %>%
    dplyr::mutate(intra.cell.number = stringr::str_sub(cell, -1)) %>%
    dplyr::mutate(small = small) %>%
    dplyr::mutate(rotation.deg = rotation.deg)

  return(cells.unparam)

}
