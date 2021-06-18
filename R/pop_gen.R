#' @title pop_gen
#'
#' @description Generates a fully synthetic mobile phone population and geographical area.
#'
#' @param tile.num numeric value, number of tiles in total
#' @param base.tile.size numeric value, size of tiles (possibly in meters, however in general without dimension)
#' @param city.num numeric value, number of city clusters
#' @param city.size numeric value, size of city cluster
#' @param city.shape character value, the shape of the city cluster, either "SQUARE", ...
#' @param hole.num numeric value, number of hole clusters
#' @param hole.size numeric value, size of hole clusters
#' @param hole.shape character value, the shape of the hole cluster, either "SQUARE", ...
#' @param pop.dist.df tibble with 2 columns, "type" and "expression". See Details for further instructions
#'
#' @return A list object with 7 elements, `area.params`, containing the specified parameters,
#'     `area.sf`, the area tibble with geometry column (tile polygons),
#'     `area.df`, idential to the area tibble, however, without the geometry column,
#'     `area.union` an sfc polygon object that contains the outter shape of the complete area
#'     `area.bbox` named vector with boundary coordinates of the area
#'     `area.raster` raster layer of the area with population values on the tile level
#'     `area.elevation` raster layer of the area with elevation values on the tile level
#'
#' @examples
#' # tile.num <- 100 # number of tiles
#' base.tile.size <- 100 # size of a single tile
#' city.num <- 1 # number of single cities
#' city.size <- 10 # size of a single city polygon
#' hole.num <- 1 # number of single holes
#' hole.size <- 1 # size of a single hole polygon
#' pop.dist.df <- tibble(type = c("Urban", "Hole", "Rural"),
#'                       expression = c("ReIns::rtpareto(shape = 0.05, scale = 10, endpoint = 200",
#'                                     "ReIns::rtpareto(shape = 0.7, scale = 0.1, endpoint = 20",
#'                                     "ReIns::rtpareto(shape = 0.5, scale = 0.1, endpoint = 100"))
#'                                           area <- pop_gen(tile.num, base.tile.size,
#'                                                   city.num, city.size, city.shape = "SQUARE",
#'                                                   hole.num, hole.size, hole.shape = "SQUARE",
#'                                                   pop.dist.df)
#' @export
#' @importFrom dplyr "%>%"





# Population generation function
pop_gen <- function(tile.num,
                    base.tile.size,
                    city.num,
                    city.size,
                    city.shape = "SQUARE",
                    hole.num,
                    hole.size,
                    hole.shape = "SQUARE",
                    pop.dist.df) {


  # save area parameters
  area.params <- list(tile.num = tile.num,
                      base.tile.size = base.tile.size,
                      city.num = city.num,
                      city.size = city.size,
                      city.shape = city.shape,
                      hole.num = hole.num,
                      hole.size = hole.size,
                      hole.shape = hole.shape,
                      pop.dist.df = pop.dist.df)

  # calculate complete area size
  poly.size <- sqrt(tile.num) * base.tile.size

  # build basis polygon
  area.polygon <- st_polygon(list(rbind(c(0, 0), c(poly.size, 0), c(poly.size, poly.size), c(0, poly.size), c(0, 0)))) %>%
    st_sfc() %>%
    st_sf()

  # create bounding box area and corresponding raster object
  area.bbox <- st_bbox(area.polygon)
  area.raster <- create_raster(area.bbox, tile.size = base.tile.size)

  # specify elevation raster --> default at 0 currently
  area.elevation <- area.raster
  values(area.elevation) <- 0

  # retransform raster to sf for cell creation
  base.tiles <- st_as_sf(st_as_stars(area.raster))

  # build city geometry (1 obs.)
  cities <- st_sample(area.polygon, city.num) %>%
    st_buffer(dist = city.size, endCapStyle = city.shape) %>% # parameter if square or circle
    st_geometry() %>%
    st_union() %>%
    st_sf() %>%
    mutate(city = 1)

  # build hole geometry (1 obs.)
  holes <- st_sample(cities, hole.num) %>%
    st_buffer(dist = hole.size, endCapStyle = as.character(hole.shape)) %>% # parameter if square or circle
    st_geometry() %>%
    st_union() %>%
    st_sf() %>%
    mutate(hole = 1)

  # join tiles with cities and holes
  area.sf.helper <- base.tiles %>%
    st_join(cities) %>%
    st_join(holes) %>%
    mutate(type = case_when(city == 1 & is.na(hole) ~ "Urban",
                            city == 1 & hole == 1 ~ "Hole",
                            TRUE ~ "Rural")) %>%
    mutate(category = case_when(type %in% c("Urban", "Hole") ~ "Urban",
                                type == "Rural" ~ "Rural")) %>%
    rownames_to_column("tile.id") %>%
    mutate(tile.id = as.integer(tile.id)) %>%
    arrange(type)

  # find out how many tiles are of a certain type to match with respective pop-distribution function
  summary.area <- area.sf.helper %>%
    st_drop_geometry() %>%
    dplyr::select(tile.id, type) %>%
    group_by(type) %>%
    summarise(n = n()) %>%
    left_join(pop.dist.df, by = "type") %>%
    mutate(final = paste0(expression, ", n = ", n, ")")) %>%
    arrange(type)

  # sample pop vector from input distribution function
  pop.helper <- unlist(map(summary.area$final, ~eval(parse(text = .x))))

  # append pop to the sf data frame, apply necessary rounding and create final sf dataframe
  area.sf <- area.sf.helper %>%
    mutate(pop = pop.helper) %>%
    mutate(pop = round(pop, 0)) %>%
    mutate(pop = if_else(pop < 0, 0, pop)) %>%
    mutate(centroid.geometry = st_centroid(.$geometry)) %>%
    mutate(X.centroid = unlist(map(.$centroid.geometry, 1)),
           Y.centroid = unlist(map(.$centroid.geometry, 2))) %>%
    dplyr::select(tile.id, type, category, pop, X.centroid, Y.centroid) %>%
    arrange(tile.id)

  # create non-sf data frame version
  area.df <- area.sf %>%
    st_drop_geometry() %>%
    arrange(tile.id)

  # put everything into a output list
  final <- list(area.params = area.params,
                area.sf = area.sf,
                area.df = area.df,
                area.union = area.polygon,
                area.bbox = area.bbox,
                area.raster = area.raster,
                area.elevation = area.elevation)

  return(final)
}
