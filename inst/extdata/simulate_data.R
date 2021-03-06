## Initialization
# initialize session
set.seed(500)
source("R/simulate.R")
library(RandomFields)
RFoptions(seed = 500)

# define functions
rasterToLines <- function(x) {
  xres <- raster::xres(x)
  yres <- raster::yres(x)

  pts <- as.data.frame(raster::rasterToPoints(x, spatial = FALSE))
  pts$vertical_start_x <- pts$x
  pts$vertical_start_y <- pts$y + (yres * 0.5)
  pts$vertical_end_x <- pts$x
  pts$vertical_end_y <- pts$y - (yres * 0.5)
  pts$horizontal_start_x <- pts$x + (xres * 0.5)
  pts$horizontal_start_y <- pts$y
  pts$horizontal_end_x <- pts$x - (xres * 0.5)
  pts$horizontal_end_y <- pts$y

  lines <- lapply(seq_len(nrow(pts)), function(i) {
    sp::Lines(ID = as.character(i), list(
      sp::Line(
        matrix(c(
          pts$vertical_start_x[i], pts$vertical_start_y[i],
          pts$vertical_end_x[i], pts$vertical_end_y[i]
        ), ncol = 2, byrow = TRUE)
      ),
      sp::Line(
        matrix(c(
          pts$horizontal_start_x[i], pts$horizontal_start_y[i],
          pts$horizontal_end_x[i], pts$horizontal_end_y[i]
        ), ncol = 2, byrow = TRUE)
      )
    ))
  })

  SpatialLinesDataFrame(sp::SpatialLines(lines, x@crs),
                                 data = pts[, 2 + seq_len(raster::nlayers(x))])
}

## Simulate data
# create landscape
sim_landscape <- raster::setValues(raster::raster(ncol = 10, nrow = 10), 1)
raster::extent(sim_landscape) <- c(0, 1, 0, 1)
sim_landscape@crs <- sp::CRS()

# create planning units as raster
sim_pu_raster <- simulate_cost(sim_landscape, n = 1)
sim_pu_raster[raster::Which(sim_pu_raster == 0)] <- NA

# make 10 cells in the raster NA
sim_pu_raster[sample.int(raster::ncell(sim_pu_raster), size = 10)] <- NA

# create feature distributions
sim_features <- simulate_species(sim_landscape, n = 5)

# generate locked in and out planning units
cells <-  raster::Which(!is.na(sim_pu_raster), cells = TRUE)
locked_in_cells <- sample(cells, size = floor(raster::ncell(sim_landscape) *
                                              0.1))
locked_out_cells <- sample(cells, size = floor(raster::ncell(sim_landscape) *
                                              0.1))
locked_out_cells <- locked_out_cells[!locked_out_cells %in% locked_in_cells]
sim_locked_in_raster <- raster::setValues(sim_pu_raster, FALSE)
sim_locked_in_raster[locked_in_cells] <- TRUE
sim_locked_out_raster <- raster::setValues(sim_pu_raster, FALSE)
sim_locked_out_raster[locked_out_cells] <- TRUE

# convert raster data to Spatial-class objects
stk <- raster::stack(sim_pu_raster, sim_locked_in_raster,
                     sim_locked_out_raster)
sim_pu_polygons <- raster::rasterToPolygons(stk, n = 4)
sim_pu_polygons <- sim_pu_polygons[is.finite(sim_pu_polygons@data[[1]]), ]
sim_pu_lines <- rasterToLines(stk)
sim_pu_lines <- sim_pu_lines[is.finite(sim_pu_lines@data[[1]]), ]
sim_pu_points <- raster::rasterToPoints(stk, spatial = TRUE)
sim_pu_points <- sim_pu_points[is.finite(sim_pu_points@data[[1]]), ]

# clean up Spatial-class objects
names(sim_pu_polygons) <- c("cost", "locked_in", "locked_out")
names(sim_pu_lines) <- c("cost", "locked_in", "locked_out")
names(sim_pu_points) <- c("cost", "locked_in", "locked_out")

# convert integer values to boolean values
sim_pu_polygons$locked_in <- as.logical(sim_pu_polygons$locked_in)
sim_pu_polygons$locked_out <- as.logical(sim_pu_polygons$locked_out)
sim_pu_lines$locked_in <- as.logical(sim_pu_lines$locked_in)
sim_pu_lines$locked_out <- as.logical(sim_pu_lines$locked_out)
sim_pu_points$locked_in <- as.logical(sim_pu_points$locked_in)
sim_pu_points$locked_out <- as.logical(sim_pu_points$locked_out)

# simulate species phylogeny
sim_phylogeny <- ape::rtree(n = raster::nlayers(sim_features))
sim_phylogeny$tip.label <- names(sim_features)

## Export data
# save data
save(sim_pu_raster, file = "data/sim_pu_raster.rda", compress = "xz")
save(sim_locked_in_raster, file = "data/sim_locked_in_raster.rda",
     compress = "xz")
save(sim_locked_out_raster, file = "data/sim_locked_out_raster.rda",
     compress = "xz")
save(sim_pu_polygons, file = "data/sim_pu_polygons.rda", compress = "xz")
save(sim_pu_lines, file = "data/sim_pu_lines.rda", compress = "xz")
save(sim_pu_points, file = "data/sim_pu_points.rda", compress = "xz")
save(sim_features, file = "data/sim_features.rda", compress = "xz")
save(sim_phylogeny, file = "data/sim_phylogeny.rda", compress = "xz")
