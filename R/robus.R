

## polygons can be really annoying

## what can we do about that? decompose to triangles and piece back together

## helpers

#' convert sf to mesh3d
from_sf <- function(x, ...) {
  ## remotes::install_github("hypertidy/anglr")
  anglr::as.mesh3d(anglr::DEL0(x, ...))
}
#' project sf to sfc (fairly) safely, we dunk any triangle parts that aren't valid (and could do more here)
proj_sf <- function(x, crs, ...) {
  x1 <- from_sf(x, ...)
  suppressWarnings(x1$vb[1:2, ] <- t(sf::sf_project(t(x1$vb[1:2, ]), to = crs, from = sf::st_crs(x)$wkt, keep = TRUE)))

  ## set up a template so we don't have to churn through st_polygon machine every time
  template <- sf::st_polygon(list(cbind(c(0, 1, 0.5, 0), c(0, 0, 0.5, 0))))

  out <- lapply(apply(x1$it, 2, \(.x) t(x1$vb[1:2,c(.x, .x[1])]), simplify = FALSE), \(.y) {if (anyNA(.y)) {return(NULL)}; sfx <- template; sfx[[1]] <- .y; sfx})
  bad <- vapply(out, is.null, FALSE)
  out[!bad]
}

## so, now call that for every object so we can keep all the fields values together

safe_proj_sf <- function(x, crs, a = NULL) {
  if (is.null(a)) {
    a <- prod(diff(sf::st_bbox(world)[c(1, 3, 2, 4)])[c(1, 3)]/20)
  }
  l <- vector("list", nrow(x))
  for (i in seq_along(l)) {
    x1 <- proj_sf(world[i, ], a = a, crs = crs)
    if (length(x1) > 0) {
      l[[i]] <- sf::st_set_geometry(x[i, ], sf::st_union(sf::st_sfc(x1, crs = crs)))
    } else {
      l[[i]] <- sf::st_set_geometry(x[i, ], sf::st_as_sfc("POLYGON EMPTY", crs = crs))

    }
  }
  sfx <- do.call(rbind, l)
}

