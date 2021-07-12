# d3calc.R - Symmetries of the Equilateral square

btn_sidebar <- function(input_id, lbl_str) {
  actionButton(input_id, span(HTML(lbl_str), style = "text-align:center;"),
               style = "width:80px;")
}

D4.makeDataFrame <- function() {
  DF <- data.frame(name = rep("", 6), cfg = rep("", 6),
                   stringsAsFactors = FALSE)
  DF[1, ] <- c("i", "ABCD")
  DF[2, ] <- c("r", "DABC")
  DF[3, ] <- c("r2", "CDAB")
  DF[4, ] <- c("r3", "BCDA")
  DF[5, ] <- c("s", "BADC")
  DF[6, ] <- c("rs", "CBAD")
  DF[7, ] <- c("r2s", "DCBA")
  DF[8, ] <- c("r3s", "ADCB")
  
  return(DF)
}
# BiggsDF <- D4.makeDataFrame()
# BiggsDF

D4.showConfigs <- function(DF) {
  par(mar = c(1, 1, 1, 1))
  plot(NULL, xlim = c(-0.35, 17.65), ylim = c(-1, 3), asp = 1, axes = FALSE)
  for (i in 0:7) {
    # points() determines the positioning of the squares' vertices.
    # Positive values increase positioning in the right/up direction, and
    # negative values increase positioning in the left/down direction.
    # For instance, the first value in the first vector is used to set the
    # left/right position of the bottom left vertex of each square. The first
    # value in the second vector determines the up/down position of the bottom
    # left vertex of each square.
    points(c(0, 2, 2, 0, 0) + 3 * i, c(0, 0, 2, 2, 0), type = "l")
    lbl <- strsplit(DF[i + 1, 2], "")[[1]]
    # The text() function below determines the position of the As, Bs, and Cs
    # within the smaller squares.
    text(c(.4, 1.6, 1.6, .4) + 3 * i, c(1.74, 1.74, .26, .26), lbl)
    # The text() function below determines the position of the i, r, s, x, y,
    # and z labels under the squares.
    text(1 + 3 * i, -0.5, DF[i + 1, 1])
    # segments() graphs the dotted lines
    # Positive values increase positioning in the right/up direction, and
    # negative values increase positioning in the left/down direction.
    # For example, the first value in each vector is used to determine the
    # display of x's vertical axis of reflection. Respectively, the entries
    # describe the position of that axis' bottom vertex (right/left), bottom
    # vertex (up/down), top vertex (right/left), and top vertex (up/down).
    segments(c(13, 15, 18, 23),
             c(0, 0, 1, 0),
             c(13, 17, 20, 21),
             c(2, 2, 1, 2), lty = 2)
  }
}
# D4.showConfigs(BiggsDF)

# cfg is a string of symbols, reading clockwise from the top of the square
# D4.showsquare() determines the positioning of the large square
D4.showsquare <- function(cfg) {
  par(mar = c(1, 1, 1, 1))
  plot(NULL, xlim = c(-0.35, 2.65), ylim = c(-1, 2), asp = 1, axes = FALSE)
  points(c(0, 2, 2, 0, 0), c(0, 0, 2, 2, 0), type = "l", lwd = 2)
  lbl <- strsplit(cfg, "")[[1]]
  text(c(.4, 1.6, 1.6, .4), c(1.74, 1.74, .26, .26), lbl)
}
# D4.showsquare("ABC")

# a is one of the Biggs symbols for an operation.

# The return value is the new configuration
D4.apply <- function(a, cfg) {
  v <- strsplit(cfg, "")[[1]]   # select first component of list
  w <- switch(a,
              "i" = v,
              "r" = c(v[4], v[1], v[2], v[3]),
              "r2" = c(v[3], v[4], v[1], v[2]),
              "r3" = c(v[2], v[3], v[4], v[1]),
              "s" = c(v[2], v[1], v[4], v[3]),
              "rs" = c(v[3], v[2], v[1], v[4]),
              "r2s" = c(v[4], v[3], v[2], v[1]),
              "r3s" = c(v[1], v[4], v[3], v[2])
  )
  s <- paste(w, collapse = "") 
  return(s)
}
# D4.apply("r", "BCA")


D4.multiply <- function(DF, b, a) {
  # Look up the name, which occurs once and only once
  idx <- which.max(DF$name == a)
  # Find the corresponding configuration
  cfg <- DF$cfg[idx]
  # Apply the group operation to it
  newcfg <- D4.apply(b, cfg)
  # Look up the configuration
  idx <- which.max(DF$cfg == newcfg)
  return (DF$name[idx])
}
# D4.multiply(BiggsDF, "r", "r")

# To use D4.multiply() with outer(), we must vectorize it.
vD4.multiply <- Vectorize(D4.multiply, c("a", "b"))
