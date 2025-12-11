# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser

library(R6)

GridPath <- R6Class("GridPath",
                    public = list(
                      # öffentliche Felder
                      path = NULL,        # n x 2 matrix (x,y)
                      direction = NULL,   # "U","R","D","L" (kann beim Input auch "W" für West sein)
                      
                      # interne Hilfsdaten
                      .dir_vecs = list(U = c(0, 1), R = c(1, 0), D = c(0, -1), L = c(-1, 0)),
                      .order = c("U", "R", "D", "L"),   # Reihenfolge im Uhrzeigersinn
                      .synonyms = list(W = "L"),       # akzeptierte Synonyme (z.B. "W" -> "L")
                      
                      # Konstruktor: direction (default "U"), start (default c(0,0))
                      initialize = function(direction = "U", start = c(0, 0)) {
                        if (!is.numeric(start) || length(start) != 2) {
                          stop("start muss ein numerischer Vektor der Länge 2 sein.")
                        }
                        # Normalisiere direction (akzeptiere z.B. "W" als Synonym für "L")
                        dir_key <- private$canonical_direction(direction)
                        self$direction <- dir_key
                        self$path <- matrix(as.numeric(start), nrow = 1)
                        invisible(self)
                      },
                      
                      # Drehe nach rechts (im Uhrzeigersinn). Gibt self zurück (method chaining).
                      rotate_right = function() {
                        idx <- match(self$direction, self$.order)
                        if (is.na(idx)) stop("Ungültige direction.")
                        new_idx <- idx %% length(self$.order) + 1
                        self$direction <- self$.order[new_idx]
                        return(self)
                      },
                      
                      # Drehe nach links (gegen den Uhrzeigersinn). Gibt self zurück.
                      rotate_left = function() {
                        idx <- match(self$direction, self$.order)
                        if (is.na(idx)) stop("Ungültige direction.")
                        new_idx <- (idx - 2) %% length(self$.order) + 1
                        self$direction <- self$.order[new_idx]
                        return(self)
                      },
                      
                      # Move: gehe `steps` Schritte in Blickrichtung (natural number, 0 erlaubt).
                      move = function(steps) {
                        if (length(steps) != 1 || !is.numeric(steps) || steps < 0 || steps != floor(steps)) {
                          stop("steps muss eine natürliche Zahl (inkl. 0) sein.")
                        }
                        steps <- as.integer(steps)
                        if (steps == 0) return(self)
                        
                        if (!(self$direction %in% names(self$.dir_vecs))) {
                          stop("Ungültige direction.")
                        }
                        dir_vec <- self$.dir_vecs[[self$direction]]
                        last_pt <- as.numeric(self$path[nrow(self$path), , drop = TRUE])
                        
                        new_pts <- matrix(0, nrow = steps, ncol = 2)
                        for (i in seq_len(steps)) {
                          new_pts[i, ] <- last_pt + i * dir_vec
                        }
                        
                        self$path <- rbind(self$path, new_pts)
                        return(self)
                      },
                      
                      # Print-Methode: zeichnet Pfad (type = "b") und Pfeil für aktuelle Blickrichtung.
                      print = function(...) {
                        pts <- as.matrix(self$path)
                        if (nrow(pts) < 1) {
                          cat("<GridPath: leer>\n")
                          invisible(self)
                        }
                        
                        # Achsenbereich mit kleinem Rand
                        xrange <- range(pts[, 1], na.rm = TRUE)
                        yrange <- range(pts[, 2], na.rm = TRUE)
                        margin <- max(1, max(diff(xrange), diff(yrange)) * 0.1)
                        xlim <- c(xrange[1] - margin, xrange[2] + margin)
                        ylim <- c(yrange[1] - margin, yrange[2] + margin)
                        
                        # Plot: Punkte + Linien (type = "b")
                        plot(pts[, 1], pts[, 2],
                             type = "b", pch = 19, xlab = "x", ylab = "y",
                             xlim = xlim, ylim = ylim, asp = 1,
                             main = sprintf("GridPath: %d Punkte, direction = %s", nrow(pts), self$direction))
                        
                        # Pfeil für die aktuelle Blickrichtung an der letzten Position
                        last_pt <- as.numeric(pts[nrow(pts), ])
                        dir_vec <- self$.dir_vecs[[self$direction]]
                        if (!is.null(dir_vec)) {
                          # Länge des Pfeils relativ zur Plotgröße
                          arrow_len_factor <- 0.4
                          to_pt <- last_pt + dir_vec * arrow_len_factor
                          arrows(x0 = last_pt[1], y0 = last_pt[2], x1 = to_pt[1], y1 = to_pt[2],
                                 length = 0.15, lwd = 2)
                        }
                        
                        invisible(self)
                      }
                    ),
                    
                    private = list(
                      # Liefert einen kanonischen Richtungsbuchstaben ("U","R","D","L")
                      # Akzeptiert ebenfalls Synonyme, z.B. "W" -> "L".
                      canonical_direction = function(dir) {
                        if (!is.character(dir) || length(dir) != 1) {
                          stop("direction muss ein einzelner character sein.")
                        }
                        dir <- toupper(dir)
                        # direkt vorhanden?
                        if (dir %in% self$.order) return(dir)
                        # Synonyme prüfen
                        if (dir %in% names(self$.synonyms)) {
                          return(self$.synonyms[[dir]])
                        }
                        stop("direction muss eines von 'U','R','D','L' (oder Synonym z.B. 'W') sein.")
                      }
                    )
)



# Test
pfad1 <- GridPath$new()
pfad1$direction <- "U"
pfad1$rotate_right()
pfad1$direction
## [1] "R"
pfad1$rotate_right()
pfad1$direction
## [1] "D"
pfad1$rotate_left()$rotate_left() # method chaining; Wir könnten alles aneinander hängen
pfad1$direction
## [1] "U"
pfad1$path <- matrix(c(0, 0), nrow=1)
pfad1$direction <- "U"
pfad1$move(3)
pfad1$path
## [,1] [,2]
## [1,] 0 0
## [2,] 0 1
## [3,] 0 2
## [4,] 0 3
pfad1$rotate_right()
pfad1$move(2)
pfad1$path
## [,1] [,2]
## [1,] 0 0
## [2,] 0 1
## [3,] 0 2
## [4,] 0 3
## [5,] 1 3
## [6,] 2 3
