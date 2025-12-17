# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser


x <- 1
y <- 2
z <- 3
create_fct <- function() {
  y <- 0
  function() {
    x <- 0
    # here
    env_chain_graph()
    c(x=x, y=y, z=z)
  }
}
call_fct <- function(g) {
  x <- -1
  y <- -1
  z <- -1
  print(g())
}
f <- create_fct()
z <- 0
call_fct(f)
## x y z
## 0 0 0


ls_all_envs <- function(env = environment()) {
  i <- 1
  out <- list()
  while (!identical(env, emptyenv())) {
    out[[paste0("env_", i, " (", environmentName(env), ")")]] <- ls(env, all.names = TRUE)
    env <- parent.env(env)
    i <- i + 1
  }
  out
}

# Beispiel an deiner Funktion:
create_fct <- function() {
  y <- 0
  function() {
    x <- 0
    ls_all_envs()
  }
}

f <- create_fct()
str(f())

library(igraph)

env_chain_graph <- function(env = environment(), include_vars = TRUE) {
  nodes <- c()
  edges <- c()
  
  e <- env
  i <- 1
  
  # Environment-Kette aufbauen
  while (!identical(e, emptyenv())) {
    e_name <- environmentName(e)
    if (e_name == "") {
      e_name <- paste0("env_", i)
    }
    
    nodes <- c(nodes, e_name)
    
    # Variablen einfügen (optional)
    if (include_vars) {
      vars <- ls(envir = e, all.names = TRUE)
      full_vars <- paste0(e_name, "::", vars)
      nodes <- c(nodes, full_vars)
      # Kanten Env → Var
      for (v in full_vars) {
        edges <- c(edges, e_name, v)
      }
    }
    
    p <- parent.env(e)
    if (!identical(p, emptyenv())) {
      p_name <- environmentName(p)
      if (p_name == "") p_name <- paste0("parent_", i)
      edges <- c(edges, e_name, p_name)
    }
    
    e <- p
    i <- i + 1
  }
  
  g <- graph(edges, directed = TRUE)
  plot(
    g,
    layout = layout_as_tree,
    vertex.size = 20,
    vertex.label.cex = 0.7,
    vertex.label.dist = 0.3,
    main = "Environment Chain Graph"
  )
  
  invisible(g)
}

