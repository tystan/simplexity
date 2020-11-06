

gsi.mystructure <- function(x, ...){
  if(is.null(x)) return(x)
  else return(structure(x, ...))
}




get_ellipse_points <- function(mean,var,r,steps=72) {
  # mean and variance in clr/cpt
  ei   <- eigen(clrvar2ilr(var),symmetric=TRUE)
  w <- seq(0,2*pi,length.out=steps+1)
  sw<-sin(w)
  cw<-cos(w)
  if( min(ei$values) / max(ei$values) < -1E-8) {
    warning("Non positive Semidefinite Matrix used in Ellipses")
    print(list(problem="Non positive Semidefinite Matrix used in Ellipses",var=var,eigen=ei))
  }
  rs <- sqrt(abs(ei$values))*r
  # Loop over ellipse centers
  meFull <- oneOrDataset(idt(mean))
  if(length(dim(meFull))==0) dim(meFull) = c(1, length(meFull))
  
  allX <- NULL
  for(k in 1:nrow(meFull) ) {
    # me   <- gsi.mystructure(meFull[k,],class="rmult")
    me   <- meFull[k,]
    aux = cbind(me[1]+rs[1]*ei$vectors[1,1]*sw+rs[2]*ei$vectors[1,2]*cw,
                me[2]+rs[1]*ei$vectors[2,1]*sw+rs[2]*ei$vectors[2,2]*cw
    )
    X <- idtInv(aux, orig=mean, V=ilrBase(D=ncol(oneOrDataset(mean))))
    allX <- rbind(allX, X)
    # noreplot(lines(gsi.mystructure(X,trafoed=TRUE),aspanel=TRUE))
  }
  allX
}


get_ellipse_points_ <- Vectorize(get_ellipse_points, vectorize.args = "r", SIMPLIFY = FALSE, USE.NAMES = TRUE)
list_concat <- function(x) foreach(i = 1:length(x), .combine = "rbind") %do% {x[[i]]}

get_ellipse_points_vec <- function(mean,var,r,steps=72) {
  interim_res <- get_ellipse_points_(mean=mean,var=var,r=r,steps=steps) 
  interim_res
  list_concat(interim_res)
}



library("deltacomp") # see: https://github.com/tystan/deltacomp for installation
library("compositions")
library("dplyr")
library("foreach")

base::source('clip_to_ref_hull.R')

### ref_dat 


# real data to imitate
data("fairclough", package = "deltacomp")
fc5 <- fairclough %>% select(sed, lpa, mpa, vpa, sleep)

head(fc5)
summary(fc5)
fc5_ref <- ilr(acomp(fc5))
head(fc5_ref)
plot(fc5_ref)


?mean.acomp
?var.acomp
?ellipses
?ellipses.acomp




fc3 <- 
  fairclough %>% 
  mutate(pa = lpa + mpa + vpa) %>%
  select(sed, pa, sleep) %>%
  acomp(.)

fc4 <- 
  fairclough %>% 
  mutate(mvpa = mpa + vpa) %>%
  select(sed, lpa, mvpa, sleep) %>%
  acomp(.)

fc5 <- 
  fairclough %>% 
  select(sed, lpa, mpa, vpa, sleep) %>%
  acomp(.)


plot(fc3)
# ellipses.acomp0(mean.acomp(fc3), var.acomp(fc3), r = 7, aspanel = TRUE)
n_grid <- 100
ps <- seq(0.01, 0.99, length = n_grid)
r <- sqrt(qchisq(p = ps, df = 2))

# points_to_add <- get_ellipse_points(mean.acomp(fc3), var.acomp(fc3), r = r[50], steps = 100)

pts <- get_ellipse_points_vec(mean.acomp(fc3), var.acomp(fc3), r = r, steps = 100)


# ?plot.acomp
plot.acomp(acomp(pts), col = "red", add=TRUE)
# acomp(points_to_add)[1:10, ]
# fc3[1:10, ]


plot(ilr(pts), col = "red", pch = 16)
points(ilr(fc3))

simplex_gen <- function(ref_comp_df, n = 1e+4) {
  
  if (attr(ref_comp_df, "class") != "acomp") {
    warning("Converting 'ref_comp_df' to a composition (closure) object")
    ref_comp_df <- acomp(ref_comp_df)
  }
  
  n_grid <- ceiling(sqrt(n))
  if (n_grid < 4) {
    stop("Please choose an 'n' that is at least 10 points")
  }
  ps <- seq(0.01, 0.99, length = n_grid)
  r <- sqrt(qchisq(p = ps, df = 2))
  
  simplex_mean <- mean.acomp(ref_comp_df)
  simplex_vcov <- var.acomp(ref_comp_df)
  grid_pts <- get_ellipse_points_vec(simplex_mean, simplex_vcov, r = r, steps = n_grid - 1)
  return(grid_pts)
  
}


my_grid <- simplex_gen(fc3, n = 100)
plot.acomp(acomp(my_grid), col = "dodgerblue", pch = 16)
plot.acomp(acomp(fc3), add = TRUE)


my_grid <- simplex_gen(fc4, n = 100)
plot.acomp(acomp(my_grid), col = "dodgerblue", pch = 16)
plot.acomp(acomp(fc3), add = TRUE)

n_grid <- 10
ps <- seq(0.01, 0.99, length = n_grid)
r <- sqrt(qchisq(p = ps, df = 2))

get_ellipse_points(mean(fc4),var(fc4),r[5],steps=10)

r <- r[5]
steps <- 9
var <- var(fc4)
mean <- mean(fc4)
ei   <- eigen(clrvar2ilr(var),symmetric=TRUE)
w <- seq(0,2*pi,length.out=steps+1)
sw<-sin(w)
cw<-cos(w)
if( min(ei$values) / max(ei$values) < -1E-8) {
  warning("Non positive Semidefinite Matrix used in Ellipses")
  print(list(problem="Non positive Semidefinite Matrix used in Ellipses",var=var,eigen=ei))
}
rs <- sqrt(abs(ei$values))*r
# Loop over ellipse centers
meFull <- oneOrDataset(idt(mean))
if(length(dim(meFull))==0) dim(meFull) = c(1, length(meFull))

allX <- NULL
D <- ncol(oneOrDataset(mean))
for(k in 1:nrow(meFull) ) { # k <- 1
  # me   <- gsi.mystructure(meFull[k,],class="rmult")
  me   <- meFull[k,]
  aux <-
    foreach(d = 1:D, .combine = cbind) %do% {
      
      
    }
  
  = cbind(me[1]+rs[1]*ei$vectors[1,1]*sw+rs[2]*ei$vectors[1,2]*cw,
              me[2]+rs[1]*ei$vectors[2,1]*sw+rs[2]*ei$vectors[2,2]*cw
  )
  X <- idtInv(aux, orig=mean, V=ilrBase(D=D))
  allX <- rbind(allX, X)
  # noreplot(lines(gsi.mystructure(X,trafoed=TRUE),aspanel=TRUE))
}
allX

