n <- c(200,500)
magDif <- c(0, .2, .4, .6)
facLoad.e <- c(.4,.8)
facLoad.o <- c(.4,.8)
percItems <- c(2,4,6)
min.dif <- c(-1,0)
nCause <- c(1)
nIndicator <- c(20)
magEff <- c(.2, .4, .6)
modCount <- 100
all.perms <- expand.grid(n, magDif, facLoad.e, facLoad.o,percItems,nCause, nIndicator, magEff, min.dif)
colnames(all.perms) <- c("n", "magDif", "facLoad.e","facLoad.o",
                         "percItems", "nCause", "nIndicator", 
                         "magEff", "minDif")
all.perms.list <- split(all.perms, f=1:nrow(all.perms))
