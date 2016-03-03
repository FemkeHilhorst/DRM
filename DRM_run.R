
#######################
# Force DRM with WALRUS
#######################

# Make a function of the WALRUS output
t=seq(0, 3600*(nrow(mod)-1), 3600)
Q.walrus=approxfun(t,mod$Q,rule=2)
xas=seq(bottom.x[1],bottom.x[3], 335)

# Construct spatial WALRUS forcing
time=0
Q.spatial = function(time)
{
  return(Q.walrus(time)/length(xas))
}

# Force WALRUS in DRM as spatial flux
for (i in 1:length(xas))
{
  add.pointflux(stat.model, xas[i], Q.spatial(time))
}


######################
# Solve stationair DRM  
######################

# Initializes the state values
do.initialize(stat.model,constdepth)

# Solve the first time step of DRM
control = solve.steps(stat.model, maxiteracceptability=50)


#############################
# Preprocessing DRM Transient 
#############################

# Make a function of the previous state
prevstate = approxfun(stat.model$nodes, stat.model$states, rule = 2)

# Construct DRM transient  
trans.model = newFLOW1D(domain, Q)

# Implement nodes in model
set.discretisation(trans.model, nodes, "FV")

# Initializes the state values
do.initialize(trans.model, prevstate)

# Because state gets numerical unstable close to ground 
mindepth = function(x,state)  
{
  return((state-zb(x)>0.01))
}
set.isacceptable(trans.model,mindepth)

# Implement boundary conditions
set.BC.fixedflux(trans.model,"right","right.discharge")
set.BC.fluxstate(trans.model,"left", Q.weir)

# Force WALRUS in DRM trans as spatial flux
for (i in 1:length(xas))
{
  add.pointflux(trans.model, xas[i], Q.spatial(time), paste("Q.point", i,sep=""))
}


############################
# Changing Discharge in Time 
############################

# Implement change in discharge as a storage forcing term 
Agen  = function(x,state) 
{
  a=state-zb(x)
  return(A(x,a))
}

Q.storage = function(x, state) 
{
  return(-((Agen(x,state)-Agen(x,prevstate(x)))/delt))
}

add.spatialflux(trans.model, Q.storage)


######################
# Time Characteristics  
######################

# Total run time in seconds 
total.time=24*3600*nrow(mod)/24

# Time step (delta t) in seconds
delt = 3600*6 # in seconds
time = delt


#####################
# Solve Transient DRM
#####################

Qdrm=c()
while (time < total.time) 
{
  # Solve steps for each time step 
  crits = solve.steps(trans.model, maxiteracceptability = 50)
  
  # Add time step 
  time = time + delt
  
  # Force DRM with WALRUS for each time step
  for (i in 1:length(xas))
  {
    add.pointflux(trans.model, xas[i], Q.spatial(time), paste("Q.point", i,sep=""))
  }
  
  # Calulate previous state
  prevstate = approxfun(trans.model$nodes, trans.model$states, rule = 2)
 
  # Returns dataframe of internal fluxes between nodes 
  Qdrm=c(Qdrm,dataframe.internalfluxes(trans.model)$intflux[1])
}












