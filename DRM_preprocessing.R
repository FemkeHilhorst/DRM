
###########
# Geometry             
###########

# Convert table from columns to rows:
b.a0=t(b.a0)
bot.depth=t(bot.depth)
bottom.x=t(bottom.x)

# Calculate mean value of the initial water level a0
a0.mean=rowMeans(a0)

# Calculate the bottom slope, S0, from the bottom height, zb
o=t(data.frame(bot.depth[2]-bot.depth[1], bot.depth[3]-bot.depth[2])
    /data.frame( bottom.x[2]-bottom.x[1], bottom.x[3]-bottom.x[2]))
bot.slope=rbind(o[1],mean(o), o[2])

# Make a function of geometric input
zb = approxfun(bottom.x, bot.depth, rule=2) 
S0 = approxfun(bottom.x,bot.slope,rule=2)
width = approxfun(bottom.x,b.a0,rule=2) 

# Wetted Area (A)
A = function(x,a) # m^2
{
  return(width(x)*a)
}

# Hydraulic Radius (R)
R = function(x,a) # m
{
  return((width(x)*a)/(2*a+width(x)))
}

# Width at water level (b)
b = function(a) # m
{
  return(width(x))
}


###############
# Flow function 
###############

# The theoretical basis DRM is a flow function where 
# discharge is related to water level and water level slope. 
# This equation depends on celerity as well as diffusion.
# It comprises two components: f1 and f2. 


# One component is linearized and depends on the water level slope(da/dx) 
f1.lin=function(x,state,grad)
{
  return(-1/n*sqrt(abs(S0(x)))-1/(2*n)*1/(sqrt(abs(S0(x))))*grad)
}

# One component depends on the water level 
f2=function(x,state)
{ 
  a=state-zb(x)
  return(A(x,a)*(R(x,a)^(2/3)))
}

# The flow function
Q = function(x,state,grad)
{ 
  return(f1.lin(x,state,grad)*f2(x,state))
}  


#################
# Construct Model
#################

# Define domain of the model; the length of the river
domain = c(0,bottom.x[length(bottom.x)])

# Define the spatial, stationair DRM model 
stat.model = newFLOW1D(domain,Q)

# Because state gets numerical unstable close to ground
mindepth = function(x,state)  
{
  return((state-zb(x)>0.01))
}
set.isacceptable(stat.model,mindepth)


######################
# Boundary Conditions
######################

# Left Boundary
# -----------------------------
# The left boundary of DRM has a fixed flux and state (Cauchy boundary)
# because a weir is present at the outlet of most rivers. The rectangular 
# weir equation is used to calculate the flow rate over the weir:

weir.height = zb(1)+1 # height of the weir [m]
weir.width=b.a0[1]  # width of weir [m]
c.d=0.8 # discharge coefficient [-]
g=9.81 # acceleration of gravity [m/s^2]

  Q.weir <- function(state) # discharge over weir [m^3/s]
  {
    a=state-weir.height
    if(a<=0) {return(0)}
    else {return(-(2/3)*c.d*weir.width*sqrt(2*g)*a^(3/2))}
  }
Q.weir=Vectorize(Q.weir)
set.BC.fluxstate(stat.model,"left",Q.weir)

# Right Boundary
# ---------------------------
# The right boundary of DRM has a fixed flux (Neumann boundary) 
# of 0.1 mm/hour
right.discharge=0.1
set.BC.fixedflux(stat.model,"right","right.discharge")


#########################
# Numerical Solution    
#########################

# Place nodes and discretization with Finite Volume 
nodes=seq(from=domain[1],to=domain[2],length=100)
set.discretisation(stat.model,nodes,"FV")

# Initial water level, first guess of water level
constdepth = function(x)
{
  return(max(a0.mean+zb(x),0.01+zb(x)))
}





























