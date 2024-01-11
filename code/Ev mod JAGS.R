# JAGS model file
model {
  
  # define parameters and prior distributions
  # all are uninformative priors
  
  TC ~ dunif(range.TC[1], range.TC[2])
  
  rh ~ dunif(range.rh[1], range.rh[2])
  
  d18Oi ~ dunif(range.d18Oi[1], range.d18Oi[2])
  
  dDi ~ dunif(range.dDi[1], range.dDi[2])
  
  d18Op ~ dunif(range.d18Op[1], range.d18Op[2])
  
  dDp ~ dunif(range.dDp[1], range.dDp[2])
  
  k ~ dunif(range.k[1], range.k[2])
  
  x ~ dunif(range.x[1], range.x[2])

  # Celsius to Kelvin conversion
  TK = TC + 273.15
  
  # liquid-vapor fractionation factors (Horita & Wesolowski 1994)
  aD = exp( ((1158.8 * TK^3)/1e12) - ((1620.1 * TK^2)/1e9) + ((794.84 * TK)/1e6) - (161.04/1e3)  + (2.992e6/TK^3) )
  a18O = exp( (-7.685e-3) + (6.7123 / TK) - (1.6664e3 / TK^2) + (0.35041e6 / TK^3) )
  
  # equilibrium isotopic separation (ibid.)
  eD = (aD - 1) * 1000
  e18O = (a18O - 1) * 1000
  
  # diffusion controlled fractionation (Horita et al 2008)
  ekD = 12.5 * (1 - rh)
  ek18O = 14.2 * (1 - rh)
  
  # atmospheric isotope ratios (Gibson et al 2016 eqn 18) 
  dDA = (dDp - (k * eD)) / (1 + (1e-3 * k * eD))
  d18OA = (d18Op - (k * e18O)) / (1 + (1e-3 * k * e18O))
  
  # limiting isotope ratios (Gibson et al 2016 eqn 7)
  dstarD = ((rh * dDA) + ekD + (eD / aD)) / (rh - (1e-3 * (ekD + (eD / aD))))
  dstar18O = ((rh * d18OA) + ek18O + (e18O / a18O)) / (rh - (1e-3 * (ek18O + (e18O / a18O))))
  
  # temporal enrichment slope (Gibson et al 2016 eqn 6)
  mD = (rh - (1e-3 * (ekD + (eD / aD)))) / ((1 - rh) + (1e-3 * ekD))
  m18O = (rh - (1e-3 * (ek18O + (e18O / a18O)))) / ((1 - rh) + (1e-3 * ek18O))
  
  # calculate lake water isotopes for values of x (Gibson et al 2016 eqn 10) and vapor isotopes (eqn 3)
  
  dDL = (dDi  + (mD * x * dstarD)) / (1 + mD * x)
  d18OL = (d18Oi + (m18O * x * dstar18O)) / (1 + m18O * x)
  
  dDv = (((dDL - eD) / aD) - (rh * dDA) - ekD) / (1 - rh + (1e-3 * ekD))
  d18Ov = (((d18OL - e18O) / a18O) - (rh * d18OA) - ek18O) / (1 - rh + (1e-3 * ek18O))
  
  # use dDL, d18OL, and dDi, d18Oi, to calculate slope and intercept of the evaporative line
  # the line should go through these two points
  
  # calculate evaporative slope
  sl = (dDL - dDi) / (d18OL - d18Oi)
  
  # calculate intercept 
  intc = dDL - sl * d18OL

  # likelihood calculation
  # this is essentially a Bayesian linear regression
  
  for (i in 1:N){
    # (1 / variance) = precision, which is the second term in dnorm(,)
    
    m.d18O[i] ~ dnorm(lw.d18O[i], 1/sd.d18O^2) #use real lake d18O to generate modeled d18O
    
    m.dD[i] = intc + sl * m.d18O[i] #use modeled d18O to generate modeled dD
    
    lw.dD[i] ~ dnorm(m.dD[i], 1/sd.dD^2) #final likelyhood evaluation
    
  }
  
}