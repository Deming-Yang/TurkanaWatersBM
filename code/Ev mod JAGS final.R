# JAGS model file
model {
  
  # define parameters and prior distributions

  # T() truncated at upper and lower limits (to avoid unrealistic values)
  
  # informative prior on rh centering around 0.6, Hopson (1982)
  rh ~ dbeta(16, 12) T(0.25, 0.95) 
  
  # informative k prior
  k ~ dbeta(15, 2)
  
  # uninformative x priors
  x ~ dbeta(1.5, 1.5) T(0.05, 0.95)
  
  # informative prior based on water temperature
  TC ~ dnorm(mean.TC, 1 / sd.TC^2) 
    
  # informative prior based on water isotope values of the Omo River
  # (1 / variance) = precision, which is the second term in dnorm(,)
  # the input is also constrained by measured river water values
  d18Oi ~ dnorm(mean.d18Oi, 1 / sd.d18Oi^2) 
  
  # meteoric source, so d18O and dD should fall along the GMWL
  # constrained by measured river water values
  dDi ~ dnorm(d18Oi * 8 + 10, 1 / (sd.d18Oi * 8)^2) 
  
  # informative prior
  # precip means and sds are from OIPC
  d18Op ~ dnorm(mean.d18Op, 1 / sd.d18Op^2)
  
  dDp ~ dnorm(mean.dDp, 1 / sd.dDp^2)
  
  # start model calculation
  
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
  
  # calculate evaporative slope
  sl = (dstarD - dDi) / (dstar18O - d18Oi)
  
  # calculate intercept 
  intc = dstarD - sl * dstar18O
  
  # likelihood calculation
  # this is essentially a Bayesian linear regression
  
  pre.d18OL ~ dgamma(1, 1) #uninformative priors for the precision term of d18OL
  
  pre.dDL ~ dgamma(1, 1) #weakly informative priors for the precision term of d18OL
  
  for (i in 1:N){
    
    m.d18O[i] ~ dnorm(d18OL, pre.d18OL) #use evaporated lake d18O to generate modeled d18O
    
    m.dD[i] ~ dnorm(intc + sl * m.d18O[i], pre.dDL) #use slope and intercept to generate modeled dD
    
    lw.dD[i] ~ dnorm(m.dD[i], 1 / sd.dD^2) #likelihood evaluation of measured dD
    
    lw.d18O[i] ~ dnorm(m.d18O[i], 1 / sd.d18O^2) #likelihood evaluation of measured d18O
    
  }
  
}