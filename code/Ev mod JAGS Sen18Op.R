# JAGS model file
model {
  
  # define parameters and prior distributions
  
  x = 1 # terminal lake in a steady state
  
  # T() truncated at upper and lower limits (to avoid unrealistic values)
  
  # initial relative humidity (upwind)
  rh.int ~ dbeta(16, 12) T(0.15, 0.95)
  # rh over Fergurson's Gulf was 0.6, Hopson (1982)
  
  # informative k prior
  k ~ dbeta(20, 1)
  
  # weakly informative f priors
  
  f ~ dbeta(1, 2) T(0.01, 0.4)
  
  # informative prior based on water temperature
  TC ~ dnorm(mean.TC, 1 / sd.TC^2) 
  
  # informative prior based on water isotope values of the Omo River
  # (1 / variance) = precision, which is the second term in dnorm(,)
  # the input is also constrained by measured river water values
  # input is relatively well constrained, so use input as part of the evaluation for the model
  
  # meteoric source, so d18O and dD should fall along the GMWL
  # constrained by measured river water values
  
  d18Oi ~ dnorm( mean.d18Oi, 1 / sd.d18Oi^2)   
  
  # meteoric source, so d18O and dD should fall along the GMWL
  # constrained by measured river water values
  dDi ~ dnorm(d18Oi * 8 + 10, 1 / (sd.d18Oi * 8)^2) 
  
  # uninformative prior for precipitation isotopes
  
  d18Op ~ dunif(range.d18Op[1], range.d18Op[2])
  
  dDp ~ dunif(range.dDp[1], range.dDp[2])
  
  rh[1] = rh.int
  
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
  ekD[1] = 12.5 * (1 - rh[1])
  ek18O[1] = 14.2 * (1 - rh[1])
  
  # temporal enrichment slope (Gibson et al 2016 eqn 6)
  mD[1] = (rh[1] - (1e-3 * (ekD[1] + (eD / aD)))) / ((1 - rh[1]) + (1e-3 * ekD[1]))
  m18O[1] = (rh[1] - (1e-3 * (ek18O[1] + (e18O / a18O)))) / ((1 - rh[1]) + (1e-3 * ek18O[1]))
  
  # atmospheric isotope ratios (Gibson et al 2016 eqn 18) 
  dDA[1] = (dDp - (k * eD)) / (1 + (1e-3 * k * eD))
  d18OA[1] = (d18Op - (k * e18O)) / (1 + (1e-3 * k * e18O))
  
  # limiting isotope ratios (Gibson et al 2016 eqn 7)
  dstarD[1] = ((rh[1] * dDA[1]) + ekD[1] + (eD / aD)) / (rh[1] - (1e-3 * (ekD[1] + (eD / aD))))
  dstar18O[1] = ((rh[1] * d18OA[1]) + ek18O[1] + (e18O / a18O)) / (rh[1] - (1e-3 * (ek18O[1] + (e18O / a18O))))
  
  # (Gibson et al 2016 eqn 9)
  dDL[1] = (dDi  + (mD[1] * x * dstarD[1])) / (1 + mD[1] * x)
  d18OL[1] = (d18Oi + (m18O[1] * x * dstar18O[1])) / (1 + m18O[1] * x)
  
  # (Gibson et al 2016 eqn 21)
  dDv[1] = (((dDL[1] - eD) / aD) - (rh[1] * dDA[1]) - ekD[1]) / (1 - rh[1] + (1e-3 * ekD[1]))
  d18Ov[1] = (((d18OL[1] - e18O) / a18O) - (rh[1] * d18OA[1]) - ek18O[1]) / (1 - rh[1] + (1e-3 * ek18O[1]))
  
  # record temporary slope and intercept
  sl[1] = (dstarD[1] - dDv[1]) / (dstar18O[1] - d18Ov[1])
  
  intc[1] = dstarD[1] - sl[1] * dstar18O[1]
  
  # mixing will increase rh
  rh.inc[1] = (1 - rh[1]) * f
  
  # the following section will iteratively resolve the mixing between air and evaporated lake water 
  for(i in 2:t){
    # mixing (Gibson et al 2016 eqn 20) 
    dDA[i] = (1 - f) * dDA[i - 1] + f * dDv[i - 1]
    d18OA[i] = (1 - f) * d18OA[i - 1] + f * d18Ov[i - 1]
    
    # the influx of evaporated lake water increases relative humidity above the lake
    rh[i] = rh[i - 1] + rh.inc[i - 1]
    
    rh.inc[i] = (1 - rh[i]) * f
    
    # diffusion controlled fractionation (Horita et al 2008)
    ekD[i] = 12.5 * (1 - rh[i])
    ek18O[i] = 14.2 * (1 - rh[i])
    
    # temporal enrichment slope (Gibson et al 2016 eqn 6)
    mD[i] = (rh[i] - (1e-3 * (ekD[i] + (eD / aD)))) / ((1 - rh[i]) + (1e-3 * ekD[i]))
    m18O[i] = (rh[i] - (1e-3 * (ek18O[i] + (e18O / a18O)))) / ((1 - rh[i]) + (1e-3 * ek18O[i]))
    
    # limiting isotope ratios (Gibson et al 2016 eqn 7)
    dstarD[i] = ((rh[i] * dDA[i]) + ekD[i] + (eD / aD)) / (rh[i] - (1e-3 * (ekD[i] + (eD / aD))))
    dstar18O[i] = ((rh[i] * d18OA[i]) + ek18O[i] + (e18O / a18O)) / (rh[i] - (1e-3 * (ek18O[i] + (e18O / a18O))))
    
    # (Gibson et al 2016 eqn 9)
    dDL[i] = (dDi  + (mD[i] * x * dstarD[i])) / (1 + mD[i] * x)
    d18OL[i] = (d18Oi + (m18O[i] * x * dstar18O[i])) / (1 + m18O[i] * x)
    
    # (Gibson et al 2016 eqn 21)
    dDv[i] = (((dDL[i] - eD) / aD) - (rh[i] * dDA[i]) - ekD[i]) / (1 - rh[i] + (1e-3 * ekD[i]))
    d18Ov[i] = (((d18OL[i] - e18O) / a18O) - (rh[i] * d18OA[i]) - ek18O[i]) / (1 - rh[i] + (1e-3 * ek18O[i]))
    
    
    sl[i] = (dstarD[i] - dDv[i]) / (dstar18O[i] - d18Ov[i])
    
    # calculate intercept 
    intc[i] = dstarD[i] - sl[i] * dstar18O[i]
    
  }
  
  # (Gibson et al 2016 eqn 9)
  dDL.ev = (dDi  + (mD[t] * dstarD[t])) / (1 + mD[t])
  d18OL.ev = (d18Oi + (m18O[t] * dstar18O[t])) / (1 + m18O[t])
  
  # record the evolved isotope values
  dstarD.ev = dstarD[t]
  
  dstar18O.ev = dstar18O[t]
  
  dDA.ev = dDA[t]
  
  d18OA.ev = d18OA[t]
  
  dDv.ev = dDv[t]
  
  d18Ov.ev = d18Ov[t]
  
  rh.ev = rh[t]
  
  # use mass balance to calculate added evaporated lake water vapor fraction
  # Derived from Gibson et al. eqn 20
  f.ev =  (dDA.ev - dDA[1])/ (dDv.ev - dDA[1]) 
  
  # calculate evaporative slope
  sl.ev = (dstarD.ev - dDv.ev) / (dstar18O.ev - d18Ov.ev)
  
  # calculate intercept
  intc.ev = dstarD.ev - sl.ev * dstar18O.ev
  
  # likelihood calculation
  # this is essentially a Bayesian linear regression
  
  pre.d18OL ~ dgamma(1, 1) #uninformative priors for the precision term of d18OL
  
  pre.dDL ~ dgamma(1, 1) #weakly informative priors for the precision term of d18OL
  
  for (i in 1:N){
    
    m.d18O[i] ~ dnorm(d18OL.ev, pre.d18OL) #use evaporated lake d18O to generate modeled d18O
    
    m.dD[i] ~ dnorm(intc.ev + sl.ev * m.d18O[i], pre.dDL) #use slope and intercept to generate modeled dD
    
    lw.dD[i] ~ dnorm(m.dD[i], 1 / sd.dD^2) #likelihood evaluation of measured dD
    
    lw.d18O[i] ~ dnorm(m.d18O[i], 1 / sd.d18O^2) #likelihood evaluation of measured d18O
    
  }
  
  
}
