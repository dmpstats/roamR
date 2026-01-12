# show method 

    Code
      VarDist(units = "m")
    Output
      <VarDist>
      NA [m]

---

    Code
      VarDist(10, "kW/hr")
    Output
      <VarDist>
      10 [kW h-1]

---

    Code
      VarDist(distributional::dist_normal(mu = 1, sigma = 3), "radians")
    Output
      <VarDist>
      N(1, 9) [rad]

---

    Code
      VarDist(distributional::dist_sample(list(23:43)), "m/s")
    Output
      <VarDist>
      sample[21] [m s-1]

---

    Code
      VarDist(distributional::dist_percentile(list(sort(runif(10))), list(seq(1, 100,
        10))), "m/l/kg")
    Output
      <VarDist>
      percentile[10] [m L-1 kg-1]

