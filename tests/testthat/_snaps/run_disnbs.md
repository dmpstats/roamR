# run_disnbs() fails when driver IDs required under specified `scen` are not specified

    Code
      run_disnbs(rover_ibm_disnbs, run_scen = "baseline", dens_id = "dens",
        feed_state_id = "f", roost_state_id = "ro")
    Condition
      Error in `run_disnbs()`:
      ! `intake_id` must be provided when `run_scen = "baseline"`

---

    Code
      run_disnbs(rover_ibm_disnbs, run_scen = "baseline-and-impact", dens_id = "dens",
        feed_state_id = "f", roost_state_id = "ro")
    Condition
      Error in `run_disnbs()`:
      ! `intake_id` must be provided when `run_scen = "baseline-and-impact"`

---

    Code
      run_disnbs(rover_ibm_disnbs, run_scen = "impact", dens_id = "dens", intake_id = "intake",
        feed_state_id = "f", roost_state_id = "ro")
    Condition
      Error in `run_disnbs()`:
      ! `imp_dens_id` must be provided when `run_scen = "impact"`

---

    Code
      run_disnbs(rover_ibm_disnbs, run_scen = "baseline-and-impact", dens_id = "dens",
        intake_id = "intake", feed_state_id = "f", roost_state_id = "ro")
    Condition
      Error in `run_disnbs()`:
      ! `imp_dens_id` must be provided when `run_scen = "baseline-and-impact"`

---

    Code
      run_disnbs(rover_ibm_disnbs, run_scen = "impact", dens_id = "dens", intake_id = "intake",
        imp_dens_id = "imp_dens", feed_state_id = "f", roost_state_id = "ro")
    Condition
      Error in `run_disnbs()`:
      ! `imp_intake_id` must be provided when `run_scen = "impact"`

# run_disnbs() fails when baseline and impacted density maps have inconsistent dimensions

    Code
      run_disnbs(x, run_scen = "baseline-and-impact", dens_id = "dens", intake_id = "intake",
        imp_intake_id = "imp_intake", imp_dens_id = "imp_dens", feed_state_id = "f",
        roost_state_id = "ro")
    Condition
      Error in `run_disnbs()`:
      ! <stars> objects of drivers "dens" and "imp_dens" must have identical dimensions.
      x Driver "dens" cube dimensions: [x:76, y:56, months:6, iter:5]
      x Driver "imp_dens" cube dimensions: [x:76, y:56, months:6, iter:2]
      i Note: make sure to ru `rmr_initiate()` after adjustments made to data contained in drivers.

---

    Code
      run_disnbs(x, run_scen = "baseline-and-impact", dens_id = "dens", intake_id = "intake",
        imp_intake_id = "imp_intake", imp_dens_id = "imp_dens", feed_state_id = "f",
        roost_state_id = "ro")
    Condition
      Error in `run_disnbs()`:
      ! <stars> objects of drivers "dens" and "imp_dens" must have identical dimensions.
      x Driver "dens" cube dimensions: [x:76, y:56, months:6, iter:5]
      x Driver "imp_dens" cube dimensions: [x:76, y:56, months:2, iter:5]
      i Note: make sure to ru `rmr_initiate()` after adjustments made to data contained in drivers.

# run_disnbs() fails when inputs are not specified in expected contextual units

    Code
      run_disnbs(ibm = x, dens_id = "dens", intake_id = "intake", feed_state_id = "f",
        roost_state_id = "ro", waypnts_res = units::set_units(2, "g"))
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `map_()`:
      ! Slot `@stars_obj` of Driver "dens" must have one unique attribute.
      x Provided object has 2 attributes: "counts" and "extra_dim".

---

    Code
      run_disnbs(ibm = x, dens_id = "dens", intake_id = "intake", feed_state_id = "f",
        roost_state_id = "ro", feed_avg_net_energy = units::set_units(2, "km/h"))
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `map_()`:
      ! Slot `@stars_obj` of Driver "dens" must have one unique attribute.
      x Provided object has 2 attributes: "counts" and "extra_dim".

---

    Code
      run_disnbs(ibm = x, dens_id = "dens", intake_id = "intake", feed_state_id = "f",
        roost_state_id = "ro", target_energy = units::set_units(2, "J/h"))
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `map_()`:
      ! Slot `@stars_obj` of Driver "dens" must have one unique attribute.
      x Provided object has 2 attributes: "counts" and "extra_dim".

---

    Code
      run_disnbs(ibm = x, dens_id = "dens", intake_id = "bla", feed_state_id = "f",
        roost_state_id = "ro")
    Condition
      Error in `run_disnbs()`:
      ! Input values in `intake_id` are expected to carry a valid unit of energy per unit-of-time.
      x "kJ" is not a recognized energy per unit-of-time unit.
      i Use e.g., "kJ/hr" instead.

# run_disnbs() fails when expected: misc

    Code
      run_disnbs(ibm = rover_ibm_disnbs, dens_id = "NON-EXISTENT_DRIVER", intake_id = "intake",
        feed_state_id = "foraging", roost_state_id = "water_resting")
    Condition
      Error in `run_disnbs()`:
      ! Failed to find driver ID "NON-EXISTENT_DRIVER" in the provided <IBM> object.
      i Ensure all required driver IDs are present in slot @drivers of the `ibm` object.

---

    Code
      run_disnbs(ibm = rover_ibm_disnbs, run_scen = "baseline-and-impact", dens_id = "dens",
        intake_id = "intake", imp_dens_id = "GONE_DRIVER", imp_intake_id = "ABSENT-DRIVER",
        feed_state_id = "foraging", roost_state_id = "water_resting")
    Condition
      Error in `run_disnbs()`:
      ! Failed to find driver IDs "GONE_DRIVER" and "ABSENT-DRIVER" in the provided <IBM> object.
      i Ensure all required driver IDs are present in slot @drivers of the `ibm` object.

---

    Code
      run_disnbs(ibm = x, run_scen = "baseline", dens_id = "dens", intake_id = "intake",
        imp_dens_id = "GONE_DRIVER", imp_intake_id = "ABSENT-DRIVER", feed_state_id = "foraging",
        roost_state_id = "water_resting")
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `map_()`:
      ! Slot `@stars_obj` of Driver "dens" must contain a populated <stars> object.

---

    Code
      run_disnbs(ibm = x, run_scen = "baseline", dens_id = "dens", intake_id = "intake",
        imp_dens_id = "GONE_DRIVER", imp_intake_id = "ABSENT-DRIVER", feed_state_id = "foraging",
        roost_state_id = "water_resting")
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `map_()`:
      ! Slot `@stars_obj` of Driver "dens" must have one unique attribute.
      x Provided object has 2 attributes: "counts" and "extra_dim".

---

    Code
      run_disnbs(ibm = x, dens_id = "d", intake_id = "intake", feed_state_id = "foraging",
        roost_state_id = "water_resting")
    Condition
      Error in `run_disnbs()`:
      ! Temporal dimension in datacube of driver "d" does not fully cover all time steps under modeling.
      i Ensure the driver provides data for the entirety of the simulation period.

---

    Code
      run_disnbs(x, run_scen = "baseline-and-impact", dens_id = "dens", intake_id = "intake",
        imp_intake_id = "imp_intake", imp_dens_id = "imp_dens", feed_state_id = "f",
        roost_state_id = "ro")
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `map_()`:
      ! Driver "dens" must have the same CRS as specified in slot `@model_config` of argument `ibm`.
      x Detected CRS of <stars> object for "dens": "ETRS89 / UTM zone 33N + DHHN92 height" (EPSG: 5556)
      x Expected CRS from `ibm@model_config@ref_sys`: "WGS 84" (EPSG: 4326)

---

    Code
      run_disnbs(rover_ibm_disnbs, run_scen = "baseline", dens_id = "dens",
        intake_id = "intake", feed_state_id = "f", roost_state_id = "ro")
    Condition
      Error in `run_disnbs()`:
      ! States IDs "f" and "ro" are not defined in the <IBM> object provided to `ibm`.
      i Please ensure the inputs to `feed_state_id` and `roost_state_id` is listed in `ibm@species@states_profile`.
      i Available state IDs are "flying", "foraging", "swimming", and "water_resting".

---

    Code
      run_disnbs(rover_ibm_disnbs, run_scen = "baseline", dens_id = "dens",
        intake_id = "intake", feed_state_id = "foraging", roost_state_id = "ro")
    Condition
      Error in `run_disnbs()`:
      ! State ID "ro" is not defined in the <IBM> object provided to `ibm`.
      i Please ensure the input to `roost_state_id` is listed in `ibm@species@states_profile`.
      i Available state IDs are "flying", "foraging", "swimming", and "water_resting".

