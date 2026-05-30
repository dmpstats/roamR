# simulate_agent_disnbs() fails when inputs are invalid

    Code
      simulate_agent_disnbs(a, d, s, "impact", npr, dnbs_cfg = cfg, fane)
    Condition
      Error in `simulate_agent_disnbs()`:
      ! `dnbs_cfg` must be an object of class <disnbs_config>.
      i Build required object via `create_dnbs_config()`.

---

    Code
      simulate_agent_disnbs(a, d, s, "impact", npr, cfg, fane)
    Condition
      Error in `simulate_agent_disnbs()`:
      ! not_null(dnbs_cfg$imp_dens_id) is not TRUE

---

    Code
      simulate_agent_disnbs(a, d, s, "impact", npr, cfg, fane)
    Condition
      Error in `simulate_agent_disnbs()`:
      ! not_null(dnbs_cfg$imp_intake_id) is not TRUE

# snap_to_nearest_pop_cell() fails when expected

    Code
      snap_to_nearest_pop_cell(p_orig, rover_drivers$drv_sst@stars_obj)
    Condition
      Error in `snap_to_nearest_pop_cell()`:
      ! length(dim(rst)) == 2 is not TRUE

---

    Code
      snap_to_nearest_pop_cell(p_orig, dplyr::mutate(rover_drivers$drv_prey@stars_obj[,
        , , 1, drop = TRUE], r = prey^2))
    Condition
      Error in `snap_to_nearest_pop_cell()`:
      ! length(rst) == 1 is not TRUE

---

    Code
      snap_to_nearest_pop_cell(p_orig, rover_drivers$drv_prey@stars_obj[, , , 1,
        drop = TRUE])
    Condition
      Error in `snap_to_nearest_pop_cell()`:
      ! sf::st_crs(pnt) == sf::st_crs(rst) is not TRUE

