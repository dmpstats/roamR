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

