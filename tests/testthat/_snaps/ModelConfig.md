# Validation of @movement_type works as expected

    Code
      movement_type(x) <- "xxx"
    Condition
      Error in `validObject()`:
      ! invalid class "ModelConfig" object: 
      - slot @movement_type: Invalid value "xxx". Must be one of "di" or "crw".

# Missing values for required slots raises errors

    Code
      ModelConfig(n_agents = NA_integer_)
    Condition
      Error in `validObject()`:
      ! invalid class "ModelConfig" object: 
      - slot @n_agents: Provide the number of agents to simulate.

---

    Code
      ModelConfig(ref_sys = sf::st_crs(NA))
    Condition
      Error in `validObject()`:
      ! invalid class "ModelConfig" object: 
      - slot @ref_sys: Missing value. Provide valid CRS for the AOC.

---

    Code
      ModelConfig(aoc_bbx = c(1, 2, NA, NA))
    Condition
      Error in `validObject()`:
      ! invalid class "ModelConfig" object: 
      - slot @aoc_bbx: Invalid input. Bounding box must be a vector of 4 non-NA values.

---

    Code
      ModelConfig(movement_type = "crw", delta_x = NA_real_, delta_y = NA_real_)
    Condition
      Error in `validObject()`:
      ! invalid class "ModelConfig" object: 
      - slot @delta_y: Missing value. Cell size in x dimension must be provided when `@movement_type = "crw"`. 
      - slot @delta_y: Missing value. Cell size in y dimension must be provided when `@movement_type = "crw"`.

---

    Code
      ModelConfig(start_date = as.Date(NA), end_date = as.Date(NA))
    Condition
      Error in `validObject()`:
      ! invalid class "ModelConfig" object: 
      - slot @start_date: Missing value. Provide the model's start date. 
      - slot @end_date: Missing value. Provide the model's end date.

---

    Code
      ModelConfig(delta_time = NA_character_)
    Condition
      Error in `validObject()`:
      ! invalid class "ModelConfig" object: 
      - slot @delta_time: Missing value. Provide the model's temporal resolution.

# Error raised when units of @delta_time are invalid

    Code
      ModelConfig(delta_time = "WRONG_UNITS")
    Condition
      Error in `validObject()`:
      ! invalid class "ModelConfig" object: 
      - Slot @delta_time: Failed to parse "WRONG_UNITS" as valid time units. Use recognized units (e.g. "1 day", "2 months").

# 'show' method prints out configuration as expected

    Code
      ModelConfig()
    Output
      <ModelConfig> instance with attributes:
      * Movement Model:      Density-informed
      * No. Agents:          100
      * Simulation period:   2026-01-01 -- 2026-01-05 (4 days)
      * Temporal resolution: 1 day
      * Bounding box:        xmin: 0  ymin: 0  xmax: 10  ymax: 10 [°]
      * Geodetic CRS:        WGS 84
      * Start site:          NA
      * End site:            NA

---

    Code
      ModelConfig(movement_type = "crw")
    Output
      <ModelConfig> instance with attributes:
      * Movement Model:      Correlated Random Walk
      * No. Agents:          100
      * Simulation period:   2026-01-01 -- 2026-01-05 (4 days)
      * Temporal resolution: 1 day
      * Bounding box:        xmin: 0  ymin: 0  xmax: 10  ymax: 10 [°]
      * Spatial resolution:  0.25 x 0.25 [°]
      * Geodetic CRS:        WGS 84
      * Start site:          NA
      * End site:            NA

---

    Code
      ModelConfig(start_sites = s)
    Output
      <ModelConfig> instance with attributes:
      * Movement Model:      Density-informed
      * No. Agents:          100
      * Simulation period:   2026-01-01 -- 2026-01-05 (4 days)
      * Temporal resolution: 1 day
      * Bounding box:        xmin: 0  ymin: 0  xmax: 10  ymax: 10 [°]
      * Geodetic CRS:        WGS 84
      * Start sites:         Simple feature with 3 features and 2 fields [WGS 84]
           id prop        geom
         1  A  0.3 POINT (1 1)
         2  B  0.3 POINT (2 2)
         3  C  0.4 POINT (3 3)
      * End site:            NA

---

    Code
      ModelConfig(start_sites = s, end_sites = e)
    Output
      <ModelConfig> instance with attributes:
      * Movement Model:      Density-informed
      * No. Agents:          100
      * Simulation period:   2026-01-01 -- 2026-01-05 (4 days)
      * Temporal resolution: 1 day
      * Bounding box:        xmin: 0  ymin: 0  xmax: 10  ymax: 10 [°]
      * Geodetic CRS:        WGS 84
      * Start sites:         Simple feature with 3 features and 2 fields [WGS 84]
           id prop        geom
         1  A  0.3 POINT (1 1)
         2  B  0.3 POINT (2 2)
         3  C  0.4 POINT (3 3)
      * End sites:           Simple feature with 15 features and 2 fields [WGS 84]
         First 5 sites:
                 prop id                  geometry
         1 0.06666667  A               POINT (1 9)
         2 0.06666667  B POINT (1.571429 8.428571)
         3 0.06666667  C POINT (2.142857 7.857143)
         4 0.06666667  D POINT (2.714286 7.285714)
         5 0.06666667  E POINT (3.285714 6.714286)

---

    Code
      ModelConfig(movement_type = "crw", ref_sys = sf::st_crs(32630))
    Output
      <ModelConfig> instance with attributes:
      * Movement Model:      Correlated Random Walk
      * No. Agents:          100
      * Simulation period:   2026-01-01 -- 2026-01-05 (4 days)
      * Temporal resolution: 1 day
      * Bounding box:        xmin: 0  ymin: 0  xmax: 10  ymax: 10 [m]
      * Spatial resolution:  0.25 x 0.25 [m]
      * Projected CRS:       WGS 84 / UTM zone 30N
      * Start site:          NA
      * End site:            NA

