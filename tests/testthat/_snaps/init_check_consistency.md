# top-level input errors are detected

    Code
      init_check_consistency(rover, d)
    Condition <err-multiple-driverid>
      Error:
      ! Driver IDs provided to `drivers` must be unique.
      x Driver ID "sst" found at positions 1 and 4 in `drivers`.
      i Ensure that each driver ID is assigned to a unique entry in the `drivers` list.

---

    Code
      init_check_consistency(rover, d)
    Condition <err-multiple-driverid>
      Error:
      ! Driver IDs provided to `drivers` must be unique.
      x Driver ID "sst" found at positions 1 and 4 in `drivers`.
      x Driver ID "prey_distr" found at positions 5 and 7 in `drivers`.
      i Ensure that each driver ID is assigned to a unique entry in the `drivers` list.

---

    Code
      init_check_consistency(r, rover_drivers)
    Condition <err-nonexistent-driverid>
      Error:
      ! Driver responses specified in `species@driver_responses` must refer to valid driver IDs in `drivers`.
      x Driver ID "poo" not found in <Driver> objects whithin `drivers`.
      i Check if `@driver_id`s in <DriverResponse> objects listed under `species@driver_responses` match those defined in `drivers`.

---

    Code
      init_check_consistency(Species(), d, m)
    Condition <err-driver-outside-aoc>
      Error:
      ! The geometric feature specified for driver ID "trawling_area" is located outside the AOC's spatial extent.
      i To include this driver, consider expanding the AOC defined in argument `model_config`.

---

    Code
      init_check_consistency(Species(), d, m)
    Condition <err-driver-outside-aoc>
      Error:
      ! Driver ID "sst" lies completely outside the spatial extent of the specified AOC.
      i To include this driver, consider expanding the AOC defined in argument `model_config`.

