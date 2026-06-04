# State() throws an error when invalid input is provided

    Code
      State(id = 123)
    Condition
      Error in `State()`:
      ! Argument `id` must be an object of class <character>
      x You've provided an object of class <numeric>

---

    Code
      State(type = "invalid_type")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "foraging", "resting", "travelling", "other"

---

    Code
      State(type = c("foraging", "sleeping"))
    Condition
      Error in `match.arg()`:
      ! 'arg' must be of length 1

---

    Code
      State(energy_cost = "not_a_VarDist")
    Condition
      Error in `State()`:
      ! Argument `energy_cost` must be an object of class <VarDist>
      x You've provided an object of class <character>

---

    Code
      State(time_budget = "not_a_VarDist")
    Condition
      Error in `State()`:
      ! Argument `time_budget` must be an object of class <VarDist>
      x You've provided an object of class <character>

---

    Code
      State(speed = "not_a_VarDist")
    Condition
      Error in `State()`:
      ! Argument `speed` must be an object of class <VarDist>
      x You've provided an object of class <character>

