# Validation works as expected

    Code
      prep_stars_obj(dt)
    Condition
      Error:
      ! Issue found in dimension "t" of `dt`.
      ! Character-valued, non-raster dimensions are assumed to represent months.
      x The following values in dimension "t" are not valid month names: "a", "b", "c", "d", "e", "f", "g", "h", "i", and "j".
      i See month.name (`?base::month.name()`) and month.abb (`?base::month.abb()`) for accepted values.

---

    Code
      prep_stars_obj(dt)
    Condition
      Error:
      ! Invalid dimension name detected in `dt`.
      x "WRONG_NAME" is not an accepted name for a non-raster dimension with numeric values.
      i Valid names for such dimensions are: "month", "year", "quarter", "yearweek", "yearday", "iter", "iteration", "boot", "bootstrap" or "sample".

---

    Code
      Driver(id = "a", stars_obj = dt)
    Condition
      Error in `Driver()`:
      ! Invalid dimension name detected in `stars_obj`.
      x "WRONG_NAME" is not an accepted name for a non-raster dimension with numeric values.
      i Valid names for such dimensions are: "month", "year", "quarter", "yearweek", "yearday", "iter", "iteration", "boot", "bootstrap" or "sample".

---

    Code
      prep_stars_obj(dt)
    Condition
      Error:
      ! Dimension named "year" of `dt` contains negative value(s).
      x Numeric-valued, non-raster dimensions must contain only positive integers.

---

    Code
      Driver(id = "a", stars_obj = dt)
    Condition
      Error in `Driver()`:
      ! Dimension named "year" of `stars_obj` contains negative value(s).
      x Numeric-valued, non-raster dimensions must contain only positive integers.

---

    Code
      prep_stars_obj(dt)
    Condition
      Error:
      ! Dimension named "yearweek" of `dt` contains fractional value(s).
      x Numeric-valued, non-raster dimensions must contain only positive integers.

---

    Code
      Driver(id = "a", stars_obj = dt)
    Condition
      Error in `Driver()`:
      ! Dimension named "yearweek" of `stars_obj` contains fractional value(s).
      x Numeric-valued, non-raster dimensions must contain only positive integers.

---

    Code
      prep_stars_obj(dt)
    Condition
      Error:
      ! Issue found in dimension "year" of `dt`.
      x Years must be represented as 4-digit integer numbers (e.g., 1990, 2024).

---

    Code
      Driver(id = "a", stars_obj = dt)
    Condition
      Error in `Driver()`:
      ! Issue found in dimension "year" of `stars_obj`.
      x Years must be represented as 4-digit integer numbers (e.g., 1990, 2024).

---

    Code
      prep_stars_obj(dt)
    Condition
      Error in `.f()`:
      ! Issue found in dimension "month" of `dt`.
      x Dimension contains values outside the accepted range.
      x Numeric months must be represented by integer numbers between 1 and 12 (inclusive).

---

    Code
      Driver(id = "a", stars_obj = dt)
    Condition
      Error in `Driver()`:
      ! Issue found in dimension "month" of `stars_obj`.
      x Dimension contains values outside the accepted range.
      x Numeric months must be represented by integer numbers between 1 and 12 (inclusive).

---

    Code
      prep_stars_obj(dt)
    Condition
      Error in `.f()`:
      ! Issue found in dimension "quarter" of `dt`.
      x Dimension contains values outside the accepted range.
      x Quarter of the year must be represented by integer numbers between 1 and 4 (inclusive).

---

    Code
      Driver(id = "a", stars_obj = dt)
    Condition
      Error in `Driver()`:
      ! Issue found in dimension "quarter" of `stars_obj`.
      x Dimension contains values outside the accepted range.
      x Quarter of the year must be represented by integer numbers between 1 and 4 (inclusive).

---

    Code
      prep_stars_obj(dt)
    Condition
      Error in `.f()`:
      ! Issue found in dimension "yearday" of `dt`.
      x Dimension contains values outside the accepted range.
      x Day of the year must be represented by integer numbers between 1 and 365 (inclusive).

---

    Code
      Driver(id = "a", stars_obj = dt)
    Condition
      Error in `Driver()`:
      ! Issue found in dimension "yearday" of `stars_obj`.
      x Dimension contains values outside the accepted range.
      x Day of the year must be represented by integer numbers between 1 and 365 (inclusive).

---

    Code
      prep_stars_obj(dt)
    Condition
      Error in `.f()`:
      ! Issue found in dimension "yearweek" of `dt`.
      x Dimension contains values outside the accepted range.
      x Week of the year must be represented by integer numbers between 1 and 52 (inclusive).

---

    Code
      Driver(id = "a", stars_obj = dt)
    Condition
      Error in `Driver()`:
      ! Issue found in dimension "yearweek" of `stars_obj`.
      x Dimension contains values outside the accepted range.
      x Week of the year must be represented by integer numbers between 1 and 52 (inclusive).

---

    Code
      attr(prep_stars_obj(dt), "dim_meta")
    Condition
      Warning:
      Non-raster dimensions of `dt` must represent distinct covariate types.
      ! Both dimensions "time" and "month" appear to convey time-related variables.
      ! Only the first of these dimensions, "time", will be used in the model.
    Output
      $raster
      $raster$dims
      [1] 1 2
      
      $raster$names
      [1] "x" "y"
      
      
      $non_raster
      $non_raster$dims
      [1] 3 4
      
      $non_raster$names
      [1] "time"  "month"
      
      $non_raster$types
      [1] "temporal" "temporal"
      
      $non_raster$procs
      [1] "asis"      "month_chr"
      
      $non_raster$cls
      [1] "POSIXct"   "character"
      
      

---

    Code
      attr(prep_stars_obj(dt), "dim_meta")
    Condition
      Warning:
      Non-raster dimensions of `dt` must represent distinct covariate types.
      ! Both dimensions "iter" and "boot" appear to convey resampling-based replicates.
      ! Only the first of these dimensions, "iter", will be used in the model.
    Output
      $raster
      $raster$dims
      [1] 1 2
      
      $raster$names
      [1] "x" "y"
      
      
      $non_raster
      $non_raster$dims
      [1] 3 4
      
      $non_raster$names
      [1] "iter" "boot"
      
      $non_raster$types
      [1] "iteration" "iteration"
      
      $non_raster$procs
      [1] "asis" "asis"
      
      $non_raster$cls
      [1] "numeric" "numeric"
      
      

