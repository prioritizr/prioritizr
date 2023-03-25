# assert_required (simple examples)

    Code
      f1(1 + "a")
    Condition
      Error in `f1()`:
      i In argument to `y`.
      Caused by `1 + "a"`:
      ! non-numeric argument to binary operator

---

    Code
      f1(object_not_exist)
    Condition
      Error in `f1()`:
      i In argument to `y`.
      Caused by error:
      ! object 'object_not_exist' not found

---

    Code
      f2(1 + "a")
    Condition
      Error in `f2()`:
      i In argument to `x`.
      Caused by `1 + "a"`:
      ! non-numeric argument to binary operator

---

    Code
      f2(object_not_exist)
    Condition
      Error in `f2()`:
      i In argument to `x`.
      Caused by error:
      ! object 'object_not_exist' not found

# assert_required (magrittr pipe)

    Code
      f2(1 + "a") %>% f1() %>% print()
    Condition
      Error in `f2()`:
      i In argument to `x`.
      Caused by `1 + "a"`:
      ! non-numeric argument to binary operator

---

    Code
      f2(object_not_exist) %>% f1() %>% print()
    Condition
      Error in `f2()`:
      i In argument to `x`.
      Caused by error:
      ! object 'object_not_exist' not found

---

    Code
      f2(2) %>% f1(1 + "a") %>% print()
    Condition
      Error in `f1()`:
      i In argument to `w`.
      Caused by `1 + "a"`:
      ! non-numeric argument to binary operator

---

    Code
      f2(2) %>% f1(object_not_exist) %>% print()
    Condition
      Error in `f1()`:
      i In argument to `w`.
      Caused by error:
      ! object 'object_not_exist' not found

# assert_required (native pipe)

    Code
      print(f1(f2(1 + "a")))
    Condition
      Error in `f2()`:
      i In argument to `x`.
      Caused by `1 + "a"`:
      ! non-numeric argument to binary operator

---

    Code
      print(f1(f2(object_not_exist)))
    Condition
      Error in `f2()`:
      i In argument to `x`.
      Caused by error:
      ! object 'object_not_exist' not found

---

    Code
      print(f1(f2(2), 1 + "a"))
    Condition
      Error in `f1()`:
      i In argument to `w`.
      Caused by `1 + "a"`:
      ! non-numeric argument to binary operator

---

    Code
      print(f1(f2(2), object_not_exist))
    Condition
      Error in `f1()`:
      i In argument to `w`.
      Caused by error:
      ! object 'object_not_exist' not found

# assert_required (prioritizr examples, magrittr pipe)

    Code
      problem(sim_pu_raster, sim_features) %>% add_relative_targets(object_not_exist) %>%
        add_min_set_objective() %>% add_binary_decisions() %>% print()
    Condition
      Error in `add_relative_targets()`:
      i In argument to `targets`.
      Caused by error:
      ! object 'object_not_exist' not found

---

    Code
      problem(sim_pu_raster, sim_features) %>% add_relative_targets(1 + "a") %>%
        add_min_set_objective() %>% add_binary_decisions() %>% print()
    Condition
      Error in `add_relative_targets()`:
      i In argument to `targets`.
      Caused by `1 + "a"`:
      ! non-numeric argument to binary operator

---

    Code
      problem(object_not_exist, sim_features) %>% add_relative_targets(1 + "a") %>%
        add_min_set_objective() %>% add_binary_decisions() %>% print()
    Condition
      Error in `problem()`:
      i In argument to `x`.
      Caused by error:
      ! object 'object_not_exist' not found

---

    Code
      problem(sim_pu_raster, sim_features) %>% add_min_set_objective() %>%
        add_binary_decisions() %>% add_relative_targets(1 + "a") %>% print()
    Condition
      Error in `add_relative_targets()`:
      i In argument to `targets`.
      Caused by `1 + "a"`:
      ! non-numeric argument to binary operator

---

    Code
      problem(sim_pu_raster, sim_features) %>% add_min_set_objective() %>%
        add_binary_decisions() %>% add_relative_targets(object_not_exist) %>% print()
    Condition
      Error in `add_relative_targets()`:
      i In argument to `targets`.
      Caused by error:
      ! object 'object_not_exist' not found

# assert_required (prioritizr examples, native pipe)

    Code
      print(add_binary_decisions(add_min_set_objective(add_relative_targets(problem(
        sim_pu_raster, sim_features), object_not_exist))))
    Condition
      Error in `add_relative_targets()`:
      i In argument to `targets`.
      Caused by error:
      ! object 'object_not_exist' not found

---

    Code
      print(add_binary_decisions(add_min_set_objective(add_relative_targets(problem(
        sim_pu_raster, sim_features), 1 + "a"))))
    Condition
      Error in `add_relative_targets()`:
      i In argument to `targets`.
      Caused by `1 + "a"`:
      ! non-numeric argument to binary operator

---

    Code
      print(add_binary_decisions(add_min_set_objective(add_relative_targets(problem(
        object_not_exist, sim_features), 1 + "a"))))
    Condition
      Error in `problem()`:
      i In argument to `x`.
      Caused by error:
      ! object 'object_not_exist' not found

---

    Code
      print(add_relative_targets(add_binary_decisions(add_min_set_objective(problem(
        sim_pu_raster, sim_features))), 1 + "a"))
    Condition
      Error in `add_relative_targets()`:
      i In argument to `targets`.
      Caused by `1 + "a"`:
      ! non-numeric argument to binary operator

---

    Code
      print(add_relative_targets(add_binary_decisions(add_min_set_objective(problem(
        sim_pu_raster, sim_features))), object_not_exist))
    Condition
      Error in `add_relative_targets()`:
      i In argument to `targets`.
      Caused by error:
      ! object 'object_not_exist' not found

