# input_manifest_record() rejects missing required fields

    Code
      input_manifest_record(id = "x", name = "X", source = list(type = "t"),
      local_path = "p")
    Condition
      Error:
      ! input_manifest_record() validation failed:
        field `source` is missing required subfield `url`

# input_manifest_record() rejects source without type or url

    Code
      input_manifest_record(id = "x", name = "X", local_path = "p", source = list(
        type = "t"))
    Condition
      Error:
      ! input_manifest_record() validation failed:
        field `source` is missing required subfield `url`

# register_input() rejects invalid records

    Code
      register_input(bad, path)
    Condition
      Error:
      ! record `x` failed validation:
        required field `source` is missing or NA
        required field `retrieved_at` is missing or NA
        required field `local_path` is missing or NA

