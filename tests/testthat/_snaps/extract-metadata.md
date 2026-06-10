# extract_metadata() errors on unknown classes

    Code
      extract_metadata(123L)
    Condition
      Error:
      ! No `extract_metadata()` method for class `integer`.
        Supported classes: bcdc_record, character (URL).

# metadata_bcdata() errors when bcdata is not installed

    Code
      metadata_bcdata("anything")
    Condition
      Error:
      ! Package `bcdata` is required by `metadata_bcdata()`. Install it with `install.packages('bcdata')`.

