# fallback re-throws when it cannot handle the archive

    Code
      .extract_unzip_fallback("foo.tar.gz", tempdir(), NULL, simpleError("boom"))
    Condition
      Error:
      ! boom

