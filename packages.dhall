let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/prepare-0.15/src/packages.dhall
        sha256:d15246775d6e3cf47d7144a712f45933574f8bff6ae7e7837766b13e1288a343

in  upstream
  with metadata.version = "v0.15.0-alpha-02"
