# Method coverage

The method catalogue is stored in `data/methods.csv`.

Implementation labels:

- **Method-aware adapter:** the app has method-aware R code generation. Execution still depends on the relevant package and valid data/specification.
- **Diagnostic scaffold:** the app generates a diagnostic workflow around the current model specification.
- **Generic R scaffold:** the method is indexed, searchable and linked to a package/function, but complete package-specific arguments must be supplied in Advanced mode.

Adding a method requires one registry row and, when a specialised visual form is desired, an adapter in `R/code_generator.R`.
