# Immutable Towers save exporter

This isolated tool reads the existing Haskell saves through their current decoders and writes a versioned JSON transfer package. It never changes or removes source files.

From the repository root, run `tools\save-exporter\export-saves.bat`. Optional PowerShell parameters are `-InputRoot` and `-OutputPath`.

The output is written to `.tmp`, decoded again for validation, then promoted. An existing output is preserved as `.bak`.

Run `export-catalog.bat` to regenerate `godot/data/domain-catalog-v1.json` directly from the current Haskell tower, enemy, map, mode, and wave definitions.
