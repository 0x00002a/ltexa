# Changelog for ltexa

## Unreleased changes

## 0.5.0 (dev) (branch: v2)

### Fixes

- Changed the parsing code to go file by file, main fixes:
    - File start/end detection is now _much_ better
    - Rerun detection is much better and shown in the code structure. Reruns should no longer cause some messages to be lost
    - Errors immediately after file starts should no longer get missed
- Fixed some issues with the parsing of `! <error>`, should no longer cause later messages to be missed
  (although line unwrapping may still cause issues, I'm not sure this can be fixed)

### New features

- All current messages will be shown on a parse error occurring, rather than simply printing the error and stopping
- Parse errors are now formatted in line with the rest of the output and show up under the `LTeXa` provider
