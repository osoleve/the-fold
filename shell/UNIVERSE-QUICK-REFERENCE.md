# Universe Serialization - Quick Reference

## Most Common Commands

### Dump Entire Universe (Pretty)
```bash
scheme --script shell/universe-dump.ss --pretty
```
Output: `universe-dump.sexp` (pretty-printed)

### Dump to Custom File
```bash
scheme --script shell/universe-dump.ss --output my-dump.sexp --pretty
```

### Dump Forum Only
```bash
scheme --script shell/universe-dump.ss --filter forum --pretty
```

### Dump Multiple Directories
```bash
scheme --script shell/universe-dump.ss --filter forum,scripture --pretty
```

### Show Help
```bash
scheme --script shell/universe-dump.ss --help
```

## Library Quick Reference

### Import
```scheme
(import (shell universe-serialize))
```

### Scan Files
```scheme
;; All .sexp files
(scan-sexp-files ".")

;; Filtered by directory
(scan-sexp-files-filtered "." '("forum" "scripture"))
```

### Serialize
```scheme
;; Full universe
(define universe (serialize-universe "."))

;; Filtered
(define forum (serialize-universe-filtered "." '("forum")))
```

### Write Output
```scheme
;; Pretty printed
(write-universe-pretty universe "output.sexp")

;; Compact
(write-universe universe "output.sexp" #f)
```

### Read Single File
```scheme
(define contents (read-sexp-file "forum/poetry/0001-genesis.sexp"))
```

## Common Use Cases

### Daily Backup
```bash
scheme --script shell/universe-dump.ss \
  --pretty \
  --output "backups/universe-$(date +%Y%m%d).sexp"
```

### Archive Forum Posts
```bash
scheme --script shell/universe-dump.ss \
  --filter forum \
  --pretty \
  --output "archives/forum-$(date +%Y%m%d).sexp"
```

### Export Scripture
```bash
scheme --script shell/universe-dump.ss \
  --filter scripture \
  --pretty \
  --output docs/scripture-export.sexp
```

## Output Format
```scheme
((files
  (("path/to/file.sexp" . <contents>)
   ("another/file.sexp" . <contents>)
   ...)))
```

## Testing
```bash
scheme --script shell/universe-serialize-test.ss
```

## Example Code
```bash
scheme --script shell/universe-example.ss
```

## Files Location
- Library: `shell/universe-serialize.ss`
- Tests: `shell/universe-serialize-test.ss`
- CLI: `shell/universe-dump.ss`
- Docs: `shell/UNIVERSE-SERIALIZE-README.md`
- Summary: `shell/UNIVERSE-SERIALIZE-SUMMARY.md`
- Example: `shell/universe-example.ss`

## Current Universe Size
- Total .sexp files: 49
- Forum: 47 files
- Scripture: 1 file
- Playpen: 1 file
