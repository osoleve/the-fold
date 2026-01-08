;;; shell/io/README.sexp — IO and Data Format Modules
;;;
;;; File system operations and data serialization.

((name . io)
 (purpose . "File system capabilities and data format handling")
 (tier-access . shell)
 (purity . impure)

 (description . "
IO operations and data format serialization for the Shell layer.
Provides capability-gated filesystem access and JSON handling.

Key design: Filesystem access requires capability tokens that
only the Shell can mint. Core code cannot directly access files.
")

 (modules
  ((file . fs.ss)
   (purpose . "Filesystem capability layer with content-addressed storage")
   (api . (mint-fs-capability fs-store! fs-fetch fs-stored?
           fs-pin! fs-sync! fs-object-count fs-all-hashes
           fs-write-head! fs-read-head
           path-parent path-basename path-stem
           string-prefix? string-suffix? string-rindex)))

  ((file . json.ss)
   (purpose . "JSON parsing and serialization")
   (api . (parse-json-string json->string json-escape))))

 (dependencies
  (internal . (core/base/prelude.ss)))

 (usage . "
Load filesystem capability:
  (load \"shell/io/fs.ss\")
  (define fs (mint-fs-capability \".store\"))
  (fs-store! fs block)
  (fs-fetch fs hash)

Load JSON utilities:
  (load \"shell/io/json.ss\")
  (parse-json-string \"{\\\"key\\\": \\\"value\\\"}\")
  (json->string '((key . \"value\")))
")

 (see-also . (
   "shell/storage/README.sexp"
   "core/blocks/README.sexp")))
