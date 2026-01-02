((name "_compat")
 (purpose "Backward compatibility shims (temporary)")
 (description "During the migration from flat core/ structure to
subdirectories, this module provides load path redirects so existing
code continues to work. This directory will be removed once all
code is updated to use new paths.")
 (modules
  ((load-shims.ss "Mapping from old paths to new paths")))
 (temporary #t)
 (migration-status "in-progress"))
