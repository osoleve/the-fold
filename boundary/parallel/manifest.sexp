(module parallel
  (version "0.1.0")
  (layer boundary)
  (purity partial)
  (stability experimental)
  (description "Work-stealing parallel scheduler with Chase-Lev deques")
  (deps (data))
  (exports
    (scheduler spawn await parallel-invoke parallel-map parallel-for-each
               pool-stats pool-shutdown-global!)
    (task make-task task? task-done? task-result make-future future?
          future-done? future-result)
    (thread-pool make-thread-pool pool-start! pool-shutdown!
                 cpu-count default-worker-count)))
