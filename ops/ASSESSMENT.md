# The Fold Operations Assessment

**Date:** 2025-12-27
**Server:** debian-8gb-ash-1 (8GB RAM, 75GB disk)
**Status:** First production deployment of The Fold

---

## Current State Summary

### ✅ What's Working Well

1. **Systemd Services** — Properly configured and running
   - `the-fold-daemon.service` — Running (41min uptime, 40.6M memory)
   - `the-fold-tests.timer` — Scheduled nightly at 3am
   - `the-fold-cleanup.timer` — Scheduled weekly Sunday 4:30am
   - All services use user systemd (no root required)

2. **Resource Usage** — Healthy and sustainable
   - Disk: 8.4GB / 75GB (12% used)
   - Memory: 2.8GB / 7.6GB (36% used)
   - Load: 0.17 (very light)
   - Runtime dirs: 792KB (.store), 80KB (.fold-repl), 16KB (.fold-sessions)

3. **Service Reliability**
   - Daemon auto-restarts on failure (RestartSec=2)
   - MCP server running on localhost:24444
   - Multi-session support working

4. **Scheduled Maintenance**
   - Nightly test runs with randomized delays
   - Weekly cleanup of old requests/responses (7-day retention)
   - Session cleanup (30-day retention)

5. **Development Velocity**
   - 215 commits in last 7 days
   - Active development across multiple subsystems

### ⚠️ Gaps and Risks

#### Critical (Fix Immediately)

1. **No Monitoring/Alerting**
   - Daemon crashes are silent (only systemd restart)
   - Test failures go unnoticed
   - Disk/memory growth not tracked
   - No uptime monitoring

2. **No Backup Strategy**
   - `.store/` contains all content-addressed blocks (irreplaceable)
   - No automated backups configured
   - 792KB currently, but will grow
   - Single point of failure

3. **Logrotate Not Installed**
   - Config file exists but contains placeholder: `__FOLD_ROOT__`
   - Not symlinked to `/etc/logrotate.d/`
   - Logs will grow unbounded

4. **Test Results Not Monitored**
   - Tests run nightly but failures are silent
   - No email/notification on test failure
   - Log file grows without analysis

#### High Priority

5. **No Health Checks**
   - No endpoint to verify daemon is responsive
   - No automated verification that REPL is accepting requests
   - Could be running but deadlocked

6. **No Metrics Collection**
   - Can't track trends: disk growth, memory creep, request rates
   - No performance baselines
   - No capacity planning data

7. **No Deployment Automation**
   - Manual systemd service installation
   - No CI/CD pipeline
   - No rollback mechanism

8. **Network Security Unclear**
   - Port 24444 (MCP server) listening on localhost only (good)
   - Port 2222 open to 0.0.0.0 (SSH?) — needs documentation
   - No firewall rules documented

#### Medium Priority

9. **No Automated Recovery**
   - Daemon restarts but no investigation of repeated failures
   - No circuit breaker for resource exhaustion
   - No rate limiting per session

10. **Incomplete Documentation**
    - No ops runbook
    - No incident response procedures
    - No disaster recovery plan

11. **Session Management Gaps**
    - No max sessions limit
    - No resource quotas per session
    - No detection of runaway sessions

12. **Cleanup Script Limitations**
    - Deletes files but doesn't verify daemon health
    - No cleanup of zombie sessions
    - Dry-run mode not tested automatically

---

## Recommended Enhancements

### Phase 1: Critical Infrastructure (Do First)

#### 1.1 Implement Backup Strategy

**Option A: Git-based (Simple, Homoiconic)**
```bash
# Daily backup of .store to git
ops/scripts/backup-store.sh:
  - git add .store/
  - git commit -m "Daily store backup $(date)"
  - git push backup-remote

# Add to systemd timer
ops/systemd/user/the-fold-backup.timer (daily)
```

**Option B: Rsync to Remote (Traditional)**
```bash
# Rsync to backup server
ops/scripts/backup-rsync.sh:
  - rsync -az .store/ backup-server:/backups/the-fold/store/
  - Keep 30 days of snapshots
```

**Recommendation:** Start with git-based (aligns with homoiconic philosophy), add rsync later for redundancy.

#### 1.2 Fix Logrotate Configuration

```bash
# Fix the config
ops/scripts/install-logrotate.sh:
  - Replace __FOLD_ROOT__ with /home/oso/the-fold
  - Symlink to ~/.config/logrotate/ (user-level)
  - Test with: logrotate -d the-fold.conf

# Or use systemd-based rotation
ops/systemd/user/the-fold-logrotate.timer (weekly)
```

#### 1.3 Add Health Check Script

```bash
ops/scripts/health-check.sh:
  - Check .fold-repl/ready exists and is fresh (<5min old)
  - Send test expression: "(+ 1 1)"
  - Verify response within 5 seconds
  - Check memory usage < 80%
  - Check disk usage < 90%
  - Exit non-zero on failure

ops/systemd/user/the-fold-healthcheck.timer:
  - Run every 5 minutes
  - Alert on 3 consecutive failures
```

#### 1.4 Add Monitoring/Alerting

**Minimal: Email Alerts**
```bash
ops/scripts/alert.sh:
  - Send email via mailx or curl (to mailgun/sendgrid)
  - Include: hostname, service, error, timestamp
  - Used by health-check, test-runner

# Requires:
  - Email credentials in ~/.fold-env
  - Modify systemd services to call alert.sh on failure
```

**Better: Prometheus + Grafana (Future)**
- Export metrics from daemon
- Scrape with node_exporter
- Alert on anomalies

### Phase 2: Operational Excellence

#### 2.1 Enhanced Test Monitoring

```bash
ops/scripts/run-tests.sh (enhanced):
  - Capture test output
  - Parse for PASS/FAIL counts
  - On failure:
    - Send alert with failed test names
    - Create GitHub issue automatically
  - On success:
    - Update status badge
    - Log metrics (duration, count)
```

#### 2.2 Metrics Collection

```bash
ops/scripts/collect-metrics.sh:
  - Run hourly
  - Collect:
    - .store size and object count
    - Active sessions count
    - Memory/CPU of daemon
    - Request/response queue depths
  - Append to .fold-repl/metrics.jsonl
  - Rotate monthly
```

#### 2.3 Session Resource Management

Add to `boundary/session-manager.ss`:
```scheme
; Per-session limits
(define *max-sessions* 100)
(define *max-memory-per-session* (* 50 1024 1024)) ; 50MB
(define *max-request-rate* 10) ; per minute

; Enforcement in daemon
```

#### 2.4 Automated Deployment

```bash
ops/scripts/install-services.sh:
  - Install all systemd services
  - Enable timers
  - Reload systemd
  - Start daemon
  - Verify health

ops/scripts/update-fold.sh:
  - git pull
  - Run tests
  - If pass: restart daemon
  - If fail: rollback and alert
```

### Phase 3: Production Hardening

#### 3.1 Disaster Recovery Documentation

```markdown
ops/RUNBOOK.md:
  - Emergency contacts
  - Recovery procedures
  - Backup restoration steps
  - Common failure modes
```

#### 3.2 Network Security Audit

```bash
ops/scripts/audit-security.sh:
  - Document all open ports
  - Verify firewall rules
  - Check SSH config (port 2222?)
  - Ensure MCP server localhost-only
```

#### 3.3 Resource Quotas

```bash
# Add to systemd services
[Service]
MemoryMax=2G
TasksMax=100
CPUQuota=200%  # 2 cores max
```

#### 3.4 Log Analysis

```bash
ops/scripts/analyze-logs.sh:
  - Parse daemon.log for errors
  - Detect patterns (repeated failures)
  - Generate weekly summary report
```

---

## Implementation Priority

### Week 1 (Immediate)
1. ✅ Backup strategy (git-based) — 2 hours
2. ✅ Fix logrotate config — 30 minutes
3. ✅ Basic health check script — 2 hours
4. ✅ Email alerting setup — 1 hour

### Week 2 (High Value)
5. Enhanced test monitoring — 3 hours
6. Metrics collection — 4 hours
7. Deployment automation — 4 hours

### Week 3 (Polish)
8. Session resource limits — 3 hours
9. Runbook documentation — 2 hours
10. Security audit — 2 hours

---

## Metrics to Track

### Service Health
- Daemon uptime percentage
- Restart count per day
- Health check success rate
- Average response time

### Resource Usage
- .store size growth rate (MB/day)
- Memory usage trend
- Disk usage trend
- Active sessions count

### Development
- Commits per day
- Test pass rate
- Failed test frequency
- Lines of code

### Operational
- Backup success rate
- Time to detect failures
- Time to recovery
- Alert noise ratio

---

## Quick Wins (Do Today)

1. **Install logrotate config** (15 min)
   ```bash
   sed 's|~/the-fold|/home/oso/the-fold|g' ops/logrotate/the-fold.conf > ~/.config/logrotate/the-fold.conf
   ```

2. **Add basic health check** (30 min)
   ```bash
   # Create ops/scripts/health-check.sh
   # Add to cron: */5 * * * *
   ```

3. **Document port usage** (10 min)
   ```bash
   echo "2222: SSH (external)" >> ops/PORTS.md
   echo "24444: MCP server (localhost)" >> ops/PORTS.md
   ```

4. **Set resource limits** (5 min)
   ```bash
   # Add to the-fold-daemon.service:
   MemoryMax=1G
   CPUQuota=100%
   ```

5. **Enable test failure alerts** (20 min)
   ```bash
   # Modify run-tests.sh to check exit code
   # Email on failure
   ```

---

## Long-Term Considerations

### Scaling
- When .store grows > 10GB: Add object pruning
- When sessions > 50: Add session prioritization
- When load > 1.0: Profile and optimize

### High Availability
- Not needed yet (single user, development)
- Consider when production users onboard
- Would need: Load balancer, replica daemons, shared .store

### Observability
- Current: Logs + systemd status
- Future: Prometheus + Grafana dashboards
- Eventual: Distributed tracing for requests

---

## Summary

**Current Grade: B-**
- Core services running reliably
- Good foundation with systemd
- Missing critical safety nets

**With Phase 1 Fixes: A-**
- Backups protect data
- Monitoring catches failures
- Alerts enable fast response

**Recommended Next Steps:**
1. Implement backup script (git-based)
2. Fix logrotate and install
3. Add health check with alerts
4. Document current state in ops/RUNBOOK.md

The infrastructure is solid for a development/first-deployment server. Adding monitoring, backups, and alerting will make it production-ready.
