# The Fold Operations Directory

Infrastructure, deployment, and operational tooling for The Fold.

---

## Quick Start

### Initial Setup

```bash
# Install all systemd services
./scripts/install-services.sh

# Test health check
./scripts/health-check.sh

# Run backup manually
./scripts/backup-store.sh
```

### Daily Operations

```bash
# Check system status
systemctl --user status the-fold-daemon.service

# View logs
journalctl --user -u the-fold-daemon.service -f

# Manual health check
./scripts/health-check.sh
```

---

## Directory Structure

```
ops/
├── README.md              # This file
├── ASSESSMENT.md          # Infrastructure assessment and recommendations
├── RUNBOOK.md             # Operational procedures and troubleshooting
├── scripts/               # Operational scripts
│   ├── backup-store.sh         # Backup .store to git
│   ├── cleanup-fold.sh         # Clean old sessions/requests
│   ├── health-check.sh         # Daemon health verification
│   ├── install-services.sh     # Install systemd services
│   ├── alert.sh                # Send alerts (email/webhook)
│   ├── run-daemon-mcp.sh       # Start daemon (used by systemd)
│   └── run-tests.sh            # Run test suite (used by systemd)
├── systemd/user/          # Systemd service definitions
│   ├── the-fold-daemon.service        # Main REPL daemon
│   ├── the-fold-backup.service        # Backup job
│   ├── the-fold-backup.timer          # Backup schedule (daily 2am)
│   ├── the-fold-cleanup.service       # Cleanup job
│   ├── the-fold-cleanup.timer         # Cleanup schedule (weekly Sun 4:30am)
│   ├── the-fold-healthcheck.service   # Health check job
│   ├── the-fold-healthcheck.timer     # Health check schedule (every 5min)
│   ├── the-fold-tests.service         # Test suite
│   └── the-fold-tests.timer           # Test schedule (nightly 3am)
└── logrotate/             # Log rotation configuration
    └── the-fold.conf      # Rotate daemon, test, backup, health logs
```

---

## Scripts Reference

### backup-store.sh

Backs up `.store/` (content-addressed blocks) to git.

```bash
./scripts/backup-store.sh           # Interactive mode
./scripts/backup-store.sh --auto    # Automated mode (for cron/systemd)
```

**Scheduled:** Daily at 2am (via systemd timer)

**Why git?** Homoiconic philosophy — blocks are immutable, git is perfect for this.

### health-check.sh

Comprehensive health check of The Fold daemon.

**Checks:**
- Daemon process running
- REPL responsive (sends test expression)
- Memory usage within limits
- Disk usage within limits
- .fold-repl/ready file fresh

```bash
./scripts/health-check.sh
```

**Exit codes:**
- 0 = Healthy
- 1 = Unhealthy (critical)
- 2 = Warning (degraded)

**Scheduled:** Every 5 minutes (via systemd timer)

### cleanup-fold.sh

Cleans old runtime files.

**Cleans:**
- `.fold-repl/requests/*.ss` (older than 7 days)
- `.fold-repl/responses/*.txt` (older than 7 days)
- `.fold-sessions/*.session` (older than 30 days)

```bash
./scripts/cleanup-fold.sh                    # Normal mode
FOLD_CLEANUP_DRY_RUN=1 ./scripts/cleanup-fold.sh  # Dry-run
```

**Scheduled:** Weekly Sunday 4:30am (via systemd timer)

### install-services.sh

Installs all systemd user services and enables timers.

```bash
./scripts/install-services.sh           # Install and enable
./scripts/install-services.sh --dry-run # Preview only
```

**Installs to:** `~/.config/systemd/user/`

### alert.sh

Sends alerts via email or webhook.

```bash
./scripts/alert.sh "Subject" "Message body"
```

**Configuration:** Create `~/.fold-env`:
```bash
FOLD_ALERT_EMAIL="your-email@example.com"
# FOLD_ALERT_WEBHOOK="https://hooks.slack.com/..."
```

**Used by:**
- Health check failures
- Test failures (future)
- Manual alerts

### run-daemon-mcp.sh

Wrapper to start the daemon (used by systemd). Finds Chez Scheme and starts `start-daemon-mcp.ss`.

### run-tests.sh

Wrapper to run test suite (used by systemd). Runs `test-all.ss`.

---

## Systemd Services

### Main Services

**the-fold-daemon.service**
- Runs the REPL daemon with MCP support
- Auto-restarts on failure (RestartSec=2)
- Logs to `.fold-repl/daemon.log`

**Status:**
```bash
systemctl --user status the-fold-daemon.service
```

**Restart:**
```bash
systemctl --user restart the-fold-daemon.service
```

### Scheduled Jobs (Timers)

**the-fold-backup.timer** (Daily 2am)
- Backs up .store to git
- Randomized delay: 30min

**the-fold-tests.timer** (Nightly 3am)
- Runs full test suite
- Randomized delay: 30min
- Logs to `.fold-repl/test-all.log`

**the-fold-cleanup.timer** (Weekly Sunday 4:30am)
- Cleans old requests/responses/sessions
- Randomized delay: 1hr

**the-fold-healthcheck.timer** (Every 5 minutes)
- Verifies daemon health
- Sends alerts on failure
- Logs to `.fold-repl/healthcheck.log`

**View timer status:**
```bash
systemctl --user list-timers 'the-fold*'
```

**View next scheduled run:**
```bash
systemctl --user list-timers the-fold-backup.timer
```

---

## Monitoring

### Current Monitoring Stack

1. **Health Checks** — Every 5 minutes
   - Process alive
   - REPL responsive
   - Resource usage

2. **Logs** — Centralized in `.fold-repl/`
   - daemon.log
   - test-all.log
   - backup.log
   - healthcheck.log

3. **Systemd** — Built-in service monitoring
   - Auto-restart on crash
   - Journald logging
   - Timer scheduling

### Viewing Logs

```bash
# Live daemon logs (systemd)
journalctl --user -u the-fold-daemon.service -f

# Live daemon logs (file)
tail -f .fold-repl/daemon.log

# Recent health checks
tail -50 .fold-repl/healthcheck.log

# Recent backups
tail -20 .fold-repl/backup.log

# Test results
cat .fold-repl/test-all.log
```

### Setting Up Alerts

Create `~/.fold-env`:

```bash
# Email alerts
FOLD_ALERT_EMAIL="your-email@example.com"

# Webhook alerts (Discord, Slack, etc.)
FOLD_ALERT_WEBHOOK="https://your-webhook-url"
```

Test:
```bash
./scripts/alert.sh "Test" "This is a test alert"
```

---

## Common Tasks

### Check System Health

```bash
# Quick health check
./scripts/health-check.sh

# Service status
systemctl --user status the-fold-daemon.service

# Resource usage
free -h                  # Memory
df -h /home/oso         # Disk
du -sh .store .fold-repl # Runtime dirs
```

### Restart Daemon

```bash
systemctl --user restart the-fold-daemon.service

# Verify it started
./scripts/health-check.sh
```

### View Recent Activity

```bash
# Daemon logs (last hour)
journalctl --user -u the-fold-daemon.service --since "1 hour ago"

# All recent logs
tail -50 .fold-repl/*.log

# Check timers
systemctl --user list-timers 'the-fold*'
```

### Manual Backup

```bash
./scripts/backup-store.sh

# Dry-run first (recommended)
./scripts/backup-store.sh --dry-run
```

### Clean Up Runtime Files

```bash
# Preview what would be deleted
FOLD_CLEANUP_DRY_RUN=1 ./scripts/cleanup-fold.sh

# Actually delete
./scripts/cleanup-fold.sh
```

### Update Services

After modifying systemd files:

```bash
# Reinstall services
./scripts/install-services.sh

# Or manually:
systemctl --user daemon-reload
systemctl --user restart the-fold-daemon.service
```

---

## Troubleshooting

See [RUNBOOK.md](RUNBOOK.md) for detailed troubleshooting procedures.

### Quick Diagnostics

```bash
# Is daemon running?
systemctl --user is-active the-fold-daemon.service

# Can it respond?
/home/oso/the-fold/fold-agent.py "(+ 1 1)"

# Any errors in logs?
journalctl --user -u the-fold-daemon.service -p err

# Disk space?
df -h /home/oso

# Memory usage?
free -h
```

---

## Future Enhancements

See [ASSESSMENT.md](ASSESSMENT.md) for comprehensive recommendations.

### Planned Improvements

- [ ] Metrics collection (track trends)
- [ ] Grafana dashboards
- [ ] Automated deployment (CI/CD)
- [ ] Session resource quotas
- [ ] Log analysis/alerting
- [ ] Performance profiling

### Metrics to Track

Once metrics collection is implemented:

- .store growth rate (MB/day)
- Active sessions count
- Request rate (requests/min)
- Memory/CPU trends
- Test pass rate over time
- Daemon uptime percentage

---

## Security Notes

### Network Exposure

- **SSH:** Port 2222 (external)
- **MCP Server:** Port 24444 (localhost only)
- **REPL:** No network exposure (file-based IPC)

### Permissions

All files should be owned by user `oso`:

```bash
# Verify
ls -la /home/oso/the-fold
ls -la ~/.config/systemd/user/the-fold-*
```

### Secrets

- No secrets in repository
- SSH keys in `~/.ssh/`
- Alert credentials in `~/.fold-env` (gitignored)

---

## Resources

- **Main Docs:** `/home/oso/the-fold/CLAUDE.md`
- **Operations:** `/home/oso/the-fold/ops/RUNBOOK.md`
- **Assessment:** `/home/oso/the-fold/ops/ASSESSMENT.md`
- **Repository:** `git@github.com:osoleve/the-fold`
