# The Fold Operations Runbook

**Server:** debian-8gb-ash-1
**Location:** /home/oso/the-fold
**Repository:** git@github.com:osoleve/the-fold

---

## Quick Reference

### Service Management

```bash
# Check daemon status
systemctl --user status the-fold-daemon.service

# Restart daemon
systemctl --user restart the-fold-daemon.service

# View daemon logs (live)
journalctl --user -u the-fold-daemon.service -f

# View daemon logs (recent)
journalctl --user -u the-fold-daemon.service --since "1 hour ago"

# Check all timers
systemctl --user list-timers 'the-fold*'

# Manual health check
./ops/scripts/health-check.sh

# Manual backup
./ops/scripts/backup-store.sh
```

### Emergency Contacts

- **Primary:** Andy (Outsider/Progenitor)
- **System Owner:** oso@debian-8gb-ash-1

### Service Ports

- **2222** — SSH (external access)
- **24444** — MCP server (localhost only, used by Claude Code integrations)

---

## Common Procedures

### Installing/Updating Services

```bash
cd /home/oso/the-fold

# Install all systemd services
./ops/scripts/install-services.sh

# Dry-run to see what would happen
./ops/scripts/install-services.sh --dry-run
```

### Checking System Health

```bash
# Quick health check
./ops/scripts/health-check.sh

# Check resource usage
free -h                    # Memory
df -h /home/oso           # Disk
du -sh .store .fold-repl  # Runtime dirs
ps aux | grep scheme      # Daemon process
```

### Manual Testing

```bash
# Test REPL is responsive
./fold-agent.py "(+ 1 2)"

# Should output JSON with result: "3"

# Run test suite
scheme --script test-all.ss

# Run core tests only
scheme --script core/run-tests.ss
```

### Logs and Monitoring

```bash
# Daemon logs
cat .fold-repl/daemon.log
journalctl --user -u the-fold-daemon.service --since today

# Test logs
cat .fold-repl/test-all.log

# Backup logs
cat .fold-repl/backup.log

# Health check logs
cat .fold-repl/healthcheck.log

# All logs
ls -lh .fold-repl/*.log
```

---

## Failure Scenarios

### Scenario 1: Daemon Won't Start

**Symptoms:**
- `systemctl --user status the-fold-daemon` shows "failed" or "inactive (dead)"
- `.fold-repl/ready` file doesn't exist

**Diagnosis:**
```bash
# Check logs
journalctl --user -u the-fold-daemon.service -n 50

# Check daemon log
cat .fold-repl/daemon.log

# Try starting manually
./daemon.sh fg
```

**Common Causes:**
1. Chez Scheme not installed
2. Syntax error in start-daemon-mcp.ss
3. Port 24444 already in use
4. Corrupt .fold-repl directory

**Resolution:**
```bash
# 1. Verify Scheme installed
scheme --version

# 2. Test daemon script directly
./daemon.sh fg  # Run in foreground to see errors

# 3. Check port usage
ss -tulpn | grep 24444

# 4. Reset REPL directory
./daemon.sh stop
rm -rf .fold-repl
mkdir .fold-repl
./daemon.sh start
```

### Scenario 2: Daemon Running But Not Responding

**Symptoms:**
- `.fold-repl/ready` exists
- Process is running
- Requests timeout or no responses

**Diagnosis:**
```bash
# Check if process is actually running
ps aux | grep "scheme.*daemon"

# Send test request
./fold-agent.py "(+ 1 1)"

# Check ready file freshness
stat .fold-repl/ready
```

**Common Causes:**
1. Daemon deadlocked
2. Full disk (can't write responses)
3. Corrupted session state

**Resolution:**
```bash
# 1. Check disk space
df -h /home/oso

# 2. Restart daemon
systemctl --user restart the-fold-daemon.service

# 3. Clear sessions
rm -rf .fold-sessions/*

# 4. If persistent, inspect process
gdb -p $(pgrep -f "scheme.*daemon")
# Then: (gdb) bt  # backtrace
```

### Scenario 3: High Memory Usage

**Symptoms:**
- Daemon using > 1GB RAM
- System becoming sluggish
- Health checks warning about memory

**Diagnosis:**
```bash
# Check daemon memory
ps aux | grep "scheme.*daemon"

# Check total system memory
free -h

# Count active sessions
ls .fold-sessions | wc -l
```

**Resolution:**
```bash
# 1. Check for runaway sessions
ls -lh .fold-sessions/

# 2. Clean old sessions
./ops/scripts/cleanup-fold.sh

# 3. Restart daemon (clears memory)
systemctl --user restart the-fold-daemon.service

# 4. Add memory limit (if recurring)
# Edit: ops/systemd/user/the-fold-daemon.service
# Add: MemoryMax=1G
```

### Scenario 4: Disk Space Low

**Symptoms:**
- Health checks show disk > 90%
- Backup or test jobs failing
- Can't write new blocks to .store

**Diagnosis:**
```bash
# Check overall disk usage
df -h /home/oso

# Check what's using space
du -sh /home/oso/* | sort -h

# Check Fold directories
du -sh .store .fold-repl .fold-sessions
```

**Resolution:**
```bash
# 1. Clean old logs
./ops/scripts/cleanup-fold.sh

# 2. Manually rotate logs
cat .fold-repl/daemon.log > /dev/null
: > .fold-repl/daemon.log

# 3. Remove old sessions
find .fold-sessions -mtime +30 -delete

# 4. Check .store growth
# If excessive, may need garbage collection
ls -lh .store/objects | wc -l
```

### Scenario 5: Tests Failing

**Symptoms:**
- Nightly test timer shows failures
- Test log shows errors

**Diagnosis:**
```bash
# Check test log
cat .fold-repl/test-all.log

# Run tests manually
scheme --script test-all.ss

# Run specific failing test
scheme --script core/test-<module>.ss
```

**Resolution:**
1. Review test output for specific failure
2. Check if code changes broke tests
3. Verify test environment (Scheme version, dependencies)
4. If persistent, git bisect to find breaking commit

### Scenario 6: Backup Failing

**Symptoms:**
- Backup timer shows failures
- Backup log shows errors
- .store not being committed

**Diagnosis:**
```bash
# Check backup log
cat .fold-repl/backup.log

# Run backup manually
./ops/scripts/backup-store.sh

# Check git status
git status .store/
```

**Resolution:**
```bash
# 1. Check git config
git config user.name
git config user.email

# 2. Check remote
git remote -v

# 3. Manually commit
git add .store/
git commit -m "Manual backup"

# 4. Check permissions
ls -la .store/
```

---

## Maintenance Procedures

### Weekly Checklist

1. Review logs for errors
   ```bash
   grep -i error .fold-repl/*.log
   ```

2. Check disk/memory trends
   ```bash
   df -h /home/oso
   free -h
   ```

3. Verify backups running
   ```bash
   git log --oneline --grep="Backup" --since="7 days ago"
   ```

4. Review test results
   ```bash
   tail -100 .fold-repl/test-all.log
   ```

### Monthly Checklist

1. Update system packages
   ```bash
   sudo apt update && sudo apt upgrade
   ```

2. Review and rotate logs manually if needed
   ```bash
   logrotate -f ~/.config/logrotate/the-fold.conf
   ```

3. Audit .store size and growth
   ```bash
   du -sh .store
   git log --stat .store/ --since="30 days ago"
   ```

4. Review security: open ports, SSH keys
   ```bash
   ss -tulpn
   cat ~/.ssh/authorized_keys
   ```

### Deployment Updates

When deploying code changes:

```bash
# 1. Pull latest
git fetch origin
git log HEAD..origin/main  # Review changes

# 2. Run tests locally first
scheme --script test-all.ss

# 3. If tests pass, merge and restart
git merge origin/main
systemctl --user restart the-fold-daemon.service

# 4. Verify health
./ops/scripts/health-check.sh

# 5. Monitor logs
journalctl --user -u the-fold-daemon.service -f
```

---

## Disaster Recovery

### Full System Recovery

If the server is lost or corrupted:

1. **Provision new server**
   - Debian-based, 8GB+ RAM
   - Install git, curl, build-essential

2. **Clone repository**
   ```bash
   git clone git@github.com:osoleve/the-fold ~/the-fold
   cd ~/the-fold
   ```

3. **Install Chez Scheme**
   ```bash
   sudo apt install chezscheme
   ./daemon.sh start
   ```

4. **Restore .store from backup**
   ```bash
   # .store is in git, so just check out
   git checkout main -- .store/
   ```

5. **Install services**
   ```bash
   ./ops/scripts/install-services.sh
   ```

6. **Verify health**
   ```bash
   ./ops/scripts/health-check.sh
   ```

### Rollback Procedure

If new code causes issues:

```bash
# 1. Stop daemon
systemctl --user stop the-fold-daemon.service

# 2. Revert to last known good commit
git log --oneline -10  # Find good commit
git reset --hard <commit-hash>

# 3. Restart daemon
systemctl --user start the-fold-daemon.service

# 4. Verify
./ops/scripts/health-check.sh
```

---

## Monitoring and Alerts

### Current Setup

- **Health checks:** Every 5 minutes
- **Backups:** Daily at 2am
- **Tests:** Nightly at 3am
- **Cleanup:** Weekly Sunday 4:30am

### Alert Configuration

To enable email/webhook alerts:

```bash
# Create config file
cat > ~/.fold-env <<EOF
FOLD_ALERT_EMAIL="your-email@example.com"
# FOLD_ALERT_WEBHOOK="https://hooks.slack.com/services/..."
EOF

# Test alert
./ops/scripts/alert.sh "Test Alert" "This is a test"
```

### Metrics to Track

Review these weekly:

1. Daemon uptime percentage
2. .store size growth rate
3. Memory usage trends
4. Test pass rate
5. Backup success rate

---

## Security

### SSH Access

- Port: 2222 (non-standard for security)
- Key-based auth only (no passwords)
- Managed by system admin

### Network Exposure

- MCP server (24444): localhost only
- REPL daemon: No network exposure (file-based IPC)

### File Permissions

```bash
# Verify permissions
ls -la ~/.ssh/
ls -la ~/the-fold/.store/
ls -la ~/the-fold/.fold-repl/

# Should be readable only by user oso
```

---

## Additional Resources

- **Main Documentation:** `/home/oso/the-fold/claude.md`
- **Architecture:** `/home/oso/the-fold/CLAUDE.md`
- **Assessment:** `/home/oso/the-fold/ops/ASSESSMENT.md`
- **Quickstart:** `/home/oso/the-fold/QUICKSTART-COMMANDS.md`
