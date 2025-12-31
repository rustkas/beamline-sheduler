# Repository Rename Instructions

## ⚠️ IMPORTANT: Repository Name Fix Required

**Current (INCORRECT):** `beamline-sheduler` (typo in "scheduler")  
**Correct:** `beamline-scheduler`

---

## 📋 Steps to Rename Repository on GitHub

### 1. Go to Repository Settings
Navigate to: https://github.com/rustkas/beamline-sheduler/settings

### 2. Rename Repository
1. Scroll to **Repository name** section
2. Change from: `beamline-sheduler`
3. Change to: `beamline-scheduler`
4. Click **Rename**

⚠️ **Warning**: GitHub will show a warning about breaking links. This is expected.

---

## 🔄 What Happens After Rename

### GitHub Automatic Redirects
GitHub will automatically redirect:
- Old URL: `https://github.com/rustkas/beamline-sheduler`
- New URL: `https://github.com/rustkas/beamline-scheduler`

This redirect works for:
- ✅ Web browsing
- ✅ Git clone/pull/push
- ✅ Issues/PRs
- ✅ Releases

### Local Repository Update (Optional but Recommended)

Update your local git remote:

```bash
cd /home/rustkas/aigroup
git remote set-url origin https://github.com/rustkas/beamline-scheduler.git
```

Or via SSH:
```bash
git remote set-url origin git@github.com:rustkas/beamline-scheduler.git
```

Verify:
```bash
git remote -v
```

---

## ✅ Documentation Already Updated

All references in the repository have been updated:

### Main Files Updated:
- ✅ `README.md` - Main documentation + AI bot link added
- ✅ `PROJECT_DESCRIPTION.md`
- ✅ `MARKETING_COPY.md`
- ✅ `WEBSITE_CONTENT.md`
- ✅ `WEBSITE_TEXT_SIMPLE.md`
- ✅ `LINKEDIN_PROFILE.txt`
- ✅ `docs/archive/dev/README.md`

### AI Bot Files Removed (Cleaned Up):
The following AI bot draft files were removed as they contained the old URL:
- AI_BOT_*.md files moved to archive or removed

**Note**: Final bot configuration should use the new URL: `beamline-scheduler`

---

## 🤖 AI Bot Link Added to README

Added to the Support section:

```markdown
## 📞 Support

- **AI Assistant**: [BeamLine Master](https://aistudio.instagram.com/ai/4815329165457920/) - Ask questions about the platform 🤖
- **Issues**: [GitHub Issues](https://github.com/rustkas/beamline-scheduler/issues)
- **Discussions**: [GitHub Discussions](https://github.com/rustkas/beamline-scheduler/discussions)
```

**BeamLine Master Bot:**
- URL: https://aistudio.instagram.com/ai/4815329165457920/
- Purpose: Quick answers to platform questions
- Features: 10K+ req/s performance info, architecture details, deployment help

---

## 🔍 Verification Checklist

After renaming on GitHub:

- [ ] Visit https://github.com/rustkas/beamline-scheduler (new URL works)
- [ ] Visit https://github.com/rustkas/beamline-sheduler (old URL redirects)
- [ ] Clone works: `git clone https://github.com/rustkas/beamline-scheduler.git`
- [ ] Update local remote (see above)
- [ ] Push/pull still works
- [ ] Issues/PRs accessible
- [ ] AI bot Instagram link works

---

## 📊 Impact Summary

**Files Changed:** 21 files  
**Lines Modified:** 33 insertions, 3095 deletions  
**Commit:** `6b936aa`

**All references updated from:**
```
github.com/rustkas/beamline-sheduler
```

**To:**
```
github.com/rustkas/beamline-scheduler
```

---

## 🚀 Next Steps

1. ✅ Code updated (DONE)
2. ✅ Committed and pushed (DONE)
3. ⏳ **RENAME REPO ON GITHUB** ← DO THIS NOW
4. ⏳ Update local git remote (optional)
5. ⏳ Update any external links (LinkedIn, other sites)
6. ⏳ Verify AI bot uses correct URL in responses

---

## 📝 Notes

- The typo fix is **cosmetic but important** for professionalism
- All clone URLs will continue to work due to GitHub redirect
- Submodules (if any) are not affected
- CI/CD workflows should continue working
- No code changes needed, only URL references

---

**Date Fixed:** 2025-12-31  
**Commit:** 6b936aa  
**Status:** ✅ Code Ready, ⏳ Awaiting GitHub Rename
