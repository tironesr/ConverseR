# ConverseR Public Launch Roadmap & Release Strategy

This document serves as the operational guide and checklist for releasing **ConverseR** to the global R, data science, and academic research community.

---

## Executive Summary: Release Pipeline Overview

```
               [ Phase 1: GitHub & Public Beta ]
             Direct installation via pak / remotes
                               │
                               ▼
              [ Phase 2: R-Universe Distribution ]
           Pre-compiled macOS & Windows binaries (<1 min)
                               │
                               ▼
            [ Phase 3: Community Outreach & Media ]
          Posit Community, #rstats, Social Science Labs
                               │
                               ▼
               [ Phase 4: Formal CRAN Submission ]
           Official R package repository archive
```

---

## Phase 1: GitHub Public Beta Release (Immediate)

The package is already functional, feature-complete, and verified. These immediate steps prepare the repository for public consumption.

### 1.1 Complete Metadata in `DESCRIPTION`
Ensure the author details and repository links are complete in `DESCRIPTION`:
- Set your full author name and active email: `person("Tyrone", "...", email = "your.email@...", role = c("aut", "cre"))`.
- Add official project links:
  ```dcf
  URL: https://github.com/tironesr/ConverseR, https://tironesr.github.io/ConverseR/ConverseR-guide.html
  BugReports: https://github.com/tironesr/ConverseR/issues
  ```

### 1.2 GitHub Actions Automated CI/CD
Add a standard `R-CMD-check` workflow across operating systems (`windows-latest`, `macos-latest`, `ubuntu-latest`) so every commit is continuously tested.

Create `.github/workflows/R-CMD-check.yaml`:
```yaml
name: R-CMD-check

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  R-CMD-check:
    runs-on: ${{ matrix.config.os }}
    name: ${{ matrix.config.os }} (${{ matrix.config.r }})
    strategy:
      fail-fast: false
      matrix:
        config:
          - {os: macos-latest,   r: 'release'}
          - {os: windows-latest, r: 'release'}
          - {os: ubuntu-latest,  r: 'release'}
    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: ${{ matrix.config.r }}
          use-public-rspm: true
      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: any::rcmdcheck
          needs: check
      - uses: r-lib/actions/setup-pandoc@v2
      - uses: r-lib/actions/check-r-package@v2
```

### 1.3 Tag a GitHub Release (`v0.4.0`)
Create an official release on GitHub (`https://github.com/tironesr/ConverseR/releases/new`):
- Tag: `v0.4.0`
- Title: `ConverseR 0.4.0: Next-Gen Context-Aware AI Sidebar for RStudio & Positron`
- Attach a summary of key features:
  - Non-blocking Viewer Pane sidebar with live R console freedom.
  - Multi-turn context caching & incremental line-level document diffs.
  - Automatic AST-based environment inspection & survey variable/value labels.
  - Seamless OpenRouter model selection, reasoning control, and Zero Data Retention (ZDR).
  - Single-click YAML `SKILL.md` generator and `.zip` bundle importer.

---

## Phase 2: R-Universe Distribution (Zero-Friction Installs)

**Why R-Universe?**
Many R users (especially students and researchers) struggle with `remotes::install_github()` because their computers lack C/C++ build tools (Rtools on Windows, Xcode command line tools on macOS).
[R-Universe](https://r-universe.dev) (powered by Jeroen Ooms / rOpenSci) automatically builds **pre-compiled binary packages for Windows and macOS (Intel & Apple Silicon)** within 2 minutes of pushing to GitHub.

### Setup Instructions:
1. Create a GitHub repository named `tironesr.r-universe.dev` (or create a repository named `universe` under your account).
2. Add a `packages.json` file inside that repository:
   ```json
   [
     {
       "package": "ConverseR",
       "url": "https://github.com/tironesr/ConverseR"
     }
   ]
   ```
3. Enable R-Universe app on your GitHub account at `https://github.com/apps/r-universe/installations/new`.
4. Users can now install ConverseR with standard `install.packages()` with zero compilation needed:
   ```r
   # Enable R-Universe repository
   install.packages("ConverseR", repos = c("https://tironesr.r-universe.dev", "https://cloud.r-project.org"))
   ```

---

## Phase 3: Documentation, Media, & Public Outreach

To build traction, prospective users need to see ConverseR in action within 15 seconds.

### 3.1 Media Assets (Record 3 Short Animated GIFs / MP4s)
- **Clip 1: The Non-Blocking Advantage**: Showing you asking ConverseR a question in the Viewer Pane while simultaneously running code in the R Console (which other gadgets freeze).
- **Clip 2: Statistical & Survey Awareness**: Asking a question about an unmentioned SPSS/Qualtrics survey dataset or regression model and showing ConverseR instantly knowing question wordings, value labels, and 95% CIs.
- **Clip 3: Inline Diff & One-Click Insert**: Highlighting code in an R script, opening `converser_inline()`, previewing a side-by-side red/green diff, and clicking `Insert After`.

### 3.2 Public Launch Channels
1. **Posit Community Forum**:
   - Post under the *Package Development* or *RStudio IDE* showcase section: *"ConverseR: A non-blocking, context-cached LLM assistant for RStudio"*.
2. **Mastodon & LinkedIn (#rstats)**:
   - Tag `#rstats`, `#dataanalytics`, `#socialscience`.
   - Highlight privacy: OpenRouter Zero Data Retention (ZDR), local session hash caching, and no hardcoded API keys.
3. **Reddit r/rstats & r/dataisbeautiful**:
   - Share a short workflow walkthrough demonstrating how ConverseR accelerates empirical research without leaving RStudio.
4. **Academic & Lab Distribution**:
   - Share the `SKILL.md` capability with university lab PIs, demonstrating how they can bundle lab-specific coding conventions, helper packages, and datasets into a single `.zip` file for students.

---

## Phase 4: CRAN Submission Preparation

Submitting to CRAN establishes ConverseR as an official, permanent R package available via `install.packages("ConverseR")`.

### 4.1 CRAN Compliance Checklist
- [ ] **No Internet Access in Tests**: In all `testthat` tests making external network requests, guard with:
  ```r
  skip_if_offline()
  skip_if_not(nzchar(Sys.getenv("OPENROUTER_API_KEY")))
  ```
- [ ] **Clean `R CMD check --as-cran`**:
  Must return:
  ```
  0 errors ✔ | 0 warnings ✔ | 0 notes ✔
  ```
- [ ] **Check Remote Systems via `rhub` & Win-Builder**:
  ```r
  # Test on CRAN's incoming Windows servers
  devtools::check_win_devel()
  devtools::check_win_release()
  
  # Test on modern R-hub platforms
  rhub::rhub_check()
  ```
- [ ] **Draft `cran-comments.md`**:
  Summarize test results, platforms checked, and explain that `ConverseR` is an interactive RStudio addin utilizing `httpuv` for local loopback IPC.

---

## Summary Timeline

| Stage | Milestone | Primary Deliverable |
| :--- | :--- | :--- |
| **Day 1** | GitHub Public Beta | Tag `v0.4.0`, update author info in `DESCRIPTION` |
| **Day 2** | Binary Builds | Setup R-Universe (`tironesr.r-universe.dev`) |
| **Week 1** | Media & Showcase | Record 3 GIFs, post to Posit Community & #rstats |
| **Month 1** | CRAN Hardening | Full `R CMD check --as-cran`, submit to CRAN |
