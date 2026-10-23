# Releasing ConverseR to Beta Testers (macOS & Windows)

This document contains:
1. **Steps for You (Author)** to share `ConverseR` and the Getting Started Guide with your 3–5 beta testers on macOS and Windows.
2. **A Ready-to-Send Onboarding Email / Slack Message** you can copy and send directly to your beta testers.

---

## Part 1: How to Distribute `ConverseR` to Beta Testers

Because `ConverseR` is a pure R package (with no compiled C/C++ code in the package itself) and all of its dependencies (`ellmer`, `httpuv`, `later`, `shiny`, `miniUI`, `treesitter`, `treesitter.r`, `commonmark`, `broom`, etc.) are standard CRAN packages with prebuilt macOS and Windows binaries, **the exact same source package works identically on both macOS and Windows.**

In addition, the HTML Getting Started Guide (`inst/doc/ConverseR-guide.html`) is **bundled inside the R package itself**, so as soon as a tester installs `ConverseR`, running `converser_guide()` (or clicking **Settings -> Help** in the Sidebar) opens the full guide locally in their browser.

You have **two easy ways** to share the package with your 3–5 beta testers:

### Option A: Share via a Public (or Private) GitHub Repository *(Recommended)*

This is the cleanest experience for macOS and Windows users because `remotes::install_github()` downloads `ConverseR` and installs every CRAN dependency in a single line.

1. **Create a new repository on GitHub** named `ConverseR` (e.g. `https://github.com/tironesr/ConverseR`).
2. **Push this folder to GitHub** from your terminal or PowerShell:
   ```powershell
   git init
   git add .
   git commit -m "Initial beta release of ConverseR (v0.4.0)"
   git branch -M main
   git remote add origin https://github.com/tironesr/ConverseR.git
   git push -u origin main
   ```
3. **Optional — Host the Getting Started Guide at a public URL via GitHub Pages**:
   - On GitHub, go to **Settings -> Pages**.
   - Under **Build and deployment**, select **Deploy from a branch** -> `main` -> `/ (root)` and click **Save**.
   - Within 60 seconds, your standalone guide (`ConverseR-guide.html`) will be live on the web at:
     `https://tironesr.github.io/ConverseR/ConverseR-guide.html`
4. **Tester Installation Command (Mac & Windows)**:
   ```r
   install.packages("remotes")
   remotes::install_github("tironesr/ConverseR", dependencies = TRUE)
   ```

---

### Option B: Share the Pre-Built `ConverseR_0.4.0.tar.gz` + `ConverseR-guide.html` Directly *(No GitHub Needed)*

We have already built a self-contained source bundle in your project folder:
- **`ConverseR_0.4.0.tar.gz`** (the installable R package for both macOS and Windows)
- **`ConverseR-guide.html`** (the standalone HTML Getting Started Guide)

1. Attach **`ConverseR_0.4.0.tar.gz`** and **`ConverseR-guide.html`** to an email, Slack message, Google Drive, or Dropbox link for your 3–5 testers.
2. Testers can double-click `ConverseR-guide.html` to read the guide before installing, and install the package in RStudio on **either macOS or Windows** by running:
   ```r
   install.packages("remotes")
   remotes::install_local(file.choose(), dependencies = TRUE)
   ```
   *(When `file.choose()` opens a file picker window, they simply select `ConverseR_0.4.0.tar.gz`, and `remotes` automatically downloads all required CRAN dependencies and installs `ConverseR`.)*

---

## Part 2: Copy-Paste Invitation Message for Your Beta Testers

Below is a ready-to-send email or Slack message you can send to your 3–5 beta testers:

---

**Subject:** Beta Testing `ConverseR` — OpenRouter AI Sidebar & Inline Assistant for RStudio (Mac & Windows)

Hi everyone,

Thank you for helping beta test **ConverseR**, a new RStudio package that integrates OpenRouter language models directly into RStudio with:
- A **non-blocking Viewer Sidebar (`converser()`)** that automatically reads your active script and `.GlobalEnv` data frames/models while keeping your R Console free for coding.
- A **keyboard-driven Inline Code Assistant (`converser_inline()`)** for quick code replacements and insertions.
- **One-click Plot Sharing (`Share Plot`)** and **Console/Script Error Diagnosis (`Fix Error`)**: Automatically captures the failing line of code, line number, script context, and error backtrace for instant fixes.
- **Persistent Settings & Custom Skills (`SKILL.md` / `Skill.zip`)** so the assistant follows your personal R coding and statistical conventions across sessions without editing `.Rprofile`.

### Step 1: Install `ConverseR` (Works on both macOS & Windows)

Open **RStudio** (R >= 4.3 recommended) and run the following in your R Console:

*(If installing from GitHub)*:
```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("tironesr/ConverseR", dependencies = TRUE)
```

*(Or if installing from the attached `ConverseR_0.4.0.tar.gz` file)*:
```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_local(file.choose(), dependencies = TRUE)
# Select ConverseR_0.4.0.tar.gz in the file dialog
```

### Step 2: Load the Package & Open the Getting Started Guide

Run:
```r
library(ConverseR)
converser_guide()
```
This opens the **Getting Started Guide** in your browser (also attached as `ConverseR-guide.html`), which walks through every feature step by step.

### Step 3: Set Up Your OpenRouter API Key (Takes 2 Minutes)

1. Go to [https://openrouter.ai](https://openrouter.ai), sign in, add a small prepaid balance (e.g., $5 under **Settings -> Credits**), and create an API key under **Settings -> Keys** (`sk-or-v1-...`).
2. In RStudio, launch the Sidebar:
   ```r
   converser()
   ```
3. Click **Settings** in the top-right header of the Sidebar.
4. Paste your OpenRouter API key, choose your model (defaults to `deepseek/deepseek-v4.1-flash`, or pick any model from the **Top 20 OpenRouter Models** dropdown), keep **Save as default across R sessions** checked, and click **Save & Apply**.

### Step 4: Bind the Inline Assistant Keyboard Shortcut

1. In RStudio, go to **Tools -> Modify Keyboard Shortcuts...**
2. Filter for `ConverseR`.
3. Click in the **Shortcut** column next to **ConverseR: Inline Code Assistant** and press:
   - **Mac:** `Cmd+Option+I`
   - **Windows:** `Ctrl+Alt+I`
4. Click **Apply**.

### Things to Try During Beta Testing

1. **Sidebar Chat (`converser()`):** Open an R script with some data in `.GlobalEnv`, highlight a block of code, and ask a question or request a modification. Try the **Run in Console**, **Preview Difference**, **Replace**, and **Copy** buttons on the generated code block.
2. **Inline Code Edits (`converser_inline()`):** Highlight a few lines in your script, press your keyboard shortcut (`Cmd+Option+I` / `Ctrl+Alt+I`), and test both direct streaming and **Preview Difference first**.
3. **Share Plot & Fix Error:** Create a `ggplot2` or base R plot, click **Share Plot** in the Sidebar, and ask for specific visual adjustments. Trigger an error in the console and click **Fix Error**.
4. **Custom `SKILL.md`:** Follow Section 9 of the Getting Started Guide (`converser_guide()`) to generate a custom YAML-formatted `SKILL.md` from one of your existing R scripts, save it in **Settings**, and test whether subsequent code follows your conventions.

### Sending Feedback
Please let me know:
- Any installation issues on your OS (macOS / Windows) or R version
- Anything in the UI or Getting Started Guide that felt confusing or could be smoother
- Any bugs or unexpected behavior when chatting, editing scripts, or switching models

Thank you!
