# ConverseR

**Context-Aware OpenRouter Sidebar & Inline Code Assistant for RStudio**

`ConverseR` integrates OpenRouter language models directly into RStudio. Chat with an assistant in the RStudio Viewer pane while keeping your R Console free for coding, trigger quick inline code edits via keyboard shortcut, share plots, diagnose console/script errors, and customize coding conventions using `SKILL.md` or `Skill.zip` bundles.

---

## 1. Installation (macOS & Windows)

### Prerequisites
- **R** (`>= 4.3.0` recommended)
- **RStudio Desktop** (`2023.06` or newer)
- An **[OpenRouter](https://openrouter.ai)** API key (`sk-or-v1-...`) with a small prepaid credit balance (e.g., $5)

### Option A: Install from GitHub (Recommended)

Run the following in your RStudio Console. This automatically installs `ConverseR` along with all required CRAN dependencies (`ellmer`, `httpuv`, `later`, `shiny`, `miniUI`, `treesitter`, `treesitter.r`, `commonmark`, `broom`, etc.):

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Replace YOUR_GITHUB_USERNAME with the repository owner's GitHub username:
remotes::install_github("YOUR_GITHUB_USERNAME/ConverseR", dependencies = TRUE)
```

### Option B: Install from a `.tar.gz` Package Bundle

If you received the `ConverseR_0.4.0.tar.gz` file directly:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install all CRAN dependencies + ConverseR from the local .tar.gz file:
remotes::install_local("path/to/ConverseR_0.4.0.tar.gz", dependencies = TRUE)
```

---

## 2. Getting Started in 60 Seconds

```r
# 1. Load ConverseR
library(ConverseR)

# 2. Open the interactive Getting Started Guide in your browser:
converser_guide()

# 3. Open the ConverseR Sidebar in the RStudio Viewer pane:
converser()
```

### Saving Your OpenRouter API Key & Defaults (No `.Rprofile` Editing Needed)
1. Run `converser()` to open the Sidebar.
2. Click **Settings** in the top-right header of the Sidebar.
3. Paste your OpenRouter API key (`sk-or-v1-...`), select your preferred model (defaults to `deepseek/deepseek-v4.1-flash`), and choose your reasoning effort.
4. Keep **Save as default across R sessions** checked and click **Save & Apply**.

Or configure and save your defaults directly from the R Console:

```r
converser_use_openrouter(
  api_key          = "sk-or-v1-your-openrouter-key",
  model            = "deepseek/deepseek-v4.1-flash",
  reasoning_effort = "low",
  zdr              = TRUE,
  data_collection  = "deny",
  persist          = TRUE
)
```

---

## 3. Binding Keyboard Shortcuts (macOS & Windows)

1. In RStudio, open **Tools** -> **Modify Keyboard Shortcuts...**
2. Type `ConverseR` in the **Filter...** box.
3. Bind **ConverseR: Inline Code Assistant**:
   - **Windows:** `Ctrl+Alt+I`
   - **macOS:** `Cmd+Option+I`
4. *(Optional)* Bind **ConverseR: Open Sidebar** to `Ctrl+Alt+C` (Windows) or `Cmd+Option+C` (macOS).
5. Click **Apply**.

---

## 4. Core Functions

| Function | Description |
| :--- | :--- |
| `converser()` | Open the non-blocking ConverseR Sidebar in the RStudio Viewer pane |
| `converser_inline()` | Open the inline code assistant popup for quick script edits |
| `converser_guide()` | Open the full HTML Getting Started Guide (also accessible via **Settings -> Help**) |
| `converser_use_openrouter()` | Configure OpenRouter API key, model, reasoning effort, ZDR, and skills (`persist = TRUE` saves across sessions) |
| `converser_save_config()` | Save current settings and API key across future R sessions |
| `converser_inspect_config()` | Print active model, OpenRouter parameters, reasoning effort, and skill status |
| `converser_undo()` | Revert the most recent script edit made by ConverseR |
| `converser_reset()` | Clear the current conversation history |
