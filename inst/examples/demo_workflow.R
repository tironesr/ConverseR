# ==============================================================================
# RCompanion Interactive Walkthrough & Testing Script
# ==============================================================================
# Open this file in RStudio or Positron to test all features of RCompanion!

library(RCompanion)
library(ggplot2)

# ------------------------------------------------------------------------------
# STEP 1: Configure your LLM (or reuse your existing `gander.chat` option!)
# ------------------------------------------------------------------------------
# If you already have `options(gander.chat = ...)` in your ~/.Rprofile,
# RCompanion detects it automatically! Otherwise, uncomment one of the lines
# below to configure your preferred provider via `ellmer`:

# RCompanion::rcompanion_config(
#   chat = ellmer::chat_claude(model = "claude-sonnet-4-6"),
#   # Or: chat = ellmer::chat_openai(model = "gpt-4o"),
#   # Or: chat = ellmer::chat_google_gemini(),
#   # Or: chat = ellmer::chat_ollama(model = "qwen2.5-coder"),
#   window_lines = 75
# )

# ------------------------------------------------------------------------------
# STEP 2: Populate `.GlobalEnv` with realistic Data Science & Academic objects
# ------------------------------------------------------------------------------
# Run these lines so RCompanion has real data frames and models in memory:

set.seed(42)
library(dplyr)
library(stringr)

set.seed(42)
#Clinical Trial Data
clinical_trial <- tibble::tibble(
  patient_id    = str_glue("PT-{str_pad(1:120, width = 3, pad = '0')}"),
  treatment_arm = factor(sample(c("Placebo", "LowDose", "HighDose"), 120, replace = TRUE)),
  baseline_bp   = round(rnorm(120, 135, 12), 1),
  age           = sample(28:78, 120, replace = TRUE),
  followup_bp   = round(rnorm(120, 124, 14), 1)
)
#tibble() never converts strings to factors, so stringsAsFactors is not needed
#patient_id is built with stringr::str_glue + str_pad, which gives the same values as sprintf("PT-%03d", 1:120)
#patient_id: PT-001 to PT-120 (sprintf replaces str_glue + str_pad)
#treatment_arm levels: HighDose, LowDose, Placebo

# Introduce a few realistic missing values (NAs)
clinical_trial$followup_bp[c(5, 19, 44)] <- NA

# Fit a statistical linear model
bp_model <- lm(followup_bp ~ treatment_arm + baseline_bp + age, data = clinical_trial)

# Verify how RCompanion summarizes your objects vs raw `str()`:
cat(paste(describe_object(clinical_trial, "clinical_trial"), collapse = "\n"), "\n\n")
cat(paste(describe_object(bp_model, "bp_model"), collapse = "\n"), "\n")

# ------------------------------------------------------------------------------
# STEP 3: Test the Fast Inline Assistant (`rcompanion_inline()`)
# ------------------------------------------------------------------------------
# Highlight the code block below and run `RCompanion::rcompanion_inline()`
# (or bind "RCompanion: Inline Assistant" to Ctrl+Alt+G in Tools > Modify Keyboard Shortcuts).
# Try typing:
#   "Filter out missing followup_bp, group by treatment_arm, and summarize mean & SD"
# Try checking "Preview Diff first" in the popup to see the red/green diff!

clinical_trial


# ------------------------------------------------------------------------------
# STEP 4: Test Plot Vision Critique (`rcompanion_critique_plot()`)
# ------------------------------------------------------------------------------
# Run this basic plot so it appears in your RStudio Plots pane, then run:
#   RCompanion::rcompanion_critique_plot()

# Simplified version: minimal styling, default theme, no custom scales
#Reorder arms so Placebo is the reference level
clinical_trial$treatment_arm <- factor(clinical_trial$treatment_arm,
                                       levels = c("Placebo", "LowDose", "HighDose"))

#Reorder arms so Placebo is the reference level
clinical_trial$treatment_arm <- factor(clinical_trial$treatment_arm,
                                       levels = c("Placebo", "LowDose", "HighDose"))

p_pub <- ggplot(
  clinical_trial[which(!is.na(clinical_trial$followup_bp)), ],
  aes(x = baseline_bp, y = followup_bp, color = treatment_arm, fill = treatment_arm)
) +
  geom_point(size = 2.2, alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.15) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  coord_equal() +
  labs(
    title = "Follow-up vs. Baseline Systolic BP by Treatment Arm",
    x = "Baseline Systolic BP (mmHg)",
    y = "Follow-up Systolic BP (mmHg)",
    color = "Treatment Arm", fill = "Treatment Arm"
  ) +
  theme_minimal(base_size = 13)
p_pub

iris[1:232]

#Select rows (note the comma), capped at the actual number of rows
iris_Sub <- iris[1:min(212, nrow(iris)), ];nrow(iris_Sub)
# Save print-ready at a fixed size
ggsave("bp_followup_vs_baseline.png", p_pub,
       width = 6, height = 4.5, dpi = 300, units = "in")


# ------------------------------------------------------------------------------
# STEP 5: Test the Persistent Companion Sidebar (`rcompanion_sidebar()`)
# ------------------------------------------------------------------------------
# Run the command below to launch the interactive Companion Sidebar in your
# RStudio Viewer pane:
#
#   RCompanion::rcompanion_sidebar()
#
# Inside the Sidebar, try:
#   1. Clicking "Interpret Model / Table" to get an academic interpretation and
#      LaTeX equation for `bp_model`.
#   2. Asking: "Check the documentation for stats::confint using your help tool
#      and compute 95% confidence intervals for bp_model."
#   3. Switching to the "Diff & Apply" tab to preview changes and click
#      "Replace Selection" or "Undo Edit".
