# SArf Package Setup Script
# Run this script to prepare the package for use/publication

# Install required packages for development
cat("Installing development packages...\n")
if (!require("devtools")) install.packages("devtools")
if (!require("roxygen2")) install.packages("roxygen2")
if (!require("knitr")) install.packages("knitr")
if (!require("rmarkdown")) install.packages("rmarkdown")
if (!require("testthat")) install.packages("testthat")

# Set working directory to package root
# CHANGE THIS to your SArf directory:
pkg_dir <- "/path/to/SArf"
setwd(pkg_dir)

cat("\n=== STEP 1: Generate Documentation ===\n")
devtools::document()
cat("✓ Documentation generated\n")

cat("\n=== STEP 2: Check Package ===\n")
check_results <- devtools::check()
cat("\nCheck complete. Review any warnings or notes above.\n")

cat("\n=== STEP 3: Build Vignettes ===\n")
devtools::build_vignettes()
cat("✓ Vignettes built\n")

cat("\n=== STEP 4: Install Package Locally ===\n")
devtools::install()
cat("✓ Package installed\n")

cat("\n=== STEP 5: Test Package ===\n")
cat("Try running:\n")
cat("  library(SArf)\n")
cat("  ?SArf\n")
cat("  browseVignettes('SArf')\n")

cat("\n=== Package Ready! ===\n")
cat("\nNext steps:\n")
cat("1. Test with your data\n")
cat("2. Fix any issues from check results\n")
cat("3. Push to GitHub:\n")
cat("   git init\n")
cat("   git add .\n")
cat("   git commit -m 'Initial SArf package'\n")
cat("   git remote add origin https://github.com/yourusername/SArf.git\n")
cat("   git push -u origin main\n")
cat("\n4. Share with others:\n")
cat("   devtools::install_github('yourusername/SArf')\n")

cat("\n\n🎉 SArf package setup complete! 🎉\n")
