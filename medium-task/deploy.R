# Deployment Script for Wildlife Explorer App
# Run this in RStudio or R Console

# Step 1: Set working directory
setwd("C:/Users/Atharv Raskar/Desktop/Ecotourism- GSoC 2026/medium-task")

# Step 2: Test locally first (IMPORTANT!)
cat("Testing app locally first...\n")
cat("Close the app window when done testing.\n")
shiny::runApp()

# Step 3: After local testing passes, deploy
cat("\n=== DEPLOYMENT TO SHINYAPPS.IO ===\n\n")

# SECURITY WARNING
cat("IMPORTANT SECURITY NOTICE\n")
cat("Your previous token was exposed publicly.\n")
cat("You MUST revoke it and generate a new one:\n")
cat("1. Visit https://www.shinyapps.io/admin/\n")
cat("2. Click 'Tokens' in Account menu\n")
cat("3. Revoke the old token\n")
cat("4. Generate a new token\n\n")

# Get new credentials from user
cat("Enter your NEW shinyapps.io credentials:\n")
account_name <- readline(prompt = "Account name (ecotourism-atharvraskar): ")
if (account_name == "") account_name <- "ecotourism-atharvraskar"

token <- readline(prompt = "New token: ")
secret <- readline(prompt = "New secret: ")

# Validate inputs
if (token == "" || secret == "") {
  stop("Error: Token and secret cannot be empty!")
}

# Set account info
cat("\nConfiguring shinyapps.io account...\n")
rsconnect::setAccountInfo(
  name = account_name,
  token = token,
  secret = secret
)

cat("Account configured successfully\n\n")

# Deploy
cat("Starting deployment...\n")
cat("This may take 2-5 minutes.\n\n")

rsconnect::deployApp(
  appDir = ".",
  appName = "medium-task",
  appTitle = "Australian Wildlife Explorer",
  forceUpdate = TRUE
)

cat("\nDEPLOYMENT COMPLETE!\n\n")
cat("Your app is now live at:\n")
cat("https://ecotourism-atharvraskar.shinyapps.io/medium-task/\n\n")

cat("Next steps:\n")
cat("1. Visit the URL above\n")
cat("2. Test all 4 organisms × 4 tabs (16 combinations)\n")
cat("3. Use TESTING_CHECKLIST.md for systematic testing\n")
cat("4. If everything works, submit to mentors!\n")
