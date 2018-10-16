# Use this script -- or some tweaked version of it -- to push the code
# to shinyapps.io. Comment out the deployApp() call that you do NOT 
# want to run. This script requires having rsconnect installed, as well
# as having authenticated with a shinyapps.io 

library(rsconnect)

# Deploy the split-the-bill calculator
deployApp(appName = "split-the-bill",
          appTitle = "Split the BillY")
