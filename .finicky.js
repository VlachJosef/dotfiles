// Use https://finicky-kickstart.now.sh to generate basic configuration
// Learn more about configuration options: https://github.com/johnste/finicky/wiki/Configuration

module.exports = {
  defaultBrowser: "Google Chrome",
  options: {
    // Hide the finicky icon from the top bar. Default: false
    hideIcon: false,
    // Check for update on startup. Default: true
    checkForUpdate: false,
    // Change the internal list of url shortener services. Default: undefined
    // urlShorteners: (list) => [...list, "custom.urlshortener.com"],
    // Log every request with basic information to console. Default: false
    logRequests: false
  },

  handlers: [
    {
      match: [
        "github.com/hmrc/*",
        "https://confluence.tools.tax.service.gov.uk/*",
        "https://kibana.tools.production.tax.service.gov.uk/*",
        "https://jira.tools.tax.service.gov.uk/*",
        "https://hmrcdigital.slack.com/*",
      ],
      browser: "Vivaldi"
    }
  ]

}
