(defconst gform-environments-for-handlebarstemplates
  '("http://localhost:9196/gform/handlebarstemplates"
    "https://www.development.tax.service.gov.uk/submissions/test-only/proxy-to-gform/gform/handlebarstemplates"
    "https://www.qa.tax.service.gov.uk/submissions/test-only/proxy-to-gform/gform/handlebarstemplates"
    "https://www.staging.tax.service.gov.uk/submissions/test-only/proxy-to-gform/gform/handlebarstemplates"
    "https://test-www.tax.service.gov.uk/submissions/test-only/proxy-to-gform/gform/handlebarstemplates"))

(defvar gform-last-handlebarstemplates-url (car gform-environments-for-handlebarstemplates))

(defun upload-handlebarstemplates (prefix-arg)
  (interactive "p")

  (when (eq 4 prefix-arg)
    (let ((url (completing-read  "Choose environment: " gform-environments-for-handlebarstemplates)))
      (setq gform-last-handlebarstemplates-url url)))

  (let ((json (buffer-string))
        (hbs-file-name (file-name-base (buffer-file-name))))
    (with-temp-buffer
      (insert (format "# -*- restclient -*-

POST %s/%s
Content-Type: text/plain
Csrf-Token: nocheck

" gform-last-handlebarstemplates-url hbs-file-name))
      (insert json)
      (restclient-mode)
      (set-buffer-multibyte nil)
      (restclient-http-send-current-stay-in-window))
    (message "Uploading handlebar buffer %s to %s" (buffer-name (current-buffer)) gform-last-handlebarstemplates-url)))
