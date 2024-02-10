(defconst gform-environments
  '("http://localhost:9196/gform/formtemplates"
    "http://localhost:9396/gform/formtemplates"
    "https://www.development.tax.service.gov.uk/submissions/test-only/proxy-to-gform/gform/formtemplates"
    "https://www.qa.tax.service.gov.uk/submissions/test-only/proxy-to-gform/gform/formtemplates"
    "https://www.staging.tax.service.gov.uk/submissions/test-only/proxy-to-gform/gform/formtemplates"
    "https://test-www.tax.service.gov.uk/submissions/test-only/proxy-to-gform/gform/formtemplates"))

(defvar gform-last-url (car gform-environments))

(defun upload-template (prefix-arg)
  (interactive "p")

  (when (eq 4 prefix-arg)
    (let ((url (completing-read  "Choose environment: " gform-environments)))
      (setq gform-last-url url)))

  (let ((json (buffer-string)))
    (with-temp-buffer
      (insert (format "# -*- restclient -*-

POST %s
Content-Type: application/json; charset=utf-8
Csrf-Token: nocheck

" gform-last-url))
      (insert json)
      (restclient-mode)
      (set-buffer-multibyte nil)
      (restclient-http-send-current-stay-in-window))
    (message "Uploading json-mode buffer %s to %s" (buffer-name (current-buffer)) gform-last-url)))


(defconst gform-environments-new-form
  '("http://localhost/submissions/new-form/"
    "http://localhost:9295/submissions/new-form/"
    "https://www.development.tax.service.gov.uk/submissions/new-form/"
    "https://www.qa.tax.service.gov.uk/submissions/new-form/"
    "https://www.staging.tax.service.gov.uk/submissions/new-form/"
    "https://test-www.tax.service.gov.uk/submissions/new-form/"))

(defvar gform-last-new-form-url (car gform-environments-new-form))

(defun open-template-in-browser (prefix-arg)
  (interactive "p")
  (let ((find-template-id-regex "\"_id\"[[:space:]]*:[[:space:]]*\"\\([a-zA-Z0-9-]*\\)\""))
    (when (eq 4 prefix-arg)
      (let ((url (completing-read  "Choose environment: " gform-environments-new-form)))
        (setq gform-last-new-form-url url)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward find-template-id-regex)
      (browse-url (concat gform-last-new-form-url (match-string 1))))))
