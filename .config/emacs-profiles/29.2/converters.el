;;

(global-set-key (kbd "C-M-s-p") 'itv-convert-identifier)

(defun itv-programme-id-to-api-encoded ()
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (subst-char-in-region beg end ?# ?.)
        (subst-char-in-region beg end ?/ ?_))
    (message "No active region.")))

(defun itv-programme-id-from-api-encoded ()
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (subst-char-in-region beg end ?. ?#)
        (subst-char-in-region beg end ?_ ?/))
    (message "No active region.")))

(defun pipa-iso-time-to-timestamp ()
  "Timestamop is in milliseconds"
  (let* ((str (thing-at-point 'string))
         (timestamp (time-convert (date-to-time str) 'integer))
         (bounds (bounds-of-thing-at-point 'string)))
    (or bounds (error "No %s here" 'string))
    (goto-char (car bounds))
    (delete-region (car bounds) (cdr bounds))
    (insert (format "%s000" timestamp))))

(defun pipa-timestamp-to-iso-time ()
  (let* ((thing 'number)
         (time (thing-at-point thing))
         (iso-time (format-time-string "%Y-%m-%dT%H:%M:%SZ" (/ time 1000) t))
         (bounds (bounds-of-thing-at-point 'word)))
    (or bounds (error "No %s here" thing))
    (goto-char (car bounds))
    (delete-region (car bounds) (cdr bounds))
    (insert (format "\"%s\"" iso-time))
    (backward-char)))

(defun pipa-toggle-timestamp-and-iso-time ()
  (interactive)
  (let ((tap (thing-at-point 'string)))
    (if (stringp tap)
        (pipa-iso-time-to-timestamp)
      (pipa-timestamp-to-iso-time))))

(defconst itv-production-id-api-encoded-chars "a-zA-Z0-9._\\-")
(defconst itv-production-id-chars "a-zA-Z0-9#\\/-")
(defconst itv-ccid-chars "bcdfghjklmnpqrstvwxyz0123456789")

(defun itv-detect-ccid ()
  (save-excursion
    (let (start end)
      (skip-chars-forward itv-ccid-chars)
      (setq end (point))
      (skip-chars-backward itv-ccid-chars)
      (setq start (point))
      (when (and
             (looking-at (format "[%s]\\{7\\}" itv-ccid-chars))
             (equal 7 (- end start)))
        (buffer-substring-no-properties start end)))))

(defun itv-ccid-to-production-id (ccid)
  (let* ((url (format "https://metadatacataloguemapping.prd.cm.itv.com/translateByCCID?ccid=%s" ccid))
         (payload (get-json url (format "404 - ProductionId for ccid %s not found. Link %s" ccid url))))
    (cdr (assoc 'historicalCatalogueId payload))))

(defun itv-programme-id-to-api-encoded-str (production-id)
  (subst-char-in-string ?# ?. production-id t)
  (subst-char-in-string ?/ ?_ production-id t))

(defun itv-production-id-to-ccid (production-id)
  (let* ((production-id-encoded (itv-programme-id-to-api-encoded-str production-id))
         (url (format "https://metadatacataloguemapping.prd.cm.itv.com/translateByHistoricalCatalogueId?catalogueId=%s&level=PRODUCTION" production-id-encoded)))
    (cdr (assoc
          'ccid
          (get-json
           url
           (format "404 - ccid for ProductionId %s not found. Link %s" production-id-encoded  url))))))

(defun get-json (uri not-found-msg)
  "Fetch the contents of URI and parse."
  (with-current-buffer (url-retrieve-synchronously uri nil nil 1)
    (goto-char (point-min))
    (pcase (url-http-parse-response)
      (404 (user-error not-found-msg))
      (_ (goto-char url-http-end-of-headers)
         (prog1 (json-read)
           (kill-buffer))))))

(defun itv-convert-identifier ()
  (interactive)
  (let ((identifier (itv-detect-production-id
                     'itv-ccid-to-production-id
                     'itv-production-id-to-ccid
                     'itv-production-id-to-ccid)))
    (kill-new identifier)
    (message "%s" identifier)))

(defun itv-detect-production-id (on-ccid on-production-id on-production-id-encoded)
  (let ((pid-encoded (itv-detect-identifier itv-production-id-api-encoded-chars))
        (pid (itv-detect-identifier itv-production-id-chars))
        (ccid (itv-detect-ccid)))
    (cond
     ((not (null ccid)) (funcall on-ccid ccid))
     ((or
       (> (length pid-encoded) 19) ;; shortest: 12345/D, longest: 1/1234/12345/12#001
       (> (length pid) 19)
       (equal (length pid-encoded) (length pid)))
      (user-error "Not a ProductionId %s" pid-encoded))
     ((> (length pid-encoded) (length pid))
      (funcall on-production-id-encoded pid-encoded))
     (t (funcall on-production-id pid)))))

(defun itv-detect-identifier (chars)
  (save-excursion
    (let (start end)
      (skip-chars-forward chars)
      (setq end (point))
      (skip-chars-backward chars)
      (setq start (point))
      (buffer-substring-no-properties start end))))

(defface hi-custom-3
  '((((background dark)) (:background "LightPink3" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-pink-2
  '((((background dark)) (:background "pink" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-pink-3
  '((((background dark)) (:background "snow3" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-custom-4
  '((((background dark)) (:background "lavender" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-custom-5
  '((((background dark)) (:background "light coral" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-custom-6
  '((((background dark)) (:background "sandy brown" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-custom-7
  '((((background dark)) (:background "burlywood" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-custom-8
  '((((background dark)) (:background "LightGoldenrod3" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-custom-9
  '((((background dark)) (:background "LemonChiffon2" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-custom-10
  '((((background dark)) (:background "OliveDrab3" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-custom-11
  '((((background dark)) (:background "PaleGreen3" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-custom-12
  '((((background dark)) (:background "DarkSlateGray3" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-custom-13
  '((((background dark)) (:background "LightCyan3" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-custom-14
  '((((background dark)) (:background "chartreuse3" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-custom-15
  '((((background dark)) (:background "SteelBlue2" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-custom-16
  '((((background dark)) (:background "MediumOrchid1" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-custom-17
  '((((background dark)) (:background "khaki" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defun itv-platforms-highlight ()
  (interactive)
  (highlight-regexp "cdb76838-a046-4338-8c24-c6071209c757" 'hi-yellow)
  (highlight-regexp "f3a556e0-791d-11ec-9362-1b56e272d2a6" 'hi-custom-3)
  (highlight-regexp "24613b1d-e348-46c4-81f1-bb362fcc89d2" 'hi-pink)
  (highlight-regexp "4700c398-e8af-48a8-af89-25d48b42c258" 'hi-green)
  (highlight-regexp "f08925e0-1220-11ec-a924-1f5965d06c71" 'hi-blue)
  (highlight-regexp "690a7430-d4ae-4cca-af9d-7162e0408582" 'hi-salmon)
  (highlight-regexp "e3cbea7c-0061-4946-896f-7f605412ed24" 'hi-aquamarine)

  (highlight-regexp "0a571bcd-9a0b-4f49-8f3d-41fc483dea52" 'hi-pink-2)
  (highlight-regexp "b925833c-ddc2-11ec-a479-3788fa70d32f" 'hi-pink-3)
  (highlight-regexp "4ae57817-6847-4777-ab31-1bf961c8f832" 'hi-custom-4)
  (highlight-regexp "b925871a-ddc2-11ec-a47a-679eeff246f3" 'hi-custom-5)
  (highlight-regexp "e5fdc8a9-e76f-44c8-a381-acc4a5eecdfe" 'hi-custom-6)
  (highlight-regexp "25003fae-1d5e-11ed-80ac-db091b4fa872" 'hi-custom-7)

  (highlight-regexp "3d8bd14d-215f-43a9-999f-804750aacfa7" 'hi-custom-8)
  (highlight-regexp "35058ac0-ddc2-11ec-a478-ff848dd90c7e" 'hi-custom-9)

  (highlight-regexp "4df87609-46e2-41a6-9b27-669061383242" 'hi-custom-10)
  (highlight-regexp "de52e1ce-ddc0-11ec-a475-27160b90ec32" 'hi-custom-11)

  (highlight-regexp "7d01d9d5-6a59-45ff-94a8-779db1906756" 'hi-custom-12)
  (highlight-regexp "5826f472-ddc1-11ec-a476-236883a6bc1a" 'hi-custom-13)

  (highlight-regexp "37f039c2-f098-4f3f-a032-45de51615769" 'hi-custom-14)
  (highlight-regexp "f2a54cd2-ddbd-11ec-a474-1784efd9cc6e" 'hi-custom-15)

  (highlight-regexp "5da975c2-6534-48f5-bdb9-39285eda4a8b" 'hi-custom-16)
  (highlight-regexp "d839f204-ddc1-11ec-a477-9fd03670212e" 'hi-custom-17)
  )

(defun itv-platforms-unhighlight ()
  (interactive)
  (unhighlight-regexp "cdb76838-a046-4338-8c24-c6071209c757")
  (unhighlight-regexp "24613b1d-e348-46c4-81f1-bb362fcc89d2")
  (unhighlight-regexp "4700c398-e8af-48a8-af89-25d48b42c258")
  (unhighlight-regexp "f08925e0-1220-11ec-a924-1f5965d06c71")
  (unhighlight-regexp "690a7430-d4ae-4cca-af9d-7162e0408582")
  (unhighlight-regexp "e3cbea7c-0061-4946-896f-7f605412ed24")
  (unhighlight-regexp "f3a556e0-791d-11ec-9362-1b56e272d2a6")
  (unhighlight-regexp "0a571bcd-9a0b-4f49-8f3d-41fc483dea52")
  (unhighlight-regexp "b925833c-ddc2-11ec-a479-3788fa70d32f")
  (unhighlight-regexp "4ae57817-6847-4777-ab31-1bf961c8f832")

  (unhighlight-regexp "b925871a-ddc2-11ec-a47a-679eeff246f3")
  (unhighlight-regexp "e5fdc8a9-e76f-44c8-a381-acc4a5eecdfe")
  (unhighlight-regexp "25003fae-1d5e-11ed-80ac-db091b4fa872")

  (unhighlight-regexp "3d8bd14d-215f-43a9-999f-804750aacfa7")
  (unhighlight-regexp "35058ac0-ddc2-11ec-a478-ff848dd90c7e")

  (unhighlight-regexp "4df87609-46e2-41a6-9b27-669061383242")
  (unhighlight-regexp "de52e1ce-ddc0-11ec-a475-27160b90ec32")

  (unhighlight-regexp "7d01d9d5-6a59-45ff-94a8-779db1906756")
  (unhighlight-regexp "5826f472-ddc1-11ec-a476-236883a6bc1a")

  (unhighlight-regexp "37f039c2-f098-4f3f-a032-45de51615769")
  (unhighlight-regexp "f2a54cd2-ddbd-11ec-a474-1784efd9cc6e")

  (unhighlight-regexp "5da975c2-6534-48f5-bdb9-39285eda4a8b")
  (unhighlight-regexp "d839f204-ddc1-11ec-a477-9fd03670212e")

  )

(defun itv-platforms-highlight-enabled-publishing ()
  (interactive)
  (highlight-regexp "f3a556e0-791d-11ec-9362-1b56e272d2a6" 'hi-custom-3)
  (highlight-regexp "f08925e0-1220-11ec-a924-1f5965d06c71" 'hi-blue)
  (highlight-regexp "35058ac0-ddc2-11ec-a478-ff848dd90c7e" 'hi-custom-9)
  (highlight-regexp "b925833c-ddc2-11ec-a479-3788fa70d32f" 'hi-pink-3)
  (highlight-regexp "25003fae-1d5e-11ed-80ac-db091b4fa872" 'hi-custom-7)
  )

(defun gform-internal-auth-refresh ()
  (interactive)
  (let ((output (process-lines "internal-auth-token-refresher")))
    (message "%s" (car output))))
