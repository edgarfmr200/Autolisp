;; OCMEMA core module (AutoLISP only). No runtime logic on load.

;; A) Constantes / variables globales
(setq ocmema:*core-version* "0.1")
(setq ocmema:*default-project-folder* "C:\\Users\\edgar\\OneDrive - ITESO\\OCMEMA_IE\\01. PROYECTOS")
(setq ocmema:*debug* nil)

;; B) Logging consistente
(defun ocmema:log (msg /)
  (princ (strcat "OCMEMA: " msg "\n"))
)

(defun ocmema:dbg (msg /)
  (if ocmema:*debug*
    (princ (strcat "DBG: " msg "\n"))
  )
)

;; C) Wrapper de errores (sin VisualLISP)
(defun ocmema:errwrap (fn onerr / olderr result)
  (setq olderr *error*)
  (setq *error*
    (function
      (lambda (msg)
        (if (and msg (/= msg ""))
          (if (or (wcmatch msg "*Function cancelled*")
                  (wcmatch msg "*quit / exit abort*"))
            (ocmema:log "Cancelado por usuario.")
            (ocmema:log (strcat "ERROR: " msg))
          )
        )
        (setq *error* olderr)
        (if onerr (apply onerr (list msg)))
        (princ)
      )
    )
  )
  (setq result (apply fn '()))
  (setq *error* olderr)
  result
)

;; D) OSNAP manager para captura de puntos
(defun ocmema:with-osnaps (snapList fn / old bits item)
  (setq old (getvar "OSMODE"))
  (setq bits 0)
  (if snapList
    (progn
      (foreach item snapList
        (cond
          ((or (= item "END") (= item 'END)) (setq bits (+ bits 1)))
          ((or (= item "MID") (= item 'MID)) (setq bits (+ bits 2)))
          ((or (= item "PER") (= item 'PER)) (setq bits (+ bits 128)))
          ((or (= item "EXT") (= item 'EXT)) (setq bits (+ bits 1024)))
        )
      )
      (setvar "OSMODE" bits)
    )
  )
  (ocmema:errwrap
    (function (lambda () (apply fn '())))
    (function (lambda (e /) (setvar "OSMODE" old)))
  )
  (setvar "OSMODE" old)
)

;; E) Utilerias de strings (AutoLISP puro)
(defun ocmema:str-trim (s / len start end ch)
  (if (not s)
    ""
    (progn
      (setq len (strlen s))
      (setq start 1)
      (setq end len)
      (while (and (<= start len)
                  (= (substr s start 1) " "))
        (setq start (1+ start))
      )
      (while (and (>= end 1)
                  (= (substr s end 1) " "))
        (setq end (1- end))
      )
      (if (> start end)
        ""
        (substr s start (- end start -1))
      )
    )
  )
)

(defun ocmema:split (s delim / i ch token out)
  (setq out '())
  (if (not s)
    (setq s "")
  )
  (setq token "")
  (setq i 1)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (= ch delim)
      (progn
        (setq out (append out (list token)))
        (setq token "")
      )
      (setq token (strcat token ch))
    )
    (setq i (1+ i))
  )
  (setq out (append out (list token)))
  out
)

(defun ocmema:join (lst delim / out)
  (setq out "")
  (if lst
    (progn
      (setq out (car lst))
      (foreach item (cdr lst)
        (setq out (strcat out delim item))
      )
    )
  )
  out
)

;; F) Conversion segura a numero
(defun ocmema:to-number-safe (s / v)
  (cond
    ((numberp s) s)
    ((= (type s) 'STR)
     (setq v (distof s 2))
     (if v
       v
       (progn
         (setq v (atof s))
         (if (/= v 0.0)
           v
           (if (or (= s "0") (= s "0.0") (= s "0.00"))
             0.0
             nil
           )
         )
       )
     )
    )
    (T nil)
  )
)

;; G) File I/O basico (sin VisualLISP)
(defun ocmema:file-exists-p (path / fh)
  (setq fh (open path "r"))
  (if fh
    (progn
      (close fh)
      T
    )
    nil
  )
)

(defun ocmema:read-lines (path / fh line out)
  (setq out '())
  (setq fh (open path "r"))
  (if fh
    (progn
      (while (setq line (read-line fh))
        (setq out (append out (list line)))
      )
      (close fh)
      out
    )
    nil
  )
)

(defun ocmema:write-lines (path lines / fh)
  (setq fh (open path "w"))
  (if fh
    (progn
      (foreach line lines
        (write-line line fh)
      )
      (close fh)
      T
    )
    nil
  )
)

;; H) Helper generico safe-get de asociacion
(defun ocmema:assoc-get (key alist / pair)
  (setq pair (assoc key alist))
  (if pair (cdr pair) nil)
)

;; I) Final del archivo
(princ)
