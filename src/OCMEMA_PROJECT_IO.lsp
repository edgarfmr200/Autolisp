;; OCMEMA Project IO module (AutoLISP only). No runtime logic on load.

;; Global variables
(setq ocmema:*project* nil)
(setq ocmema:*project-path* nil)
(setq ocmema:*project-io-version* "OCMEMA_PROJECT_V1")
(setq ocmema:*proj-default-dir* "C:\\Users\\edgar\\OneDrive - ITESO\\OCMEMA_IE\\01. PROYECTOS\\2025\\")
(setq ocmema:*beam-replace-name* nil)
(setq ocmema:*beam-force-name* nil)
(setq ocmema:*beam-single* nil)
(setq ocmema:*rib-force-name* nil)
(setq ocmema:*rib-single* nil)

;; TXT keys esperadas (writer actual):
;; VERSION: OCMEMA_PROJECT_V1
;; PROJECT_NAME, N_PLANTS, WALL_CM, NX, NY, X_NAMES, Y_NAMES
;; [PLANT n] ... PLANT_NAME, X_AXES, Y_AXES ... [/PLANT]
(setq ocmema:*proj-debug* nil)

;; Logging (local, no dependencies)
(defun ocmema:proj-log (msg /)
  (princ (strcat "\nOCMEMA: " msg))
)

(defun ocmema:proj-warn (msg /)
  (prompt (strcat "\nOCMEMA WARN: " msg))
)

(defun ocmema:proj-debug (msg /)
  (if ocmema:*proj-debug*
    (prompt (strcat "\nOCMEMA DBG: " msg))
  )
)

(defun ocmema:proj-cancelled (/)
  (ocmema:proj-log "Operacion cancelada.")
)

(defun ocmema:proj-msg (msg /)
  (prompt (strcat "\nOCMEMA: " msg))
)

(defun ocmema:proj-axes-complete-p (/ proj plants nx ny ok plant x_axes y_axes item)
  (setq proj ocmema:*project*)
  (if (not proj)
    nil
    (progn
      (setq nx (ocmema:pio-assoc-get "nx" proj))
      (setq ny (ocmema:pio-assoc-get "ny" proj))
      (setq plants (ocmema:pio-assoc-get "plants" proj))
      (setq ok T)
      (if (or (not nx) (<= nx 0) (not ny) (<= ny 0) (not plants))
        (setq ok nil)
      )
      (foreach plant plants
        (if ok
          (progn
            (setq x_axes (ocmema:pio-assoc-get "x_axes" plant))
            (setq y_axes (ocmema:pio-assoc-get "y_axes" plant))
            (if (or (not x_axes) (not y_axes)
                    (/= (length x_axes) nx)
                    (/= (length y_axes) ny))
              (setq ok nil)
              (progn
                (foreach item x_axes
                  (if ok
                    (if (or (not (car item)) (not (numberp (cdr item))))
                      (setq ok nil)
                    )
                  )
                )
                (foreach item y_axes
                  (if ok
                    (if (or (not (car item)) (not (numberp (cdr item))))
                      (setq ok nil)
                    )
                  )
                )
              )
            )
          )
        )
      )
      ok
    )
  )
)

(defun ocmema:proj-ready-for-generators-p (/)
  (cond
    ((not ocmema:*project*)
      (ocmema:proj-msg "No hay proyecto cargado. Usa Nuevo o Cargar.")
      nil
    )
    ((not (ocmema:proj-axes-complete-p))
      (ocmema:proj-msg "Primero captura ejes (X/Y) para todas las plantas.")
      nil
    )
    (T T)
  )
)

(defun ocmema:proj-get-beams (/)
  (ocmema:pio-assoc-get "beams" ocmema:*project*)
)

(defun ocmema:proj-get-ribs (/)
  (ocmema:pio-assoc-get "ribs" ocmema:*project*)
)

(defun ocmema:proj-rib-name-exists (name / ribs item found norm)
  (setq ribs (ocmema:proj-get-ribs))
  (setq norm (ocmema:pio-normalize-name name))
  (setq found nil)
  (foreach item ribs
    (if (= (ocmema:pio-normalize-name (ocmema:pio-assoc-get "name" item)) norm)
      (setq found T)
    )
  )
  found
)

(defun ocmema:proj-find-rib (name / ribs item found norm)
  (setq ribs (ocmema:proj-get-ribs))
  (setq norm (ocmema:pio-normalize-name name))
  (setq found nil)
  (foreach item ribs
    (if (= (ocmema:pio-normalize-name (ocmema:pio-assoc-get "name" item)) norm)
      (setq found item)
    )
  )
  found
)

(defun ocmema:proj-upsert-rib (rib / proj ribs out item name norm)
  (setq proj ocmema:*project*)
  (setq ribs (ocmema:proj-get-ribs))
  (if (not ribs) (setq ribs '()))
  (setq out '())
  (setq name (ocmema:pio-assoc-get "name" rib))
  (setq norm (ocmema:pio-normalize-name name))
  (foreach item ribs
    (if (= (ocmema:pio-normalize-name (ocmema:pio-assoc-get "name" item)) norm)
      (setq item rib)
    )
    (setq out (append out (list item)))
  )
  (if (not (ocmema:proj-rib-name-exists name))
    (setq out (append out (list rib)))
  )
  (setq proj (ocmema:pio-alist-set "ribs" out proj))
  (setq ocmema:*project* proj)
)

(defun ocmema:proj-get-plant-name (idx / plants plant name)
  (setq plants (ocmema:pio-assoc-get "plants" ocmema:*project*))
  (setq name nil)
  (foreach plant plants
    (if (= (ocmema:pio-assoc-get "idx" plant) idx)
      (setq name (ocmema:pio-assoc-get "name" plant))
    )
  )
  name
)

(defun ocmema:proj-beam-name-exists (name / beams item found norm)
  (setq beams (ocmema:proj-get-beams))
  (setq norm (ocmema:pio-normalize-name name))
  (setq found nil)
  (foreach item beams
    (if (= (ocmema:pio-normalize-name (ocmema:pio-assoc-get "name" item)) norm)
      (setq found T)
    )
  )
  found
)

(defun ocmema:proj-find-beam (name / beams item found norm)
  (setq beams (ocmema:proj-get-beams))
  (setq norm (ocmema:pio-normalize-name name))
  (setq found nil)
  (foreach item beams
    (if (= (ocmema:pio-normalize-name (ocmema:pio-assoc-get "name" item)) norm)
      (setq found item)
    )
  )
  found
)

(defun ocmema:proj-add-beam (beam / proj beams)
  (setq proj ocmema:*project*)
  (setq beams (ocmema:proj-get-beams))
  (if (not beams) (setq beams '()))
  (setq beams (append beams (list beam)))
  (setq proj (ocmema:pio-alist-set "beams" beams proj))
  (setq ocmema:*project* proj)
)

(defun ocmema:proj-upsert-beam (beam / proj beams out item name norm)
  (setq proj ocmema:*project*)
  (setq beams (ocmema:proj-get-beams))
  (if (not beams) (setq beams '()))
  (setq out '())
  (setq name (ocmema:pio-assoc-get "name" beam))
  (setq norm (ocmema:pio-normalize-name name))
  (foreach item beams
    (if (= (ocmema:pio-normalize-name (ocmema:pio-assoc-get "name" item)) norm)
      (setq item beam)
    )
    (setq out (append out (list item)))
  )
  (if (not (ocmema:proj-beam-name-exists name))
    (setq out (append out (list beam)))
  )
  (setq proj (ocmema:pio-alist-set "beams" out proj))
  (setq ocmema:*project* proj)
)

(defun ocmema:proj-update-beam-points (name points / proj beams out item)
  (setq proj ocmema:*project*)
  (setq beams (ocmema:proj-get-beams))
  (setq out '())
  (foreach item beams
    (if (= (ocmema:pio-normalize-name (ocmema:pio-assoc-get "name" item))
           (ocmema:pio-normalize-name name))
      (progn
        (setq item (ocmema:pio-alist-set "points_raw" points item))
        (setq item (ocmema:pio-alist-set "n_points" (length points) item))
      )
    )
    (setq out (append out (list item)))
  )
  (setq proj (ocmema:pio-alist-set "beams" out proj))
  (setq ocmema:*project* proj)
)

(defun ocmema:proj-get-units (/ proj unit)
  (setq proj ocmema:*project*)
  (setq unit (ocmema:pio-assoc-get "units" proj))
  (if (and unit (/= unit "")) unit nil)
)

(defun ocmema:proj-get-scale (/ proj sc)
  (setq proj ocmema:*project*)
  (setq sc (ocmema:pio-assoc-get "scale" proj))
  (if (numberp sc) sc nil)
)

(defun ocmema:proj-set-units-scale (unit scale / proj)
  (setq proj ocmema:*project*)
  (setq proj (ocmema:pio-alist-set "units" unit proj))
  (setq proj (ocmema:pio-alist-set "scale" scale proj))
  (setq ocmema:*project* proj)
)

(defun ocmema:proj-unit-factor (unit / u)
  (setq u (strcase unit))
  (cond
    ((= u "M") 100.0)
    ((= u "CM") 1.0)
    ((= u "C") 1.0)
    ((= u "MM") 0.1)
    (T 1.0)
  )
)

(defun ocmema:proj-capture-points (/ old pts pt idx done)
  (setq old (getvar "OSMODE"))
  (setvar "OSMODE" 2179)
  (setq pts '())
  (setq idx 1)
  (setq pt (getpoint (strcat "\nPunto " (itoa idx) ": ")))
  (if pt
    (progn
      (setq pts (append pts (list pt)))
      (setq idx (1+ idx))
      (setq done nil)
      (while (not done)
        (setq pt (getpoint (strcat "\nPunto " (itoa idx) " <Enter terminar>: ")))
        (cond
          (pt
           (setq pts (append pts (list pt)))
           (setq idx (1+ idx))
          )
          ((>= (length pts) 2) (setq done T))
          (T (ocmema:proj-log "Se requieren al menos 2 puntos."))
        )
      )
    )
  )
  (setvar "OSMODE" old)
  (if (>= (length pts) 2) pts nil)
)

(defun ocmema:proj-vec-sub (a b)
  (list (- (car a) (car b)) (- (cadr a) (cadr b)) (- (caddr a) (caddr b)))
)

(defun ocmema:proj-vec-dot (a b)
  (+ (* (car a) (car b)) (* (cadr a) (cadr b)) (* (caddr a) (caddr b)))
)

(defun ocmema:proj-vec-len (a)
  (sqrt (+ (* (car a) (car a)) (* (cadr a) (cadr a)) (* (caddr a) (caddr a))))
)

(defun ocmema:proj-vec-unit (a / len)
  (setq len (ocmema:proj-vec-len a))
  (if (and len (> len 0.0))
    (list (/ (car a) len) (/ (cadr a) len) (/ (caddr a) len))
    nil
  )
)

(defun ocmema:proj-points-to-local-x (points unit / origin dir unitv factor out p v proj)
  (setq origin (car points))
  (setq dir (ocmema:proj-vec-sub (cadr points) origin))
  (setq unitv (ocmema:proj-vec-unit dir))
  (setq factor (ocmema:proj-unit-factor unit))
  (setq out '())
  (if unitv
    (foreach p points
      (setq v (ocmema:proj-vec-sub p origin))
      (setq proj (ocmema:proj-vec-dot v unitv))
      (setq out (append out (list (* proj factor))))
    )
  )
  out
)

(defun ocmema:proj-std-data-line-p (line / t first)
  (setq t (ocmema:str-trim line))
  (if (> (strlen t) 0)
    (progn
      (setq first (substr t 1 1))
      (wcmatch first "[0-9]")
    )
    nil
  )
)

(defun ocmema:proj-std-find-header (lines header / idx found)
  (setq idx 0)
  (setq found -1)
  (foreach line lines
    (if (and (= found -1) (wcmatch (strcase line) (strcase (strcat "*" header "*"))))
      (setq found idx)
    )
    (setq idx (1+ idx))
  )
  found
)

(defun ocmema:proj-std-replace-block (lines header newlines / start idx end)
  (setq start (ocmema:proj-std-find-header lines header))
  (if (< start 0)
    nil
    (progn
      (setq idx (1+ start))
      (setq end idx)
      (while (and (< end (length lines)) (ocmema:proj-std-data-line-p (nth end lines)))
        (setq end (1+ end))
      )
      (append
        (ocmema:pio-sublist lines 0 (1+ start))
        newlines
        (ocmema:pio-sublist lines end (length lines))
      )
    )
  )
)

(defun ocmema:proj-update-std-geometry (path points unit / lines xs nodes members i xval newlines)
  (setq lines (ocmema:pio-read-lines path))
  (if (not lines)
    nil
    (progn
      (setq xs (ocmema:proj-points-to-local-x points unit))
      (setq newlines '())
      (setq i 1)
      (foreach xval xs
        (setq newlines (append newlines (list (strcat (itoa i) " " (rtos xval 2 2) " 0 0;"))))
        (setq i (1+ i))
      )
      (setq lines (ocmema:proj-std-replace-block lines "JOINT COORDINATES" newlines))
      (if (not lines)
        nil
        (progn
          (setq newlines '())
          (setq i 1)
          (while (< i (length xs))
            (setq newlines (append newlines (list (strcat (itoa i) " " (itoa i) " " (itoa (1+ i)) ";"))))
            (setq i (1+ i))
          )
          (setq lines (ocmema:proj-std-replace-block lines "MEMBER INCIDENCES" newlines))
          (if (not lines)
            nil
            (ocmema:pio-write-lines path lines)
          )
        )
      )
    )
  )
)

(defun ocmema:proj-ensure-generators-loaded (/ this src-dir)
  (vl-load-com)
  (setq this (findfile "OCMEMA_PROJECT_IO.lsp"))
  (if this
    (progn
      (setq src-dir (vl-filename-directory this))
      (load (strcat src-dir "\\GEN_TRABES.lsp") nil)
      (load (strcat src-dir "\\GEN_NERV.lsp") nil)
    )
  )
)

(defun ocmema:proj-run-generator (cmd / olderr msg)
  (setq olderr *error*)
  (defun *error* (msg)
    (if (and msg (wcmatch (strcase msg) "*CANCEL*"))
      (princ "\nOCMEMA: Cancelado.")
    )
    (if olderr (setq *error* olderr))
    (princ "\nOCMEMA: Regresando al menu...")
    (ocmema:menu-general)
    (princ)
  )
  (command cmd)
  (if olderr (setq *error* olderr))
  T
)

(defun ocmema:proj-run-gen-trabes (/ olderr msg)
  (setq olderr *error*)
  (defun *error* (msg)
    (if (and msg (wcmatch (strcase msg) "*CANCEL*"))
      (princ "\nOCMEMA: Cancelado.")
    )
    (if olderr (setq *error* olderr))
    (setq ocmema:*beam-replace-name* nil)
    (setq ocmema:*beam-force-name* nil)
    (setq ocmema:*beam-single* nil)
    (princ "\nOCMEMA: Regresando al menu...")
    (ocmema:menu-general)
    (princ)
  )
  (C:GEN_TRABES)
  (if olderr (setq *error* olderr))
  (setq ocmema:*beam-replace-name* nil)
  (setq ocmema:*beam-force-name* nil)
  (setq ocmema:*beam-single* nil)
  T
)

(defun ocmema:proj-run-gen-nerv (/ olderr msg)
  (setq olderr *error*)
  (defun *error* (msg)
    (if (and msg (wcmatch (strcase msg) "*CANCEL*"))
      (princ "\nOCMEMA: Cancelado.")
    )
    (if olderr (setq *error* olderr))
    (setq ocmema:*rib-force-name* nil)
    (setq ocmema:*rib-single* nil)
    (princ "\nOCMEMA: Regresando al menu...")
    (ocmema:menu-general)
    (princ)
  )
  (C:GEN_NERV)
  (if olderr (setq *error* olderr))
  (setq ocmema:*rib-force-name* nil)
  (setq ocmema:*rib-single* nil)
  T
)

(defun ocmema:pio-int-str (v /)
  (if (numberp v) (itoa v) "nil")
)

(defun ocmema:pio-debug-project (proj / plants plant cx cy)
  (if ocmema:*proj-debug*
    (progn
      (prompt (strcat "\nOCMEMA DBG: version=" ocmema:*project-io-version*))
      (prompt (strcat "\nOCMEMA DBG: project_name=" (ocmema:pio-assoc-get "project_name" proj)))
      (prompt (strcat "\nOCMEMA DBG: n_plants=" (ocmema:pio-int-str (ocmema:pio-assoc-get "n_plants" proj))))
      (prompt
        (strcat
          "\nOCMEMA DBG: n_axes_x=" (ocmema:pio-int-str (ocmema:pio-assoc-get "nx" proj))
          " n_axes_y=" (ocmema:pio-int-str (ocmema:pio-assoc-get "ny" proj))
        )
      )
      (setq plants (ocmema:pio-assoc-get "plants" proj))
      (foreach plant plants
        (setq cx (length (ocmema:pio-assoc-get "x_axes" plant)))
        (setq cy (length (ocmema:pio-assoc-get "y_axes" plant)))
        (prompt
          (strcat
            "\nOCMEMA DBG: PLANT "
            (ocmema:pio-int-str (ocmema:pio-assoc-get "idx" plant))
            " X_AXES=" (ocmema:pio-int-str cx)
            " Y_AXES=" (ocmema:pio-int-str cy)
          )
        )
      )
    )
  )
)

;; Auto-save with default directory
(defun ocmema:proj-default-path (/ proj pname fname)
  (setq proj ocmema:*project*)
  (setq pname (ocmema:pio-assoc-get "project_name" proj))
  (if (or (not pname) (= pname ""))
    (setq fname "OCMEMA_PROJECT.txt")
    (setq fname (strcat pname "_OCMEMA_PROJECT.txt"))
  )
  (strcat ocmema:*proj-default-dir* fname)
)

(defun ocmema:proj-autosave (/ path ok)
  (if (not ocmema:*project*)
    nil
    (progn
      (setq path (if ocmema:*project-path* ocmema:*project-path* (ocmema:proj-default-path)))
      (setq ok (ocmema:pio-save-project path))
      (if ok
        (ocmema:proj-log "Proyecto guardado.")
        (ocmema:proj-log "No se pudo guardar el proyecto.")
      )
      ok
    )
  )
)

;; File dialogs (local, no VisualLISP)
(defun ocmema:proj-getfile-open (prompt / f)
  (setq f (getfiled prompt (strcat ocmema:*proj-default-dir* "OCMEMA_PROJECT.txt") "txt" 0))
  f
)

(defun ocmema:proj-getfile-save (prompt suggest / f)
  (setq f (getfiled prompt (strcat ocmema:*proj-default-dir* suggest) "txt" 1))
  f
)

;; Basic string helpers
(defun ocmema:pio-str-trim (s / len start end)
  (if (not s)
    ""
    (progn
      (setq len (strlen s))
      (setq start 1)
      (setq end len)
      (while (and (<= start len) (= (substr s start 1) " "))
        (setq start (1+ start))
      )
      (while (and (>= end 1) (= (substr s end 1) " "))
        (setq end (1- end))
      )
      (if (> start end)
        ""
        (substr s start (- end start -1))
      )
    )
  )
)

(defun ocmema:str-trim (s / len start end ch)
  (if (not s)
    ""
    (progn
      (setq len (strlen s))
      (setq start 1)
      (setq end len)
      (while (and (<= start len)
                  (or (= (substr s start 1) " ")
                      (= (substr s start 1) "\t")
                      (= (substr s start 1) "\r")
                      (= (substr s start 1) "\n")))
        (setq start (1+ start))
      )
      (while (and (>= end 1)
                  (or (= (substr s end 1) " ")
                      (= (substr s end 1) "\t")
                      (= (substr s end 1) "\r")
                      (= (substr s end 1) "\n")))
        (setq end (1- end))
      )
      (if (> start end)
        ""
        (substr s start (- end start -1))
      )
    )
  )
)
(defun ocmema:pio-split (s delim / i ch token out)
  (setq out '())
  (if (not s) (setq s ""))
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

(defun ocmema:pio-split-list (s delim / raw out item)
  (setq out '())
  (setq raw (ocmema:pio-split s delim))
  (foreach item raw
    (setq item (ocmema:str-trim item))
    (if (/= item "")
      (setq out (append out (list item)))
    )
  )
  out
)

(defun ocmema:pio-join (lst delim / out)
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

(defun ocmema:pio-sublist (lst start end / out idx)
  (setq out '())
  (setq idx 0)
  (foreach item lst
    (if (and (>= idx start) (< idx end))
      (setq out (append out (list item)))
    )
    (setq idx (1+ idx))
  )
  out
)

(defun ocmema:pio-sanitize-name (s / i ch out)
  (setq out "")
  (setq i 1)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (or (= ch ":") (= ch ","))
      (setq out (strcat out "-"))
      (setq out (strcat out ch))
    )
    (setq i (1+ i))
  )
  out
)

;; Name normalization (trim + case-insensitive)
(defun ocmema:pio-normalize-name (s / trim)
  (setq trim (ocmema:str-trim s))
  (if (= trim "") "" (strcase trim))
)

(defun ocmema:pio-name-exists-ci (lst name / norm item found)
  (setq norm (ocmema:pio-normalize-name name))
  (setq found nil)
  (if (/= norm "")
    (foreach item lst
      (if (= (ocmema:pio-normalize-name item) norm)
        (setq found T)
      )
    )
  )
  found
)

(defun ocmema:pio-find-name-ci (lst name / norm item found)
  (setq norm (ocmema:pio-normalize-name name))
  (setq found nil)
  (if (/= norm "")
    (foreach item lst
      (if (= (ocmema:pio-normalize-name item) norm)
        (setq found item)
      )
    )
  )
  found
)

(defun ocmema:pio-getint-min (prompt minval / v done)
  (setq done nil)
  (setq v nil)
  (while (not done)
    (setq v (getint prompt))
    (cond
      ((not v) (setq done T v nil))
      ((< v minval) (ocmema:proj-log "Valor invalido."))
      (T (setq done T))
    )
  )
  v
)

(defun ocmema:pio-getreal-min (prompt minval / v done)
  (setq done nil)
  (setq v nil)
  (while (not done)
    (setq v (getreal prompt))
    (cond
      ((not v) (setq done T v nil))
      ((<= v minval) (ocmema:proj-log "Valor invalido."))
      (T (setq done T))
    )
  )
  v
)

(defun ocmema:pio-get-nonempty-string (prompt / v trim done)
  (setq done nil)
  (setq trim nil)
  (while (not done)
    (setq v (getstring T prompt))
    (cond
      ((not v) (setq done T trim nil))
      (T
        (setq trim (ocmema:str-trim v))
        (if (= trim "")
          (ocmema:proj-log "Nombre invalido.")
          (setq done T)
        )
      )
    )
  )
  trim
)

(defun ocmema:pio-get-unique-axis-name (prompt names / v trim done)
  (setq done nil)
  (setq trim nil)
  (while (not done)
    (setq v (getstring T prompt))
    (cond
      ((not v) (setq done T trim nil))
      (T
        (setq trim (ocmema:str-trim v))
        (cond
          ((= trim "") (ocmema:proj-log "Nombre de eje invalido."))
          ((ocmema:pio-name-exists-ci names trim)
           (ocmema:proj-log "Nombre de eje duplicado.")
          )
          (T (setq done T))
        )
      )
    )
  )
  trim
)

;; Points serialization for BEAM entries
(defun ocmema:pio-point-to-string (pt / x y z)
  (setq x (rtos (car pt) 2 8))
  (setq y (rtos (cadr pt) 2 8))
  (if (and (>= (length pt) 3) (numberp (caddr pt)))
    (setq z (rtos (caddr pt) 2 8))
    (setq z "0.0")
  )
  (strcat x "," y "," z)
)

(defun ocmema:pio-points-to-string (pts / out)
  (setq out '())
  (foreach pt pts
    (setq out (append out (list (ocmema:pio-point-to-string pt))))
  )
  (ocmema:pio-join out ";")
)

(defun ocmema:pio-point2d-to-string (pt / x y)
  (setq x (rtos (car pt) 2 8))
  (setq y (rtos (cadr pt) 2 8))
  (strcat x "," y)
)

(defun ocmema:pio-points2d-to-string (pts / out)
  (setq out '())
  (foreach pt pts
    (setq out (append out (list (ocmema:pio-point2d-to-string pt))))
  )
  (ocmema:pio-join out ";")
)

(defun ocmema:pio-parse-point (s / parts x y z)
  (setq parts (ocmema:pio-split s ","))
  (if (>= (length parts) 2)
    (progn
      (setq x (ocmema:pio-to-number (ocmema:str-trim (nth 0 parts))))
      (setq y (ocmema:pio-to-number (ocmema:str-trim (nth 1 parts))))
      (setq z (if (>= (length parts) 3)
                (ocmema:pio-to-number (ocmema:str-trim (nth 2 parts)))
                0.0
              )
      )
      (if (and x y)
        (list x y (if z z 0.0))
        nil
      )
    )
    nil
  )
)

(defun ocmema:pio-parse-points (s / items out pt)
  (setq out '())
  (if (and s (/= s ""))
    (progn
      (setq items (ocmema:pio-split s ";"))
      (foreach item items
        (setq pt (ocmema:pio-parse-point (ocmema:str-trim item)))
        (if pt (setq out (append out (list pt))))
      )
    )
  )
  out
)

(defun ocmema:pio-parse-point2d (s / parts x y)
  (setq parts (ocmema:pio-split s ","))
  (if (>= (length parts) 2)
    (progn
      (setq x (ocmema:pio-to-number (ocmema:str-trim (nth 0 parts))))
      (setq y (ocmema:pio-to-number (ocmema:str-trim (nth 1 parts))))
      (if (and x y)
        (list x y 0.0)
        nil
      )
    )
    nil
  )
)

(defun ocmema:pio-parse-points2d (s / items out pt)
  (setq out '())
  (if (and s (/= s ""))
    (progn
      (setq items (ocmema:pio-split s ";"))
      (foreach item items
        (setq pt (ocmema:pio-parse-point2d (ocmema:str-trim item)))
        (if pt (setq out (append out (list pt))))
      )
    )
  )
  out
)

;; Key/Value parsing
(defun ocmema:pio-split-kv (line / i len ch pos key val)
  (setq len (strlen line))
  (setq pos 0)
  (setq i 1)
  (while (and (<= i len) (= pos 0))
    (setq ch (substr line i 1))
    (if (= ch "=") (setq pos i))
    (setq i (1+ i))
  )
  (if (= pos 0)
    (progn
      (setq i 1)
      (while (and (<= i len) (= pos 0))
        (setq ch (substr line i 1))
        (if (= ch ":") (setq pos i))
        (setq i (1+ i))
      )
    )
  )
  (if (> pos 0)
    (progn
      (setq key (ocmema:str-trim (substr line 1 (1- pos))))
      (setq val (ocmema:str-trim (substr line (1+ pos) (- len pos))))
      (if (= key "")
        nil
        (list key val)
      )
    )
    nil
  )
)

;; Simple number conversion
(defun ocmema:pio-to-number (s / v)
  (cond
    ((numberp s) s)
    ((= (type s) 'STR)
     (setq v (distof s 2))
     (if v v nil)
    )
    (T nil)
  )
)

;; Assoc helpers
(defun ocmema:pio-assoc-get (key alist / pair)
  (setq pair (assoc key alist))
  (if pair (cdr pair) nil)
)

(defun ocmema:pio-alist-set (key val alist / pair)
  (setq pair (assoc key alist))
  (if pair
    (subst (cons key val) pair alist)
    (append alist (list (cons key val)))
  )
)

;; Sorting helper (bubble sort by coord, no recursion)
(defun ocmema:pio-swap (lst i j / idx out)
  (setq out '())
  (setq idx 0)
  (while (< idx (length lst))
    (cond
      ((= idx i) (setq out (append out (list (nth j lst)))))
      ((= idx j) (setq out (append out (list (nth i lst)))))
      (T (setq out (append out (list (nth idx lst)))))
    )
    (setq idx (1+ idx))
  )
  out
)

(defun ocmema:pio-sort-pairs (lst / out swapped i len a b)
  (setq out lst)
  (setq len (length out))
  (if (> len 1)
    (progn
      (setq swapped T)
      (while swapped
        (setq swapped nil)
        (setq i 0)
        (while (< i (1- len))
          (setq a (nth i out))
          (setq b (nth (1+ i) out))
          (if (> (cdr a) (cdr b))
            (progn
              (setq out (ocmema:pio-swap out i (1+ i)))
              (setq swapped T)
            )
          )
          (setq i (1+ i))
        )
      )
    )
  )
  out
)

;; File I/O (local, no VisualLISP)
(defun ocmema:pio-read-lines (path / fh line out)
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

(defun ocmema:pio-write-lines (path lines / fh)
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

;; Axes parsing helpers
(defun ocmema:pio-parse-axes (s / parts out pair kv name coord)
  (setq out '())
  (if (and s (/= s ""))
    (progn
      (setq parts (ocmema:pio-split-list s ","))
      (foreach pair parts
        (setq kv (ocmema:pio-split pair ":"))
        (if (>= (length kv) 2)
          (progn
            (setq name (ocmema:str-trim (car kv)))
            (setq coord (ocmema:pio-to-number (ocmema:str-trim (cadr kv))))
            (if (and (/= name "") coord)
              (setq out (append out (list (cons name coord))))
            )
          )
        )
      )
    )
  )
  out
)

(defun ocmema:pio-axes-to-string (axes / out item name coord)
  (setq out '())
  (foreach item axes
    (setq name (ocmema:pio-sanitize-name (car item)))
    (setq coord (rtos (cdr item) 2 8))
    (setq out (append out (list (strcat name ":" coord))))
  )
  (ocmema:pio-join out ",")
)

;; Osnap point getter (END+MID+PERP+EXT)
(defun ocmema:pio-getpoint (prompt / old pt)
  (setq old (getvar "OSMODE"))
  (setvar "OSMODE" 2179)
  (setq pt (getpoint prompt))
  (setvar "OSMODE" old)
  pt
)

;; Capture axes for all plants
(defun ocmema:pio-capture-all-axes (/ proj plants xnames ynames newplants plant pname x_axes y_axes pt cancel idx total abort)
  (setq proj ocmema:*project*)
  (if (not proj)
    (progn (ocmema:proj-log "No hay proyecto cargado.") nil)
    (progn
      (setq plants (ocmema:pio-assoc-get "plants" proj))
      (setq xnames (ocmema:pio-assoc-get "x_names" proj))
      (setq ynames (ocmema:pio-assoc-get "y_names" proj))
      (setq newplants '())
      (setq idx 0)
      (setq total (length plants))
      (setq abort nil)
      (while (and (< idx total) (not abort))
        (setq plant (nth idx plants))
        (setq pname (ocmema:pio-assoc-get "name" plant))
        (setq x_axes '())
        (setq y_axes '())
        (setq cancel nil)
        (foreach nm xnames
          (if (not cancel)
            (progn
              (setq pt (ocmema:pio-getpoint (strcat "\n" pname " - Eje X " nm ": ")))
              (if pt
                (setq x_axes (append x_axes (list (cons nm (car pt)))))
                (setq cancel T)
              )
            )
          )
        )
        (foreach nm ynames
          (if (not cancel)
            (progn
              (setq pt (ocmema:pio-getpoint (strcat "\n" pname " - Eje Y " nm ": ")))
              (if pt
                (setq y_axes (append y_axes (list (cons nm (cadr pt)))))
                (setq cancel T)
              )
            )
          )
        )
        (if cancel
          (progn
            (ocmema:proj-log "Captura cancelada.")
            (setq abort T)
          )
          (progn
            (setq x_axes (ocmema:pio-sort-pairs x_axes))
            (setq y_axes (ocmema:pio-sort-pairs y_axes))
            (setq plant (ocmema:pio-alist-set "x_axes" x_axes plant))
            (setq plant (ocmema:pio-alist-set "y_axes" y_axes plant))
            (setq newplants (append newplants (list plant)))
          )
        )
        (setq idx (1+ idx))
      )
      (if (and newplants (not abort))
        (progn
          (setq ocmema:*project* (ocmema:pio-alist-set "plants" newplants proj))
          ocmema:*project*
        )
        nil
      )
    )
  )
)

;; Save project
(defun ocmema:pio-temp-path (path / len base)
  (setq len (strlen path))
  (if (and (>= len 4) (= (strcase (substr path (- len 3) 4)) ".TXT"))
    (setq base (substr path 1 (- len 4)))
    (setq base path)
  )
  (strcat base "_tmp.txt")
)

(defun ocmema:pio-bak-path (path / len base)
  (setq len (strlen path))
  (if (and (>= len 4) (= (strcase (substr path (- len 3) 4)) ".TXT"))
    (setq base (substr path 1 (- len 4)))
    (setq base path)
  )
  (strcat base "_bak.txt")
)

(defun ocmema:pio-atomic-replace (path tmp / bak)
  (vl-load-com)
  (setq bak (ocmema:pio-bak-path path))
  (if (findfile path)
    (progn
      (if (findfile bak) (vl-file-delete bak))
      (if (not (vl-file-rename path bak))
        nil
        (if (vl-file-rename tmp path)
          (progn
            (if (findfile bak) (vl-file-delete bak))
            T
          )
          (progn
            (if (findfile bak) (vl-file-rename bak path))
            nil
          )
        )
      )
    )
    (if (vl-file-rename tmp path) T nil)
  )
)

(defun ocmema:pio-save-project (path / proj lines plants plant x_axes y_axes pname xnames ynames tmp ok beams beam idx ribs rib)
  (setq proj ocmema:*project*)
  (if (not proj)
    (progn (ocmema:proj-log "No hay proyecto cargado.") nil)
    (progn
      (setq lines '())
      (setq lines (append lines (list ocmema:*project-io-version*)))
      (setq lines (append lines (list (strcat "PROJECT_NAME=" (ocmema:pio-assoc-get "project_name" proj)))))
      (setq lines (append lines (list (strcat "N_PLANTS=" (itoa (ocmema:pio-assoc-get "n_plants" proj))))))
      (setq lines (append lines (list (strcat "WALL_CM=" (rtos (ocmema:pio-assoc-get "wall_cm" proj) 2 8)))))
      (setq lines (append lines (list (strcat "NX=" (itoa (ocmema:pio-assoc-get "nx" proj))))))
      (setq lines (append lines (list (strcat "NY=" (itoa (ocmema:pio-assoc-get "ny" proj))))))
      (setq xnames (ocmema:pio-assoc-get "x_names" proj))
      (setq ynames (ocmema:pio-assoc-get "y_names" proj))
      (setq lines (append lines (list (strcat "X_NAMES=" (ocmema:pio-join xnames ",")))))
      (setq lines (append lines (list (strcat "Y_NAMES=" (ocmema:pio-join ynames ",")))))
      (if (or (ocmema:pio-assoc-get "units" proj) (ocmema:pio-assoc-get "scale" proj))
        (progn
          (setq lines (append lines (list "[UNITS]")))
          (if (ocmema:pio-assoc-get "units" proj)
            (setq lines (append lines (list (strcat "UNITS=" (ocmema:pio-assoc-get "units" proj)))))
          )
          (if (ocmema:pio-assoc-get "scale" proj)
            (setq lines (append lines (list (strcat "SCALE=" (rtos (ocmema:pio-assoc-get "scale" proj) 2 6)))))
          )
          (setq lines (append lines (list "[/UNITS]")))
        )
      )
      (setq plants (ocmema:pio-assoc-get "plants" proj))
      (foreach plant plants
        (setq lines (append lines (list (strcat "[PLANT " (itoa (ocmema:pio-assoc-get "idx" plant)) "]"))))
        (setq pname (ocmema:pio-assoc-get "name" plant))
        (setq lines (append lines (list (strcat "PLANT_NAME=" pname))))
        (setq x_axes (ocmema:pio-assoc-get "x_axes" plant))
        (setq y_axes (ocmema:pio-assoc-get "y_axes" plant))
        (setq lines (append lines (list (strcat "X_AXES=" (ocmema:pio-axes-to-string x_axes)))))
        (setq lines (append lines (list (strcat "Y_AXES=" (ocmema:pio-axes-to-string y_axes)))))
        (setq lines (append lines (list "[/PLANT]")))
      )
      (setq beams (ocmema:pio-assoc-get "beams" proj))
      (if beams
        (progn
          (setq lines (append lines (list "[BEAMS]")))
          (foreach beam beams
            (setq lines
              (append lines
                (list
                  (strcat
                    "B|"
                    (ocmema:pio-assoc-get "name" beam)
                    "|"
                    (itoa (ocmema:pio-assoc-get "n_points" beam))
                    "|"
                    (ocmema:pio-points2d-to-string (ocmema:pio-assoc-get "points_raw" beam))
                  )
                )
              )
            )
          )
          (setq lines (append lines (list "[/BEAMS]")))
        )
      )
      (setq ribs (ocmema:pio-assoc-get "ribs" proj))
      (if ribs
        (progn
          (setq lines (append lines (list "[RIBS]")))
          (foreach rib ribs
            (setq lines
              (append lines
                (list
                  (strcat
                    "N|"
                    (ocmema:pio-assoc-get "name" rib)
                    "|"
                    (ocmema:pio-assoc-get "dir" rib)
                    "|"
                    (rtos (ocmema:pio-assoc-get "spacing" rib) 2 6)
                    "|"
                    (itoa (ocmema:pio-assoc-get "n_clear" rib))
                  )
                )
              )
            )
          )
          (setq lines (append lines (list "[/RIBS]")))
        )
      )
      (setq tmp (ocmema:pio-temp-path path))
      (if (ocmema:pio-write-lines tmp lines)
        (progn
          (setq ok (ocmema:pio-atomic-replace path tmp))
          (if ok (setq ocmema:*project-path* path))
          ok
        )
        nil
      )
    )
  )
)

;; Load project
(defun ocmema:pio-load-project-lines (lines / len i line vline key val kv
                                            in-plant plant-index plant-name x-axes y-axes
                                            in-beam beam-index beam-name beam-plant-idx beam-plant-name
                                            beam-npoints beam-unit beam-std-path beam-points
                                            in-units units scale
                                            in-beams in-ribs
                                            ribs rib-name rib-dir rib-spacing rib-nclear
                                            plants beams nplants nx ny wall xnames ynames projname proj-beam-unit)
  (setq len (length lines))
  (setq i 0)
  (setq vline nil)

  ;; buscar primera linea no vacia / no comentario como version
  (while (and (< i len) (not vline))
    (setq line (ocmema:str-trim (nth i lines)))
    (if (and line (/= line "")
             (/= (substr line 1 1) ";")
             (/= (substr line 1 1) "#"))
      (setq vline line)
    )
    (setq i (1+ i))
  )

  (if (not vline)
    (progn (ocmema:proj-log "TXT invalido: sin version.") nil)
    (if (/= vline ocmema:*project-io-version*)
      (progn (ocmema:proj-log (strcat "TXT invalido: version no coincide (" vline ")")) nil)
      (progn
        (setq in-plant nil)
        (setq plant-index 0)
        (setq plant-name "")
        (setq x-axes '())
        (setq y-axes '())
        (setq in-beam nil)
        (setq beam-index 0)
        (setq beam-name "")
        (setq beam-plant-idx 0)
        (setq beam-plant-name "")
        (setq beam-npoints 0)
        (setq beam-unit "")
        (setq beam-std-path "")
        (setq beam-points '())
        (setq in-units nil)
        (setq units nil)
        (setq scale nil)
        (setq in-beams nil)
        (setq in-ribs nil)
        (setq rib-name "")
        (setq rib-dir "")
        (setq rib-spacing 0.0)
        (setq rib-nclear 0)
        (setq plants '())
        (setq beams '())
        (setq ribs '())
        (setq nplants 0)
        (setq nx 0)
        (setq ny 0)
        (setq wall 0.0)
        (setq xnames '())
        (setq ynames '())
        (setq projname "")
        (setq proj-beam-unit nil)

        (while (< i len)
          (setq line (ocmema:str-trim (nth i lines)))
          (cond
            ((= line "") nil)
            ((or (= (substr line 1 1) ";") (= (substr line 1 1) "#")) nil)
            ((and (= (substr line 1 1) "[") (= (substr line (strlen line) 1) "]"))
             (setq key (ocmema:str-trim (substr line 2 (- (strlen line) 2))))
             (cond
               ((= (strcase key) "/PLANT")
                (if (not in-plant)
                  (ocmema:proj-warn "Se encontro [/PLANT] sin bloque abierto.")
                  (progn
                    (setq plants
                      (append plants
                        (list
                          (list
                            (cons "idx" plant-index)
                            (cons "name" plant-name)
                            (cons "x_axes" x-axes)
                            (cons "y_axes" y-axes)
                            (cons 'PLANT_INDEX plant-index)
                            (cons 'PLANT_NAME plant-name)
                            (cons 'X_AXES x-axes)
                            (cons 'Y_AXES y-axes)
                          )
                        )
                      )
                    )
                    (setq in-plant nil)
                    (setq plant-index 0)
                    (setq plant-name "")
                    (setq x-axes '())
                    (setq y-axes '())
                  )
                )
               )
               ((= (strcase key) "/UNITS")
                (setq in-units nil)
               )
               ((= (strcase key) "/BEAMS")
                (setq in-beams nil)
               )
               ((= (strcase key) "/RIBS")
                (setq in-ribs nil)
               )
               ((= (strcase key) "/BEAM")
                (if (not in-beam)
                  (ocmema:proj-warn "Se encontro [/BEAM] sin bloque abierto.")
                  (progn
                    (setq beams
                      (append beams
                        (list
                          (list
                            (cons "idx" beam-index)
                            (cons "name" beam-name)
                            (cons "plant_idx" beam-plant-idx)
                            (cons "plant_name" beam-plant-name)
                            (cons "n_points" beam-npoints)
                            (cons "unit" beam-unit)
                            (cons "std_path" beam-std-path)
                            (cons "points_raw" beam-points)
                          )
                        )
                      )
                    )
                    (setq in-beam nil)
                    (setq beam-index 0)
                    (setq beam-name "")
                    (setq beam-plant-idx 0)
                    (setq beam-plant-name "")
                    (setq beam-npoints 0)
                    (setq beam-unit "")
                    (setq beam-std-path "")
                    (setq beam-points '())
                  )
                )
               )
               ((wcmatch (strcase key) "PLANT*")
                (if in-plant
                  (ocmema:proj-warn "Se encontro nuevo [PLANT] antes de cerrar el anterior.")
                )
                (setq plant-index 0)
                (setq plant-name "")
                (setq x-axes '())
                (setq y-axes '())
                (setq in-plant T)
                (setq val (ocmema:str-trim (substr key 6)))
                (if (/= val "")
                  (setq plant-index (atoi val))
                )
                (if (<= plant-index 0)
                  (setq plant-index (1+ (length plants)))
                )
               )
               ((= (strcase key) "UNITS")
                (setq in-units T)
               )
               ((= (strcase key) "BEAMS")
                (setq in-beams T)
               )
               ((= (strcase key) "RIBS")
                (setq in-ribs T)
               )
               ((wcmatch (strcase key) "BEAM*")
                (if in-beam
                  (ocmema:proj-warn "Se encontro nuevo [BEAM] antes de cerrar el anterior.")
                )
                (setq beam-index 0)
                (setq beam-name "")
                (setq beam-plant-idx 0)
                (setq beam-plant-name "")
                (setq beam-npoints 0)
                (setq beam-unit "")
                (setq beam-std-path "")
                (setq beam-points '())
                (setq in-beam T)
                (setq val (ocmema:str-trim (substr key 5)))
                (if (/= val "")
                  (setq beam-index (atoi val))
                )
                (if (<= beam-index 0)
                  (setq beam-index (1+ (length beams)))
                )
               )
               (T
                (ocmema:proj-warn (strcat "Encabezado desconocido: " line))
               )
             )
            )
            (T
             (if (and in-beams (wcmatch line "B|*"))
               (progn
                 (setq kv (ocmema:pio-split line "|"))
                 (if (>= (length kv) 4)
                   (progn
                     (setq beam-name (nth 1 kv))
                     (setq beam-npoints (atoi (nth 2 kv)))
                     (setq beam-points (ocmema:pio-parse-points2d (nth 3 kv)))
                     (setq beams
                       (append beams
                         (list
                           (list
                             (cons "name" beam-name)
                             (cons "n_points" beam-npoints)
                             (cons "points_raw" beam-points)
                           )
                         )
                       )
                     )
                   )
                   (ocmema:proj-warn (strcat "Linea BEAMS invalida: " line))
                 )
               )
               (if (and in-ribs (wcmatch line "N|*"))
                 (progn
                   (setq kv (ocmema:pio-split line "|"))
                   (if (>= (length kv) 5)
                     (progn
                       (setq rib-name (nth 1 kv))
                       (setq rib-dir (nth 2 kv))
                       (setq rib-spacing (ocmema:pio-to-number (nth 3 kv)))
                       (setq rib-nclear (atoi (nth 4 kv)))
                       (setq ribs
                         (append ribs
                           (list
                             (list
                               (cons "name" rib-name)
                               (cons "dir" rib-dir)
                               (cons "spacing" rib-spacing)
                               (cons "n_clear" rib-nclear)
                             )
                           )
                         )
                       )
                     )
                     (ocmema:proj-warn (strcat "Linea RIBS invalida: " line))
                   )
                 )
                 (progn
                   (setq kv (ocmema:pio-split-kv line))
                   (if kv
                     (progn
                       (setq key (car kv))
                       (setq val (cadr kv))
                       (if in-plant
                         (cond
                           ((= key "PLANT_NAME") (setq plant-name val))
                           ((= key "X_AXES") (setq x-axes (ocmema:pio-parse-axes val)))
                           ((= key "Y_AXES") (setq y-axes (ocmema:pio-parse-axes val)))
                           (T (ocmema:proj-warn (strcat "Clave desconocida en PLANT: " key)))
                         )
                         (if in-beam
                           (cond
                             ((= key "BEAM_NAME") (setq beam-name val))
                             ((= key "PLANT_IDX") (setq beam-plant-idx (atoi val)))
                             ((= key "PLANT_NAME") (setq beam-plant-name val))
                             ((= key "N_POINTS") (setq beam-npoints (atoi val)))
                             ((= key "UNIT") (setq beam-unit val))
                             ((= key "STD_PATH") (setq beam-std-path val))
                             ((= key "POINTS") (setq beam-points (ocmema:pio-parse-points val)))
                             (T (ocmema:proj-warn (strcat "Clave desconocida en BEAM: " key)))
                           )
                           (if in-units
                             (cond
                               ((= key "UNITS") (setq units val))
                               ((= key "SCALE") (setq scale (ocmema:pio-to-number val)))
                               (T (ocmema:proj-warn (strcat "Clave desconocida en UNITS: " key)))
                             )
                             (cond
                               ((= key "PROJECT_NAME") (setq projname val))
                               ((= key "N_PLANTS") (setq nplants (atoi val)))
                               ((= key "WALL_CM") (setq wall (ocmema:pio-to-number val)))
                               ((= key "NX") (setq nx (atoi val)))
                               ((= key "NY") (setq ny (atoi val)))
                               ((= key "X_NAMES") (setq xnames (ocmema:pio-split-list val ",")))
                               ((= key "Y_NAMES") (setq ynames (ocmema:pio-split-list val ",")))
                               ((= key "BEAM_UNIT") (setq proj-beam-unit val))
                               (T (ocmema:proj-warn (strcat "Clave desconocida: " key)))
                             )
                           )
                         )
                       )
                     )
                     (ocmema:proj-warn (strcat "Linea sin clave: " line))
                   )
                 )
               )
             )
            )
          )
          (setq i (1+ i))
        )

        ;; cerrar bloque abierto si falta [/PLANT]
        (if in-plant
          (progn
            (ocmema:proj-warn "Bloque PLANT sin cierre; se cierra automaticamente.")
            (setq plants
              (append plants
                (list
                  (list
                    (cons "idx" plant-index)
                    (cons "name" plant-name)
                    (cons "x_axes" x-axes)
                    (cons "y_axes" y-axes)
                    (cons 'PLANT_INDEX plant-index)
                    (cons 'PLANT_NAME plant-name)
                    (cons 'X_AXES x-axes)
                    (cons 'Y_AXES y-axes)
                  )
                )
              )
            )
          )
        )

        ;; cerrar bloque abierto si falta [/BEAM]
        (if in-beam
          (progn
            (ocmema:proj-warn "Bloque BEAM sin cierre; se cierra automaticamente.")
            (setq beams
              (append beams
                (list
                  (list
                    (cons "idx" beam-index)
                    (cons "name" beam-name)
                    (cons "plant_idx" beam-plant-idx)
                    (cons "plant_name" beam-plant-name)
                    (cons "n_points" beam-npoints)
                    (cons "unit" beam-unit)
                    (cons "std_path" beam-std-path)
                    (cons "points_raw" beam-points)
                  )
                )
              )
            )
          )
        )

        (if (= (length plants) 0)
          (progn (ocmema:proj-log "TXT invalido: no se encontraron plantas.") nil)
          (progn
            (if (or (<= nplants 0) (/= nplants (length plants)))
              (progn
                (ocmema:proj-warn (strcat "N_PLANTS no coincide; usando " (itoa (length plants)) "."))
                (setq nplants (length plants))
              )
            )
            (if (and (> nx 0) (/= nx (length xnames)))
              (ocmema:proj-warn "NX no coincide con X_NAMES.")
            )
            (if (and (> ny 0) (/= ny (length ynames)))
              (ocmema:proj-warn "NY no coincide con Y_NAMES.")
            )
            (if (<= nx 0) (setq nx (length xnames)))
            (if (<= ny 0) (setq ny (length ynames)))

            (list
              (cons "version" ocmema:*project-io-version*)
              (cons "project_name" projname)
              (cons "n_plants" nplants)
              (cons "wall_cm" wall)
              (cons "nx" nx)
              (cons "ny" ny)
              (cons "x_names" xnames)
              (cons "y_names" ynames)
              (cons "units" units)
              (cons "scale" scale)
              (cons "plants" plants)
              (cons "beams" beams)
              (cons "ribs" ribs)
              (cons 'PROJECT_NAME projname)
              (cons 'N_PLANTS nplants)
              (cons 'WALL_CM wall)
              (cons 'NX nx)
              (cons 'NY ny)
              (cons 'X_NAMES xnames)
              (cons 'Y_NAMES ynames)
              (cons 'UNITS units)
              (cons 'SCALE scale)
              (cons 'PLANTS plants)
              (cons 'BEAMS beams)
              (cons 'RIBS ribs)
            )
          )
        )
      )
    )
  )
)

(defun ocmema:pio-load-project (path / lines proj)
  (setq lines (ocmema:pio-read-lines path))
  (if (not lines)
    (progn (ocmema:proj-log "TXT invalido: no se pudo leer.") nil)
    (progn
      (setq proj (ocmema:pio-load-project-lines lines))
      (if proj
        (progn
          (setq ocmema:*project* proj)
          (setq ocmema:*project-path* path)
          (ocmema:proj-log
            (strcat "Proyecto cargado: " (ocmema:pio-assoc-get "project_name" proj)
                    " (Plantas=" (itoa (ocmema:pio-assoc-get "n_plants" proj)) ")")
          )
          proj
        )
        nil
      )
    )
  )
)

(defun ocmema:pio-test-load (/ lines1 lines2 proj)
  (setq lines1
    (list
      "OCMEMA_PROJECT_V1"
      "PROJECT_NAME=Demo"
      "N_PLANTS=2"
      "WALL_CM=14"
      "NX=3"
      "NY=2"
      "X_NAMES=A,D,E"
      "Y_NAMES=1,2"
      "[PLANT 1]"
      "PLANT_NAME=Planta Baja"
      "X_AXES=A:96.68,D:102.28,E:150.00"
      "Y_AXES=1:9.87,2:10.21"
      "[/PLANT]"
      "[PLANT 2]"
      "PLANT_NAME=Planta Alta"
      "X_AXES=A:96.68,D:102.28,E:150.00"
      "Y_AXES=1:9.87,2:10.21"
      "[/PLANT]"
    )
  )
  (setq lines2
    (list
      "OCMEMA_PROJECT_V1"
      "PROJECT_NAME=ConApostrofe"
      "N_PLANTS=1"
      "X_NAMES=A',B'"
      "Y_NAMES=1,2"
      "[PLANT1]"
      "PLANT_NAME=P1"
      "X_AXES=A':10.5,B':20.0"
      "Y_AXES=1:5.0,2:7.5"
      "[/PLANT]"
    )
  )
  (setq proj (ocmema:pio-load-project-lines lines1))
  (if proj
    (progn
      (prompt (strcat "\nTEST1 OK: plants=" (itoa (length (ocmema:pio-assoc-get "plants" proj)))))
      (prompt (strcat "\nTEST1 NAME: " (ocmema:pio-assoc-get "project_name" proj)))
    )
    (prompt "\nTEST1 FAIL")
  )
  (setq proj (ocmema:pio-load-project-lines lines2))
  (if proj
    (progn
      (prompt (strcat "\nTEST2 OK: plants=" (itoa (length (ocmema:pio-assoc-get "plants" proj)))))
      (prompt (strcat "\nTEST2 NAME: " (ocmema:pio-assoc-get "project_name" proj)))
    )
    (prompt "\nTEST2 FAIL")
  )
  (princ)
)

;; Update name across all plants
(defun ocmema:pio-rename-axis (axisType oldName newName / proj plants newplants plant axes newaxes item)
  (setq proj ocmema:*project*)
  (if (not proj)
    nil
    (progn
      (if (= axisType "X")
        (progn
          (setq proj (ocmema:pio-alist-set "x_names" (ocmema:pio-rename-in-list (ocmema:pio-assoc-get "x_names" proj) oldName newName) proj))
        )
        (progn
          (setq proj (ocmema:pio-alist-set "y_names" (ocmema:pio-rename-in-list (ocmema:pio-assoc-get "y_names" proj) oldName newName) proj))
        )
      )
      (setq plants (ocmema:pio-assoc-get "plants" proj))
      (setq newplants '())
      (foreach plant plants
        (setq axes (if (= axisType "X") (ocmema:pio-assoc-get "x_axes" plant) (ocmema:pio-assoc-get "y_axes" plant)))
        (setq newaxes '())
        (foreach item axes
          (if (= (car item) oldName)
            (setq newaxes (append newaxes (list (cons newName (cdr item)))))
            (setq newaxes (append newaxes (list item)))
          )
        )
        (if (= axisType "X")
          (setq plant (ocmema:pio-alist-set "x_axes" newaxes plant))
          (setq plant (ocmema:pio-alist-set "y_axes" newaxes plant))
        )
        (setq newplants (append newplants (list plant)))
      )
      (setq proj (ocmema:pio-alist-set "plants" newplants proj))
      (setq ocmema:*project* proj)
      T
    )
  )
)

(defun ocmema:pio-rename-in-list (lst oldName newName / out)
  (setq out '())
  (foreach item lst
    (if (= item oldName)
      (setq out (append out (list newName)))
      (setq out (append out (list item)))
    )
  )
  out
)

(defun ocmema:pio-name-exists (lst name / found)
  (setq found nil)
  (foreach item lst
    (if (= item name) (setq found T))
  )
  found
)

;; Change coord for one axis in one plant
(defun ocmema:pio-change-axis-coord (plantIdx axisType axisName / proj plants newplants plant axes newaxes item pt coord changed)
  (setq proj ocmema:*project*)
  (if (not proj)
    nil
    (progn
      (setq plants (ocmema:pio-assoc-get "plants" proj))
      (setq newplants '())
      (setq changed nil)
      (foreach plant plants
        (if (= (ocmema:pio-assoc-get "idx" plant) plantIdx)
          (progn
            (setq axes (if (= axisType "X") (ocmema:pio-assoc-get "x_axes" plant) (ocmema:pio-assoc-get "y_axes" plant)))
            (setq pt (ocmema:pio-getpoint (strcat "\nNuevo punto para eje " axisName ": ")))
            (if pt
              (progn
                (setq coord (if (= axisType "X") (car pt) (cadr pt)))
                (setq newaxes '())
                (foreach item axes
                  (if (= (car item) axisName)
                    (setq newaxes (append newaxes (list (cons axisName coord))))
                    (setq newaxes (append newaxes (list item)))
                  )
                )
                (setq newaxes (ocmema:pio-sort-pairs newaxes))
                (if (= axisType "X")
                  (setq plant (ocmema:pio-alist-set "x_axes" newaxes plant))
                  (setq plant (ocmema:pio-alist-set "y_axes" newaxes plant))
                )
                (setq changed T)
              )
              (progn
                (ocmema:proj-cancelled)
              )
            )
          )
        )
        (setq newplants (append newplants (list plant)))
      )
      (setq proj (ocmema:pio-alist-set "plants" newplants proj))
      (setq ocmema:*project* proj)
      changed
    )
  )
)

;; Menus (while loops, no recursion)
(defun ocmema:menu-entry (/ opt done)
  (setq done nil)
  (while (not done)
    (initget "N C S")
    (setq opt (getkword "\nOCMEMA [N Nuevo/C Cargar/S Salir] <S>: "))
    (cond
      ((or (not opt) (= opt "S"))
       (setq done T)
      )
      ((= opt "N")
       (if (ocmema:proj-new)
         (progn
           (ocmema:menu-general)
         )
       )
      )
      ((= opt "C")
       (if (ocmema:proj-load)
         (progn
           (ocmema:menu-general)
         )
         (ocmema:proj-log "No se pudo cargar el proyecto.")
       )
      )
    )
  )
  (princ)
)

(defun ocmema:menu-general (/ opt done)
  (setq done nil)
  (while (not done)
    (initget "M G D S")
    (setq opt (getkword "\nOCMEMA [M ModificarEjes/G GenerarElementos/D Dibujar/S Salir] <S>: "))
    (cond
      ((or (not opt) (= opt "S")) (setq done T))
      ((= opt "M") (ocmema:menu-modificar-ejes))
      ((= opt "G") (ocmema:menu-generar-elementos))
      ((= opt "D") (ocmema:menu-dibujar))
    )
  )
  (princ)
)

(defun ocmema:menu-modificar-ejes (/ opt done)
  (setq done nil)
  (while (not done)
    (initget "U T R")
    (setq opt (getkword "\nModificar ejes [U Uno/T Todos/R Regresar] <R>: "))
    (cond
      ((or (not opt) (= opt "R")) (setq done T))
      ((= opt "U") (ocmema:menu-modificar-ejes-uno))
      ((= opt "T") (ocmema:menu-modificar-ejes-todos))
    )
  )
  (princ)
)

(defun ocmema:menu-modificar-ejes-uno (/ proj axisType axisName opt names actualName newName nplants plantIdx)
  (setq proj ocmema:*project*)
  (if (not proj)
    (ocmema:proj-log "No hay proyecto cargado.")
    (progn
      (initget "X Y")
      (setq axisType (getkword "\nDireccion [X/Y]: "))
      (if (not axisType)
        (ocmema:proj-cancelled)
        (progn
          (setq axisName (getstring T "\nNombre exacto del eje: "))
          (if (not axisName)
            (ocmema:proj-cancelled)
            (progn
              (if (= axisType "X")
                (setq names (ocmema:pio-assoc-get "x_names" proj))
                (setq names (ocmema:pio-assoc-get "y_names" proj))
              )
              (setq actualName (ocmema:pio-find-name-ci names axisName))
              (if (not actualName)
                (ocmema:proj-log "Eje no existe.")
                (progn
                  (initget "N C R")
                  (setq opt (getkword "\nEje [N CambiarNombre/C CambiarCoordenada/R Regresar] <R>: "))
                  (cond
                    ((or (not opt) (= opt "R")) nil)
                    ((= opt "N")
                     (setq newName (ocmema:pio-get-unique-axis-name "\nNuevo nombre: " names))
                     (if (not newName)
                       (ocmema:proj-cancelled)
                       (progn
                         (if (= (ocmema:pio-normalize-name newName) (ocmema:pio-normalize-name actualName))
                           (ocmema:proj-log "Nombre de eje duplicado.")
                           (progn
                             (ocmema:pio-rename-axis axisType actualName newName)
                             (ocmema:proj-autosave)
                           )
                         )
                       )
                     )
                    )
                    ((= opt "C")
                     (setq nplants (ocmema:pio-assoc-get "n_plants" proj))
                     (setq plantIdx (ocmema:pio-getint-min "\nNumero de planta: " 1))
                     (if (not plantIdx)
                       (ocmema:proj-cancelled)
                       (if (> plantIdx nplants)
                         (ocmema:proj-log "Planta invalida.")
                         (progn
                           (if (ocmema:pio-change-axis-coord plantIdx axisType actualName)
                             (ocmema:proj-autosave)
                           )
                         )
                       )
                     )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  (princ)
)

(defun ocmema:proj-apply-new-axes (nx ny xnames ynames / proj plants newplants plant)
  (setq proj ocmema:*project*)
  (setq proj (ocmema:pio-alist-set "nx" nx proj))
  (setq proj (ocmema:pio-alist-set "ny" ny proj))
  (setq proj (ocmema:pio-alist-set "x_names" xnames proj))
  (setq proj (ocmema:pio-alist-set "y_names" ynames proj))
  (setq plants (ocmema:pio-assoc-get "plants" proj))
  (setq newplants '())
  (foreach plant plants
    (setq plant (ocmema:pio-alist-set "x_axes" '() plant))
    (setq plant (ocmema:pio-alist-set "y_axes" '() plant))
    (setq newplants (append newplants (list plant)))
  )
  (setq proj (ocmema:pio-alist-set "plants" newplants proj))
  (setq ocmema:*project* proj)
)

(defun ocmema:menu-modificar-ejes-todos (/ opt done nx ny xnames ynames ok oldproj oldpath name)
  (setq done nil)
  (while (not done)
    (initget "C N R")
    (setq opt (getkword "\nModificar todos [C CapturarPuntos/N NuevosEjes/R Regresar] <R>: "))
    (cond
      ((or (not opt) (= opt "R")) (setq done T))
      ((= opt "C")
       (if (ocmema:pio-capture-all-axes)
         (ocmema:proj-autosave)
       )
      )
      ((= opt "N")
       (setq oldproj ocmema:*project*)
       (setq oldpath ocmema:*project-path*)
       (setq nx (ocmema:pio-getint-min "\nNumero de ejes X: " 1))
       (if (not nx)
         (ocmema:proj-cancelled)
         (progn
           (setq ny (ocmema:pio-getint-min "\nNumero de ejes Y: " 1))
           (if (not ny)
             (ocmema:proj-cancelled)
             (progn
               (setq xnames '())
               (setq ok T)
               (repeat nx
                 (if ok
                   (progn
                     (setq name (ocmema:pio-get-unique-axis-name (strcat "\nNombre eje X " (itoa (1+ (length xnames))) ": ") xnames))
                     (if name
                       (setq xnames (append xnames (list name)))
                       (setq ok nil)
                     )
                   )
                 )
               )
               (if (not ok)
                 (progn
                   (setq ocmema:*project* oldproj)
                   (setq ocmema:*project-path* oldpath)
                   (ocmema:proj-cancelled)
                 )
                 (progn
                   (setq ynames '())
                   (repeat ny
                     (if ok
                       (progn
                         (setq name (ocmema:pio-get-unique-axis-name (strcat "\nNombre eje Y " (itoa (1+ (length ynames))) ": ") ynames))
                         (if name
                           (setq ynames (append ynames (list name)))
                           (setq ok nil)
                         )
                       )
                     )
                   )
                   (if (not ok)
                     (progn
                       (setq ocmema:*project* oldproj)
                       (setq ocmema:*project-path* oldpath)
                       (ocmema:proj-cancelled)
                     )
                     (progn
                       (ocmema:proj-apply-new-axes nx ny xnames ynames)
                       (if (ocmema:pio-capture-all-axes)
                         (ocmema:proj-autosave)
                         (progn
                           (setq ocmema:*project* oldproj)
                           (setq ocmema:*project-path* oldpath)
                         )
                       )
                     )
                   )
                 )
               )
             )
           )
         )
       )
      )
    )
  )
  (princ)
)

(defun ocmema:menu-generar-elementos (/ opt done)
  (setq done nil)
  (while (not done)
    (initget "T N R")
    (setq opt (getkword "\nGenerar [T Trabes/N Nervaduras/R Regresar] <R>: "))
    (cond
      ((or (not opt) (= opt "R")) (setq done T))
      ((= opt "T")
       (ocmema:menu-generar-trabes)
      )
      ((= opt "N")
       (ocmema:menu-generar-nervaduras)
      )
    )
  )
  (princ)
)

(defun ocmema:menu-generar-trabes (/ opt done beams)
  (setq done nil)
  (while (not done)
    (if (not (ocmema:proj-ready-for-generators-p))
      (setq done T)
      (progn
        (setq beams (ocmema:proj-get-beams))
        (if (not beams)
          (progn
            (ocmema:proj-ensure-generators-loaded)
            (princ "\nOCMEMA: Ejecutando GEN_TRABES...")
            (ocmema:proj-run-gen-trabes)
            (princ "\nOCMEMA: Regresando al menu...")
            (setq done T)
          )
          (progn
            (initget "G M R")
            (setq opt (getkword "\nTrabes [G GenerarNueva/M ModificarExistente/R Regresar] <R>: "))
            (cond
              ((or (not opt) (= opt "R")) (setq done T))
              ((= opt "G")
               (ocmema:proj-ensure-generators-loaded)
               (princ "\nOCMEMA: Ejecutando GEN_TRABES...")
               (ocmema:proj-run-gen-trabes)
               (princ "\nOCMEMA: Regresando al menu...")
               (setq done T)
              )
              ((= opt "M")
               (ocmema:menu-modificar-trabe)
               (setq done T)
              )
            )
          )
        )
      )
    )
  )
  (princ)
)

(defun ocmema:menu-modificar-trabe (/ name beam opt points)
  (setq name (getstring T "\nNombre exacto de la trabe: "))
  (if (not name)
    (ocmema:proj-cancelled)
    (progn
      (setq beam (ocmema:proj-find-beam name))
      (if (not beam)
        (ocmema:proj-log "Trabe no existe.")
        (progn
          (initget "P R X")
          (setq opt (getkword "\nModificar [P PuntosSoloTXT/R RehacerTrabeSTD/X Regresar] <X>: "))
          (cond
            ((or (not opt) (= opt "X")) nil)
            ((= opt "P")
             (setq points (ocmema:proj-capture-points))
             (if (not points)
               (ocmema:proj-cancelled)
               (progn
                 (ocmema:proj-update-beam-points (ocmema:pio-assoc-get "name" beam) points)
                 (ocmema:proj-autosave)
               )
             )
            )
            ((= opt "R")
             (setq ocmema:*beam-force-name* (ocmema:pio-assoc-get "name" beam))
             (setq ocmema:*beam-single* T)
             (ocmema:proj-ensure-generators-loaded)
             (princ "\nOCMEMA: Ejecutando GEN_TRABES...")
             (ocmema:proj-run-gen-trabes)
             (princ "\nOCMEMA: Regresando al menu...")
            )
          )
        )
      )
    )
  )
  (princ)
)

(defun ocmema:menu-generar-nervaduras (/ opt done ribs)
  (setq done nil)
  (while (not done)
    (if (not (ocmema:proj-ready-for-generators-p))
      (setq done T)
      (progn
        (setq ribs (ocmema:proj-get-ribs))
        (if (not ribs)
          (progn
            (ocmema:proj-ensure-generators-loaded)
            (princ "\nOCMEMA: Ejecutando GEN_NERV...")
            (ocmema:proj-run-gen-nerv)
            (princ "\nOCMEMA: Regresando al menu...")
            (setq done T)
          )
          (progn
            (initget "G M R")
            (setq opt (getkword "\nNervaduras [G GenerarNueva/M Modificar/R Regresar] <R>: "))
            (cond
              ((or (not opt) (= opt "R")) (setq done T))
              ((= opt "G")
               (ocmema:proj-ensure-generators-loaded)
               (princ "\nOCMEMA: Ejecutando GEN_NERV...")
               (ocmema:proj-run-gen-nerv)
               (princ "\nOCMEMA: Regresando al menu...")
               (setq done T)
              )
              ((= opt "M")
               (ocmema:menu-modificar-nervadura)
               (setq done T)
              )
            )
          )
        )
      )
    )
  )
  (princ)
)

(defun ocmema:menu-modificar-nervadura (/ name rib opt dir)
  (setq name (getstring T "\nNombre exacto de la nervadura: "))
  (if (not name)
    (ocmema:proj-cancelled)
    (progn
      (setq rib (ocmema:proj-find-rib name))
      (if (not rib)
        (ocmema:proj-log "Nervadura no existe.")
        (progn
          (initget "D N R")
          (setq opt (getkword "\nModificar [D DireccionSoloTXT/N NuevoSTD/R Regresar] <R>: "))
          (cond
            ((or (not opt) (= opt "R")) nil)
            ((= opt "D")
             (initget "H V")
             (setq dir (getkword "\nDireccion [H Horizontal/V Vertical] <H>: "))
             (if (not dir) (setq dir "H"))
             (ocmema:proj-upsert-rib
               (list
                 (cons "name" (ocmema:pio-assoc-get "name" rib))
                 (cons "dir" dir)
                 (cons "spacing" (ocmema:pio-assoc-get "spacing" rib))
                 (cons "n_clear" (ocmema:pio-assoc-get "n_clear" rib))
               )
             )
             (ocmema:proj-autosave)
            )
            ((= opt "N")
             (setq ocmema:*rib-force-name* (ocmema:pio-assoc-get "name" rib))
             (setq ocmema:*rib-single* T)
             (ocmema:proj-ensure-generators-loaded)
             (princ "\nOCMEMA: Ejecutando GEN_NERV...")
             (ocmema:proj-run-gen-nerv)
             (princ "\nOCMEMA: Regresando al menu...")
            )
          )
        )
      )
    )
  )
  (princ)
)

(defun ocmema:menu-dibujar (/ opt done)
  (setq done nil)
  (while (not done)
    (initget "A P R")
    (setq opt (getkword "\nDibujar [A Armados/P Planta/R Regresar] <R>: "))
    (cond
      ((or (not opt) (= opt "R")) (setq done T))
      ((= opt "A") (ocmema:proj-log "Pendiente: Dibujar Armados"))
      ((= opt "P") (ocmema:proj-log "Pendiente: Dibujar Planta"))
    )
  )
  (princ)
)

;; Public: new project flow
(defun ocmema:proj-new (/ pname nplants i plname wall nx ny xnames ynames plants path ok name oldproj oldpath)
  (setq oldproj ocmema:*project*)
  (setq oldpath ocmema:*project-path*)
  (setq pname (ocmema:pio-get-nonempty-string "\nNombre del proyecto: "))
  (if (not pname)
    (progn (ocmema:proj-cancelled) nil)
    (progn
      (setq nplants (ocmema:pio-getint-min "\nNumero de plantas: " 1))
      (if (not nplants)
        (progn (ocmema:proj-cancelled) nil)
        (progn
          (setq i 1)
          (setq plname "")
          (setq plants '())
          (while (<= i nplants)
            (setq plname (getstring T (strcat "\nNombre planta " (itoa i) " <Planta " (itoa i) ">: ")))
            (if (not plname)
              (progn (ocmema:proj-cancelled) (setq plants nil) (setq i (1+ nplants)))
              (progn
                (setq plname (ocmema:str-trim plname))
                (if (= plname "") (setq plname (strcat "Planta " (itoa i))))
                (setq plants
                  (append plants
                    (list
                      (list
                        (cons "idx" i)
                        (cons "name" plname)
                        (cons "x_axes" '())
                        (cons "y_axes" '())
                      )
                    )
                  )
                )
                (setq i (1+ i))
              )
            )
          )
          (if (not plants)
            nil
            (progn
              (setq wall (ocmema:pio-getreal-min "\nEspesor de muro (cm): " 0.0))
              (if (not wall)
                (progn (ocmema:proj-cancelled) nil)
                (progn
                  (setq nx (ocmema:pio-getint-min "\nNumero de ejes X: " 1))
                  (if (not nx)
                    (progn (ocmema:proj-cancelled) nil)
                    (progn
                      (setq ny (ocmema:pio-getint-min "\nNumero de ejes Y: " 1))
                      (if (not ny)
                        (progn (ocmema:proj-cancelled) nil)
                        (progn
                          (setq xnames '())
                          (setq ok T)
                          (repeat nx
                            (if ok
                              (progn
                                (setq name (ocmema:pio-get-unique-axis-name (strcat "\nNombre eje X " (itoa (1+ (length xnames))) ": ") xnames))
                                (if name
                                  (setq xnames (append xnames (list name)))
                                  (setq ok nil)
                                )
                              )
                            )
                          )
                          (if (not ok)
                            (progn (ocmema:proj-cancelled) nil)
                            (progn
                              (setq ynames '())
                              (repeat ny
                                (if ok
                                  (progn
                                    (setq name (ocmema:pio-get-unique-axis-name (strcat "\nNombre eje Y " (itoa (1+ (length ynames))) ": ") ynames))
                                    (if name
                                      (setq ynames (append ynames (list name)))
                                      (setq ok nil)
                                    )
                                  )
                                )
                              )
                              (if (not ok)
                                (progn (ocmema:proj-cancelled) nil)
                                (progn
                                  (setq ocmema:*project*
                                    (list
                                      (cons "version" ocmema:*project-io-version*)
                                      (cons "project_name" pname)
                                      (cons "n_plants" nplants)
                                      (cons "wall_cm" wall)
                                      (cons "nx" nx)
                                      (cons "ny" ny)
                                      (cons "x_names" xnames)
                                      (cons "y_names" ynames)
                                      (cons "units" nil)
                                      (cons "scale" nil)
                                      (cons "beams" '())
                                      (cons "ribs" '())
                                      (cons "plants" plants)
                                    )
                                  )
                                  (if (ocmema:pio-capture-all-axes)
                                    (progn
                                      (setq path (ocmema:proj-default-path))
                                      (if (ocmema:pio-save-project path)
                                        (progn
                                          (ocmema:proj-log "Proyecto guardado.")
                                          ocmema:*project*
                                        )
                                        (progn
                                          (setq ocmema:*project* oldproj)
                                          (setq ocmema:*project-path* oldpath)
                                          (ocmema:proj-log "No se pudo guardar el proyecto.")
                                          nil
                                        )
                                      )
                                    )
                                    (progn
                                      (setq ocmema:*project* oldproj)
                                      (setq ocmema:*project-path* oldpath)
                                      nil
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

;; Public: load project flow
(defun ocmema:proj-load (/ path)
  (setq path (ocmema:proj-getfile-open "Cargar proyecto"))
  (if (not path)
    nil
    (ocmema:pio-load-project path)
  )
)

;; Command entry point
(defun C:OCMEMA_PROJ (/)
  (ocmema:proj-ensure-generators-loaded)
  (ocmema:menu-entry)
  (princ)
)

(princ)






