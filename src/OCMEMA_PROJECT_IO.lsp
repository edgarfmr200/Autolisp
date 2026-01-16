;; OCMEMA Project IO module (AutoLISP only). No runtime logic on load.

;; Global variables
(setq ocmema:*project* nil)
(setq ocmema:*project-path* nil)
(setq ocmema:*project-io-version* "OCMEMA_PROJECT_V1")
(setq ocmema:*proj-default-dir* "C:\\Users\\edgar\\OneDrive - ITESO\\OCMEMA_IE\\01. PROYECTOS\\2025\\")

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
    (princ "\nOCMEMA: Regresando al menu...")
    (ocmema:menu-general)
    (princ)
  )
  (C:GEN_TRABES)
  (if olderr (setq *error* olderr))
  T
)

(defun ocmema:proj-run-gen-nerv (/ olderr msg)
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
  (C:GEN_NERV)
  (if olderr (setq *error* olderr))
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

(defun ocmema:pio-save-project (path / proj lines plants plant x_axes y_axes pname xnames ynames tmp ok)
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
                                            plants nplants nx ny wall xnames ynames projname)
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
        (setq plants '())
        (setq nplants 0)
        (setq nx 0)
        (setq ny 0)
        (setq wall 0.0)
        (setq xnames '())
        (setq ynames '())
        (setq projname "")

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
               (T
                (ocmema:proj-warn (strcat "Encabezado desconocido: " line))
               )
             )
            )
            (T
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
                   (cond
                     ((= key "PROJECT_NAME") (setq projname val))
                     ((= key "N_PLANTS") (setq nplants (atoi val)))
                     ((= key "WALL_CM") (setq wall (ocmema:pio-to-number val)))
                     ((= key "NX") (setq nx (atoi val)))
                     ((= key "NY") (setq ny (atoi val)))
                     ((= key "X_NAMES") (setq xnames (ocmema:pio-split-list val ",")))
                     ((= key "Y_NAMES") (setq ynames (ocmema:pio-split-list val ",")))
                     (T (ocmema:proj-warn (strcat "Clave desconocida: " key)))
                   )
                 )
               )
               (ocmema:proj-warn (strcat "Linea sin clave: " line))
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
              (cons "plants" plants)
              (cons 'PROJECT_NAME projname)
              (cons 'N_PLANTS nplants)
              (cons 'WALL_CM wall)
              (cons 'NX nx)
              (cons 'NY ny)
              (cons 'X_NAMES xnames)
              (cons 'Y_NAMES ynames)
              (cons 'PLANTS plants)
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
       (if (ocmema:proj-ready-for-generators-p)
         (progn
           (ocmema:proj-ensure-generators-loaded)
           (princ "\nOCMEMA: Ejecutando GEN_TRABES...")
           (ocmema:proj-run-gen-trabes)
           (princ "\nOCMEMA: Regresando al menu...")
           (setq done T)
         )
       )
      )
      ((= opt "N")
       (if (ocmema:proj-ready-for-generators-p)
         (progn
           (ocmema:proj-ensure-generators-loaded)
           (princ "\nOCMEMA: Ejecutando GEN_NERV...")
           (ocmema:proj-run-gen-nerv)
           (princ "\nOCMEMA: Regresando al menu...")
           (setq done T)
         )
       )
      )
    )
  )
  (princ)
)

(defun ocmema:menu-generar-trabes (/ opt done)
  (setq done nil)
  (while (not done)
    (initget "N M R")
    (setq opt (getkword "\nTrabes [N GenerarNueva/M ModificarExistente/R Regresar] <R>: "))
    (cond
      ((or (not opt) (= opt "R")) (setq done T))
      ((= opt "N") (ocmema:proj-log "Pendiente: Generar Trabes"))
      ((= opt "M") (ocmema:proj-log "No existen trabes en este proyecto. Primero genere uno."))
    )
  )
  (princ)
)

(defun ocmema:menu-generar-nervaduras (/ opt done)
  (setq done nil)
  (while (not done)
    (initget "N M R")
    (setq opt (getkword "\nNervaduras [N GenerarNueva/M ModificarExistente/R Regresar] <R>: "))
    (cond
      ((or (not opt) (= opt "R")) (setq done T))
      ((= opt "N") (ocmema:proj-log "Pendiente: Generar Nervaduras"))
      ((= opt "M") (ocmema:proj-log "No existen nervaduras en este proyecto. Primero genere uno."))
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






