;; OCMEMA Project IO module (AutoLISP only). No runtime logic on load.

;; Global variables
(setq ocmema:*project* nil)
(setq ocmema:*project-path* nil)
(setq ocmema:*last-project-dir* nil)
(setq ocmema:*project-io-version* "OCMEMA_PROJECT_V2")
(setq ocmema:*proj-default-dir* "C:\\Users\\edgar\\OneDrive - ITESO\\OCMEMA_IE\\01. PROYECTOS\\2025\\")
(setq ocmema:*beam-replace-name* nil)
(setq ocmema:*beam-force-name* nil)
(setq ocmema:*beam-single* nil)
(setq ocmema:*rib-force-name* nil)
(setq ocmema:*rib-single* nil)

;; TXT keys esperadas (writer actual):
;; VERSION: OCMEMA_PROJECT_V2
;; PROJECT_NAME, N_PLANTS, WALL_CM, NX, NY, X_NAMES, Y_NAMES
;; [PLANT n] ... PLANT_NAME, X_AXES, Y_AXES ... [/PLANT]
(setq ocmema:*proj-debug* nil)
(setq ocmema:*debug-io* nil)
(setq ocmema:*save-seq* 0)
(setq ocmema:*save-caller* nil)
(setq ocmema:*pl-beam-layer* "TRABE-PROY")
(setq ocmema:*pl-centerline-layer* "TRABES")
(setq ocmema:*pl-beam-color* 256)
(setq ocmema:*pl-raw-layer* "OCMEMA_TRABES_RAW")
(setq ocmema:*pl-center-layer* "OCMEMA_TRABES_CENTER")
(setq ocmema:*pl-outline-layer* "TRABE-PROY")
(setq OCM_DEBUG_DRAW nil)
(setq *ocm_pts_reason* nil)
(setq *ocm_anl_reason* nil)
(setq *ocm_anl_sample* nil)

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

(defun ocmema:dbg-io (msg /)
  (if ocmema:*debug-io*
    (prompt (strcat "\n" msg))
  )
)

(defun ocmema:dbg-verify-skipped (name /)
  (if ocmema:*debug-io*
    (ocmema:dbg-io (strcat "[SAVE] verify skipped (missing: " name ")"))
  )
)

(defun ocmema:dbg-bool (v /)
  (if v "T" "NIL")
)

(defun ocmema:dbg-starts-with (s prefix / n)
  (setq n (strlen prefix))
  (if (and s (>= (strlen s) n) (= (substr s 1 n) prefix))
    T
    nil
  )
)

(defun ocmema:dbg-has-prefix (lines prefix / found line)
  (setq found nil)
  (foreach line lines
    (if (and (not found) (ocmema:dbg-starts-with line prefix))
      (setq found T)
    )
  )
  found
)

(defun ocmema:dbg-find-tag (lines tag / found line)
  (setq found nil)
  (foreach line lines
    (if (and (not found) (= (strcase (ocmema:str-trim line)) (strcase tag)))
      (setq found T)
    )
  )
  found
)

(defun ocmema:dbg-section-lines (lines open-tag close-tag / in out line)
  (setq in nil)
  (setq out '())
  (foreach line lines
    (if (not in)
      (if (= (strcase (ocmema:str-trim line)) (strcase open-tag))
        (progn
          (setq in T)
          (setq out (append out (list line)))
        )
      )
      (progn
        (setq out (append out (list line)))
        (if (= (strcase (ocmema:str-trim line)) (strcase close-tag))
          (setq in nil)
        )
      )
    )
  )
  out
)

(defun ocmema:dbg-plant-block (lines plant-idx / tag)
  (setq tag (strcat "[PLANT " (itoa plant-idx) "]"))
  (ocmema:dbg-section-lines lines tag "[/PLANT]")
)

(defun ocmema:dbg-file-size (path / sz r lines line hasV)
  (setq sz nil)
  (setq hasV (vl-catch-all-apply 'vl-file-size (list path)))
  (if (vl-catch-all-error-p hasV)
    (ocmema:dbg-verify-skipped "vl-file-size")
    (if (findfile path)
      (setq sz hasV)
    )
  )
  (if (not sz)
    (progn
      (setq lines (ocmema:pio-read-lines path))
      (if lines
        (progn
          (setq sz 0)
          (foreach line lines
            (setq sz (+ sz (strlen line) 2))
          )
        )
        (setq sz 0)
      )
    )
  )
  sz
)

(defun ocmema:dbg-print-lines-window (label lines / cnt head tail i)
  (setq cnt (length lines))
  (ocmema:dbg-io (strcat "[SAVE] " label "_count=" (itoa cnt)))
  (setq i 0)
  (while (and (< i cnt) (< i 10))
    (ocmema:dbg-io (strcat "[SAVE] " label "_first[" (itoa i) "]=" (nth i lines)))
    (setq i (1+ i))
  )
  (setq tail (if (> cnt 10) (- cnt 10) 0))
  (setq i tail)
  (while (< i cnt)
    (ocmema:dbg-io (strcat "[SAVE] " label "_last[" (itoa i) "]=" (nth i lines)))
    (setq i (1+ i))
  )
)

(defun ocmema:proj-cancelled (/)
  (ocmema:proj-log "Operacion cancelada.")
)

(defun ocmema:proj-msg (msg /)
  (prompt (strcat "\nOCMEMA: " msg))
)

(defun ocmema:get-scale (proj / raw sc)
  (setq raw (ocm-get proj "scale"))
  (cond
    ((numberp raw) (setq sc raw))
    ((= (type raw) 'STR) (setq sc (ocmema:pio-to-number raw)))
    (T (setq sc nil))
  )
  (if (or (not sc) (<= sc 0.0))
    (progn
      (ocmema:proj-warn "SCALE invalido o ausente; usando 1.0")
      (setq sc 1.0)
    )
  )
  sc
)

(defun ocm-k->sym (k / s r)
  (cond
    ((= (type k) 'SYM) k)
    ((= (type k) 'STR)
     (setq s (ocmema:str-trim k))
     (if (= s "")
       nil
       (progn
         (setq r (vl-catch-all-apply 'read (list s)))
         (if (vl-catch-all-error-p r) nil r)
       )
     )
    )
    (T k)
  )
)

(defun ocm-assoc-get (key alist / ksym kstr pair item target)
  (setq pair nil)
  (setq ksym (ocm-k->sym key))
  (if ksym (setq pair (assoc ksym alist)))
  (if (not pair)
    (progn
      (setq kstr
        (cond
          ((= (type key) 'STR) key)
          ((= (type key) 'SYM) (vl-symbol-name key))
          (T nil)
        )
      )
      (if kstr
        (progn
          (setq pair (assoc kstr alist))
          (if (not pair) (setq pair (assoc (strcase kstr) alist)))
        )
      )
    )
  )
  (if (not pair)
    (progn
      (setq target (strcase (vl-princ-to-string key)))
      (foreach item alist
        (if (and (not pair) (listp item))
          (if (= (strcase (vl-princ-to-string (car item))) target)
            (setq pair item)
          )
        )
      )
    )
  )
  pair
)

(defun ocmema:alist-get-kv (alist key /)
  (ocm-assoc-get key alist)
)

(defun ocmema:alist-get-any (alist key / kv)
  (setq kv (ocm-assoc-get key alist))
  (if kv (cdr kv) nil)
)

(defun ocmema:key-name (k / s)
  (setq s
    (cond
      ((= (type k) 'SYM) (vl-symbol-name k))
      ((= (type k) 'STR) (ocmema:str-trim k))
      (T (vl-princ-to-string k))
    )
  )
  (strcase s)
)

(defun ocmema:alist-del-key-any (alist key / out target item)
  (setq out '())
  (setq target (ocmema:key-name key))
  (foreach item alist
    (if (and (listp item) (car item))
      (if (/= (ocmema:key-name (car item)) target)
        (setq out (append out (list item)))
      )
      (setq out (append out (list item)))
    )
  )
  out
)

(defun ocm-get (alist key / kv)
  (setq kv (ocm-assoc-get key alist))
  (if kv (cdr kv) nil)
)

(defun ocmema:alist-keys (alist / out item)
  (setq out '())
  (if (and alist (listp alist))
    (foreach item alist
      (if (and (listp item) (car item))
        (setq out (append out (list (car item))))
      )
    )
  )
  out
)

(defun ocmema:pl-name->str (v / s)
  (cond
    ((= (type v) 'STR) (setq s v))
    ((= (type v) 'SYM) (setq s (vl-symbol-name v)))
    (T (setq s (vl-princ-to-string v)))
  )
  (setq s (vl-string-translate " " "" s))
  (strcase s)
)

(defun ocmema:pl-safe-apply (step fn args / r)
  (setq r (vl-catch-all-apply fn args))
  (if (vl-catch-all-error-p r)
    (progn
      (ocmema:proj-log (strcat "Error en " step ": " (vl-catch-all-error-message r)))
      nil
    )
    r
  )
)

(defun ocmema:pl-ensure-layer (name color / rec)
  (if (not (tblsearch "LAYER" name))
    (entmake (list (cons 0 "LAYER") (cons 2 name) (cons 70 0) (cons 62 color)))
  )
)

(defun ocmema:pl-valid-points-p (pts / ok p)
  (setq ok T)
  (if (and pts (listp pts))
    (foreach p pts
      (if ok
        (if (and (listp p)
                 (>= (length p) 2)
                 (numberp (car p))
                 (numberp (cadr p)))
          nil
          (setq ok nil)
        )
      )
    )
    (setq ok nil)
  )
  ok
)

(defun ocmema:pl-normalize-points (pts / out)
  (setq out pts)
  (if (and pts (listp pts) (numberp (car pts)) (numberp (cadr pts)))
    (setq out (ocmema:pl-flat->points pts))
  )
  out
)

(defun ocm-pts-valid-p (pts / ok p)
  (setq *ocm_pts_reason* nil)
  (cond
    ((not (and pts (listp pts)))
     (setq *ocm_pts_reason* "points_raw no es lista")
     nil
    )
    ((< (length pts) 2)
     (setq *ocm_pts_reason* "menos de 2 puntos")
     nil
    )
    (T
      (setq ok T)
      (foreach p pts
        (if ok
          (if (and (listp p)
                   (>= (length p) 2)
                   (numberp (car p))
                   (numberp (cadr p)))
            nil
            (progn
              (setq ok nil)
              (setq *ocm_pts_reason* "punto invalido (no numerico)")
            )
          )
        )
      )
      ok
    )
  )
)

(defun ocmema:pl-infer-beam-name (path / base)
  (if (and path (/= path ""))
    (setq base (vl-filename-base path))
  )
  base
)

(defun ocmema:pl-list-anl-files (folder / files)
  (if (and folder (/= folder ""))
    (setq files (vl-directory-files folder "*.ANL" 1))
  )
  files
)

(defun ocmema:list-anl (folder / files)
  (setq files (ocmema:pl-list-anl-files folder))
  (if (and files (listp files))
    (vl-sort files '<)
    nil
  )
)

(defun ocmema:load-safe (path / r)
  (setq r (vl-catch-all-apply 'load (list path)))
  (if (vl-catch-all-error-p r)
    (progn
      (ocmema:proj-warn (strcat "OCMEMA WARN: no se pudo cargar " path " (" (vl-catch-all-error-message r) ")"))
      nil
    )
    T
  )
)

(defun ocmema:safe-call (fn args / r)
  (setq r (vl-catch-all-apply fn args))
  (if (vl-catch-all-error-p r)
    (list nil (vl-catch-all-error-message r))
    (list T r)
  )
)

(defun ocmema:require-nerv-armado (/ path src-dir project-dir io-path io-dir dwg-dir candidates resolved loaded)
  (setq src-dir (if ocmema:*project* (ocmema:pio-assoc-get "src_dir" ocmema:*project*) nil))
  (setq project-dir (if (and ocmema:*project-path* (/= ocmema:*project-path* ""))
                      (vl-filename-directory ocmema:*project-path*)
                      nil
                    ))
  (setq io-path (findfile "OCMEMA_PROJECT_IO.lsp"))
  (setq io-dir (if io-path (vl-filename-directory io-path) nil))
  (setq dwg-dir (getvar "DWGPREFIX"))
  (setq candidates
    (list
      (if (and src-dir (/= src-dir "")) (ocmema:proj-join-path src-dir "DIBUJAR_NERV.lsp") nil)
      (if (and project-dir (/= project-dir "")) (ocmema:proj-join-path project-dir "DIBUJAR_NERV.lsp") nil)
      (if (and io-dir (/= io-dir "")) (ocmema:proj-join-path io-dir "DIBUJAR_NERV.lsp") nil)
      (if (and dwg-dir (/= dwg-dir "")) (ocmema:proj-join-path dwg-dir "DIBUJAR_NERV.lsp") nil)
      "DIBUJAR_NERV.lsp"
    )
  )
  (setq resolved nil)
  (foreach path candidates
    (if (and (not resolved) path (/= path ""))
      (progn
        (ocmema:dbg-io (strcat "require-nerv-armado: probando ruta " path))
        (setq resolved (findfile path))
      )
    )
  )
  (if resolved
    (progn
      (ocmema:dbg-io (strcat "require-nerv-armado: usando ruta " resolved))
      (setq loaded (ocmema:load-safe resolved))
      (if loaded
        T
        (progn
          (ocmema:proj-warn "No se pudo cargar DIBUJAR_NERV.lsp o no define ocmema:nerv:armar-from-anl")
          nil
        )
      )
    )
    (progn
      (ocmema:proj-warn "No se pudo cargar DIBUJAR_NERV.lsp o no define ocmema:nerv:armar-from-anl")
      nil
    )
  )
)

(defun ocmema:require-project-sidecar-lsp (filename / path src-dir project-dir io-path io-dir dwg-dir candidates resolved loaded)
  (setq src-dir (if ocmema:*project* (ocmema:pio-assoc-get "src_dir" ocmema:*project*) nil))
  (setq project-dir (if (and ocmema:*project-path* (/= ocmema:*project-path* ""))
                      (vl-filename-directory ocmema:*project-path*)
                      nil
                    ))
  (setq io-path (findfile "OCMEMA_PROJECT_IO.lsp"))
  (setq io-dir (if io-path (vl-filename-directory io-path) nil))
  (setq dwg-dir (getvar "DWGPREFIX"))
  (setq candidates
    (list
      (if (and src-dir (/= src-dir "")) (ocmema:proj-join-path src-dir filename) nil)
      (if (and project-dir (/= project-dir "")) (ocmema:proj-join-path project-dir filename) nil)
      (if (and io-dir (/= io-dir "")) (ocmema:proj-join-path io-dir filename) nil)
      (if (and dwg-dir (/= dwg-dir "")) (ocmema:proj-join-path dwg-dir filename) nil)
      filename
    )
  )
  (setq resolved nil)
  (foreach path candidates
    (if (and (not resolved) path (/= path ""))
      (progn
        (ocmema:dbg-io (strcat "require-sidecar-lsp: probando ruta " path))
        (setq resolved (findfile path))
      )
    )
  )
  (if resolved
    (progn
      (ocmema:dbg-io (strcat "require-sidecar-lsp: usando ruta " resolved))
      (setq loaded (ocmema:load-safe resolved))
      (if loaded
        T
        nil
      )
    )
    nil
  )
)

(defun ocmema:require-beam-const ()
  (if (ocmema:require-project-sidecar-lsp "DIBUJAR_TRABES_V0.lsp")
    T
    (progn
      (ocmema:proj-warn "No se pudo cargar DIBUJAR_TRABES_V0.lsp")
      nil
    )
  )
)

(defun ocmema:require-beam-var ()
  (if (ocmema:require-project-sidecar-lsp "DIBUJAR_TRABE_V_FINAL_FIXED10.lsp")
    T
    (progn
      (ocmema:proj-warn "No se pudo cargar DIBUJAR_TRABE_V_FINAL_FIXED10.lsp")
      nil
    )
  )
)

(defun ocmema:beam:line-tokens (line / s)
  (setq s (strcase (vl-string-translate "\t" " " (ocmema:str-trim line))))
  (ocmema:pio-split-list s " ")
)

(defun ocmema:beam:token-index (tokens target / idx found)
  (setq idx 0)
  (setq found nil)
  (while (and (< idx (length tokens)) (not found))
    (if (= (nth idx tokens) target)
      (setq found idx)
      (setq idx (1+ idx))
    )
  )
  found
)

(defun ocmema:beam:line-section-key (line / tokens idx key)
  (setq tokens (ocmema:beam:line-tokens line))
  (setq idx (ocmema:beam:token-index tokens "PRI"))
  (if (not idx)
    (setq idx (ocmema:beam:token-index tokens "PRIS"))
  )
  (if (and idx (< (1+ idx) (length tokens)))
    (setq key (ocmema:pio-join (ocmema:pio-sublist tokens (1+ idx) (length tokens)) " "))
    (setq key nil)
  )
  key
)

(defun ocmema:beam:member-line-p (line / u tokens first second third)
  (setq u (strcase (ocmema:str-trim line)))
  (setq tokens (ocmema:beam:line-tokens u))
  (if (and tokens (>= (length tokens) 3))
    (progn
      (setq first (nth 0 tokens))
      (setq second (nth 1 tokens))
      (setq third (nth 2 tokens))
      (if (and first second third
               (ocmema:pio-to-number (vl-string-translate "." "" first))
               (ocmema:pio-to-number (vl-string-translate "." "" second))
               (ocmema:pio-to-number (vl-string-translate "." "" third)))
        T
        nil
      )
    )
    nil
  )
)

(defun ocmema:beam:member-count-from-lines (lines / in-members count line u)
  (setq in-members nil)
  (setq count 0)
  (foreach line lines
    (setq u (strcase (ocmema:str-trim line)))
    (cond
      ((wcmatch u "*MEMBER INCIDENCES*")
       (setq in-members T)
      )
      (in-members
       (cond
         ((= u "") (setq in-members nil))
         ((or (wcmatch u "*ELEMENT PROPERTY*")
              (wcmatch u "*MEMBER PROPERTY*")
              (wcmatch u "*CONSTANTS*")
              (wcmatch u "*SUPPORTS*")
              (wcmatch u "*LOAD*")
              (wcmatch u "*FINISH*"))
          (setq in-members nil)
         )
         ((ocmema:beam:member-line-p line)
          (setq count (1+ count))
         )
       )
      )
    )
  )
  (if (> count 0) count nil)
)

(defun ocmema:beam:detect-section-info (anlPath / lines line key unique-sections pri-count member-count mode)
  (setq lines (ocmema:pio-read-lines anlPath))
  (setq unique-sections '())
  (setq pri-count 0)
  (if lines
    (foreach line lines
      (setq key (ocmema:beam:line-section-key line))
      (if key
        (progn
          (setq pri-count (1+ pri-count))
          (if (not (member key unique-sections))
            (setq unique-sections (append unique-sections (list key)))
          )
        )
      )
    )
  )
  (setq member-count (if lines (ocmema:beam:member-count-from-lines lines) nil))
  (if (>= (length unique-sections) 2)
    (setq mode "VAR")
    (setq mode "CONST")
  )
  (if (and (= mode "VAR") member-count (/= pri-count member-count))
    (ocmema:proj-warn "OCMEMA WARN: PRI count != MEMBER count (posible ANL raro). Aun asi se selecciono VAR por secciones distintas.")
  )
  (list
    (cons "mode" mode)
    (cons "uniqueSectionsCount" (length unique-sections))
    (cons "priCount" pri-count)
    (cons "memberCount" member-count)
    (cons "sections" unique-sections)
  )
)

(defun ocmema:beam:detect-section-mode (anlPath / info)
  (setq info (ocmema:beam:detect-section-info anlPath))
  (ocmema:pio-assoc-get "mode" info)
)

(defun ocmema:beam:run-const (anlPath / loaded)
  (setq loaded (ocmema:require-beam-const))
  (if loaded
    (ocmema:safe-call 'ocmema:beam:const-armar-from-anl (list anlPath))
    (list nil "No se pudo cargar DIBUJAR_TRABES_V0.lsp")
  )
)

(defun ocmema:beam:run-var (anlPath / loaded)
  (setq loaded (ocmema:require-beam-var))
  (if loaded
    (ocmema:safe-call 'ocmema:beam:var-armar-from-anl (list anlPath))
    (list nil "No se pudo cargar DIBUJAR_TRABE_V_FINAL_FIXED10.lsp")
  )
)

(defun ocmema:beam:run-by-mode (mode anlPath / res)
  (if (= mode "VAR")
    (setq res (ocmema:beam:run-var anlPath))
    (setq res (ocmema:beam:run-const anlPath))
  )
  res
)

(defun ocmema:pl-find-number-in-string (s / i ch start out)
  (setq out nil)
  (setq i 1)
  (setq start nil)
  (while (and (<= i (strlen s)) (not start))
    (setq ch (substr s i 1))
    (if (or (and (>= ch "0") (<= ch "9")) (= ch "-") (= ch "+"))
      (setq start i)
    )
    (setq i (1+ i))
  )
  (if start
    (progn
      (setq i start)
      (while (and (<= i (strlen s))
                  (wcmatch (substr s i 1) "[0-9.+-]"))
        (setq i (1+ i))
      )
      (setq out (substr s start (- i start)))
    )
  )
  out
)

(defun ocmema:anl-extract-widths-cm (path / lines line num widths tokens idx tok)
  (setq widths '())
  (setq *ocm_anl_reason* nil)
  (setq *ocm_anl_sample* nil)
  (if (and path (/= path "") (setq lines (ocmema:pio-read-lines path)))
    (foreach line lines
      (setq tokens (ocmema:pio-split-list line " "))
      (setq idx 0)
      (while (< idx (length tokens))
        (setq tok (strcase (nth idx tokens)))
        (if (or (= tok "ZD") (= tok "ZB"))
          (progn
            (setq num (ocmema:pio-to-number (nth (1+ idx) tokens)))
            (if num
              (progn
                (setq *ocm_anl_sample* line)
                (setq widths (append widths (list num)))
              )
            )
          )
        )
        (setq idx (1+ idx))
      )
    )
  )
  (if (not widths)
    (setq *ocm_anl_reason* "No se encontro Width en ANL")
  )
  widths
)

(defun ocmema:pl-find-beam-by-name-pure (name beams / norm item found iname)
  (setq found nil)
  (if (and name beams (listp beams))
    (progn
      (setq norm (ocmema:pl-name->str name))
      (foreach item beams
        (if (not found)
          (progn
            (setq iname (ocm-get item "name"))
            (if (= (ocmema:pl-name->str iname) norm)
              (setq found item)
            )
          )
        )
      )
    )
  )
  found
)

(defun ocmema:pl-flat->points (lst / out)
  (setq out '())
  (while (and lst (cdr lst))
    (setq out (append out (list (list (car lst) (cadr lst)))))
    (setq lst (cddr lst))
  )
  out
)

(defun ocmema:pt2d (p)
  (list (car p) (cadr p))
)

(defun ocm-vec-add (a b)
  (list (+ (car a) (car b)) (+ (cadr a) (cadr b)))
)

(defun ocm-vec-sub (a b)
  (list (- (car a) (car b)) (- (cadr a) (cadr b)))
)

(defun ocm-vec-scale (v s)
  (list (* (car v) s) (* (cadr v) s))
)

(defun ocm-vec-len (v)
  (sqrt (+ (* (car v) (car v)) (* (cadr v) (cadr v))))
)

(defun ocm-vec-unit (v / l)
  (setq l (ocm-vec-len v))
  (if (> l 1e-9)
    (ocm-vec-scale v (/ 1.0 l))
    (list 0.0 0.0)
  )
)

(defun ocm-perp-left (v)
  (list (- (cadr v)) (car v))
)

(defun ocmema:pl-seg-normal (p0 p1)
  (ocm-vec-unit (ocm-perp-left (ocm-vec-sub p1 p0)))
)

(defun ocmema:offset-polyline-const (pts d / n i p0 p1 p2 n0 n1 nsum out)
  (setq out '())
  (setq n (length pts))
  (if (>= n 2)
    (progn
      (setq p0 (ocmema:pt2d (nth 0 pts)))
      (setq p1 (ocmema:pt2d (nth 1 pts)))
      (setq n0 (ocmema:pl-seg-normal p0 p1))
      (setq out (append out (list (ocm-vec-add p0 (ocm-vec-scale n0 d)))))
      (setq i 1)
      (while (< i (1- n))
        (setq p0 (ocmema:pt2d (nth (1- i) pts)))
        (setq p1 (ocmema:pt2d (nth i pts)))
        (setq p2 (ocmema:pt2d (nth (1+ i) pts)))
        (setq n0 (ocmema:pl-seg-normal p0 p1))
        (setq n1 (ocmema:pl-seg-normal p1 p2))
        (setq nsum (ocm-vec-unit (ocm-vec-add n0 n1)))
        (setq out (append out (list (ocm-vec-add p1 (ocm-vec-scale nsum d)))))
        (setq i (1+ i))
      )
      (setq p0 (ocmema:pt2d (nth (- n 2) pts)))
      (setq p1 (ocmema:pt2d (nth (- n 1) pts)))
      (setq n0 (ocmema:pl-seg-normal p0 p1))
      (setq out (append out (list (ocm-vec-add p1 (ocm-vec-scale n0 d)))))
    )
  )
  out
)

(defun ocmema:offset-polyline-var (pts widths sign / out i p0 p1 n d)
  (setq out '())
  (setq i 0)
  (while (< i (1- (length pts)))
    (setq p0 (ocmema:pt2d (nth i pts)))
    (setq p1 (ocmema:pt2d (nth (1+ i) pts)))
    (setq n (ocmema:pl-seg-normal p0 p1))
    (setq d (* sign (/ (nth i widths) 2.0)))
    (if (= i 0)
      (setq out (append out (list (ocm-vec-add p0 (ocm-vec-scale n d)))))
    )
    (setq out (append out (list (ocm-vec-add p1 (ocm-vec-scale n d)))))
    (setq i (1+ i))
  )
  out
)

(defun ocmema:draw-lwpoly (pts closed layer color / data n ename)
  (setq n (length pts))
  (if (>= n 2)
    (progn
      (ocmema:pl-ensure-layer layer color)
      (setq data
        (append
          (list
            (cons 0 "LWPOLYLINE")
            (cons 100 "AcDbEntity")
            (cons 8 layer)
            (cons 62 color)
            (cons 100 "AcDbPolyline")
            (cons 90 n)
            (cons 70 (if closed 1 0))
          )
          (mapcar '(lambda (pt) (cons 10 (ocmema:pt2d pt))) pts)
        )
      )
      (setq ename (entmakex data))
      ename
    )
  )
)

(defun ocmema:dxf-set (dxf pair / key)
  (setq key (car pair))
  (if (assoc key dxf)
    (subst pair (assoc key dxf) dxf)
    (append dxf (list pair))
  )
)

(defun ocmema:safe-entmod (ename pairs / r d p)
  (setq r (vl-catch-all-apply 'entget (list ename)))
  (if (vl-catch-all-error-p r)
    (progn
      (ocmema:proj-warn (strcat "OCMEMA WARN: entget falló: " (vl-catch-all-error-message r)))
      nil
    )
    (progn
      (setq d r)
      (foreach p pairs (setq d (ocmema:dxf-set d p)))
      (setq r (vl-catch-all-apply 'entmod (list d)))
      (if (vl-catch-all-error-p r)
        (progn
          (ocmema:proj-warn (strcat "OCMEMA WARN: entmod falló: " (vl-catch-all-error-message r)))
          nil
        )
        T
      )
    )
  )
)

(defun ocmema:safe-entmakex (dxf / r)
  (setq r (vl-catch-all-apply 'entmakex (list dxf)))
  (if (vl-catch-all-error-p r)
    (progn
      (ocmema:proj-warn (strcat "OCMEMA WARN: entmakex falló: " (vl-catch-all-error-message r)))
      nil
    )
    r
  )
)

(defun ocmema:safe-leader (p1 p2 p3 / oldblk before e ent r)
  (setq oldblk (getvar "DIMBLK"))
  (setq before (entlast))
  (setvar "DIMBLK" "_CLOSED")
  (setq r (vl-catch-all-apply
    '(lambda ()
       (vl-cmdf "_.LEADER" p1 p2 p3 "")
       (vl-cmdf "O")
       (vl-cmdf "N")
       (vl-cmdf "")
     )
  ))
  (while (> (getvar "CMDACTIVE") 0) (vl-cmdf ""))
  (setvar "DIMBLK" oldblk)
  (if (vl-catch-all-error-p r)
    (progn
      (ocmema:proj-warn (strcat "OCMEMA WARN: leader omitido (" (vl-catch-all-error-message r) ")"))
      nil
    )
    (progn
      (setq ent nil)
      (setq e (if before (entnext before) (entnext)))
      (while e
        (if (= (cdr (assoc 0 (entget e))) "LEADER") (setq ent e))
        (setq e (entnext e))
      )
      (if ent
        (progn
          (ocmema:safe-entmod ent (list (cons 8 "TRABES") (cons 62 7)))
          ent
        )
        nil
      )
    )
  )
)

(defun ocmema:pl-center-midpoint (ent / dxf typ p0 p1 pts)
  (if (and ent (= (type ent) 'ENAME))
    (progn
      (setq dxf (entget ent))
      (setq typ (cdr (assoc 0 dxf)))
      (cond
        ((= typ "LINE")
         (setq p0 (cdr (assoc 10 dxf)))
         (setq p1 (cdr (assoc 11 dxf)))
        )
        ((= typ "LWPOLYLINE")
         (setq pts '())
         (foreach p dxf
           (if (= (car p) 10) (setq pts (append pts (list (cdr p)))))
         )
         (if (and pts (>= (length pts) 2))
           (progn
             (setq p0 (car pts))
             (setq p1 (nth (1- (length pts)) pts))
           )
         )
        )
      )
      (if (and p0 p1)
        (list (/ (+ (car p0) (car p1)) 2.0) (/ (+ (cadr p0) (cadr p1)) 2.0))
        nil
      )
    )
    nil
  )
)

(defun ocmema:safe-label (beamName centerEnt / before mid p1 p2 p3 ptText lead txt ok ent typ e)
  (setq ok "FAIL")
  (setq mid (ocmema:pl-center-midpoint centerEnt))
  (if mid
    (progn
      (setq before (entlast))
      (setq p1 mid)
      (setq p2 (list (+ (car p1) 0.4) (+ (cadr p1) 0.65)))
      (setq p3 (list (+ (car p2) 0.18) (cadr p2)))
      (setq lead (ocmema:safe-leader p1 p2 p3))
      (setq ptText (polar p3 0.0 0.05))
      (setq txt (ocmema:safe-entmakex
        (list
          (cons 0 "TEXT")
          (cons 100 "AcDbEntity")
          (cons 8 ocmema:*pl-centerline-layer*)
          (cons 62 3)
          (cons 100 "AcDbText")
          (cons 10 ptText)
          (cons 11 ptText)
          (cons 40 0.18)
          (cons 1 (if (= (type beamName) 'STR) beamName (vl-princ-to-string beamName)))
          (cons 50 0.0)
          (cons 72 0)
          (cons 73 2)
          (cons 7 (getvar "TEXTSTYLE"))
        )
      ))
      (if txt (setq ok "OK"))

      (setq e (if before (entnext before) (entnext)))
      (while e
        (setq typ (cdr (assoc 0 (entget e))))
        (if (and (member typ (list "SOLID" "LWPOLYLINE" "POLYLINE" "MTEXT"))
                 (not (eq e lead)))
          (entdel e)
        )
        (setq e (entnext e))
      )
    )
    (ocmema:proj-warn "OCMEMA WARN: etiqueta omitida (centerline insuficiente)")
  )
  ok
)

(defun ocmema:safe-hatch-activex (boundaryEnt / r obj ms hatch loops ok)
  (setq ok nil)
  (setq r (vl-catch-all-apply
    '(lambda ()
       (vl-load-com)
       (setq obj (vlax-ename->vla-object boundaryEnt))
       (setq ms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
       (setq hatch (vla-AddHatch ms 0 "AR-CONC" :vlax-true))
       (vla-put-PatternScale hatch 0.003)
       (vla-put-Color hatch 8)
       (setq loops (vlax-make-safearray vlax-vbObject '(0 . 0)))
       (vlax-safearray-put-element loops 0 obj)
       (vla-AppendOuterLoop hatch loops)
       (vla-Evaluate hatch)
       (setq ok "OK")
     )
  ))
  (if (vl-catch-all-error-p r)
    (progn
      (ocmema:proj-warn (strcat "OCMEMA WARN: hatch ActiveX omitido (" (vl-catch-all-error-message r) ")"))
      nil
    )
    ok
  )
)

(defun ocmema:safe-hatch (boundaryEnt / prev cur r sel ok)
  (setq ok (ocmema:safe-hatch-activex boundaryEnt))
  (if ok
    ok
    (progn
      (setq prev (entlast))
      (setq cur nil)
      (setq sel (ssadd boundaryEnt))
      (setq r (vl-catch-all-apply
        '(lambda () (command "_.-HATCH" "_S" sel "" "_P" "AR-CONC" "_S" "0.003" "_C" "8" ""))
      ))
      (if (vl-catch-all-error-p r)
        (progn
          (ocmema:proj-warn (strcat "OCMEMA WARN: hatch omitido (" (vl-catch-all-error-message r) ")"))
          nil
        )
        (progn
          (setq cur (entlast))
          (if (and cur (not (eq cur prev)) (= (cdr (assoc 0 (entget cur))) "HATCH"))
            "OK"
            (progn
              (ocmema:proj-warn "OCMEMA WARN: hatch omitido (incompatible)")
              nil
            )
          )
        )
      )
    )
  )
)

(defun ocmema:beam-post-format (beamName boundaryEnt centerEnt / labelOk hatchOk typ r)
  (setq labelOk "FAIL")
  (setq hatchOk "FAIL")
  (ocmema:pl-ensure-layer "TRABE-PROY" 256)
  (ocmema:pl-ensure-layer "TRABES" 256)
  (if (and centerEnt (= (type centerEnt) 'ENAME))
    (progn
      (setq r (vl-catch-all-apply 'entget (list centerEnt)))
      (if (vl-catch-all-error-p r)
        (progn
          (ocmema:proj-warn (strcat "OCMEMA WARN: entget centerline falló: " (vl-catch-all-error-message r)))
          (setq typ nil)
        )
        (setq typ (cdr (assoc 0 r)))
      )
      (if (= typ "LWPOLYLINE")
        (ocmema:safe-entmod centerEnt (list (cons 8 "TRABES") (cons 48 0.5) (cons 43 0.08) (cons 40 0.08) (cons 41 0.08)))
        (ocmema:safe-entmod centerEnt (list (cons 8 "TRABES") (cons 48 0.5)))
      )
      (setq labelOk (ocmema:safe-label beamName centerEnt))
    )
    (ocmema:proj-warn "OCMEMA WARN: centerline nil; etiqueta omitida")
  )
  (if (and boundaryEnt (= (type boundaryEnt) 'ENAME))
    (setq hatchOk (ocmema:safe-hatch boundaryEnt))
    (ocmema:proj-warn "OCMEMA WARN: hatch omitido (contorno nil)")
  )
  (list labelOk hatchOk)
)

(defun ocmema:pl-units-factor (units / u)
  (setq u (strcase (if units units "")))
  (cond
    ((= u "M") 0.01)
    ((= u "MM") 10.0)
    (T 1.0)
  )
)

(defun ocmema:cm->draw (cm scale / m sc)
  (setq sc scale)
  (if (or (not sc) (= sc 0.0)) (setq sc 1.0))
  (setq m (/ cm 100.0))
  (/ m sc)
)

(defun ocmema:pl-widths-cm->draw (widths units scale / out w)
  (setq out '())
  (foreach w widths
    (if (numberp w)
      (setq out (append out (list (ocmema:cm->draw w scale))))
    )
  )
  out
)

(defun ocmema:pl-cm-to-draw (cm units scale /)
  (if (numberp cm)
    (ocmema:cm->draw cm scale)
    0.0
  )
)

(defun ocmema:pl-wall-ext-draw (wall_cm units scale / ext_cm)
  (setq ext_cm (/ wall_cm 2.0))
  (ocmema:cm->draw ext_cm scale)
)

(defun ocmema:pl-get-extents (mode wall_cm units scale / ext_ini ext_fin)
  (cond
    ((= mode "M")
     (setq ext_ini (getreal "\nExt ini (cm): "))
     (setq ext_fin (getreal "\nExt fin (cm): "))
     (if (not ext_ini) (setq ext_ini 0.0))
     (if (not ext_fin) (setq ext_fin 0.0))
     (setq ext_ini (ocmema:pl-cm-to-draw ext_ini units scale))
     (setq ext_fin (ocmema:pl-cm-to-draw ext_fin units scale))
    )
    (T
     (setq ext_ini (ocmema:pl-wall-ext-draw wall_cm units scale))
     (setq ext_fin ext_ini)
    )
  )
  (list ext_ini ext_fin)
)

(defun ocmema:pl-extend-seg (p0 p1 ext0 ext1 / d)
  (setq d (ocm-vec-unit (ocm-vec-sub p1 p0)))
  (list
    (ocm-vec-sub p0 (ocm-vec-scale d ext0))
    (ocm-vec-add p1 (ocm-vec-scale d ext1))
  )
)

(defun ocmema:pl-extend-poly-ends (pts ext_ini ext_fin / n p0 p1 pN1 pN d0 dN out)
  (setq n (length pts))
  (if (< n 2)
    pts
    (progn
      (setq p0 (nth 0 pts))
      (setq p1 (nth 1 pts))
      (setq pN1 (nth (- n 2) pts))
      (setq pN (nth (1- n) pts))
      (setq d0 (ocm-vec-unit (ocm-vec-sub p1 p0)))
      (setq dN (ocm-vec-unit (ocm-vec-sub pN pN1)))
      (setq out (list (ocm-vec-sub p0 (ocm-vec-scale d0 ext_ini))))
      (if (> n 2)
        (setq out (append out (ocmema:pio-sublist pts 1 (1- n))))
      )
      (setq out (append out (list (ocm-vec-add pN (ocm-vec-scale dN ext_fin)))))
      out
    )
  )
)

(defun ocmema:pl-extend-ends-dir (pts dir ext_ini ext_fin / out p0 pN)
  (if (and pts (>= (length pts) 2))
    (progn
      (setq out pts)
      (setq p0 (car out))
      (setq pN (nth (1- (length out)) out))
      (setq out (subst (ocm-vec-sub p0 (ocm-vec-scale dir ext_ini)) p0 out))
      (setq out (subst (ocm-vec-add pN (ocm-vec-scale dir ext_fin)) pN out))
      out
    )
    pts
  )
)
(defun ocmema:pl-build-rect (edge1 edge2 / p0 p1 p2 p3)
  (setq p0 (car edge1))
  (setq p1 (cadr edge1))
  (setq p2 (cadr edge2))
  (setq p3 (car edge2))
  (list p0 p1 p2 p3)
)

(defun ocmema:pl-centerline-from-align (pts align w / a d sign)
  (setq a (strcase (if align align "")))
  (cond
    ((or (= a "") (= a "C")) pts)
    ((or (= a "I") (= a "D"))
     (setq sign (if (= a "I") -1.0 1.0))
     (setq d (* sign (/ w 2.0)))
     (ocmema:offset-polyline-const pts d)
    )
    (T pts)
  )
)

(defun ocmema:pl-build-outline-const (center w / left right)
  (setq left (ocmema:offset-polyline-const center (/ w 2.0)))
  (setq right (ocmema:offset-polyline-const center (/ (- w) 2.0)))
  (append left (reverse right))
)

(defun ocmema:pl-build-outline-var (center widths / left right)
  (setq left (ocmema:offset-polyline-var center widths 1.0))
  (setq right (ocmema:offset-polyline-var center widths -1.0))
  (append left (reverse right))
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
    (T T)
  )
)

(defun ocmema:proj-get-beams (/)
  (ocmema:proj-get 'beams)
)

(defun ocmema:proj-get-ribs (/)
  (ocmema:proj-get 'ribs)
)

(defun ocmema:proj-get (key /)
  (ocm-get ocmema:*project* key)
)

(defun ocmema:proj-set (key val / proj)
  (setq proj ocmema:*project*)
  (if proj
    (progn
      (setq proj (ocmema:pio-alist-set key val proj))
      (setq ocmema:*project* proj)
      (if ocmema:*debug-io*
        (ocmema:dbg-io (strcat "[DEBUG] proj-set key=" (ocmema:key-name key)
                               " project_object_ptr=" (vl-princ-to-string ocmema:*project*)))
      )
      val
    )
    nil
  )
)

(defun ocmema:list-replace-nth (lst n val / out i)
  (setq out '())
  (setq i 0)
  (foreach item lst
    (if (= i n)
      (setq out (append out (list val)))
      (setq out (append out (list item)))
    )
    (setq i (1+ i))
  )
  out
)

;; Update plant fields by 1-based user index (no autosave here).
(defun ocmema:proj-set-plant-fields (plant_idx_user fields / idx0 plants plant updated kv key)
  (setq idx0 (1- plant_idx_user))
  (setq plants (ocmema:proj-get 'plants))
  (if (or (not plants) (< idx0 0) (>= idx0 (length plants)))
    nil
    (progn
      (setq plant (nth idx0 plants))
      (setq updated plant)
      (foreach kv fields
        (setq key (car kv))
        (cond
          ((or (eq key 'slab_h_total_cm) (= key "slab_h_total_cm"))
           (setq updated (ocmema:pio-alist-set 'slab_h_total_cm (cdr kv) updated))
          )
          ((or (eq key 'slab_h_comp_cm) (= key "slab_h_comp_cm"))
           (setq updated (ocmema:pio-alist-set 'slab_h_comp_cm (cdr kv) updated))
          )
          ((or (eq key 'rib_spacing_cm) (= key "rib_spacing_cm"))
           (setq updated (ocmema:pio-alist-set 'rib_spacing_cm (cdr kv) updated))
          )
          (T
           (setq updated (ocmema:pio-alist-set key (cdr kv) updated))
          )
        )
      )
      (setq plants (ocmema:list-replace-nth plants idx0 updated))
      (ocmema:proj-set 'plants plants)
      (setq plants (ocmema:proj-get 'plants))
      (if ocmema:*debug-io*
        (princ
          (strcat
            "\n[DEBUG] set-plant idx_user=" (itoa plant_idx_user)
            " idx0=" (itoa idx0)
            " project_object_ptr=" (vl-princ-to-string ocmema:*project*)
            " slab_h_total=" (vl-princ-to-string (cdr (assoc 'slab_h_total_cm (nth idx0 plants))))
            " slab_h_comp=" (vl-princ-to-string (cdr (assoc 'slab_h_comp_cm (nth idx0 plants))))
            " rib_spacing=" (vl-princ-to-string (cdr (assoc 'rib_spacing_cm (nth idx0 plants))))
          )
        )
      )
      (if ocmema:*debug-io*
        (ocmema:dbg-io
          (strcat
            "[DEBUG] after_set_readback slab_h_total="
            (vl-princ-to-string (ocmema:pio-assoc-get 'slab_h_total_cm (nth idx0 plants)))
            " slab_h_comp="
            (vl-princ-to-string (ocmema:pio-assoc-get 'slab_h_comp_cm (nth idx0 plants)))
            " rib_spacing="
            (vl-princ-to-string (ocmema:pio-assoc-get 'rib_spacing_cm (nth idx0 plants)))
          )
        )
      )
      T
    )
  )
)

;; Internal CRUD API for beams/ribs (keeps project consistent + autosave)
(defun ocmema:proj-delete-beam (name / proj beams out item norm found)
  (setq proj ocmema:*project*)
  (setq beams (ocmema:proj-get-beams))
  (setq out '())
  (setq found nil)
  (setq norm (ocmema:pio-normalize-name name))
  (foreach item beams
    (if (= (ocmema:pio-normalize-name (ocmema:pio-assoc-get "name" item)) norm)
      (setq found T)
      (setq out (append out (list item)))
    )
  )
  (if found
    (progn
      (setq proj (ocmema:pio-alist-set "beams" out proj))
      (setq ocmema:*project* proj)
      (ocmema:proj-autosave)
      T
    )
    nil
  )
)

(defun ocmema:proj-delete-rib (name / proj ribs out item norm found)
  (setq proj ocmema:*project*)
  (setq ribs (ocmema:proj-get-ribs))
  (setq out '())
  (setq found nil)
  (setq norm (ocmema:pio-normalize-name name))
  (foreach item ribs
    (if (= (ocmema:pio-normalize-name (ocmema:pio-assoc-get "name" item)) norm)
      (setq found T)
      (setq out (append out (list item)))
    )
  )
  (if found
    (progn
      (setq proj (ocmema:pio-alist-set "ribs" out proj))
      (setq ocmema:*project* proj)
      (ocmema:proj-autosave)
      T
    )
    nil
  )
)

(defun ocmema:proj-rename-beam (old new / proj beams out item norm-old norm-new found)
  (setq proj ocmema:*project*)
  (setq beams (ocmema:proj-get-beams))
  (setq out '())
  (setq found nil)
  (setq norm-old (ocmema:pio-normalize-name old))
  (setq norm-new (ocmema:pio-normalize-name new))
  (if (= norm-new "") (setq norm-new norm-old))
  (if (and (/= norm-new "") (ocmema:proj-beam-name-exists new) (/= norm-new norm-old))
    nil
    (progn
      (foreach item beams
        (if (= (ocmema:pio-normalize-name (ocmema:pio-assoc-get "name" item)) norm-old)
          (progn
            (setq item (ocmema:pio-alist-set "name" new item))
            (setq found T)
          )
        )
        (setq out (append out (list item)))
      )
      (if found
        (progn
          (setq proj (ocmema:pio-alist-set "beams" out proj))
          (setq ocmema:*project* proj)
          (ocmema:proj-autosave)
          T
        )
        nil
      )
    )
  )
)

(defun ocmema:proj-rename-rib (old new / proj ribs out item norm-old norm-new found)
  (setq proj ocmema:*project*)
  (setq ribs (ocmema:proj-get-ribs))
  (setq out '())
  (setq found nil)
  (setq norm-old (ocmema:pio-normalize-name old))
  (setq norm-new (ocmema:pio-normalize-name new))
  (if (= norm-new "") (setq norm-new norm-old))
  (if (and (/= norm-new "") (ocmema:proj-rib-name-exists new) (/= norm-new norm-old))
    nil
    (progn
      (foreach item ribs
        (if (= (ocmema:pio-normalize-name (ocmema:pio-assoc-get "name" item)) norm-old)
          (progn
            (setq item (ocmema:pio-alist-set "name" new item))
            (setq found T)
          )
        )
        (setq out (append out (list item)))
      )
      (if found
        (progn
          (setq proj (ocmema:pio-alist-set "ribs" out proj))
          (setq ocmema:*project* proj)
          (ocmema:proj-autosave)
          T
        )
        nil
      )
    )
  )
)

(defun ocmema:proj-update-beam (name kv-alist / proj beams out item norm found kv)
  (setq proj ocmema:*project*)
  (setq beams (ocmema:proj-get-beams))
  (setq out '())
  (setq found nil)
  (setq norm (ocmema:pio-normalize-name name))
  (foreach item beams
    (if (= (ocmema:pio-normalize-name (ocmema:pio-assoc-get "name" item)) norm)
      (progn
        (foreach kv kv-alist
          (setq item (ocmema:pio-alist-set (car kv) (cdr kv) item))
        )
        (setq found T)
      )
    )
    (setq out (append out (list item)))
  )
  (if found
    (progn
      (setq proj (ocmema:pio-alist-set "beams" out proj))
      (setq ocmema:*project* proj)
      (ocmema:proj-autosave)
      T
    )
    nil
  )
)

(defun ocmema:proj-update-rib (name kv-alist / proj ribs out item norm found kv)
  (setq proj ocmema:*project*)
  (setq ribs (ocmema:proj-get-ribs))
  (setq out '())
  (setq found nil)
  (setq norm (ocmema:pio-normalize-name name))
  (foreach item ribs
    (if (= (ocmema:pio-normalize-name (ocmema:pio-assoc-get "name" item)) norm)
      (progn
        (foreach kv kv-alist
          (setq item (ocmema:pio-alist-set (car kv) (cdr kv) item))
        )
        (setq found T)
      )
    )
    (setq out (append out (list item)))
  )
  (if found
    (progn
      (setq proj (ocmema:pio-alist-set "ribs" out proj))
      (setq ocmema:*project* proj)
      (ocmema:proj-autosave)
      T
    )
    nil
  )
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

(defun ocmema:proj-upsert-rib (rib / ribs out item name norm)
  (setq ribs (ocmema:proj-get 'ribs))
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
  (ocmema:proj-set 'ribs out)
  (if ocmema:*debug-io*
    (ocmema:dbg-io
      (strcat
        "[UPsert] ribs=" (itoa (length (ocmema:proj-get 'ribs)))
        " beams=" (itoa (length (ocmema:proj-get 'beams)))
        " project_object_ptr=" (vl-princ-to-string ocmema:*project*)
      )
    )
  )
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

(defun ocmema:proj-upsert-beam (beam / beams out item name norm)
  (setq beams (ocmema:proj-get 'beams))
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
  (ocmema:proj-set 'beams out)
  (if ocmema:*debug-io*
    (ocmema:dbg-io
      (strcat
        "[UPsert] ribs=" (itoa (length (ocmema:proj-get 'ribs)))
        " beams=" (itoa (length (ocmema:proj-get 'beams)))
        " project_object_ptr=" (vl-princ-to-string ocmema:*project*)
      )
    )
  )
)

(defun ocmema:proj-update-beam-points (name points / proj beams out item found)
  (setq proj ocmema:*project*)
  (setq beams (ocmema:proj-get-beams))
  (setq out '())
  (setq found nil)
  (foreach item beams
    (if (= (ocmema:pio-normalize-name (ocmema:pio-assoc-get "name" item))
           (ocmema:pio-normalize-name name))
      (progn
        (setq item (ocmema:pio-alist-set "points_raw" points item))
        (setq item (ocmema:pio-alist-set "n_points" (length points) item))
        (setq item (ocmema:pio-alist-set "drawable_plan" T item))
        (setq found T)
      )
    )
    (setq out (append out (list item)))
  )
  (setq proj (ocmema:pio-alist-set "beams" out proj))
  (setq ocmema:*project* proj)
  found
)

(defun ocmema:proj-update-rib-points (name points / proj ribs out item found)
  (setq proj ocmema:*project*)
  (setq ribs (ocmema:proj-get-ribs))
  (setq out '())
  (setq found nil)
  (foreach item ribs
    (if (= (ocmema:pio-normalize-name (ocmema:pio-assoc-get "name" item))
           (ocmema:pio-normalize-name name))
      (progn
        (setq item (ocmema:pio-alist-set "points_raw" points item))
        (setq item (ocmema:pio-alist-set "n_points" (length points) item))
        (setq found T)
      )
    )
    (setq out (append out (list item)))
  )
  (setq proj (ocmema:pio-alist-set "ribs" out proj))
  (setq ocmema:*project* proj)
  found
)

(defun ocmema:proj-get-units (/ proj unit)
  (setq proj ocmema:*project*)
  (setq unit (ocmema:pio-assoc-get "units" proj))
  (if (and unit (/= unit "")) unit nil)
)

(defun ocmema:proj-get-scale (/ proj)
  (setq proj ocmema:*project*)
  (ocmema:get-scale proj)
)

(defun ocmema:proj-set-units-scale (unit scale / proj)
  (setq proj ocmema:*project*)
  (setq proj (ocmema:pio-alist-set "draw_units" unit proj))
  (setq proj (ocmema:pio-alist-set "draw_scale_factor" scale proj))
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
        (setq cx (length (if (ocmema:pio-assoc-get "x_axes" plant) (ocmema:pio-assoc-get "x_axes" plant) '())))
        (setq cy (length (if (ocmema:pio-assoc-get "y_axes" plant) (ocmema:pio-assoc-get "y_axes" plant) '())))
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

(defun ocmema:proj-ensure-dir-sep (dir / len)
  (if (or (not dir) (= dir ""))
    ""
    (progn
      (setq len (strlen dir))
      (if (= (substr dir len 1) "\\")
        dir
        (strcat dir "\\")
      )
    )
  )
)

(defun ocmema:proj-ensure-txt (path / len)
  (if (not path)
    nil
    (progn
      (setq len (strlen path))
      (if (and (>= len 4) (= (strcase (substr path (- len 3) 4)) ".TXT"))
        path
        (strcat path ".txt")
      )
    )
  )
)

(defun ocmema:proj-path-dir (path / dir)
  (setq dir nil)
  (if (and path (/= path ""))
    (setq dir (vl-filename-directory path))
  )
  dir
)

(defun ocmema:proj-default-save-suggest (/ dir base)
  (setq dir ocmema:*last-project-dir*)
  (if (or (not dir) (= dir ""))
    (setq dir (getvar "DWGPREFIX"))
  )
  (if (and dir (/= dir ""))
    (progn
      (setq base (ocmema:proj-ensure-dir-sep dir))
      (strcat base "OCMEMA_PROJECT.txt")
    )
    ""
  )
)

(defun ocmema:proj-update-path-state (path / dir)
  (if (and path (/= path ""))
    (progn
      (setq ocmema:*project-path* path)
      (setq dir (ocmema:proj-path-dir path))
      (if (and dir (/= dir "")) (setq ocmema:*last-project-dir* dir))
      (if ocmema:*project*
        (setq ocmema:*project* (ocmema:pio-alist-set "project_path" path ocmema:*project*))
      )
    )
  )
)

(defun ocmema:proj-autosave (/ path ok)
  (if (not ocmema:*project*)
    nil
    (progn
      (if ocmema:*debug-io*
        (ocmema:dbg-io (strcat "[CALL] autosave from " (if ocmema:*save-caller* ocmema:*save-caller* "unknown")))
      )
      (setq path (ocmema:pio-assoc-get "project_path" ocmema:*project*))
      (if (or (not path) (= path ""))
        (setq path (if ocmema:*project-path* ocmema:*project-path* (ocmema:proj-default-path)))
      )
      (setq ok (ocmema:pio-save-project path))
      (if ok
        (ocmema:proj-log "Proyecto guardado.")
        (ocmema:proj-log "No se pudo guardar el proyecto.")
      )
      ok
    )
  )
)

(defun ocmema:proj-autosave-from (caller / prev ok)
  (setq prev ocmema:*save-caller*)
  (setq ocmema:*save-caller* caller)
  (setq ok (ocmema:proj-autosave))
  (setq ocmema:*save-caller* prev)
  ok
)

(defun ocmema:proj-normalize-load-combo-set (raw / up)
  (setq up (if raw (strcase (ocmema:str-trim raw)) ""))
  (cond
    ((= up "13_15") "13_15")
    (T "12_16")
  )
)

(defun ocmema:proj-load-combo-set-label (comboSet / normalized)
  (setq normalized (ocmema:proj-normalize-load-combo-set comboSet))
  (if (= normalized "13_15") "B" "A")
)

(defun ocmema:proj-get-load-combo-set (/)
  (if ocmema:*project*
    (ocmema:proj-normalize-load-combo-set (ocmema:pio-assoc-get "load_combo_set" ocmema:*project*))
    "12_16"
  )
)

(defun ocmema:proj-prompt-load-combo-set-choice (/ choice)
  (initget "A B")
  (setq choice (getkword "\nCombinaciones factorizadas D+L: [A 1.2D+1.6L / B 1.3D+1.5L] <A>: "))
  (if (= choice "B") "13_15" "12_16")
)

(defun ocmema:proj-ensure-load-combo-set (/ raw current label useSaved comboSet saveChoice res)
  (if (not ocmema:*project*)
    (progn
      (ocmema:proj-warn "OCMEMA ERROR: Proyecto no cargado; no se puede obtener LOAD_COMBO_SET. Carga proyecto con OCMEMA_PROJ primero.")
      nil
    )
    (progn
      (setq raw (ocmema:pio-assoc-get "load_combo_set" ocmema:*project*))
      (setq current (ocmema:proj-normalize-load-combo-set raw))
      (if (and raw (/= (ocmema:str-trim raw) ""))
        (progn
          (setq label (ocmema:proj-load-combo-set-label current))
          (initget "S N")
          (setq useSaved (getkword (strcat "\nUsar combinaciones factorizadas guardadas (" label ")? [S/N] <S>: ")))
          (if (or (not useSaved) (= useSaved "S"))
            current
            (progn
              (setq comboSet (ocmema:proj-prompt-load-combo-set-choice))
              (setq ocmema:*project* (ocmema:pio-alist-set "load_combo_set" comboSet ocmema:*project*))
              (initget "S N")
              (setq saveChoice (getkword "\nGuardar/actualizar esta preferencia en el TXT del proyecto? [S/N] <S>: "))
              (if (or (not saveChoice) (= saveChoice "S"))
                (setq res (ocmema:proj-autosave-from "LOAD_COMBO_SET update"))
              )
              comboSet
            )
          )
        )
        (progn
          (setq comboSet (ocmema:proj-prompt-load-combo-set-choice))
          (setq ocmema:*project* (ocmema:pio-alist-set "load_combo_set" comboSet ocmema:*project*))
          (initget "S N")
          (setq saveChoice (getkword "\nGuardar esta preferencia en el TXT del proyecto? [S/N] <S>: "))
          (if (or (not saveChoice) (= saveChoice "S"))
            (setq res (ocmema:proj-autosave-from "LOAD_COMBO_SET init"))
          )
          comboSet
        )
      )
    )
  )
)

;; File dialogs (local, no VisualLISP)
(defun ocmema:proj-getfile-open (prompt / f)
  (setq f (getfiled prompt (ocmema:proj-default-save-suggest) "txt" 0))
  f
)

(defun ocmema:proj-getfile-save (prompt suggest / f)
  (setq f (getfiled prompt (ocmema:proj-default-save-suggest) "txt" 1))
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

;; Safe nth helper (returns default if index missing)
(defun ocmema:pio-nth-safe (lst idx def /)
  (if (and lst (>= (length lst) (1+ idx)))
    (nth idx lst)
    def
  )
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
  (setq pair (ocm-assoc-get key alist))
  (if pair (cdr pair) nil)
)

(defun ocmema:pio-alist-set (key val alist / cleaned)
  (setq cleaned (ocmema:alist-del-key-any alist key))
  (append cleaned (list (cons key val)))
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

(defun ocmema:pio-save-project (path / proj lines plants plant x_axes y_axes pname xnames ynames tmp ok beams beam ribs rib v r
                                      seq ts plantCount beamCount ribCount slabTotal slabComp ribSpacing hasSlab
                                      linesRead fileExists fileSize verifyBeams verifyRibs verifySlabs
                                      slabPlants slabPlant slabIdx block blockOk)
  (setq proj ocmema:*project*)
  (if ocmema:*debug-io*
    (progn
      (setq ocmema:*save-seq* (1+ (if ocmema:*save-seq* ocmema:*save-seq* 0)))
      (setq seq ocmema:*save-seq*)
      (setq ts (rtos (getvar "CDATE") 2 6))
      (ocmema:dbg-io "[SAVE] start")
      (ocmema:dbg-io (strcat "[SAVE] seq=" (itoa seq) " time=" ts))
      (ocmema:dbg-io (strcat "[SAVE] path=" (if path path "<nil>")))
      (ocmema:dbg-io (strcat "[SAVE] findfile_before=" (if (and path (findfile path)) "T" "NIL")))
      (ocmema:dbg-io (strcat "[SAVE] project_bound=" (ocmema:dbg-bool (boundp 'ocmema:*project*))
                             " project_non_nil=" (ocmema:dbg-bool ocmema:*project*)))
      (ocmema:dbg-io (strcat "[SAVE] project_object_ptr=" (vl-princ-to-string ocmema:*project*)))
    )
  )
  (if (not proj)
    (progn (ocmema:proj-log "No hay proyecto cargado.") nil)
    (progn
      (setq plants (ocmema:pio-assoc-get 'plants proj))
      (setq beams (ocmema:pio-assoc-get 'beams proj))
      (setq ribs (ocmema:pio-assoc-get 'ribs proj))
      (setq plantCount (length plants))
      (setq beamCount (length beams))
      (setq ribCount (length ribs))
      (if ocmema:*debug-io*
        (progn
          (ocmema:dbg-io (strcat "[SAVE] plants=" (itoa plantCount) " beams=" (itoa beamCount) " ribs=" (itoa ribCount)))
          (foreach plant plants
            (setq slabTotal (ocmema:pio-assoc-get 'slab_h_total_cm plant))
            (setq slabComp (ocmema:pio-assoc-get 'slab_h_comp_cm plant))
            (setq ribSpacing (ocmema:pio-assoc-get 'rib_spacing_cm plant))
            (setq hasSlab (or (numberp slabTotal) (numberp slabComp) (numberp ribSpacing)))
            (ocmema:dbg-io
              (strcat
                "[SAVE] plant "
                (itoa (if (ocmema:pio-assoc-get "idx" plant) (ocmema:pio-assoc-get "idx" plant) 0))
                " name=" (if (ocmema:pio-assoc-get "name" plant) (ocmema:pio-assoc-get "name" plant) "")
                " has_slab=" (ocmema:dbg-bool hasSlab)
                " slab_h_total=" (if (numberp slabTotal) (rtos slabTotal 2 6) "NIL")
                " slab_h_comp=" (if (numberp slabComp) (rtos slabComp 2 6) "NIL")
                " rib_spacing=" (if (numberp ribSpacing) (rtos ribSpacing 2 6) "NIL")
              )
            )
          )
          (ocmema:dbg-io
            (strcat
              "[SAVE] hasKey project fc?=" (ocmema:dbg-bool (or (ocmema:pio-assoc-get "fc" proj) (ocmema:pio-assoc-get "fc_kgcm2" proj)))
              " ec?=" (ocmema:dbg-bool (or (ocmema:pio-assoc-get "ec" proj) (ocmema:pio-assoc-get "ec_kgcm2" proj)))
              " dir_beams?=" (ocmema:dbg-bool (or (ocmema:pio-assoc-get "dir_beams" proj) (ocmema:pio-assoc-get "dir_beams_std" proj)))
              " dir_ribs?=" (ocmema:dbg-bool (or (ocmema:pio-assoc-get "dir_ribs" proj) (ocmema:pio-assoc-get "dir_ribs_std" proj)))
            )
          )
        )
      )
      (setq path (ocmema:proj-ensure-txt path))
      (if ocmema:*debug-io* (ocmema:dbg-io (strcat "[SAVE] path_canonical=" path)))
      (ocmema:proj-update-path-state path)
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
      (setq v (ocmema:pio-assoc-get "draw_units" proj))
      (if v (setq lines (append lines (list (strcat "DRAW_UNITS=" v)))))
      (setq v (ocmema:pio-assoc-get "draw_scale_factor" proj))
      (if v (setq lines (append lines (list (strcat "DRAW_SCALE=" (rtos v 2 6))))))
      (setq v (ocmema:pio-assoc-get "fc_kgcm2" proj))
      (if v (setq lines (append lines (list (strcat "FC_KGCM2=" (rtos v 2 4))))))
      (setq v (ocmema:pio-assoc-get "ec_kgcm2" proj))
      (if v (setq lines (append lines (list (strcat "EC_KGCM2=" (rtos v 2 4))))))
      (setq v (ocmema:pio-assoc-get "load_combo_set" proj))
      (if v (setq lines (append lines (list (strcat "LOAD_COMBO_SET=" v)))))
      (setq v (ocmema:pio-assoc-get "dir_beams_std" proj))
      (if v (setq lines (append lines (list (strcat "DIR_BEAMS_STD=" v)))))
      (setq v (ocmema:pio-assoc-get "dir_ribs_std" proj))
      (if v (setq lines (append lines (list (strcat "DIR_RIBS_STD=" v)))))
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
      (setq plants (ocmema:pio-assoc-get 'plants proj))
      (foreach plant plants
        (setq lines (append lines (list (strcat "[PLANT " (itoa (ocmema:pio-assoc-get "idx" plant)) "]"))))
        (setq pname (ocmema:pio-assoc-get "name" plant))
        (setq lines (append lines (list (strcat "PLANT_NAME=" pname))))
        (setq x_axes (ocmema:pio-assoc-get "x_axes" plant))
        (setq y_axes (ocmema:pio-assoc-get "y_axes" plant))
        (setq lines (append lines (list (strcat "X_AXES=" (ocmema:pio-axes-to-string x_axes)))))
        (setq lines (append lines (list (strcat "Y_AXES=" (ocmema:pio-axes-to-string y_axes)))))
        (setq v (ocmema:pio-assoc-get 'slab_h_total_cm plant))
        (if (numberp v) (setq lines (append lines (list (strcat "SLAB_H_TOTAL_CM=" (rtos v 2 6))))))
        (setq v (ocmema:pio-assoc-get 'slab_h_comp_cm plant))
        (if (numberp v) (setq lines (append lines (list (strcat "SLAB_H_COMP_CM=" (rtos v 2 6))))))
        (setq v (ocmema:pio-assoc-get 'rib_spacing_cm plant))
        (if (numberp v) (setq lines (append lines (list (strcat "RIB_SPACING_CM=" (rtos v 2 6))))))
        (setq lines (append lines (list "[/PLANT]")))
      )
      (setq beams (ocmema:pio-assoc-get 'beams proj))
      (setq lines (append lines (list "[BEAMS]")))
      (if beams
        (foreach beam beams
          (setq lines
            (append lines
              (list
                (strcat
                  "B|"
                  (ocmema:pio-assoc-get "name" beam)
                  "|"
                  (itoa (if (ocmema:pio-assoc-get "plant_idx" beam) (ocmema:pio-assoc-get "plant_idx" beam) 0))
                  "|"
                  (if (ocmema:pio-assoc-get "align" beam) (ocmema:pio-assoc-get "align" beam) "")
                  "|"
                  (itoa (if (ocmema:pio-assoc-get "n_points" beam)
                          (ocmema:pio-assoc-get "n_points" beam)
                          (length (ocmema:pio-assoc-get "points_raw" beam))
                        )
                  )
                  "|"
                  (ocmema:pio-points2d-to-string (ocmema:pio-assoc-get "points_raw" beam))
                  "|"
                  (if (ocmema:pio-assoc-get "meta_kv" beam) (ocmema:pio-assoc-get "meta_kv" beam) "")
                )
              )
            )
          )
        )
      )
      (setq lines (append lines (list "[/BEAMS]")))
      (setq ribs (ocmema:pio-assoc-get 'ribs proj))
      (setq lines (append lines (list "[RIBS]")))
      (if ribs
        (foreach rib ribs
          (setq lines
            (append lines
              (list
                (strcat
                  "N|"
                  (ocmema:pio-assoc-get "name" rib)
                  "|"
                  (itoa (if (ocmema:pio-assoc-get "plant_idx" rib) (ocmema:pio-assoc-get "plant_idx" rib) 0))
                  "|"
                  (if (ocmema:pio-assoc-get "dir" rib) (ocmema:pio-assoc-get "dir" rib) "")
                  "|"
                  (rtos (if (ocmema:pio-assoc-get "spacing" rib) (ocmema:pio-assoc-get "spacing" rib) 0.0) 2 6)
                  "|"
                  (itoa (if (ocmema:pio-assoc-get "n_clear" rib) (ocmema:pio-assoc-get "n_clear" rib) 0))
                  "|"
                  (itoa (if (ocmema:pio-assoc-get "n_points" rib)
                          (ocmema:pio-assoc-get "n_points" rib)
                          (length (ocmema:pio-assoc-get "points_raw" rib))
                        )
                  )
                  "|"
                  (ocmema:pio-points2d-to-string (ocmema:pio-assoc-get "points_raw" rib))
                  "|"
                  (if (ocmema:pio-assoc-get "meta_kv" rib) (ocmema:pio-assoc-get "meta_kv" rib) "")
                )
              )
            )
          )
        )
      )
      (setq lines (append lines (list "[/RIBS]")))
      (if ocmema:*debug-io*
        (progn
          (ocmema:dbg-io (strcat "[SAVE] lines_count=" (itoa (length lines))))
          (ocmema:dbg-print-lines-window "lines" lines)
        )
      )
      (setq tmp (ocmema:pio-temp-path path))
      (if (ocmema:pio-write-lines tmp lines)
        (progn
          (setq ok (ocmema:pio-atomic-replace path tmp))
          (if ok (setq ocmema:*project-path* path))
          (if ocmema:*debug-io*
            (progn
              (setq r
                (vl-catch-all-apply
                  (function
                    (lambda ()
                      (setq fileExists (if (findfile path) T nil))
                      (setq fileSize (if fileExists (ocmema:dbg-file-size path) 0))
                      (ocmema:dbg-io (strcat "[SAVE] wrote_ok file_exists=" (ocmema:dbg-bool fileExists) " size=" (itoa fileSize)))
                      (setq linesRead (if fileExists (ocmema:pio-read-lines path) nil))
                      (setq verifyBeams T)
                      (if (> beamCount 0)
                        (setq verifyBeams (or (ocmema:dbg-find-tag linesRead "[BEAMS]") (ocmema:dbg-has-prefix linesRead "B|")))
                      )
                      (setq verifyRibs T)
                      (if (> ribCount 0)
                        (setq verifyRibs (or (ocmema:dbg-find-tag linesRead "[RIBS]") (ocmema:dbg-has-prefix linesRead "N|")))
                      )
                      (setq verifySlabs T)
                      (setq slabPlants '())
                      (foreach slabPlant plants
                        (setq slabTotal (ocmema:pio-assoc-get 'slab_h_total_cm slabPlant))
                        (setq slabComp (ocmema:pio-assoc-get 'slab_h_comp_cm slabPlant))
                        (setq ribSpacing (ocmema:pio-assoc-get 'rib_spacing_cm slabPlant))
                        (setq hasSlab (or (numberp slabTotal) (numberp slabComp) (numberp ribSpacing)))
                        (if hasSlab
                          (setq slabPlants (append slabPlants (list slabPlant)))
                        )
                      )
                      (foreach slabPlant slabPlants
                        (setq slabIdx (if (ocmema:pio-assoc-get "idx" slabPlant) (ocmema:pio-assoc-get "idx" slabPlant) 0))
                        (setq block (ocmema:dbg-plant-block linesRead slabIdx))
                        (setq blockOk (ocmema:dbg-has-prefix block "SLAB_H_TOTAL_CM="))
                        (if (not blockOk) (setq verifySlabs nil))
                      )
                      (ocmema:dbg-io
                        (strcat "[SAVE] verify BEAMS=" (if verifyBeams "PASS" "FAIL")
                                " RIBS=" (if verifyRibs "PASS" "FAIL")
                                " SLABS=" (if verifySlabs "PASS" "FAIL")))
                      (if (not verifyBeams)
                        (progn
                          (setq block (ocmema:dbg-section-lines linesRead "[BEAMS]" "[/BEAMS]"))
                          (if block
                            (progn
                              (ocmema:dbg-io "[SAVE] BEAMS excerpt:")
                              (foreach v block (ocmema:dbg-io (strcat "[SAVE]   " v)))
                            )
                            (ocmema:dbg-io "[SAVE] BEAMS section missing in read-back")
                          )
                        )
                      )
                      (if (not verifyRibs)
                        (progn
                          (setq block (ocmema:dbg-section-lines linesRead "[RIBS]" "[/RIBS]"))
                          (if block
                            (progn
                              (ocmema:dbg-io "[SAVE] RIBS excerpt:")
                              (foreach v block (ocmema:dbg-io (strcat "[SAVE]   " v)))
                            )
                            (ocmema:dbg-io "[SAVE] RIBS section missing in read-back")
                          )
                        )
                      )
                      (if (not verifySlabs)
                        (foreach slabPlant slabPlants
                          (setq slabIdx (if (ocmema:pio-assoc-get "idx" slabPlant) (ocmema:pio-assoc-get "idx" slabPlant) 0))
                          (setq block (ocmema:dbg-plant-block linesRead slabIdx))
                          (if (not (ocmema:dbg-has-prefix block "SLAB_H_TOTAL_CM="))
                            (progn
                              (ocmema:dbg-io (strcat "[SAVE] SLAB FAIL plant " (itoa slabIdx) " block excerpt:"))
                              (if block
                                (foreach v block (ocmema:dbg-io (strcat "[SAVE]   " v)))
                                (ocmema:dbg-io (strcat "[SAVE]   [PLANT " (itoa slabIdx) "] block missing"))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                  '()
                )
              )
              (if (vl-catch-all-error-p r)
                (ocmema:dbg-io (strcat "[SAVE] verify error: " (vl-catch-all-error-message r)))
              )
            )
          )
          ok
        )
        (progn
          (if ocmema:*debug-io* (ocmema:dbg-io "[SAVE] wrote_ok file_exists=NIL size=0"))
          nil
        )
      )
    )
  )
)

;; Load project
(defun ocmema:pio-load-project-lines (lines / len i line vline key val kv
                                            in-plant plant-index plant-name x-axes y-axes plant-slab-h plant-slab-comp plant-rib-spacing
                                            in-beam beam-index beam-name beam-plant-idx beam-plant-name
                                            beam-npoints beam-align beam-unit beam-std-path beam-points beam-meta
                                            in-units units scale
                                            in-beams in-ribs
                                            ribs rib-name rib-plant-idx rib-dir rib-spacing rib-nclear rib-npoints rib-points rib-meta
                                            plants beams nplants nx ny wall xnames ynames projname proj-beam-unit
                                            file-version parse-start draw_units draw_scale fc ec dir_beams dir_ribs load_combo_set)
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
    (progn (ocmema:proj-log "TXT invalido: sin contenido.") nil)
    (progn
      (setq file-version nil)
      (setq parse-start i)
      (if (or (= vline "OCMEMA_PROJECT_V1") (= vline "OCMEMA_PROJECT_V2"))
        (setq file-version vline)
        (progn
          (if (wcmatch (strcase vline) "OCMEMA_PROJECT_V*")
            (progn
              (ocmema:proj-log (strcat "TXT invalido: version no soportada (" vline ")"))
              (setq file-version nil)
            )
            (progn
              ;; no header => asumir V1 y parsear desde el inicio
              (setq file-version "OCMEMA_PROJECT_V1")
              (setq parse-start 0)
            )
          )
        )
      )
      (if (not file-version)
        nil
        (progn
        (setq in-plant nil)
        (setq plant-index 0)
        (setq plant-name "")
        (setq x-axes '())
        (setq y-axes '())
        (setq plant-slab-h nil)
        (setq plant-slab-comp nil)
        (setq plant-rib-spacing nil)
        (setq in-beam nil)
        (setq beam-index 0)
        (setq beam-name "")
        (setq beam-plant-idx 0)
        (setq beam-plant-name "")
        (setq beam-npoints 0)
        (setq beam-align "")
        (setq beam-unit "")
        (setq beam-std-path "")
        (setq beam-points '())
        (setq beam-meta "")
        (setq in-units nil)
        (setq units nil)
        (setq scale nil)
        (setq in-beams nil)
        (setq in-ribs nil)
        (setq rib-name "")
        (setq rib-plant-idx 0)
        (setq rib-dir "")
        (setq rib-spacing 0.0)
        (setq rib-nclear 0)
        (setq rib-npoints 0)
        (setq rib-points '())
        (setq rib-meta "")
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
        (setq draw_units nil)
        (setq draw_scale nil)
        (setq fc nil)
        (setq ec nil)
        (setq dir_beams nil)
        (setq dir_ribs nil)
        (setq load_combo_set nil)

        (setq i parse-start)
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
                            (cons "slab_h_total_cm" plant-slab-h)
                            (cons "slab_h_comp_cm" plant-slab-comp)
                            (cons "rib_spacing_cm" plant-rib-spacing)
                          )
                        )
                      )
                    )
                    (setq in-plant nil)
                    (setq plant-index 0)
                    (setq plant-name "")
                    (setq x-axes '())
                    (setq y-axes '())
                    (setq plant-slab-h nil)
                    (setq plant-slab-comp nil)
                    (setq plant-rib-spacing nil)
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
                            (cons "align" beam-align)
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
                    (setq beam-align "")
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
                (setq plant-slab-h nil)
                (setq plant-slab-comp nil)
                (setq plant-rib-spacing nil)
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
                (setq beam-align "")
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
                 (cond
                   ;; V2: B|name|plant_idx|align|n_points|points2d|meta_kv
                   ((>= (length kv) 6)
                    (setq beam-name (ocmema:pio-nth-safe kv 1 ""))
                    (setq beam-plant-idx (atoi (ocmema:pio-nth-safe kv 2 "0")))
                    (setq beam-align (ocmema:pio-nth-safe kv 3 ""))
                    (setq beam-npoints (atoi (ocmema:pio-nth-safe kv 4 "0")))
                    (setq beam-points (ocmema:pio-parse-points2d (ocmema:pio-nth-safe kv 5 "")))
                    (setq beam-meta (ocmema:pio-nth-safe kv 6 ""))
                    (if (or (not beam-npoints) (<= beam-npoints 0))
                      (setq beam-npoints (length beam-points))
                    )
                    (setq beams
                      (append beams
                        (list
                          (list
                            (cons "name" beam-name)
                            (cons "plant_idx" beam-plant-idx)
                            (cons "align" beam-align)
                            (cons "n_points" beam-npoints)
                            (cons "points_raw" beam-points)
                            (cons "meta_kv" beam-meta)
                          )
                        )
                      )
                    )
                   )
                   ;; V1: B|name|n_points|points2d OR B|name|n_points|align|points2d
                   ((>= (length kv) 4)
                    (setq beam-name (ocmema:pio-nth-safe kv 1 ""))
                    (setq beam-npoints (atoi (ocmema:pio-nth-safe kv 2 "0")))
                    (if (>= (length kv) 5)
                      (progn
                        (setq beam-align (ocmema:pio-nth-safe kv 3 ""))
                        (setq beam-points (ocmema:pio-parse-points2d (ocmema:pio-nth-safe kv 4 "")))
                      )
                      (progn
                        (setq beam-align "")
                        (setq beam-points (ocmema:pio-parse-points2d (ocmema:pio-nth-safe kv 3 "")))
                      )
                    )
                    (if (or (not beam-npoints) (<= beam-npoints 0))
                      (setq beam-npoints (length beam-points))
                    )
                    (setq beams
                      (append beams
                        (list
                          (list
                            (cons "name" beam-name)
                            (cons "plant_idx" 0)
                            (cons "align" beam-align)
                            (cons "n_points" beam-npoints)
                            (cons "points_raw" beam-points)
                            (cons "meta_kv" "")
                          )
                        )
                      )
                    )
                   )
                   (T
                    (ocmema:proj-warn (strcat "Linea BEAMS invalida: " line))
                   )
                 )
               )
               (if (and in-ribs (wcmatch line "N|*"))
                 (progn
                   (setq kv (ocmema:pio-split line "|"))
                   (cond
                     ;; V2: N|name|plant_idx|dir|spacing|n_clear|n_points|points2d|meta_kv
                   ((>= (length kv) 8)
                      (setq rib-name (ocmema:pio-nth-safe kv 1 ""))
                      (setq rib-plant-idx (atoi (ocmema:pio-nth-safe kv 2 "0")))
                      (setq rib-dir (ocmema:pio-nth-safe kv 3 ""))
                      (setq rib-spacing (ocmema:pio-to-number (ocmema:pio-nth-safe kv 4 "0")))
                      (setq rib-nclear (atoi (ocmema:pio-nth-safe kv 5 "0")))
                      (setq rib-npoints (atoi (ocmema:pio-nth-safe kv 6 "0")))
                      (setq rib-points (ocmema:pio-parse-points2d (ocmema:pio-nth-safe kv 7 "")))
                      (setq rib-meta (ocmema:pio-nth-safe kv 8 ""))
                      (if (or (not rib-npoints) (<= rib-npoints 0))
                        (setq rib-npoints (length rib-points))
                      )
                      (setq ribs
                        (append ribs
                          (list
                            (list
                              (cons "name" rib-name)
                              (cons "plant_idx" rib-plant-idx)
                              (cons "dir" rib-dir)
                              (cons "spacing" rib-spacing)
                              (cons "n_clear" rib-nclear)
                              (cons "n_points" rib-npoints)
                              (cons "points_raw" rib-points)
                              (cons "meta_kv" rib-meta)
                            )
                          )
                        )
                      )
                     )
                     ;; V1: N|name|dir|spacing|n_clear
                     ((>= (length kv) 5)
                      (setq rib-name (ocmema:pio-nth-safe kv 1 ""))
                      (setq rib-dir (ocmema:pio-nth-safe kv 2 ""))
                      (setq rib-spacing (ocmema:pio-to-number (ocmema:pio-nth-safe kv 3 "0")))
                      (setq rib-nclear (atoi (ocmema:pio-nth-safe kv 4 "0")))
                      (setq ribs
                        (append ribs
                          (list
                            (list
                              (cons "name" rib-name)
                              (cons "plant_idx" 0)
                              (cons "dir" rib-dir)
                              (cons "spacing" rib-spacing)
                              (cons "n_clear" rib-nclear)
                              (cons "n_points" 0)
                              (cons "points_raw" '())
                              (cons "meta_kv" "")
                            )
                          )
                        )
                      )
                     )
                     (T
                      (ocmema:proj-warn (strcat "Linea RIBS invalida: " line))
                     )
                   )
                 )
                 (progn
                   (if (and in-plant (wcmatch line "SLAB_H_TOTAL_CM|*"))
                     (setq plant-slab-h (ocmema:pio-to-number (nth 1 (ocmema:pio-split line "|"))))
                     (if (and in-plant (wcmatch line "SLAB_H_COMP_CM|*"))
                       (setq plant-slab-comp (ocmema:pio-to-number (nth 1 (ocmema:pio-split line "|"))))
                       (if (and in-plant (wcmatch line "RIB_SPACING_CM|*"))
                         (setq plant-rib-spacing (ocmema:pio-to-number (nth 1 (ocmema:pio-split line "|"))))
                         (if (and in-plant (wcmatch line "SLAB_H_TOTAL_CM=*"))
                           (setq plant-slab-h (ocmema:pio-to-number (cadr (ocmema:pio-split-kv line))))
                           (if (and in-plant (wcmatch line "SLAB_H_COMP_CM=*"))
                             (setq plant-slab-comp (ocmema:pio-to-number (cadr (ocmema:pio-split-kv line))))
                             (if (and in-plant (wcmatch line "RIB_SPACING_CM=*"))
                               (setq plant-rib-spacing (ocmema:pio-to-number (cadr (ocmema:pio-split-kv line))))
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
                                         ((= key "SLAB_H_TOTAL_CM") (setq plant-slab-h (ocmema:pio-to-number val)))
                                         ((= key "SLAB_H_COMP_CM") (setq plant-slab-comp (ocmema:pio-to-number val)))
                                         ((= key "RIB_SPACING_CM") (setq plant-rib-spacing (ocmema:pio-to-number val)))
                                         (T (ocmema:proj-warn (strcat "Clave desconocida en PLANT: " key)))
                                       )
                                       (if in-beam
                                         (cond
                                           ((= key "BEAM_NAME") (setq beam-name val))
                                           ((= key "PLANT_IDX") (setq beam-plant-idx (atoi val)))
                                           ((= key "PLANT_NAME") (setq beam-plant-name val))
                                           ((= key "N_POINTS") (setq beam-npoints (atoi val)))
                                           ((= key "ALIGN") (setq beam-align val))
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
                                             ((= key "DRAW_UNITS") (setq draw_units val))
                                             ((= key "DRAW_SCALE") (setq draw_scale (ocmema:pio-to-number val)))
                                             ((= key "FC_KGCM2") (setq fc (ocmema:pio-to-number val)))
                                             ((= key "EC_KGCM2") (setq ec (ocmema:pio-to-number val)))
                                             ((= key "LOAD_COMBO_SET") (setq load_combo_set (ocmema:proj-normalize-load-combo-set val)))
                                             ((= key "DIR_BEAMS_STD") (setq dir_beams val))
                                             ((= key "DIR_RIBS_STD") (setq dir_ribs val))
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
                     )
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
                    (cons "slab_h_total_cm" plant-slab-h)
                    (cons "slab_h_comp_cm" plant-slab-comp)
                    (cons "rib_spacing_cm" plant-rib-spacing)
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
                    (cons "align" beam-align)
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
              (cons "draw_units" draw_units)
              (cons "draw_scale_factor" draw_scale)
              (cons "fc_kgcm2" fc)
              (cons "ec_kgcm2" ec)
              (cons "load_combo_set" load_combo_set)
              (cons "dir_beams_std" dir_beams)
              (cons "dir_ribs_std" dir_ribs)
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
          (ocmema:proj-update-path-state path)
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
    (initget "U T E R")
    (setq opt (getkword "\nModificar ejes [U Uno/T Todos/E EscalaDibujo/R Regresar] <R>: "))
    (cond
      ((or (not opt) (= opt "R")) (setq done T))
      ((= opt "U") (ocmema:menu-modificar-ejes-uno))
      ((= opt "T") (ocmema:menu-modificar-ejes-todos))
      ((= opt "E") (ocmema:menu-config-draw-scale))
    )
  )
  (princ)
)

(defun ocmema:menu-config-draw-scale (/ proj units scale)
  (setq proj ocmema:*project*)
  (if (not proj)
    (ocmema:proj-log "No hay proyecto cargado.")
    (progn
      (initget "CM M MM")
      (setq units (getkword
                    (strcat
                      "\nUnidades de captura [CM/M/MM] <"
                      (if (ocmema:pio-assoc-get "draw_units" proj)
                        (ocmema:pio-assoc-get "draw_units" proj)
                        "M")
                      ">: ")))
      (if (not units)
        (setq units (if (ocmema:pio-assoc-get "draw_units" proj)
                      (ocmema:pio-assoc-get "draw_units" proj)
                      "M"))
      )
      (setq scale (getreal
                    (strcat
                      "\nFactor de escala de dibujo <"
                      (rtos (if (ocmema:pio-assoc-get "draw_scale_factor" proj)
                              (ocmema:pio-assoc-get "draw_scale_factor" proj)
                              1.0) 2 3)
                      ">: ")))
      (if (not scale)
        (setq scale (if (ocmema:pio-assoc-get "draw_scale_factor" proj)
                      (ocmema:pio-assoc-get "draw_scale_factor" proj)
                      1.0))
      )
      (if (<= scale 0.0)
        (ocmema:proj-log "Factor invalido.")
        (progn
          (ocmema:proj-set-units-scale units scale)
          (ocmema:proj-autosave-from "project menu: set draw scale")
          (ocmema:proj-log (strcat "Escala de dibujo guardada. Unidades=" units " factor=" (rtos scale 2 3)))
        )
      )
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
              (if (not names) (setq names '()))
              (setq actualName (ocmema:pio-find-name-ci names axisName))
              (if (not actualName)
                (ocmema:proj-log "Eje no existe. Si el proyecto no tiene ejes, usa ModificarEjes > Todos > NuevosEjes.")
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
                             (ocmema:proj-autosave-from "project menu: modify axis name")
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
                             (ocmema:proj-autosave-from "project menu: modify axis coordinate")
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
         (ocmema:proj-autosave-from "project menu: capture all axes")
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
                         (ocmema:proj-autosave-from "project menu: new axes capture")
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

(defun ocmema:menu-modificar-trabe (/ name beam opt points ok)
  (setq name (getstring T "\nNombre exacto de la trabe: "))
  (if (not name)
    (ocmema:proj-cancelled)
    (progn
      (setq beam (ocmema:proj-find-beam name))
      (if (not beam)
        (ocmema:proj-log "Trabe no existe.")
        (progn
          (initget "P R X")
          (setq opt (getkword "\nModificar [P Puntos(solo TXT)/R Rehacer trabe(.STD)/X Regresar] <X>: "))
          (cond
            ((or (not opt) (= opt "X")) nil)
            ((= opt "P")
             (setq points (ocmema:proj-capture-points))
             (if (not points)
               (princ "\nOCMEMA: Cancelado. No se hicieron cambios.")
               (progn
                 (setq ok (ocmema:proj-update-beam-points (ocmema:pio-assoc-get "name" beam) points))
                 (if ok
                   (progn
                     (ocmema:proj-autosave-from "project menu: modify beam points")
                     (princ (strcat "\nOCMEMA: Puntos actualizados para trabe " (ocmema:pio-assoc-get "name" beam) ". No se rehizo el .STD."))
                   )
                   (princ "\nOCMEMA: Cancelado. No se hicieron cambios.")
                 )
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

(defun ocmema:menu-modificar-nervadura (/ name rib opt dir points ok)
  (setq name (getstring T "\nNombre exacto de la nervadura: "))
  (if (not name)
    (ocmema:proj-cancelled)
    (progn
      (setq rib (ocmema:proj-find-rib name))
      (if (not rib)
        (ocmema:proj-log "Nervadura no existe.")
        (progn
          (initget "P D N R")
          (setq opt (getkword "\nModificar [P Puntos(solo TXT)/D DireccionSoloTXT/N NuevoSTD/R Regresar] <R>: "))
          (cond
            ((or (not opt) (= opt "R")) nil)
            ((= opt "P")
             (setq points (ocmema:proj-capture-points))
             (if (not points)
               (princ "\nOCMEMA: Cancelado. No se hicieron cambios.")
               (progn
                 (setq ok (ocmema:proj-update-rib-points (ocmema:pio-assoc-get "name" rib) points))
                 (if ok
                   (progn
                     (ocmema:proj-autosave-from "project menu: modify rib points")
                     (princ (strcat "\nOCMEMA: Puntos actualizados para nervadura " (ocmema:pio-assoc-get "name" rib) "."))
                   )
                   (princ "\nOCMEMA: Cancelado. No se hicieron cambios.")
                 )
               )
             )
            )
            ((= opt "D")
             (initget "H V")
             (setq dir (getkword "\nDireccion [H Horizontal/V Vertical] <H>: "))
             (if (not dir) (setq dir "H"))
             (setq ok
               (ocmema:proj-update-rib
                 (ocmema:pio-assoc-get "name" rib)
                 (list (cons "dir" dir))
               )
             )
             (if (not ok)
               (ocmema:proj-warn "No se pudo actualizar la direccion de la nervadura.")
             )
             (ocmema:proj-autosave-from "project menu: modify rib direction")
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
      ((= opt "A") (ocmema:menu-dibujar-armados))
      ((= opt "P") (ocmema:menu-dibujar-planta))
    )
  )
  (princ)
)

(defun ocmema:menu-dibujar-armados (/ opt done)
  (setq done nil)
  (while (not done)
    (initget "N T R")
    (setq opt (getkword "\nArmados [N Nervaduras/T Trabes/R Regresar] <R>: "))
    (cond
      ((or (not opt) (= opt "R")) (setq done T))
      ((= opt "N") (ocmema:menu-armados-nervaduras))
      ((= opt "T") (ocmema:menu-armados-trabes))
    )
  )
  (princ)
)

(defun ocmema:menu-armados-nervaduras (/ opt done)
  (setq done nil)
  (while (not done)
    (initget "U T R")
    (setq opt (getkword "\nArmados Nervaduras [U Una/T Todas/R Regresar] <U>: "))
    (cond
      ((or (not opt) (= opt "R")) (setq done T))
      ((or (not opt) (= opt "U"))
       (ocmema:armados-nervaduras-una)
      )
      ((= opt "T")
       (ocmema:armados-nervaduras-todas)
      )
    )
  )
  (princ)
)

(defun ocmema:menu-dibujar-planta (/ opt done)
  (setq done nil)
  (while (not done)
    (initget "T R")
    (setq opt (getkword "\nPlanta [T Trabes/R Regresar] <R>: "))
    (cond
      ((or (not opt) (= opt "R")) (setq done T))
      ((= opt "T") (ocmema:menu-dibujar-planta-trabes))
    )
  )
  (princ)
)

(defun ocmema:pl-project-beams-valid-p (/ proj beams)
  (setq proj ocmema:*project*)
  (if (and proj (listp proj))
    (progn
      (setq beams (ocmema:proj-get-beams))
      (if (and beams (listp beams)) T nil)
    )
    nil
  )
)

(defun ocmema:pl-default-beam-dir (/ dir path base)
  (setq dir (if ocmema:*project* (ocmema:pio-assoc-get "dir_beams_std" ocmema:*project*) nil))
  (if (and dir (/= dir ""))
    (ocmema:proj-ensure-dir-sep dir)
    (progn
      (setq path ocmema:*project-path*)
      (setq base (if (and path (/= path "")) (vl-filename-directory path) nil))
      (if (and base (/= base ""))
        (ocmema:proj-ensure-dir-sep base)
        (progn
          (setq base (getvar "DWGPREFIX"))
          (if (and base (/= base ""))
            (ocmema:proj-ensure-dir-sep base)
            ocmema:*proj-default-dir*
          )
        )
      )
    )
  )
)

(defun ocmema:proj-join-path (dir leaf / base)
  (setq base (ocmema:proj-ensure-dir-sep dir))
  (if (= base "")
    leaf
    (strcat base leaf)
  )
)

(defun ocmema:pl-default-rib-dir (/ dir path base)
  (setq dir (if ocmema:*project* (ocmema:pio-assoc-get "dir_ribs_std" ocmema:*project*) nil))
  (if (and dir (/= dir ""))
    (ocmema:proj-ensure-dir-sep dir)
    (progn
      (setq path ocmema:*project-path*)
      (setq base (if (and path (/= path "")) (vl-filename-directory path) nil))
      (if (and base (/= base ""))
        (ocmema:proj-ensure-dir-sep base)
        (progn
          (setq base (getvar "DWGPREFIX"))
          (if (and base (/= base ""))
            (ocmema:proj-ensure-dir-sep base)
            ocmema:*proj-default-dir*
          )
        )
      )
    )
  )
)

(defun ocmema:pl-beam-has-points-p (beam / kv points n)
  (setq kv (ocm-assoc-get "points_raw" beam))
  (setq points (if kv (cdr kv) nil))
  (setq n (ocm-get beam "n_points"))
  (if (and n (numberp n) (>= n 2))
    T
    (if (and points (listp points) (>= (length points) 2)) T nil)
  )
)

(defun ocmema:pl-anl-exists-p (name files / target f found)
  (setq found nil)
  (if (and name files (listp files))
    (progn
      (setq target (strcase (strcat (ocmema:pl-name->str name) ".ANL")))
      (foreach f files
        (if (= (strcase f) target)
          (setq found T)
        )
      )
    )
  )
  found
)

(defun ocmema:pl-draw-beam (beam widths_cm anlPath ext_mode ext_ini ext_fin / name align kv points pnorm keys units scale wall_cm widths_draw w_used base center edge1 edge2 rect dir normal_left normal_right ext_cm ext_draw offset_draw p0 pN nPts mode align_note w_list
                              rectEnt centerEnt fmt labelOk hatchOk r
                              i n wi pA0 pA1 pB0 pB1 prevA0 prevA1 prevB0 prevB1 ipA ipB side_plus side_minus)
  (setq name (ocm-get beam "name"))
  (setq align (ocm-get beam "align"))
  (setq kv (ocm-assoc-get "points_raw" beam))
  (setq points (if kv (cdr kv) nil))
  (setq pnorm (ocmema:pl-normalize-points points))
  (setq units (ocmema:pio-assoc-get "units" ocmema:*project*))
  (setq scale (ocmema:get-scale ocmema:*project*))
  (setq wall_cm (ocmema:pio-assoc-get "wall_cm" ocmema:*project*))
  (if (and (not kv) (not points))
    (setq *ocm_pts_reason* "points_raw no encontrado")
  )
  (if (not (ocm-pts-valid-p pnorm))
    (progn
      (setq keys (ocmema:alist-keys beam))
      (ocmema:proj-warn
        (strcat
          "Trabe " (if name (vl-princ-to-string name) "<sin nombre>")
          " sin puntos validos (razon: " (if *ocm_pts_reason* *ocm_pts_reason* "desconocida") ")."
        )
      )
      (princ "\nOCMEMA DBG: keys=")
      (princ keys)
      (princ " points_raw_type=")
      (princ (type points))
      nil
    )
    (progn
      (setq widths_draw (ocmema:pl-widths-cm->draw widths_cm units scale))
      (if (or (not widths_draw) (= (length widths_draw) 0))
        (progn
          (ocmema:proj-warn
            (strcat "ANL sin Width valido. " (if *ocm_anl_reason* *ocm_anl_reason* "") " "
                    (if *ocm_anl_sample* *ocm_anl_sample* ""))
          )
          (setq w_used nil)
        )
        (progn
          (if (and (> (length widths_draw) 1) (/= (length widths_draw) (1- (length pnorm))))
            (progn
              (ocmema:proj-warn "Widths ANL no coinciden con numero de tramos; usando primer width.")
              (setq widths_draw (list (car widths_draw)))
            )
          )
          (setq w_used (car widths_draw))
        )
      )
      (setq ext_cm (if (numberp wall_cm) (/ wall_cm 2.0) 0.0))
      (setq ext_draw (if (numberp wall_cm) (ocmema:cm->draw ext_cm scale) 0.0))
      (setq offset_draw (if (numberp w_used) (/ w_used 2.0) 0.0))
      (setq nPts (length pnorm))
      (setq mode (if (and widths_draw (> (length widths_draw) 1)) "pline" "rect"))
      (setq align_note "")
      (if (or (not w_used) (< (length pnorm) 2))
        (progn
          (ocmema:proj-warn "Width invalido o puntos insuficientes; no se dibuja.")
          nil
        )
        (progn
          (setq base (mapcar 'ocmema:pt2d pnorm))
          (setq p0 (car base))
          (setq pN (nth (1- (length base)) base))
          (setq dir (ocm-vec-unit (ocm-vec-sub pN p0)))
          (setq normal_left (list (- (cadr dir)) (car dir)))
          (setq normal_right (list (cadr dir) (- (car dir))))

          (cond
            ((or (= (strcase (if align align "")) "") (= (strcase (if align align "")) "C"))
             (setq center base)
             (setq align_note "centerline=border_ref (C)")
            )
            ((= (strcase (if align align "")) "I")
             (setq center (mapcar '(lambda (pt) (ocm-vec-add pt (ocm-vec-scale normal_right (/ w_used 2.0)))) base))
             (setq align_note "reconstructed centerline from border using normal_right")
            )
            ((= (strcase (if align align "")) "D")
             (setq center (mapcar '(lambda (pt) (ocm-vec-add pt (ocm-vec-scale normal_left (/ w_used 2.0)))) base))
             (setq align_note "reconstructed centerline from border using normal_left")
            )
            (T
             (setq center base)
             (setq align_note "centerline=border_ref (C)")
            )
          )

          (if (= mode "rect")
            (progn
              (setq edge1 (mapcar '(lambda (pt) (ocm-vec-sub pt (ocm-vec-scale normal_right (/ w_used 2.0)))) center))
              (setq edge2 (mapcar '(lambda (pt) (ocm-vec-add pt (ocm-vec-scale normal_right (/ w_used 2.0)))) center))
              (setq edge1 (ocmema:pl-extend-ends-dir edge1 dir ext_ini ext_fin))
              (setq edge2 (ocmema:pl-extend-ends-dir edge2 dir ext_ini ext_fin))
              (setq rect (append edge1 (reverse edge2)))
            )
            (progn
              (setq n (1- (length center)))
              (if (and widths_draw (= (length widths_draw) n))
                (setq w_list widths_draw)
                (progn
                  (setq w_list '())
                  (repeat n (setq w_list (append w_list (list w_used))))
                )
              )
              (setq side_plus '())
              (setq side_minus '())
              (setq i 0)
              (while (< i n)
                (setq wi (nth i w_list))
                (setq pA0 (nth i center))
                (setq pA1 (nth (1+ i) center))
                (setq dir (ocm-vec-unit (ocm-vec-sub pA1 pA0)))
                (setq normal_right (list (cadr dir) (- (car dir))))
                (setq pB0 (ocm-vec-add pA0 (ocm-vec-scale normal_right (/ wi 2.0))))
                (setq pB1 (ocm-vec-add pA1 (ocm-vec-scale normal_right (/ wi 2.0))))
                (setq pA0 (ocm-vec-sub pA0 (ocm-vec-scale normal_right (/ wi 2.0))))
                (setq pA1 (ocm-vec-sub pA1 (ocm-vec-scale normal_right (/ wi 2.0))))
                (if (= i 0)
                  (progn
                    (setq side_plus (append side_plus (list pB0)))
                    (setq side_minus (append side_minus (list pA0)))
                  )
                  (progn
                    (setq ipA (inters prevB0 prevB1 pB0 pB1 nil))
                    (setq ipB (inters prevA0 prevA1 pA0 pA1 nil))
                    (setq side_plus (append side_plus (list (if ipA ipA prevB1))))
                    (setq side_minus (append side_minus (list (if ipB ipB prevA1))))
                  )
                )
                (setq prevB0 pB0)
                (setq prevB1 pB1)
                (setq prevA0 pA0)
                (setq prevA1 pA1)
                (setq i (1+ i))
              )
              (setq side_plus (append side_plus (list prevB1)))
              (setq side_minus (append side_minus (list prevA1)))
              (setq side_plus (ocmema:pl-extend-ends-dir side_plus (ocm-vec-unit (ocm-vec-sub pN p0)) ext_ini ext_fin))
              (setq side_minus (ocmema:pl-extend-ends-dir side_minus (ocm-vec-unit (ocm-vec-sub pN p0)) ext_ini ext_fin))
              (setq rect (append side_plus (reverse side_minus)))
            )
          )

          (princ
            (strcat
              "\nOCMEMA DBG: beam=" (if name (vl-princ-to-string name) "<sin nombre>")
              " align=" (if align align "")
              " wall_cm=" (vl-princ-to-string (if wall_cm wall_cm 0.0))
              " ext_cm=" (vl-princ-to-string ext_cm)
              " ext_draw=" (vl-princ-to-string ext_draw)
              " scale=" (vl-princ-to-string scale)
              " widths_draw=" (vl-princ-to-string widths_draw)
              " nPts=" (vl-princ-to-string nPts)
              " mode=" mode
              " offset_sign_info=" align_note
            )
          )

          (setq centerEnt (ocmema:draw-lwpoly center nil ocmema:*pl-centerline-layer* ocmema:*pl-beam-color*))
          (setq rectEnt (ocmema:draw-lwpoly rect T ocmema:*pl-outline-layer* ocmema:*pl-beam-color*))

          (setq labelOk "FAIL")
          (setq hatchOk "FAIL")
          (setq fmt (ocmema:beam-post-format name rectEnt centerEnt))
          (if (and fmt (listp fmt))
            (progn
              (setq labelOk (car fmt))
              (setq hatchOk (cadr fmt))
            )
          )

          (princ
            (strcat
              "\nOCMEMA DBG: beam=" (if name (vl-princ-to-string name) "<sin nombre>")
              " contour=" (if rectEnt "T" "NIL")
              " center=" (if centerEnt "T" "NIL")
              " label=" labelOk
              " hatch=" (if hatchOk hatchOk "FAIL")
            )
          )
        )
      )
      T
    )
  )
)

(defun ocmema:menu-dibujar-planta-trabes-una (/ path name beams beam widths mode ext ext_ini ext_fin wall_cm scale)
  (setq path (getfiled "Selecciona ANL de trabe" (ocmema:pl-default-beam-dir) "ANL;STD;TXT" 0))
  (if (not path)
    (ocmema:proj-cancelled)
    (progn
      (setq name (ocmema:pl-infer-beam-name path))
      (setq beams (ocmema:proj-get-beams))
      (if (not name)
        (ocmema:proj-log "No se pudo inferir nombre de trabe.")
        (progn
          (setq beam (ocmema:pl-find-beam-by-name-pure name beams))
          (if (not beam)
            (ocmema:proj-log (strcat "Trabe " name " no encontrada en proyecto."))
            (progn
              (if (not (ocmema:pl-beam-has-points-p beam))
                (ocmema:proj-log
                  (strcat
                    "OCMEMA: La trabe '" (ocmema:pl-name->str (ocm-get beam "name"))
                    "' no tiene puntos guardados en TXT (fue generada solo para análisis). Usa Modificar Puntos solo TXT para agregar puntos."
                  )
                )
                (progn
                  (initget "A M")
                  (setq mode (getkword "\nExtension extremos [A Auto/M Manual] <A>: "))
                  (if (not mode) (setq mode "A"))
                  (setq wall_cm (ocmema:pio-assoc-get "wall_cm" ocmema:*project*))
                  (setq scale (ocmema:get-scale ocmema:*project*))
                  (setq ext (ocmema:pl-get-extents mode wall_cm (ocmema:pio-assoc-get "units" ocmema:*project*) scale))
                  (setq ext_ini (car ext))
                  (setq ext_fin (cadr ext))
                  (setq widths (ocmema:anl-extract-widths-cm path))
                  (if (ocmema:pl-draw-beam beam widths path mode ext_ini ext_fin)
                    (ocmema:proj-log (strcat "Trabe " (ocmema:pl-name->str (ocm-get beam "name")) " dibujada."))
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

(defun ocmema:safe-pick-folder (prompt defaultDir / path r sh folder file)
  (setq path nil)
  (setq r
    (vl-catch-all-apply
      '(lambda ()
         (vl-load-com)
         (setq sh (vlax-create-object "Shell.Application"))
         (setq folder (vlax-invoke-method sh 'BrowseForFolder 0 prompt 0 defaultDir))
         (if folder (setq path (vlax-get-property (vlax-get-property folder 'Self) 'Path)))
         (if sh (vlax-release-object sh))
       )
    )
  )
  (if (vl-catch-all-error-p r)
    (ocmema:proj-warn (strcat "OCMEMA WARN: folder picker COM falló (" (vl-catch-all-error-message r) ")"))
  )
  (if (or (not path) (= path ""))
    (progn
      (setq file (getfiled "Selecciona ANL para ubicar carpeta" defaultDir "ANL" 0))
      (if file (setq path (vl-filename-directory file)))
    )
  )
  path
)

(defun ocmema:menu-dibujar-planta-trabes-todas (/ beams total drawn folder files filemap beam name key path widths mode ext ext_ini ext_fin wall_cm scale units
                                                 missing matched extra drawn_names skipped_no_points skipped_not_found batch_auto_ext resp)
  (setq beams (ocmema:proj-get-beams))
  (if (not (and beams (listp beams)))
    (ocmema:proj-log "No hay trabes en el proyecto.")
    (progn
      (setq total (length beams))
      (setq drawn 0)
      (setq missing 0)
      (setq matched 0)
      (setq drawn_names '())
      (setq skipped_no_points '())
      (setq skipped_not_found '())
      (setq folder (ocmema:safe-pick-folder "Selecciona carpeta con archivos .ANL" (ocmema:pl-default-beam-dir)))
      (if (not folder)
        (ocmema:proj-warn "OCMEMA WARN: Todas cancelado (sin carpeta).")
        (progn
          (setq files (vl-directory-files folder "*.anl" 1))
          (setq filemap '())
          (foreach f files
            (setq key (strcase (vl-filename-base f)))
            (setq filemap (cons (cons key (strcat folder "\\" f)) filemap))
          )
          (setq wall_cm (ocmema:pio-assoc-get "wall_cm" ocmema:*project*))
          (setq scale (ocmema:get-scale ocmema:*project*))
          (setq units (ocmema:pio-assoc-get "units" ocmema:*project*))
          (initget "S N")
          (setq resp (getkword "\n¿Usar extensión automática para TODAS las trabes? [S/N] <S>: "))
          (if (or (not resp) (= resp "S"))
            (setq batch_auto_ext T)
            (setq batch_auto_ext nil)
          )
          (foreach beam beams
            (setq name (ocm-get beam "name"))
            (setq key (strcase (ocmema:pl-name->str name)))
            (setq path (cdr (assoc key filemap)))
            (if path
              (progn
                (setq matched (1+ matched))
                (if (not (ocmema:pl-beam-has-points-p beam))
                  (setq skipped_no_points (append skipped_no_points (list (ocmema:pl-name->str name))))
                  (progn
                    (if batch_auto_ext
                      (progn
                        (setq mode "A")
                        (setq ext (ocmema:pl-get-extents mode wall_cm units scale))
                        (setq ext_ini (car ext))
                        (setq ext_fin (cadr ext))
                      )
                      (progn
                        (initget "A M")
                        (setq mode (getkword (strcat "\nExtension extremos para <" (vl-princ-to-string name) "> [A Auto/M Manual] <A>: ")))
                        (if (not mode) (setq mode "A"))
                        (setq ext (ocmema:pl-get-extents mode wall_cm units scale))
                        (setq ext_ini (car ext))
                        (setq ext_fin (cadr ext))
                      )
                    )
                    (setq widths (ocmema:anl-extract-widths-cm path))
                    (if (ocmema:pl-draw-beam beam widths path mode ext_ini ext_fin)
                      (progn
                        (setq drawn (1+ drawn))
                        (setq drawn_names (append drawn_names (list (ocmema:pl-name->str name))))
                      )
                    )
                  )
                )
              )
              (setq missing (1+ missing))
            )
          )
          (setq extra (if files (- (length files) matched) 0))
          (foreach f files
            (setq key (strcase (vl-filename-base f)))
            (if (not (ocmema:pl-find-beam-by-name-pure key beams))
              (setq skipped_not_found (append skipped_not_found (list key)))
            )
          )
          (ocmema:proj-log (strcat "OCMEMA: Dibujadas " (itoa drawn) " trabes."))
          (if skipped_no_points
            (ocmema:proj-log
              (strcat "OCMEMA: Omitidas " (itoa (length skipped_no_points)) " (sin puntos en TXT): "
                      (ocmema:pio-join skipped_no_points ", "))
            )
          )
          (if skipped_not_found
            (ocmema:proj-log
              (strcat "OCMEMA: No existe en TXT / proyecto: " (ocmema:pio-join skipped_not_found ", ") ". (Genera o guarda primero)")
            )
          )
        )
      )
    )
  )
)

(defun ocmema:armados-nervaduras-una (/ path loaded res fallback)
  (setq path (getfiled "Selecciona ANL de nervadura" (ocmema:pl-default-rib-dir) "ANL" 0))
  (if (not path)
    (ocmema:proj-cancelled)
    (if (not (findfile path))
      (ocmema:proj-warn (strcat "Archivo ANL no encontrado: " path))
      (progn
        (setq loaded (ocmema:require-nerv-armado))
        (princ (strcat "\nArmando: " (vl-filename-base path) ".ANL"))
        (setq res (ocmema:safe-call 'ocmema:nerv:armar-from-anl (list path)))
        (if (car res)
          T
          (progn
            (if (and (not loaded) (findfile "DIBUJAR_NERV.lsp"))
              (ocmema:load-safe (findfile "DIBUJAR_NERV.lsp"))
            )
            (ocmema:proj-log "Fallback: ejecutando DIBUJAR_NERV interactivo")
            (setq fallback (ocmema:safe-call 'command (list "DIBUJAR_NERV")))
            (if (not (car fallback))
              (ocmema:proj-warn (strcat "No se pudo ejecutar entrypoint ni fallback interactivo: " (cadr res) " | " (cadr fallback)))
            )
          )
        )
      )
    )
  )
  (princ)
)

(defun ocmema:armados-nervaduras-todas (/ folder files f path res total ok fail failed_list batch-ready err-upper)
  (if (not (ocmema:require-nerv-armado))
    (progn
      (ocmema:proj-log "No se puede batch porque falta ocmema:nerv:armar-from-anl. Abriendo modo UNA/Interactivo")
      (ocmema:proj-log "OCMEMA: No existe ocmema:nerv:armar-from-anl; no se puede batch.")
    )
    (progn
      (setq folder (ocmema:safe-pick-folder "Selecciona carpeta con archivos .ANL" (ocmema:pl-default-rib-dir)))
      (if (not folder)
        (ocmema:proj-cancelled)
        (progn
          (setq files (ocmema:list-anl folder))
          (if (not files)
            (ocmema:proj-log "OCMEMA: No se encontraron archivos ANL en la carpeta.")
            (progn
              (setq total (length files))
              (setq ok 0)
              (setq fail 0)
              (setq failed_list '())
              (setq batch-ready T)
              (foreach f files
                (if batch-ready
                  (progn
                    (setq path (ocmema:proj-join-path folder f))
                    (if (not (findfile path))
                      (progn
                        (setq fail (1+ fail))
                        (setq failed_list (append failed_list (list f)))
                        (ocmema:proj-warn (strcat "Archivo ANL no encontrado: " path))
                      )
                      (progn
                        (princ (strcat "\nArmando: " f))
                        (setq res (ocmema:safe-call 'ocmema:nerv:armar-from-anl (list path)))
                        (if (car res)
                          (setq ok (1+ ok))
                          (progn
                            (setq err-upper (strcase (cadr res)))
                            (if (and (= ok 0) (= fail 0) (wcmatch err-upper "*NO FUNCTION DEFINITION*"))
                              (progn
                                (setq batch-ready nil)
                                (ocmema:proj-warn "No se puede batch: falta entrypoint ocmema:nerv:armar-from-anl. Modo interactivo no es compatible con batch.")
                              )
                              (progn
                                (setq fail (1+ fail))
                                (setq failed_list (append failed_list (list f)))
                                (ocmema:proj-warn (strcat "OCMEMA WARN: fallo " f " -> " (cadr res)))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
              (ocmema:proj-log (strcat "OCMEMA: Batch nervaduras -> total=" (itoa total) " ok=" (itoa ok) " fallidas=" (itoa fail)))
              (if failed_list
                (ocmema:proj-log (strcat "OCMEMA: Fallidas: " (ocmema:pio-join failed_list ", ")))
              )
            )
          )
        )
      )
    )
  )
  (princ)
)

(defun ocmema:menu-dibujar-planta-trabes (/ opt done)
  (if (not (ocmema:pl-project-beams-valid-p))
    (ocmema:proj-log "Proyecto invalido o sin trabes.")
    (progn
      (setq done nil)
      (while (not done)
        (initget "U A R")
        (setq opt (getkword "\nTrabes Planta [U Una/A Todas/R Regresar] <R>: "))
        (cond
          ((or (not opt) (= opt "R")) (setq done T))
          ((= opt "U")
           (ocmema:pl-safe-apply "dibujar trabe (una)" 'ocmema:menu-dibujar-planta-trabes-una '())
          )
          ((= opt "A")
           (ocmema:pl-safe-apply "dibujar trabes (todas)" 'ocmema:menu-dibujar-planta-trabes-todas '())
          )
        )
      )
    )
  )
  (princ)
)

;; Public: new project flow
(defun ocmema:proj-new (/ pname nplants i plname wall nx ny xnames ynames plants path ok name oldproj oldpath olddir savepath addAxesNow)
  (setq oldproj ocmema:*project*)
  (setq oldpath ocmema:*project-path*)
  (setq olddir ocmema:*last-project-dir*)
  (setq savepath (getfiled "Guardar proyecto OCMEMA como..." (ocmema:proj-default-save-suggest) "txt" 1))
  (if (not savepath)
    (progn (ocmema:proj-cancelled) nil)
    (progn
      (setq savepath (ocmema:proj-ensure-txt savepath))
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
                  (setq ok T)
                  (initget "S N")
                  (setq addAxesNow (getkword "\nAgregar ejes al proyecto ahora? [S/N] <N>: "))
                  (if (or (not addAxesNow) (= addAxesNow "N"))
                    (progn
                      (setq nx 0)
                      (setq ny 0)
                      (setq xnames '())
                      (setq ynames '())
                      (if ocmema:*debug-io*
                        (ocmema:dbg-io "[DEBUG] Ejes omitidos al crear proyecto (modo sin ejes).")
                      )
                    )
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
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                  (if (not ok)
                    nil
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
                          (cons "project_path" savepath)
                          (cons "plants" plants)
                        )
                      )
                      (if (> (+ nx ny) 0)
                        (if (ocmema:pio-capture-all-axes)
                          (progn
                            (setq path savepath)
                            (if (ocmema:pio-save-project path)
                              (progn
                                (ocmema:proj-update-path-state path)
                                (ocmema:proj-log "Proyecto guardado.")
                                ocmema:*project*
                              )
                              (progn
                                (setq ocmema:*project* oldproj)
                                (setq ocmema:*project-path* oldpath)
                                (setq ocmema:*last-project-dir* olddir)
                                (ocmema:proj-log "No se pudo guardar el proyecto.")
                                nil
                              )
                            )
                          )
                          (progn
                            (setq ocmema:*project* oldproj)
                            (setq ocmema:*project-path* oldpath)
                            (setq ocmema:*last-project-dir* olddir)
                            nil
                          )
                        )
                        (progn
                          (setq path savepath)
                          (if (ocmema:pio-save-project path)
                            (progn
                              (ocmema:proj-update-path-state path)
                              (ocmema:proj-log "Proyecto guardado.")
                              ocmema:*project*
                            )
                            (progn
                              (setq ocmema:*project* oldproj)
                              (setq ocmema:*project-path* oldpath)
                              (setq ocmema:*last-project-dir* olddir)
                              (ocmema:proj-log "No se pudo guardar el proyecto.")
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

(defun ocmema:anl-clean-line (line / i ch code out)
  (setq out "")
  (if (not line)
    ""
    (progn
      (setq i 1)
      (while (<= i (strlen line))
        (setq ch (substr line i 1))
        (setq code (ascii ch))
        (if (or (>= code 32) (= code 9))
          (setq out (strcat out ch))
          (setq out (strcat out " "))
        )
        (setq i (1+ i))
      )
      (ocmema:str-trim out)
    )
  )
)

(defun ocmema:anl-read-lines (path / temp-file raw out line wsh cmd ok)
  (setq raw nil)
  (setq ok nil)
  (if (and path (findfile path))
    (progn
      (setq temp-file
        (strcat
          (vl-filename-directory path)
          "\\"
          (vl-filename-base path)
          "_OCMEMA_ASCII_TMP.TXT"
        )
      )
      (setq wsh (vlax-create-object "WScript.Shell"))
      (setq cmd
        (strcat
          "powershell -NoProfile -Command \"Get-Content -LiteralPath '"
          path
          "' | Set-Content -Encoding Ascii '"
          temp-file
          "'\""
        )
      )
      (vlax-invoke wsh 'Run cmd 0 :vlax-true)
      (if wsh (vlax-release-object wsh))
      (if (findfile temp-file)
        (progn
          (setq raw (ocmema:pio-read-lines temp-file))
          (vl-file-delete temp-file)
          (setq ok T)
        )
      )
    )
  )
  (if (not ok)
    (setq raw (ocmema:pio-read-lines path))
  )
  (setq out '())
  (if raw
    (foreach line raw
      (setq out (append out (list (ocmema:anl-clean-line line))))
    )
  )
  out
)

(defun ocmema:anl-tokenize (s / i ch token out)
  (setq out '())
  (setq token "")
  (setq i 1)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (or (= ch " ") (= ch "\t"))
      (progn
        (if (/= token "")
          (progn
            (setq out (append out (list token)))
            (setq token "")
          )
        )
      )
      (setq token (strcat token ch))
    )
    (setq i (1+ i))
  )
  (if (/= token "")
    (setq out (append out (list token)))
  )
  out
)

(defun ocmema:anl-number-p (token / v)
  (setq v (distof token 2))
  (if v T nil)
)

(defun ocmema:anl-int-p (token / v)
  (setq v (atoi token))
  (if (and (/= token "") (= (itoa v) token))
    T
    nil
  )
)

(defun ocmema:anl-strip-leading-record-index (tokens / first)
  (if (and tokens (> (length tokens) 1))
    (progn
      (setq first (car tokens))
      (if (= (substr first (strlen first) 1) ".")
        (cdr tokens)
        tokens
      )
    )
    tokens
  )
)

(defun ocmema:anl-unit-tag-from-line (line / u)
  (setq u (strcase line))
  (cond
    ((wcmatch u "*UNIT*CENTIMETER*") "CM")
    ((wcmatch u "*UNIT*CM*") "CM")
    ((wcmatch u "*UNIT*METER*") "M")
    (T nil)
  )
)

(defun ocmema:anl-unit-factor (unit /)
  (if (= unit "M") 100.0 1.0)
)

(defun ocmema:anl-split-segments (line / parts out item)
  (setq parts (ocmema:pio-split line ";"))
  (setq out '())
  (foreach item parts
    (setq item (ocmema:str-trim item))
    (if (/= item "")
      (setq out (append out (list item)))
    )
  )
  out
)

(defun ocmema:anl-parse-joint-line (line unit nodes / factor segments segment tokens nid x y z)
  (setq factor (ocmema:anl-unit-factor unit))
  (setq segments (ocmema:anl-split-segments line))
  (foreach segment segments
    (setq tokens (ocmema:anl-strip-leading-record-index (ocmema:anl-tokenize segment)))
    (if (and (>= (length tokens) 4)
             (ocmema:anl-int-p (nth 0 tokens))
             (ocmema:anl-number-p (nth 1 tokens))
             (ocmema:anl-number-p (nth 2 tokens))
             (ocmema:anl-number-p (nth 3 tokens)))
      (progn
        (setq nid (atoi (nth 0 tokens)))
        (setq x (* factor (distof (nth 1 tokens) 2)))
        (setq y (* factor (distof (nth 2 tokens) 2)))
        (setq z (* factor (distof (nth 3 tokens) 2)))
        (setq nodes (ocmema:pio-alist-set nid (list x y z) nodes))
      )
    )
  )
  nodes
)

(defun ocmema:anl-parse-member-line (line members / segments segment tokens mid n1 n2)
  (setq segments (ocmema:anl-split-segments line))
  (foreach segment segments
    (setq tokens (ocmema:anl-strip-leading-record-index (ocmema:anl-tokenize segment)))
    (if (and (>= (length tokens) 3)
             (ocmema:anl-int-p (nth 0 tokens))
             (ocmema:anl-int-p (nth 1 tokens))
             (ocmema:anl-int-p (nth 2 tokens)))
      (progn
        (setq mid (atoi (nth 0 tokens)))
        (setq n1 (atoi (nth 1 tokens)))
        (setq n2 (atoi (nth 2 tokens)))
        (setq members (ocmema:pio-alist-set mid (list n1 n2) members))
      )
    )
  )
  members
)

(defun ocmema:anl-first-int-in-line (line / i ch token)
  (setq i 1)
  (setq token "")
  (while (and (<= i (strlen line)) (= token ""))
    (setq ch (substr line i 1))
    (if (wcmatch ch "[0-9]")
      (progn
        (while (and (<= i (strlen line)) (wcmatch (substr line i 1) "[0-9]"))
          (setq token (strcat token (substr line i 1)))
          (setq i (1+ i))
        )
      )
      (setq i (1+ i))
    )
  )
  (if (= token "") nil (atoi token))
)

(defun ocmema:anl-member-block-start-id (line / u)
  (setq u (strcase line))
  (if (and (wcmatch u "*MEMBER*:*") (not (wcmatch u "*DESIGN ENDS*")))
    (ocmema:anl-first-int-in-line line)
    nil
  )
)

(defun ocmema:anl-member-block-end-id (line / u)
  (setq u (strcase line))
  (if (and (wcmatch u "*MEMBER*") (wcmatch u "*DESIGN ENDS*"))
    (ocmema:anl-first-int-in-line line)
    nil
  )
)

(defun ocmema:anl-expand-member-range (tokens / out i cur nxt)
  (setq out '())
  (setq i 0)
  (while (< i (length tokens))
    (setq cur (nth i tokens))
    (cond
      ((and (ocmema:anl-int-p cur)
            (< (+ i 2) (length tokens))
            (= (strcase (nth (1+ i) tokens)) "TO")
            (ocmema:anl-int-p (nth (+ i 2) tokens)))
       (setq cur (atoi cur))
       (setq nxt (atoi (nth (+ i 2) tokens)))
       (while (<= cur nxt)
         (setq out (append out (list cur)))
         (setq cur (1+ cur))
       )
       (setq i (+ i 3))
      )
      ((ocmema:anl-int-p cur)
       (setq out (append out (list (atoi cur))))
       (setq i (1+ i))
      )
      (T
       (setq i (1+ i))
      )
    )
  )
  out
)

(defun ocmema:anl-support-node-ids (tokens / out i cur nxt tmp)
  (setq out '())
  (setq i 0)
  (while (< i (length tokens))
    (setq cur (nth i tokens))
    (cond
      ((and (ocmema:anl-int-p cur)
            (< (+ i 2) (length tokens))
            (= (strcase (nth (1+ i) tokens)) "TO")
            (ocmema:anl-int-p (nth (+ i 2) tokens)))
       (setq cur (atoi cur))
       (setq nxt (atoi (nth (+ i 2) tokens)))
       (if (> cur nxt)
         (progn
           (setq tmp cur)
           (setq cur nxt)
           (setq nxt tmp)
         )
       )
       (while (<= cur nxt)
         (if (not (member cur out))
           (setq out (append out (list cur)))
         )
         (setq cur (1+ cur))
       )
       (setq i (+ i 3))
      )
      ((ocmema:anl-int-p cur)
       (if (not (member (atoi cur) out))
         (setq out (append out (list (atoi cur))))
       )
       (setq i (1+ i))
      )
      ((= (strcase cur) "TO")
       (setq i (1+ i))
      )
      (T
       (setq i (length tokens))
      )
    )
  )
  out
)

(defun ocmema:anl-format-number (v / s)
  (if (not (numberp v))
    (setq v (ocmema:pio-to-number v))
  )
  (if (not (numberp v))
    (setq v 0.0)
  )
  (setq s (rtos v 2 6))
  (if (vl-string-search "." s)
    (progn
      (while (and (> (strlen s) 0) (= (substr s (strlen s) 1) "0"))
        (setq s (substr s 1 (1- (strlen s))))
      )
      (if (and (> (strlen s) 0) (= (substr s (strlen s) 1) "."))
        (setq s (substr s 1 (1- (strlen s))))
      )
    )
  )
  s
)

(defun ocmema:anl-property-info (tokens unit / factor ids idx yd zd yb zb spec mode)
  (setq factor (ocmema:anl-unit-factor unit))
  (setq idx 0)
  (while (and (< idx (length tokens)) (/= (strcase (nth idx tokens)) "PRIS"))
    (setq idx (1+ idx))
  )
  (if (>= idx (length tokens))
    nil
    (progn
      (setq ids (ocmema:anl-expand-member-range (ocmema:pio-sublist tokens 0 idx)))
      (setq idx (1+ idx))
      (setq yd nil)
      (setq zd nil)
      (setq yb nil)
      (setq zb nil)
      (while (< idx (length tokens))
        (cond
          ((and (= (strcase (nth idx tokens)) "YD") (< (1+ idx) (length tokens)) (ocmema:anl-number-p (nth (1+ idx) tokens)))
           (setq yd (* factor (distof (nth (1+ idx) tokens) 2)))
           (setq idx (+ idx 2))
          )
          ((and (= (strcase (nth idx tokens)) "ZD") (< (1+ idx) (length tokens)) (ocmema:anl-number-p (nth (1+ idx) tokens)))
           (setq zd (* factor (distof (nth (1+ idx) tokens) 2)))
           (setq idx (+ idx 2))
          )
          ((and (= (strcase (nth idx tokens)) "YB") (< (1+ idx) (length tokens)) (ocmema:anl-number-p (nth (1+ idx) tokens)))
           (setq yb (* factor (distof (nth (1+ idx) tokens) 2)))
           (setq idx (+ idx 2))
          )
          ((and (= (strcase (nth idx tokens)) "ZB") (< (1+ idx) (length tokens)) (ocmema:anl-number-p (nth (1+ idx) tokens)))
           (setq zb (* factor (distof (nth (1+ idx) tokens) 2)))
           (setq idx (+ idx 2))
          )
          (T (setq idx (1+ idx)))
        )
      )
      (if (and yd zd ids)
        (progn
          (if (and yb zb)
            (progn
              (setq mode "VAR")
              (setq spec
                (strcat
                  "PRIS YD " (ocmema:anl-format-number yd)
                  " ZD " (ocmema:anl-format-number zd)
                  " YB " (ocmema:anl-format-number yb)
                  " ZB " (ocmema:anl-format-number zb)
                )
              )
            )
            (progn
              (setq mode "CONST")
              (setq spec
                (strcat
                  "PRIS YD " (ocmema:anl-format-number yd)
                  " ZD " (ocmema:anl-format-number zd)
                )
              )
            )
          )
          (list
            (cons "ids" ids)
            (cons "yd" yd)
            (cons "zd" zd)
            (cons "yb" yb)
            (cons "zb" zb)
            (cons "mode" mode)
            (cons "spec" spec)
          )
        )
        nil
      )
    )
  )
)

(defun ocmema:anl-parse-geometry (path / lines line u unit nodes members props supports data-mode info fc p ids supp-ids)
  (setq lines (ocmema:anl-read-lines path))
  (setq unit "CM")
  (setq nodes '())
  (setq members '())
  (setq props '())
  (setq supports '())
  (setq data-mode nil)
  (setq fc 200.0)
  (foreach line lines
    (setq u (strcase line))
    (if (ocmema:anl-unit-tag-from-line line)
      (setq unit (ocmema:anl-unit-tag-from-line line))
    )
    (cond
      ((wcmatch u "*JOINT COORDINATES*") (setq data-mode "NODES"))
      ((wcmatch u "*MEMBER INCIDENCES*") (setq data-mode "MEMBERS"))
      ((wcmatch u "*MEMBER PROPERTY*") (setq data-mode "PROPS"))
      ((wcmatch u "*SUPPORTS*") (setq data-mode "SUPPORTS"))
      ((or (wcmatch u "*ELEMENT INCIDENCES*")
           (wcmatch u "*ELEMENT PROPERTY*")
           (wcmatch u "*DEFINE MATERIAL*")
           (wcmatch u "*CONSTANTS*")
           (wcmatch u "*LOAD*")
           (wcmatch u "*UNIT *")
           (wcmatch u "*START CONCRETE DESIGN*")
           (wcmatch u "*FINISH*"))
       (setq data-mode nil)
      )
    )
    (cond
      ((= data-mode "NODES")
       (setq nodes (ocmema:anl-parse-joint-line line unit nodes))
      )
      ((= data-mode "MEMBERS")
       (setq members (ocmema:anl-parse-member-line line members))
      )
      ((and (= data-mode "PROPS") (wcmatch u "*PRIS*"))
       (setq info (ocmema:anl-property-info (ocmema:anl-strip-leading-record-index (ocmema:anl-tokenize line)) unit))
       (if info
         (progn
           (setq ids (ocmema:pio-assoc-get "ids" info))
           (foreach p ids
             (setq props
               (ocmema:pio-alist-set
                 p
                 (list
                   (cons "yd" (ocmema:pio-assoc-get "yd" info))
                   (cons "zd" (ocmema:pio-assoc-get "zd" info))
                   (cons "yb" (ocmema:pio-assoc-get "yb" info))
                   (cons "zb" (ocmema:pio-assoc-get "zb" info))
                   (cons "mode" (ocmema:pio-assoc-get "mode" info))
                   (cons "spec" (ocmema:pio-assoc-get "spec" info))
                 )
                 props
               )
             )
           )
         )
       )
      )
      ((and (= data-mode "SUPPORTS") (> (strlen u) 0))
       (setq supp-ids (ocmema:anl-support-node-ids (ocmema:anl-strip-leading-record-index (ocmema:anl-tokenize line))))
       (foreach p supp-ids
         (if (not (member p supports))
           (setq supports (append supports (list p)))
         )
       )
      )
    )
  )
  (list
    (cons "lines" lines)
    (cons "nodes" nodes)
    (cons "members" members)
    (cons "props" props)
    (cons "supports" supports)
    (cons "fc" fc)
  )
)

(defun ocmema:model3d-member-exists-p (mid mids / found item)
  (setq found nil)
  (foreach item mids
    (if (= item mid) (setq found T))
  )
  found
)

(defun ocmema:model3d-shared-node (pair1 pair2 / a b c d)
  (setq a (car pair1))
  (setq b (cadr pair1))
  (setq c (car pair2))
  (setq d (cadr pair2))
  (cond
    ((= a c) a)
    ((= a d) a)
    ((= b c) b)
    ((= b d) b)
    (T nil)
  )
)

(defun ocmema:model3d-other-node (pair node /)
  (cond
    ((= (car pair) node) (cadr pair))
    ((= (cadr pair) node) (car pair))
    (T nil)
  )
)

(defun ocmema:model3d-distance-cm (pt1 pt2 / dx dy dz)
  (setq dx (- (car pt2) (car pt1)))
  (setq dy (- (cadr pt2) (cadr pt1)))
  (setq dz (- (caddr pt2) (caddr pt1)))
  (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
)

(defun ocmema:model3d-safe-assoc (key alist / found item)
  (setq found nil)
  (foreach item alist
    (if (and (not found) (listp item) (= (car item) key))
      (setq found item)
    )
  )
  found
)

(defun ocmema:model3d-digits-only-p (s / i ch ok)
  (setq ok T)
  (setq i 1)
  (while (and ok (<= i (strlen s)))
    (setq ch (substr s i 1))
    (if (not (wcmatch ch "[0-9]"))
      (setq ok nil)
    )
    (setq i (1+ i))
  )
  ok
)

(defun ocmema:model3d-last-item (lst / item x)
  (setq item nil)
  (foreach x lst
    (setq item x)
  )
  item
)

(defun ocmema:model3d-validate-chain (chain / oriented item ok msg)
  (setq ok T)
  (setq msg nil)
  (setq oriented (ocmema:pio-assoc-get "members" chain))
  (if (not (and oriented (listp oriented)))
    (progn
      (setq ok nil)
      (setq msg "La cadena orientada de miembros es invalida.")
    )
    (foreach item oriented
      (if (and ok (not (and (listp item)
                            (>= (length item) 4)
                            (numberp (car item))
                            (numberp (nth 1 item))
                            (numberp (nth 2 item))
                            (numberp (nth 3 item)))))
        (progn
          (setq ok nil)
          (setq msg (strcat "Miembro orientado invalido: " (vl-princ-to-string item)))
        )
      )
    )
  )
  (list ok msg)
)

(defun ocmema:model3d-build-chain (member-ids members nodes / pair1 pair2 shared start current node-seq oriented mid pair nxt pt1 pt2 len dup)
  (if (or (not member-ids) (= (length member-ids) 0))
    (list nil "No se especificaron miembros.")
    (progn
      (setq oriented '())
      (setq dup nil)
      (foreach mid member-ids
        (if dup
          nil
          (if (ocmema:model3d-member-exists-p mid oriented)
            (setq dup T)
            (setq oriented (append oriented (list mid)))
          )
        )
      )
      (if dup
        (list nil "La lista de miembros contiene duplicados.")
        (progn
          (setq pair1 (cdr (ocmema:model3d-safe-assoc (car member-ids) members)))
          (if (not (and (listp pair1) (>= (length pair1) 2)))
            (list nil (strcat "No existe el miembro " (itoa (car member-ids)) " en el ANL."))
            (progn
              (if (= (length member-ids) 1)
                (progn
                  (setq node-seq (list (car pair1) (cadr pair1)))
                  (setq pt1 (cdr (ocmema:model3d-safe-assoc (car pair1) nodes)))
                  (setq pt2 (cdr (ocmema:model3d-safe-assoc (cadr pair1) nodes)))
                  (if (or (not pt1) (not pt2))
                    (setq oriented nil)
                    (progn
                      (setq len (ocmema:model3d-distance-cm pt1 pt2))
                      (if (<= len 0.0)
                        (setq oriented nil)
                        (setq oriented (list (list (car member-ids) (car pair1) (cadr pair1) len)))
                      )
                    )
                  )
                )
                (progn
                  (setq pair2 (cdr (ocmema:model3d-safe-assoc (cadr member-ids) members)))
                  (if (not (and (listp pair2) (>= (length pair2) 2)))
                    (setq pair2 nil)
                  )
                  (setq shared (if pair2 (ocmema:model3d-shared-node pair1 pair2) nil))
                  (if shared
                    (progn
                      (setq start (ocmema:model3d-other-node pair1 shared))
                      (setq node-seq (list start shared))
                      (setq pt1 (cdr (ocmema:model3d-safe-assoc start nodes)))
                      (setq pt2 (cdr (ocmema:model3d-safe-assoc shared nodes)))
                      (if (or (not pt1) (not pt2))
                        (setq oriented nil)
                        (progn
                          (setq len (ocmema:model3d-distance-cm pt1 pt2))
                          (if (<= len 0.0)
                            (setq oriented nil)
                            (setq oriented (list (list (car member-ids) start shared len)))
                          )
                        )
                      )
                    )
                    (setq node-seq nil)
                  )
                )
              )
              (if (or (not node-seq) (not oriented))
                (list nil "Los miembros no tienen conectividad consecutiva.")
                (progn
                  (setq current (ocmema:model3d-last-item node-seq))
                  (foreach mid (cdr member-ids)
                    (setq pair (cdr (ocmema:model3d-safe-assoc mid members)))
                    (if (and (listp pair) (>= (length pair) 2))
                      (cond
                        ((= (car pair) current) (setq nxt (cadr pair)))
                        ((= (cadr pair) current) (setq nxt (car pair)))
                        (T (setq nxt nil))
                      )
                      (setq nxt nil)
                    )
                    (if (not nxt)
                      (setq oriented nil)
                      (progn
                        (setq pt1 (cdr (ocmema:model3d-safe-assoc current nodes)))
                        (setq pt2 (cdr (ocmema:model3d-safe-assoc nxt nodes)))
                        (if (or (not pt1) (not pt2))
                          (setq oriented nil)
                          (progn
                            (setq len (ocmema:model3d-distance-cm pt1 pt2))
                            (if (<= len 0.0)
                              (setq oriented nil)
                              (progn
                                (setq oriented (append oriented (list (list mid current nxt len))))
                                (if (not (= nxt (ocmema:model3d-last-item node-seq)))
                                  (setq node-seq (append node-seq (list nxt)))
                                )
                                (setq current nxt)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                  (if oriented
                    (list
                      (cons "nodes" node-seq)
                      (cons "members" oriented)
                    )
                    (list nil "La cadena no es continua o tiene nodos faltantes.")
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

(defun ocmema:model3d-mode-from-props (member-ids props / unique mid spec)
  (setq unique '())
  (foreach mid member-ids
    (setq spec (ocmema:pio-assoc-get "spec" (cdr (ocmema:model3d-safe-assoc mid props))))
    (if (and spec (not (member spec unique)))
      (setq unique (append unique (list spec)))
    )
  )
  (cond
    ((= (length unique) 1) (list T "CONST"))
    ((= (length unique) 2) (list T "VAR"))
    (T (list nil (strcat "Se detectaron " (itoa (length unique)) " tipos de seccion. Modelo3D solo soporta maximo 2.")))
  )
)

(defun ocmema:model3d-find-design-block-bounds (lines member-id / idx line start-id end-id start end)
  (setq idx 0)
  (setq start nil)
  (setq end nil)
  (while (< idx (length lines))
    (setq line (nth idx lines))
    (if (not start)
      (progn
        (setq start-id (ocmema:anl-member-block-start-id line))
        (if (and start-id (= start-id member-id))
          (setq start idx)
        )
      )
      (progn
        (setq end-id (ocmema:anl-member-block-end-id line))
        (if (and end-id (= end-id member-id))
          (setq end idx)
        )
      )
    )
    (setq idx (1+ idx))
  )
  (if (and start end (<= start end))
    (list start end)
    nil
  )
)

(defun ocmema:model3d-extract-design-blocks (lines member-ids / blocks mid bounds)
  (setq blocks '())
  (foreach mid member-ids
    (setq bounds (ocmema:model3d-find-design-block-bounds lines mid))
    (if bounds
      (setq blocks
        (ocmema:pio-alist-set
          mid
          (ocmema:pio-sublist lines (car bounds) (1+ (cadr bounds)))
          blocks
        )
      )
    )
  )
  blocks
)

(defun ocmema:model3d-support-nodes (chain member-ids all-members actual-supports / node-seq out first-node last-node pair mid n1 n2 ordered nid)
  (setq node-seq (ocmema:pio-assoc-get "nodes" chain))
  (setq out '())
  (if node-seq
    (progn
      (if (and actual-supports (> (length actual-supports) 0))
        (foreach nid actual-supports
          (if (and (member nid node-seq) (not (member nid out)))
            (setq out (append out (list nid)))
          )
        )
        (progn
          (setq first-node (car node-seq))
          (setq last-node (ocmema:model3d-last-item node-seq))
          (if (and first-node (not (member first-node out)))
            (setq out (append out (list first-node)))
          )
          (if (and last-node (not (member last-node out)))
            (setq out (append out (list last-node)))
          )
          (foreach pair all-members
            (setq mid (car pair))
            (if (not (member mid member-ids))
              (progn
                (setq n1 (car (cdr pair)))
                (setq n2 (cadr (cdr pair)))
                (if (and n1 (member n1 node-seq) (not (member n1 out)))
                  (setq out (append out (list n1)))
                )
                (if (and n2 (member n2 node-seq) (not (member n2 out)))
                  (setq out (append out (list n2)))
                )
              )
            )
          )
        )
      )
      (setq ordered '())
      (foreach nid node-seq
        (if (member nid out)
          (setq ordered (append ordered (list nid)))
        )
      )
      ordered
    )
    out
  )
)

(defun ocmema:model3d-geom-lines (chain props / lines node-seq oriented xacc item pinfo)
  (setq lines '())
  (setq node-seq (ocmema:pio-assoc-get "nodes" chain))
  (setq oriented (ocmema:pio-assoc-get "members" chain))
  (setq lines (append lines (list "UNIT CM KG" "JOINT COORDINATES")))
  (setq xacc 0.0)
  (if node-seq
    (progn
      (setq lines (append lines (list (strcat (itoa (car node-seq)) " 0 0 0"))))
      (foreach item oriented
        (setq xacc (+ xacc (nth 3 item)))
        (setq lines (append lines (list (strcat (itoa (nth 2 item)) " " (ocmema:anl-format-number xacc) " 0 0"))))
      )
    )
  )
  (setq lines (append lines (list "MEMBER INCIDENCES")))
  (foreach item oriented
    (setq lines (append lines (list (strcat (itoa (car item)) " " (itoa (nth 1 item)) " " (itoa (nth 2 item))))))
  )
  (setq lines (append lines (list "MEMBER PROPERTY")))
  (foreach item oriented
    (setq pinfo (cdr (ocmema:model3d-safe-assoc (car item) props)))
    (setq lines (append lines (list (strcat (itoa (car item)) " " (ocmema:pio-assoc-get "spec" pinfo)))))
  )
  lines
)

(defun ocmema:model3d-final-anl-lines (beam-name chain props fc design-blocks support-nodes / lines node-seq oriented item first-node last-node blocks support-line)
  (setq lines (list "STAAD PLANE" "START JOB INFORMATION" (strcat "JOB NAME " beam-name) "END JOB INFORMATION"))
  (setq lines (append lines (ocmema:model3d-geom-lines chain props)))
  (setq lines
    (append lines
      (list
        "DEFINE MATERIAL START"
        "ISOTROPIC FC_200"
        "E 110000"
        "POISSON 0.17"
        "DENSITY 0.0024"
        "ALPHA 1E-05"
        "DAMP 0.05"
        "G 47008.55"
        "TYPE CONCRETE"
        "END DEFINE MATERIAL"
        "CONSTANTS"
        "MATERIAL FC_200 ALL"
        "MEMBER CRACKED"
      )
    )
  )
  (setq oriented (ocmema:pio-assoc-get "members" chain))
  (foreach item oriented
    (setq lines (append lines (list (strcat (itoa (car item)) " REDUCTION RIZ 0.5"))))
  )
  (setq node-seq (ocmema:pio-assoc-get "nodes" chain))
  (setq first-node (car node-seq))
  (setq last-node (ocmema:model3d-last-item node-seq))
  (if (and support-nodes (> (length support-nodes) 0))
    (setq support-line (ocmema:pio-join (mapcar 'itoa support-nodes) " "))
    (setq support-line (strcat (itoa first-node) " " (itoa last-node)))
  )
  (setq lines (append lines (list "SUPPORTS" (strcat support-line " PINNED"))))
  (setq lines
    (append lines
      (list
        "UNIT CM KG"
        "START CONCRETE DESIGN"
        "CODE ACI"
        (strcat "FC " (ocmema:anl-format-number fc) " ALL")
        "FYMAIN 4200 ALL"
        "FYSEC 4200 ALL"
        "TRACK 2 ALL"
        "DESIGN BEAM ALL"
      )
    )
  )
  (foreach item oriented
    (setq blocks (cdr (ocmema:model3d-safe-assoc (car item) design-blocks)))
    (if blocks
      (setq lines (append lines blocks))
    )
  )
  (setq lines (append lines (list "END CONCRETE DESIGN" "FINISH")))
  lines
)

(defun ocmema:model3d-meta-string (original-path beam-file member-ids mode /)
  (strcat
    "SOURCE=MODELO3D;"
    "ORIGINAL=" (vl-filename-base original-path) ".ANL;"
    "ANL=" (vl-filename-base beam-file) ".ANL;"
    "MODE=" mode ";"
    "MEMBERS=" (ocmema:pio-join (mapcar 'itoa member-ids) ",")
  )
)

(defun ocmema:model3d-upsert-project-beam (beam-name beam-path original-path member-ids mode / existing points npoints align plant beam)
  (setq existing (ocmema:proj-find-beam beam-name))
  (setq points (if existing (ocmema:pio-assoc-get "points_raw" existing) '()))
  (setq npoints (if existing (ocmema:pio-assoc-get "n_points" existing) 0))
  (setq align (if existing (ocmema:pio-assoc-get "align" existing) ""))
  (setq plant (if existing (ocmema:pio-assoc-get "plant_idx" existing) 0))
  (setq beam
    (list
      (cons "name" beam-name)
      (cons "plant_idx" plant)
      (cons "align" align)
      (cons "n_points" (if npoints npoints 0))
      (cons "points_raw" (if points points '()))
      (cons "meta_kv" (ocmema:model3d-meta-string original-path beam-path member-ids mode))
    )
  )
  (ocmema:proj-upsert-beam beam)
)

(defun ocmema:model3d-prompt-member-list (members / member-ids idx raw trim mid done)
  (setq member-ids '())
  (setq idx 1)
  (setq done nil)
  (while (not done)
    (setq raw (getstring T (strcat "\nMiembro " (itoa idx) " de la trabe <Enter para terminar>: ")))
    (if (not raw)
      (setq done T)
      (progn
        (setq trim (ocmema:str-trim raw))
        (cond
          ((= trim "")
           (if (> (length member-ids) 0)
             (setq done T)
             (ocmema:proj-log "Debes capturar al menos un miembro.")
           )
          )
          ((not (ocmema:model3d-digits-only-p trim))
           (ocmema:proj-log "Solo se admiten numeros enteros.")
          )
          (T
           (setq mid (atoi trim))
           (cond
             ((member mid member-ids)
              (ocmema:proj-log "Miembro repetido.")
             )
             ((not (ocmema:model3d-safe-assoc mid members))
              (ocmema:proj-log "Ese miembro no existe en el ANL.")
             )
             (T
              (setq member-ids (append member-ids (list mid)))
              (setq idx (1+ idx))
             )
           )
          )
        )
      )
    )
  )
  member-ids
)

(defun ocmema:model3d-target-dir (model-path / dir)
  (setq dir (if (and model-path (/= model-path ""))
              (vl-filename-directory model-path)
              nil
            )
  )
  (if (not (and dir (/= dir "") (vl-file-directory-p dir)))
    (setq dir (ocmema:pl-default-beam-dir))
  )
  (if (and dir (/= dir "") (vl-file-directory-p dir))
    dir
    (vl-filename-directory (or ocmema:*project-path* (findfile "OCMEMA_PROJECT_IO.lsp")))
  )
)

(defun ocmema:armados-trabes-modelo3d (/ path geom beam-count beam-idx beam-name member-ids mid chain props mode-info mode design-blocks final-lines
                                          target-dir final-path geom-path fc res saved all-found stage-run stage-check support-nodes)
  (setq path (getfiled "Selecciona archivo ANL original del modelo" (ocmema:pl-default-beam-dir) "ANL" 0))
  (if (not path)
    (ocmema:proj-cancelled)
    (if (not (findfile path))
      (ocmema:proj-warn (strcat "Archivo ANL no encontrado: " path))
      (progn
        (setq geom (ocmema:anl-parse-geometry path))
        (if (or (not (ocmema:pio-assoc-get "nodes" geom))
                (not (ocmema:pio-assoc-get "members" geom))
                (not (ocmema:pio-assoc-get "props" geom)))
          (ocmema:proj-warn "No se pudo leer geometria, incidencias o propiedades del ANL original.")
          (progn
            (setq beam-count (ocmema:pio-getint-min "\nCuantas trabes deseas definir desde Modelo3D?: " 1))
            (if (not beam-count)
              (ocmema:proj-cancelled)
              (progn
                (setq target-dir (ocmema:model3d-target-dir path))
                (if (and target-dir (/= target-dir ""))
                  (progn
                    (ocmema:proj-set 'dir_beams_std target-dir)
                    (ocmema:proj-set "dir_beams_std" target-dir)
                  )
                )
                (setq beam-idx 1)
                (while (<= beam-idx beam-count)
                  (setq beam-name (ocmema:pio-get-nonempty-string (strcat "\nNombre de la trabe " (itoa beam-idx) ": ")))
                  (if (not beam-name)
                    (setq beam-idx (1+ beam-count))
                    (progn
                      (setq member-ids (ocmema:model3d-prompt-member-list (ocmema:pio-assoc-get "members" geom)))
                      (if member-ids
                        (progn
                          (setq stage-run
                            (vl-catch-all-apply
                              'ocmema:model3d-build-chain
                              (list member-ids (ocmema:pio-assoc-get "members" geom) (ocmema:pio-assoc-get "nodes" geom))
                            )
                          )
                          (if (vl-catch-all-error-p stage-run)
                            (progn
                              (ocmema:proj-warn (strcat "Fallo en Modelo3D al construir la cadena: " (vl-catch-all-error-message stage-run)))
                              (setq beam-idx (1+ beam-count))
                            )
                            (progn
                              (setq chain stage-run)
                              (if (not (car chain))
                                (progn
                                  (ocmema:proj-warn (cadr chain))
                                  (setq beam-idx (1+ beam-count))
                                )
                                (progn
                                  (setq stage-check (ocmema:model3d-validate-chain chain))
                                  (if (not (car stage-check))
                                    (progn
                                      (ocmema:proj-warn (cadr stage-check))
                                      (setq beam-idx (1+ beam-count))
                                    )
                                    (progn
                                      (setq props (ocmema:pio-assoc-get "props" geom))
                                      (setq stage-run
                                        (vl-catch-all-apply
                                          'ocmema:model3d-mode-from-props
                                          (list member-ids props)
                                        )
                                      )
                                      (if (vl-catch-all-error-p stage-run)
                                        (progn
                                          (ocmema:proj-warn (strcat "Fallo en Modelo3D al detectar modo de seccion: " (vl-catch-all-error-message stage-run)))
                                          (setq beam-idx (1+ beam-count))
                                        )
                                        (progn
                                          (setq mode-info stage-run)
                                          (if (not (car mode-info))
                                            (progn
                                              (ocmema:proj-warn (cadr mode-info))
                                              (setq beam-idx (1+ beam-count))
                                            )
                                            (progn
                                              (setq mode (cadr mode-info))
                                              (setq stage-run
                                                (vl-catch-all-apply
                                                  'ocmema:model3d-extract-design-blocks
                                                  (list (ocmema:pio-assoc-get "lines" geom) member-ids)
                                                )
                                              )
                                              (if (vl-catch-all-error-p stage-run)
                                                (progn
                                                  (ocmema:proj-warn (strcat "Fallo en Modelo3D al extraer detallado real: " (vl-catch-all-error-message stage-run)))
                                                  (setq beam-idx (1+ beam-count))
                                                )
                                                (progn
                                                  (setq design-blocks stage-run)
                                                  (setq all-found T)
                                                  (foreach mid member-ids
                                                    (if (not (ocmema:model3d-safe-assoc mid design-blocks))
                                                      (setq all-found nil)
                                                    )
                                                  )
                                                  (if (not all-found)
                                                    (progn
                                                      (ocmema:proj-warn "No se encontro el detallado real de todos los miembros seleccionados.")
                                                      (setq beam-idx (1+ beam-count))
                                                    )
                                                    (progn
                                      (setq fc (ocmema:pio-to-number (ocmema:pio-assoc-get "fc" geom)))
                                      (if (not (numberp fc))
                                        (setq fc 200.0)
                                      )
                                                      (setq geom-path (ocmema:proj-join-path target-dir "_OCMEMA_MODELO3D_GEOM_TMP.ANL"))
                                                      (setq final-path (ocmema:proj-join-path target-dir (strcat (ocmema:pio-sanitize-name beam-name) ".ANL")))
                                                      (setq stage-run
                                                        (vl-catch-all-apply
                                                          'ocmema:model3d-geom-lines
                                                          (list chain props)
                                                        )
                                                      )
                                                      (if (vl-catch-all-error-p stage-run)
                                                        (progn
                                                          (ocmema:proj-warn (strcat "Fallo en Modelo3D al generar geometria temporal: " (vl-catch-all-error-message stage-run)))
                                                          (setq beam-idx (1+ beam-count))
                                                        )
                                                        (progn
                                                          (ocmema:pio-write-lines geom-path stage-run)
                                                          (setq support-nodes (ocmema:model3d-support-nodes chain member-ids (ocmema:pio-assoc-get "members" geom) (ocmema:pio-assoc-get "supports" geom)))
                                                          (setq stage-run
                                                            (vl-catch-all-apply
                                                              'ocmema:model3d-final-anl-lines
                                                              (list beam-name chain props fc design-blocks support-nodes)
                                                            )
                                                          )
                                                          (if (vl-catch-all-error-p stage-run)
                                                            (progn
                                                              (ocmema:proj-warn (strcat "Fallo en Modelo3D al generar ANL final: " (vl-catch-all-error-message stage-run)))
                                                              (setq beam-idx (1+ beam-count))
                                                            )
                                                            (progn
                                                              (setq final-lines stage-run)
                                                              (setq saved (ocmema:pio-write-lines final-path final-lines))
                                                              (if (not saved)
                                                                (progn
                                                                  (ocmema:proj-warn (strcat "No se pudo escribir el archivo final: " final-path))
                                                                  (setq beam-idx (1+ beam-count))
                                                                )
                                                                (progn
                                                                  (ocmema:model3d-upsert-project-beam beam-name final-path path member-ids mode)
                                                                  (ocmema:proj-autosave-from "MODELO3D beam generation")
                                                                  (princ (strcat "\nArmando trabe Modelo3D: " (vl-filename-base final-path) ".ANL"))
                                                                  (setq res (ocmema:beam:run-by-mode mode final-path))
                                                                  (if (not (car res))
                                                                    (ocmema:proj-warn (strcat "Se genero el ANL, pero fallo el armado automatico: " (cadr res)))
                                                                  )
                                                                  (setq beam-idx (1+ beam-idx))
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
                          )
                        )
                        (setq beam-idx (1+ beam-count))
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

(defun ocmema:menu-armados-trabes (/ opt done)
  (setq done nil)
  (while (not done)
    (initget "U T M R")
    (setq opt (getkword "\nArmados Trabes [U Una/T Todas/M Modelo3D/R Regresar] <U>: "))
    (cond
      ((= opt "T")
       (ocmema:armados-trabes-todas)
      )
      ((= opt "M")
       (ocmema:armados-trabes-modelo3d)
      )
      ((or (not opt) (= opt "U"))
       (ocmema:armados-trabes-una)
      )
      ((= opt "R") (setq done T))
    )
  )
  (princ)
)

(defun ocmema:armados-trabes-una (/ path info mode res fallback cmdName)
  (setq path (getfiled "Selecciona ANL de trabe" (ocmema:pl-default-beam-dir) "ANL" 0))
  (if (not path)
    (ocmema:proj-cancelled)
    (if (not (findfile path))
      (ocmema:proj-warn (strcat "Archivo ANL no encontrado: " path))
      (progn
        (setq info (ocmema:beam:detect-section-info path))
        (setq mode (ocmema:pio-assoc-get "mode" info))
        (ocmema:proj-log
          (strcat
            "OCMEMA: Trabe " (vl-filename-base path)
            " mode=" mode
            " uniqueSectionsCount=" (itoa (ocmema:pio-assoc-get "uniqueSectionsCount" info))
            " priCount=" (itoa (ocmema:pio-assoc-get "priCount" info))
            " memberCount=" (if (ocmema:pio-assoc-get "memberCount" info) (itoa (ocmema:pio-assoc-get "memberCount" info)) "nil")
          )
        )
        (princ (strcat "\nArmando trabe: " (vl-filename-base path) ".ANL"))
        (setq res (ocmema:beam:run-by-mode mode path))
        (if (not (car res))
          (progn
            (if (= mode "VAR")
              (setq cmdName "DIBUJAR_TRABE_V_FINAL")
              (setq cmdName "DIBUJAR_TRABE_SIMPLE")
            )
            (ocmema:proj-log (strcat "Fallback interactivo: ejecutando " cmdName))
            (setq fallback (ocmema:safe-call 'command (list cmdName)))
            (if (not (car fallback))
              (ocmema:proj-warn (strcat "No se pudo ejecutar armado de trabe ni fallback interactivo: " (cadr res) " | " (cadr fallback)))
            )
          )
        )
      )
    )
  )
  (princ)
)

(defun ocmema:armados-trabes-todas (/ folder files f path info mode res total ok fail failed_list batch-ready err-upper)
  (setq folder (ocmema:safe-pick-folder "Selecciona carpeta con archivos .ANL" (ocmema:pl-default-beam-dir)))
  (if (not folder)
    (ocmema:proj-cancelled)
    (progn
      (setq files (ocmema:list-anl folder))
      (if (not files)
        (ocmema:proj-log "OCMEMA: No se encontraron archivos ANL en la carpeta.")
        (progn
          (setq total (length files))
          (setq ok 0)
          (setq fail 0)
          (setq failed_list '())
          (setq batch-ready T)
          (foreach f files
            (if batch-ready
              (progn
                (setq path (ocmema:proj-join-path folder f))
                (if (not (findfile path))
                  (progn
                    (setq fail (1+ fail))
                    (setq failed_list (append failed_list (list f)))
                    (ocmema:proj-warn (strcat "Archivo ANL no encontrado: " path))
                  )
                  (progn
                    (setq info (ocmema:beam:detect-section-info path))
                    (setq mode (ocmema:pio-assoc-get "mode" info))
                    (princ (strcat "\nArmando trabe: " f " mode=" mode))
                    (setq res (ocmema:beam:run-by-mode mode path))
                    (if (car res)
                      (setq ok (1+ ok))
                      (progn
                        (setq err-upper (strcase (cadr res)))
                        (if (and (= ok 0) (= fail 0) (wcmatch err-upper "*NO FUNCTION DEFINITION*"))
                          (progn
                            (setq batch-ready nil)
                            (ocmema:proj-warn "No se puede batch: falta entrypoint del armador de trabes. Modo interactivo no es compatible con batch.")
                          )
                          (progn
                            (setq fail (1+ fail))
                            (setq failed_list (append failed_list (list f)))
                            (ocmema:proj-warn (strcat "OCMEMA WARN: fallo " f " -> " (cadr res)))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
          (ocmema:proj-log (strcat "OCMEMA: Batch trabes -> total=" (itoa total) " ok=" (itoa ok) " fallidas=" (itoa fail)))
          (if failed_list
            (ocmema:proj-log (strcat "OCMEMA: Fallidas: " (ocmema:pio-join failed_list ", ")))
          )
        )
      )
    )
  )
  (princ)
)

(defun C:OCMEMA_ARMADOS_NERV (/)
  (ocmema:menu-armados-nervaduras)
  (princ)
)

(princ "\nOCMEMA_PROJECT_IO.lsp loaded.\n")
(princ)







