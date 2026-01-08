;; OCMEMA Project IO module (AutoLISP only). No runtime logic on load.

;; Global variables
(setq ocmema:*project* nil)
(setq ocmema:*project-path* nil)
(setq ocmema:*project-io-version* "OCMEMA_PROJECT_V1")
(setq ocmema:*proj-default-dir* "C:\\Users\\edgar\\OneDrive - ITESO\\OCMEMA_IE\\01. PROYECTOS\\2025\\")

;; Logging (local, no dependencies)
(defun ocmema:proj-log (msg /)
  (princ (strcat "\nOCMEMA: " msg))
)

;; Auto-save with default directory
(defun ocmema:proj-autosave (/ proj pname suggest path)
  (setq proj ocmema:*project*)
  (if (not proj)
    nil
    (progn
      (setq pname (ocmema:pio-assoc-get "project_name" proj))
      (if (or (not pname) (= pname ""))
        (setq suggest "OCMEMA_PROJECT.txt")
        (setq suggest (strcat pname "_OCMEMA_PROJECT.txt"))
      )
      (setq path (ocmema:proj-getfile-save "Guardar proyecto" suggest))
      (if path
        (ocmema:pio-save-project path)
        nil
      )
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
  (if (> pos 0)
    (progn
      (setq key (substr line 1 (1- pos)))
      (setq val (substr line (1+ pos) (- len pos)))
      (list key val)
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
      (setq parts (ocmema:pio-split s ","))
      (foreach pair parts
        (setq kv (ocmema:pio-split pair ":"))
        (if (>= (length kv) 2)
          (progn
            (setq name (car kv))
            (setq coord (ocmema:pio-to-number (cadr kv)))
            (if coord
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
  (setvar "OSMODE" 643)
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
(defun ocmema:pio-save-project (path / proj lines plants plant x_axes y_axes pname xnames ynames)
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
      (if (ocmema:pio-write-lines path lines)
        (progn
          (setq ocmema:*project-path* path)
          T
        )
        nil
      )
    )
  )
)

;; Load project
(defun ocmema:pio-load-project (path / lines len i line kv key val plants idx pname x_axes y_axes nplants nx ny wall xnames ynames projname
                                    ok vline has-pname has-nplants has-wall has-nx has-ny has-xnames has-ynames
                                    has-plant-name has-x-axes has-y-axes found-end)
  (setq lines (ocmema:pio-read-lines path))
  (if (not lines)
    (progn (ocmema:proj-log "TXT invalido: no se pudo leer.") nil)
    (progn
      (setq vline (ocmema:str-trim (car lines)))
      (if (/= vline ocmema:*project-io-version*)
        (progn (ocmema:proj-log (strcat "TXT invalido: version no coincide (" vline ")")) nil)
        (progn
          (setq plants '())
          (setq nplants 0)
          (setq nx 0)
          (setq ny 0)
          (setq wall 0.0)
          (setq xnames '())
          (setq ynames '())
          (setq projname "")
          (setq has-pname nil)
          (setq has-nplants nil)
          (setq has-wall nil)
          (setq has-nx nil)
          (setq has-ny nil)
          (setq has-xnames nil)
          (setq has-ynames nil)
          (setq ok T)
          (setq i 1)
          (setq len (length lines))
          (while (and (< i len) ok)
            (setq line (ocmema:str-trim (nth i lines)))
            (cond
              ((= line "") nil)
              ((wcmatch line "[PLANT*")
               (setq idx 0)
               (setq pname "")
               (setq x_axes '())
               (setq y_axes '())
               (setq has-plant-name nil)
               (setq has-x-axes nil)
               (setq has-y-axes nil)
               (setq found-end nil)
               (setq idx (atoi (ocmema:str-trim (substr line 8 (- (strlen line) 8)))))
               (setq i (1+ i))
               (while (and (< i len) (not found-end) ok)
                 (setq line (ocmema:str-trim (nth i lines)))
                 (if (= line "[/PLANT]")
                   (setq found-end T)
                   (progn
                     (setq kv (ocmema:pio-split-kv line))
                     (if kv
                       (progn
                         (setq key (car kv))
                         (setq val (cadr kv))
                         (cond
                           ((= key "PLANT_NAME") (setq pname val) (setq has-plant-name T))
                           ((= key "X_AXES") (setq x_axes (ocmema:pio-parse-axes val)) (setq has-x-axes T))
                           ((= key "Y_AXES") (setq y_axes (ocmema:pio-parse-axes val)) (setq has-y-axes T))
                         )
                       )
                     )
                   )
                 )
                 (setq i (1+ i))
               )
               (if (not found-end)
                 (progn (ocmema:proj-log "TXT invalido: falta [/PLANT]") (setq ok nil))
                 (progn
                   (if (not has-plant-name)
                     (progn (ocmema:proj-log "TXT invalido: falta PLANT_NAME") (setq ok nil))
                     (if (not has-x-axes)
                       (progn (ocmema:proj-log "TXT invalido: falta X_AXES") (setq ok nil))
                       (if (not has-y-axes)
                         (progn (ocmema:proj-log "TXT invalido: falta Y_AXES") (setq ok nil))
                         (progn
                           (setq x_axes (ocmema:pio-sort-pairs x_axes))
                           (setq y_axes (ocmema:pio-sort-pairs y_axes))
                           (setq plants
                             (append plants
                               (list
                                 (list
                                   (cons "idx" idx)
                                   (cons "name" pname)
                                   (cons "x_axes" x_axes)
                                   (cons "y_axes" y_axes)
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
               (setq i (1- i))
              )
              (T
               (setq kv (ocmema:pio-split-kv line))
               (if kv
                 (progn
                   (setq key (car kv))
                   (setq val (cadr kv))
                   (cond
                     ((= key "PROJECT_NAME") (setq projname val) (setq has-pname T))
                     ((= key "N_PLANTS") (setq nplants (atoi val)) (setq has-nplants T))
                     ((= key "WALL_CM") (setq wall (ocmema:pio-to-number val)) (setq has-wall T))
                     ((= key "NX") (setq nx (atoi val)) (setq has-nx T))
                     ((= key "NY") (setq ny (atoi val)) (setq has-ny T))
                     ((= key "X_NAMES") (setq xnames (ocmema:pio-split val ",")) (setq has-xnames T))
                     ((= key "Y_NAMES") (setq ynames (ocmema:pio-split val ",")) (setq has-ynames T))
                   )
                 )
               )
              )
            )
            (setq i (1+ i))
          )
          (if (and ok (not has-pname)) (progn (ocmema:proj-log "TXT invalido: falta PROJECT_NAME") (setq ok nil)))
          (if (and ok (not has-nplants)) (progn (ocmema:proj-log "TXT invalido: falta N_PLANTS") (setq ok nil)))
          (if (and ok (not has-wall)) (progn (ocmema:proj-log "TXT invalido: falta WALL_CM") (setq ok nil)))
          (if (and ok (not has-nx)) (progn (ocmema:proj-log "TXT invalido: falta NX") (setq ok nil)))
          (if (and ok (not has-ny)) (progn (ocmema:proj-log "TXT invalido: falta NY") (setq ok nil)))
          (if (and ok (not has-xnames)) (progn (ocmema:proj-log "TXT invalido: falta X_NAMES") (setq ok nil)))
          (if (and ok (not has-ynames)) (progn (ocmema:proj-log "TXT invalido: falta Y_NAMES") (setq ok nil)))
          (if (and ok (or (<= nplants 0) (<= nx 0) (<= ny 0)))
            (progn (ocmema:proj-log "TXT invalido: valores numericos incorrectos") (setq ok nil))
          )
          (if (and ok (or (/= (length plants) nplants)
                          (/= (length xnames) nx)
                          (/= (length ynames) ny)))
            (progn (ocmema:proj-log "TXT invalido: cantidades no coinciden") (setq ok nil))
          )
          (if (not ok)
            nil
            (progn
              (setq ocmema:*project*
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
                )
              )
              (setq ocmema:*project-path* path)
              ocmema:*project*
            )
          )
        )
      )
    )
  )
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
                (ocmema:proj-log "Cambio cancelado.")
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
    (initget "Nuevo Cargar Salir")
    (setq opt (getkword "\nOCMEMA [Nuevo/Cargar/Salir] <Salir>: "))
    (cond
      ((or (not opt) (= opt "Salir"))
       (setq done T)
      )
      ((= opt "Nuevo")
       (if (ocmema:proj-new)
         (progn
           (ocmema:menu-general)
         )
       )
      )
      ((= opt "Cargar")
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
    (initget "ModificarEjes GenerarElementos Dibujar Salir")
    (setq opt (getkword "\nOCMEMA [ModificarEjes/GenerarElementos/Dibujar/Salir] <Salir>: "))
    (cond
      ((or (not opt) (= opt "Salir")) (setq done T))
      ((= opt "ModificarEjes") (ocmema:menu-modificar-ejes))
      ((= opt "GenerarElementos") (ocmema:menu-generar-elementos))
      ((= opt "Dibujar") (ocmema:menu-dibujar))
    )
  )
  (princ)
)

(defun ocmema:menu-modificar-ejes (/ opt done)
  (setq done nil)
  (while (not done)
    (initget "RecapturarTodos CambiarEje Regresar")
    (setq opt (getkword "\nEjes [RecapturarTodos/CambiarEje/Regresar] <Regresar>: "))
    (cond
      ((or (not opt) (= opt "Regresar")) (setq done T))
      ((= opt "RecapturarTodos")
       (if (ocmema:pio-capture-all-axes)
         (if ocmema:*project-path*
           (ocmema:pio-save-project ocmema:*project-path*)
           (ocmema:proj-autosave)
         )
       )
      )
      ((= opt "CambiarEje")
       (ocmema:menu-cambiar-eje)
      )
    )
  )
  (princ)
)

(defun ocmema:menu-cambiar-eje (/ proj nplants plantIdx axisType axisName opt exists names newName)
  (setq proj ocmema:*project*)
  (if (not proj)
    (ocmema:proj-log "No hay proyecto cargado.")
    (progn
      (initget "X Y")
      (setq axisType (getkword "\nTipo de eje [X/Y]: "))
      (if (not axisType)
        (ocmema:proj-log "Tipo de eje invalido.")
        (progn
          (setq axisName (getstring T "\nNombre exacto del eje: "))
          (if (= axisType "X")
            (setq names (ocmema:pio-assoc-get "x_names" proj))
            (setq names (ocmema:pio-assoc-get "y_names" proj))
          )
          (setq exists (ocmema:pio-name-exists names axisName))
          (if (not exists)
            (ocmema:proj-log "Eje no existe.")
            (progn
              (initget "Renombrar CambiarCoord Regresar")
              (setq opt (getkword "\nAccion [Renombrar/CambiarCoord/Regresar] <Regresar>: "))
              (cond
                ((or (not opt) (= opt "Regresar")) nil)
                ((= opt "Renombrar")
                 (setq newName "")
                 (while (or (= newName "") (ocmema:pio-name-exists names newName))
                   (setq newName (getstring T "\nNuevo nombre: "))
                   (cond
                     ((= newName "") (ocmema:proj-log "Nombre invalido."))
                     ((ocmema:pio-name-exists names newName) (ocmema:proj-log "Nombre ya existe."))
                   )
                 )
                 (ocmema:pio-rename-axis axisType axisName newName)
                 (if ocmema:*project-path*
                   (ocmema:pio-save-project ocmema:*project-path*)
                   (ocmema:proj-autosave)
                 )
                )
                ((= opt "CambiarCoord")
                 (setq nplants (ocmema:pio-assoc-get "n_plants" proj))
                 (setq plantIdx (getint "\nNumero de planta: "))
                 (if (or (not plantIdx) (< plantIdx 1) (> plantIdx nplants))
                   (ocmema:proj-log "Planta invalida.")
                   (progn
                     (if (ocmema:pio-change-axis-coord plantIdx axisType axisName)
                       (if ocmema:*project-path*
                         (ocmema:pio-save-project ocmema:*project-path*)
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
  (princ)
)

(defun ocmema:menu-generar-elementos (/ opt done)
  (setq done nil)
  (while (not done)
    (initget "Traves Nervaduras Regresar")
    (setq opt (getkword "\nGenerar [Traves/Nervaduras/Regresar] <Regresar>: "))
    (cond
      ((or (not opt) (= opt "Regresar")) (setq done T))
      ((= opt "Traves") (ocmema:menu-generar-traves))
      ((= opt "Nervaduras") (ocmema:menu-generar-nervaduras))
    )
  )
  (princ)
)

(defun ocmema:menu-generar-traves (/ opt done)
  (setq done nil)
  (while (not done)
    (initget "GenerarNueva ModificarExistente Regresar")
    (setq opt (getkword "\nTraves [GenerarNueva/ModificarExistente/Regresar] <Regresar>: "))
    (cond
      ((or (not opt) (= opt "Regresar")) (setq done T))
      ((= opt "GenerarNueva") (ocmema:proj-log "Pendiente: Generar Traves"))
      ((= opt "ModificarExistente") (ocmema:proj-log "No existen traves en este proyecto. Primero genere uno."))
    )
  )
  (princ)
)

(defun ocmema:menu-generar-nervaduras (/ opt done)
  (setq done nil)
  (while (not done)
    (initget "GenerarNueva ModificarExistente Regresar")
    (setq opt (getkword "\nNervaduras [GenerarNueva/ModificarExistente/Regresar] <Regresar>: "))
    (cond
      ((or (not opt) (= opt "Regresar")) (setq done T))
      ((= opt "GenerarNueva") (ocmema:proj-log "Pendiente: Generar Nervaduras"))
      ((= opt "ModificarExistente") (ocmema:proj-log "No existen nervaduras en este proyecto. Primero genere uno."))
    )
  )
  (princ)
)

(defun ocmema:menu-dibujar (/ opt done)
  (setq done nil)
  (while (not done)
    (initget "Armados Planta Regresar")
    (setq opt (getkword "\nDibujar [Armados/Planta/Regresar] <Regresar>: "))
    (cond
      ((or (not opt) (= opt "Regresar")) (setq done T))
      ((= opt "Armados") (ocmema:proj-log "Pendiente: Dibujar Armados"))
      ((= opt "Planta") (ocmema:proj-log "Pendiente: Dibujar Planta"))
    )
  )
  (princ)
)

;; Public: new project flow
(defun ocmema:proj-new (/ pname nplants i plname wall nx ny xnames ynames plants path folder defaultFile)
  (setq pname (getstring T "\nNombre del proyecto: "))
  (setq nplants (getint "\nNumero de plantas: "))
  (if (or (not nplants) (<= nplants 0))
    (progn (ocmema:proj-log "Numero de plantas invalido.") nil)
    (progn
      (setq i 1)
      (setq plname "")
      (setq plants '())
      (while (<= i nplants)
        (setq plname (getstring T (strcat "\nNombre planta " (itoa i) " <Planta " (itoa i) ">: ")))
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
      (setq wall (getreal "\nEspesor de muro (cm): "))
      (if (or (not wall) (<= wall 0.0))
        (progn (ocmema:proj-log "Espesor de muro invalido.") nil)
        (progn
          (setq nx (getint "\nNumero de ejes X: "))
          (setq ny (getint "\nNumero de ejes Y: "))
          (if (or (not nx) (<= nx 0) (not ny) (<= ny 0))
            (progn (ocmema:proj-log "Numero de ejes invalido.") nil)
            (progn
              (setq xnames '())
              (setq i 1)
              (while (<= i nx)
                (setq xnames
                  (append xnames
                    (list (getstring T (strcat "\nNombre eje X " (itoa i) ": ")))
                  )
                )
                (setq i (1+ i))
              )
              (setq ynames '())
              (setq i 1)
              (while (<= i ny)
                (setq ynames
                  (append ynames
                    (list (getstring T (strcat "\nNombre eje Y " (itoa i) ": ")))
                  )
                )
                (setq i (1+ i))
              )
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
                  (setq path (ocmema:proj-getfile-save "Guardar proyecto" (strcat pname "_OCMEMA_PROJECT.txt")))
                  (if (not path)
                    (progn (ocmema:proj-log "Guardado cancelado.") nil)
                    (progn
                      (ocmema:pio-save-project path)
                      ocmema:*project*
                    )
                  )
                )
                nil
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
  (ocmema:menu-entry)
  (princ)
)

(princ)
