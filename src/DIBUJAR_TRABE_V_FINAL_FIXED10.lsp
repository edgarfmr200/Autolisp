;; ==============================================================================
;; DIBUJAR_TRABE_V_FINAL.LSP
;; BASE: DIBUJAR_TRABE_V13.LSP (no romper lógica existente salvo lo necesario)
;;
;; NUEVO (FINAL):
;;  - Lee secciones por miembro desde ANL (token-by-token, robusto a numeración "11.")
;;  - Construye cadena continua, ordena por X y aborta si no es continua
;;  - Soporta hasta 2 tipos de sección (abort si >2)
;;  - Alineación Superior/Inferior: contorno longitudinal variable (cara plana + cara escalonada)
;;  - Ejes: en nodos + ejes rojos sin burbuja en cambios de sección
;;  - Acero longitudinal con regla de reciprocidad (cara plana continua + cara escalonada con 2 aceros)
;;  - Bastones: conserva lógica V13, pero Ld = 12*ϕ (cm) por extremo (ya no +0.20 m fijo)
;;  - Sección transversal: 1 o 2 secciones (A-A y B-B). La peraltada se dibuja compuesta tipo T invertida (simplificada)
;;
;; NOTA:
;;  - Se preservan offsets fijos, colores y estilo general. No se implementa auto-evitar solapes.
;; ==============================================================================

(defun c:DIBUJAR_TRABE_V_FINAL (/ *error* file dir filename tempFile wsh cmd fp line

                                ;; Geometría / lectura
                                dataMode tokenList strList cleanLine
                                nodeXcm memberNodes memberProp
                                coordList startX endX l_total_cm l_total_m
                                tramosOrdenados uniqueKeys keyList
                                hMax_cm bMax_cm hMin_cm bMin_cm
                                fc_val

                                ;; Dibujo base
                                ptOrigin xOrigin yPick yOrigin yAlign
                                widthName xPad rectLen p1 p2
                                alignChoice isAlignSup isAlignInf
                                yMin_global yMax_global h_draw_global
                                axisXList dimXList idx numNodes node nID nX_cm nX_m
                                drawX axisCenter pAxisBot pAxisTop centerCircle
                                yTopRed yDimLoc i

                                ;; Contorno variable
                                xDrawStart xDrawEnd
                                tramo t_x1m t_x2m t_hcm t_bcm t_key
                                ptsContour

                                ;; Ejes cambio sección
                                boundaryXList bx

                                ;; As min / acero base
                                asMinByKey asMinGlobal
                                fy_val
                                planoIsBottom escalonIsTop
                                areaBarPlan varPlan qtyPlanRec numPlan asPlan
                                varWide qtyWideRec numWide asWide
                                varDeep qtyDeepRec numDeep asDeepExtra
                                areaWide areaDeep
                                ySteelPlan ySteelWide
                                ySteelDeep tramoYFace

                                ;; Lectura acero STAAD
                                globalSteelList xOffsetGlobal isReadingSteel savedDist astTop astBot
                                memberOffsetList mIndex

                                ;; Demanda
                                maxDemBot maxDemTop

                                ;; Bastones
                                redondear-al-5 get-closest-axis dibujar-leader-blindado
                                obtener-x-interpolada procesar-zonas-v27 calc-puntos-vFINAL
                                obtener-area-varilla obtener-diametro-real
                                list_zonas zona zoneStart zoneEnd maxReq deficit
                                x_ini x_fin x_baston_ini x_baston_fin pts_fix
                                axis_near_start axis_near_end
                                strVarBast areaBastOne qtyBastRec numBast
                                bastonesInfList bastonesSupList
                                baseAreaThisFace

                                ;; Estribos / contraflecha
                                d_eff limitEstribos yTextEstribos numZonesEstribos lenZoneEst s_estribo qtyEstribos txtEstribo pTextEst currentDist
                                addCamber numCamber locCamber xCamber yCamber strCamber deltaW gap textW totalW xDelta

                                ;; Sección transversal
                                ptSectionOrigin xSec ySec
                                b_draw h_draw_sec
                                bG_cm bS_cm hG_cm hS_cm
                                bG_draw bS_draw hG_draw hS_draw
                                almaAlign almaX0
                                rec_draw radFillet
                                p1_st p2_st
                                draw-break-line
                                hasLosa typeLosa capaCompresion h_losa alignLosa yLosaStart
                                yBBMin yBBMax yBBStep bbBranch

                                ;; Ganchos
                                rBarTop xStirrupLeft yStirrupTop offsetHookFactor centerBarX centerBarY pH1 pH2

                                ;; Bastón crítico / steel layout
                                critBastInf critBastSup
                                process-layer-steel get-critical-bast

                                ;; Restore vars
                                oldOsnap old_dimasz old_dimclrd old_dimtxt old_dimclrt old_dimscale
                               )

  (vl-load-com)

  ;; ----------------------------------------------------------------------------
  ;; 0) Helpers (mantener estilo V13)
  ;; ----------------------------------------------------------------------------

  (defun clean-string (s) (vl-string-translate "|" " " s))

  (defun redondear-al-5 (val) (* 0.05 (fix (+ (/ val 0.05) 0.5))))

  (defun get-closest-axis (x_pt axis_list / min_dist best_axis axis dist)
    (setq min_dist 100000.0 best_axis nil)
    (foreach axis axis_list
      (setq dist (abs (- x_pt axis)))
      (if (< dist min_dist) (progn (setq min_dist dist) (setq best_axis axis)))
    )
    best_axis
  )

  (defun obtener-x-interpolada (x1 y1 x2 y2 target)
    (if (equal y2 y1 0.0000001) x1 (+ x1 (* (- target y1) (/ (- x2 x1) (- y2 y1)))))
  )

  (defun obtener-area-varilla (numVar / area)
     (cond ((= numVar "2.5") 0.49) ((= numVar "3") 0.71) ((= numVar "4") 1.27)
           ((= numVar "5") 1.98) ((= numVar "6") 2.85) ((= numVar "8") 5.07)
           ((= numVar "10") 7.92) ((= numVar "12") 11.40) (t 0.0))
  )

  (defun obtener-diametro-real (numVar / d)
     (cond ((= numVar "2.5") 0.79) ((= numVar "3") 0.95) ((= numVar "4") 1.27)
           ((= numVar "5") 1.59) ((= numVar "6") 1.91) ((= numVar "8") 2.54)
           ((= numVar "10") 3.18) ((= numVar "12") 3.81) (t 1.0))
  )
  

;; --- Helpers extra (V_FINAL) ---
(defun _axis-index (x axis_list / i tol)
  (setq i 0 tol 1e-6)
  (while (and (< i (length axis_list)) (not (equal x (nth i axis_list) tol)))
    (setq i (1+ i))
  )
  (if (< i (length axis_list)) i nil)
)

(defun _axis-neighbor (axis axis_list dir / idx)
  (setq idx (_axis-index axis axis_list))
  (cond
    ((null idx) nil)
    ((= dir -1) (if (> idx 0) (nth (1- idx) axis_list) nil))
    ((= dir 1)  (if (< idx (1- (length axis_list))) (nth (1+ idx) axis_list) nil))
    (t nil)
  )
)

(defun _resolve-baston-axes (x1 x2 axis_list / ax1 ax2 d1 d2 tol dir other)
  (setq tol 1e-6)
  (setq ax1 (get-closest-axis x1 axis_list))
  (setq ax2 (get-closest-axis x2 axis_list))
  (if (and ax1 ax2 (equal ax1 ax2 tol))
    (progn
      (setq d1 (abs (- x1 ax1)))
      (setq d2 (abs (- x2 ax2)))
      ;; el mas cercano se queda; el otro busca el eje vecino hacia su lado
      (if (< d1 d2)
        (progn
          (setq dir (if (< x2 ax2) -1 1))
          (setq other (_axis-neighbor ax2 axis_list dir))
          (if (null other) (setq other (_axis-neighbor ax2 axis_list (* -1 dir))))
          (setq ax2 other)
        )
        (progn
          (setq dir (if (< x1 ax1) -1 1))
          (setq other (_axis-neighbor ax1 axis_list dir))
          (if (null other) (setq other (_axis-neighbor ax1 axis_list (* -1 dir))))
          (setq ax1 other)
        )
      )
    )
  )
  (list ax1 ax2)
)

(defun _ask-small-deficit (deficit / ans)
  ;; Regla: si deficit <= 0.20 cm2 preguntar si se desea baston; default No.
  (if (and (numberp deficit) (> deficit 0.0001) (<= deficit 0.20))
    (progn
      (initget "Si No")
      (setq ans (getkword (strcat "\nDeficit muy pequeno (" (rtos deficit 2 2) " cm2). Poner baston? [Si/No] <No>: ")))
      (if (or (null ans) (= (strcase ans) "NO"))
        nil
        T
      )
    )
    T
  )
)

  ;; Motor V27 (sin modificar su lógica)
  (defun procesar-zonas-v27 (steelList limitMin x_origin_draw is_top / idx_val base ptsCorr zonas inZone startXZone endXZone maxReqZone i prev_x prev_req curr_x curr_req first_req last_x)
    (setq idx_val (if is_top 1 2))
    (setq base (+ x_origin_draw 0.5))
    (setq ptsCorr '())
    (foreach pt steelList
      (if (and (numberp (car pt)) (numberp (nth idx_val pt)))
        (setq ptsCorr (append ptsCorr (list (list (+ base (car pt)) (nth idx_val pt)))))
      )
    )
    (setq ptsCorr (vl-sort ptsCorr '(lambda (a b) (< (car a) (car b)))))
    (setq zonas '())
    (if (< (length ptsCorr) 2)
      zonas
      (progn
        (setq inZone nil startXZone nil endXZone nil maxReqZone 0.0)
        (setq first_req (cadr (car ptsCorr)))
        (if (> first_req limitMin) (progn (setq inZone T) (setq startXZone (car (car ptsCorr))) (setq maxReqZone first_req)))
        (setq i 1)
        (while (< i (length ptsCorr))
          (setq prev_x (car (nth (1- i) ptsCorr))) (setq prev_req (cadr (nth (1- i) ptsCorr)))
          (setq curr_x (car (nth i ptsCorr))) (setq curr_req (cadr (nth i ptsCorr)))
          (if (and (not inZone) (<= prev_req limitMin) (> curr_req limitMin))
            (progn
              (setq inZone T)
              (setq startXZone (obtener-x-interpolada prev_x prev_req curr_x curr_req limitMin))
              (setq maxReqZone (max prev_req curr_req))
            )
          )
          (if inZone (setq maxReqZone (max maxReqZone curr_req)))
          (if (and inZone (> prev_req limitMin) (<= curr_req limitMin))
            (progn
              (setq endXZone (obtener-x-interpolada prev_x prev_req curr_x curr_req limitMin))
              (setq zonas (append zonas (list (list startXZone endXZone maxReqZone))))
              (setq inZone nil startXZone nil endXZone nil maxReqZone 0.0)
            )
          )
          (setq i (1+ i))
        )
        (if inZone
          (progn
            (setq last_x (car (last ptsCorr)))
            (setq zonas (append zonas (list (list startXZone last_x maxReqZone))))
          )
        )
        zonas
      )
    )
  )

  ;; NUEVO: calc-puntos con Ld variable (m). Mantiene misma lógica de redondeo y ejes.
  (defun calc-puntos-vFINAL (x_interp_start x_interp_end axis_list Ld_m / t_start t_end ax_s ax_e dist_s dist_e round_s round_e final_s final_e tmp)
    (setq t_start (- x_interp_start Ld_m))
    (setq t_end   (+ x_interp_end   Ld_m))
    (setq ax_s (get-closest-axis t_start axis_list))
    (if ax_s
      (progn
        (setq dist_s (abs (- t_start ax_s)))
        (setq round_s (redondear-al-5 dist_s))
        (setq final_s (if (< t_start ax_s) (- ax_s round_s) (+ ax_s round_s)))
      )
      (setq final_s t_start)
    )
    (setq ax_e (get-closest-axis t_end axis_list))
    (if ax_e
      (progn
        (setq dist_e (abs (- t_end ax_e)))
        (setq round_e (redondear-al-5 dist_e))
        (setq final_e (if (< t_end ax_e) (- ax_e round_e) (+ ax_e round_e)))
      )
      (setq final_e t_end)
    )
    (if (> final_s final_e) (progn (setq tmp final_s) (setq final_s final_e) (setq final_e tmp)))
    (list final_s final_e)
  )

  ;; Leader fijo (igual a V13)
  (defun dibujar-leader-blindado (pt1 texto es_top es_final / sgn_y sgn_x pt2 pt3 en_lead en_txt obj_leader obj_text)
     (setvar "DIMCLRD" 7) (setvar "DIMASZ" 0.15) (setvar "DIMSCALE" 1.0)
     (setq sgn_y (if es_top -1.0 1.0))
     (setq sgn_x (if es_final -1.0 1.0))
     (setq pt2 (list (+ (car pt1) (* 0.20 sgn_x)) (+ (cadr pt1) (* 0.35 sgn_y))))
     (setq pt3 (list (+ (car pt2) (* 0.15 sgn_x)) (cadr pt2)))
     (command "_.LEADER" pt1 pt2 pt3 "" "" "N")

     (setq en_lead (entlast))
     (if en_lead
       (progn
         (setq obj_leader (vl-catch-all-apply 'vlax-ename->vla-object (list en_lead)))
         (if (not (vl-catch-all-error-p obj_leader))
           (vl-catch-all-apply 'vla-put-Color (list obj_leader 7))
         )
       )
     )

     (if (< (car pt3) (car pt2))
       (command "_.TEXT" "_J" "_MR" pt3 0.15 0 texto)
       (command "_.TEXT" "_J" "_ML" pt3 0.15 0 texto)
     )

     (setq en_txt (entlast))
     (if en_txt
       (progn
         (setq obj_text (vl-catch-all-apply 'vlax-ename->vla-object (list en_txt)))
         (if (not (vl-catch-all-error-p obj_text))
           (vl-catch-all-apply 'vla-put-Color (list obj_text 40))
         )
       )
     )
  )

  ;; Error handler
  (defun *error* (msg)
    (if oldOsnap (setvar "OSMODE" oldOsnap))
    (setvar "DIMASZ" old_dimasz)
    (setvar "DIMCLRD" old_dimclrd)
    (setvar "DIMTXT" old_dimtxt)
    (setvar "DIMCLRT" old_dimclrt)
    (setvar "DIMSCALE" old_dimscale)
    (if (and fp (= (type fp) 'FILE)) (close fp))
    (if msg
      (progn
        (princ (strcat "\n; ERROR: " msg))
        (alert (strcat "ERROR: " msg))
      )
    )
    (princ)
  )

  ;; ----------------------------------------------------------------------------
  ;; 1) Setup AutoCAD vars (igual V13)
  ;; ----------------------------------------------------------------------------
  (setq old_dimasz (getvar "DIMASZ"))
  (setq old_dimclrd (getvar "DIMCLRD"))
  (setq old_dimtxt (getvar "DIMTXT"))
  (setq old_dimclrt (getvar "DIMCLRT"))
  (setq old_dimscale (getvar "DIMSCALE"))

  (setq oldOsnap (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  (setvar "FILLETRAD" 0.05)

  (if (not (tblsearch "LTYPE" "CENTER"))
    (command "-LINETYPE" "Load" "CENTER" "acad.lin" "")
  )

  ;; ----------------------------------------------------------------------------
  ;; 2) Lectura de ANL (token-by-token, mantiene estilo V13)
  ;; ----------------------------------------------------------------------------
  (setq file (getfiled "Seleccionar archivo TRABE (ANL)" "" "ANL;TXT;OUT" 4))
  (if (not file) (exit))

  (setq filename (vl-filename-base file))
  (princ "\n1. Leyendo Geometria, Materiales y Secciones...")

  (setq fp (open file "r"))
  (setq dataMode nil fc_val 200.0)
  (setq nodeXcm '() memberNodes '() memberProp '())

  ;; Func auxiliar: guardar en assoc-list (id -> val) sin duplicados
  (defun _put (alist key val)
    (if (assoc key alist)
      (subst (cons key val) (assoc key alist) alist)
      (cons (cons key val) alist)
    )
  )

  ;; Parsear f'c desde "FC 200" o "FC_200"
  (defun _try-parse-fc (ln / p s numStr ch i)
    (cond
      ((wcmatch ln "* FC *")
        (setq cleanLine (clean-string ln))
        (setq tokenList (read (strcat "(" cleanLine ")")))
        (setq i 0)
        (while (< i (length tokenList))
          (if (and (eq (nth i tokenList) 'FC) (numberp (nth (1+ i) tokenList)))
            (progn (setq fc_val (nth (1+ i) tokenList)) (setq i (length tokenList)))
            (setq i (1+ i))
          )
        )
      )
      ((wcmatch (strcase ln) "*FC_*")
        (setq p (vl-string-search "FC_" (strcase ln)))
        (if p
          (progn
            (setq s (substr ln (+ p 4))) ;; después de FC_
            ;; tomar números iniciales
            (setq numStr "")
            (setq i 1)
            (while (and (<= i (strlen s)) (setq ch (substr s i 1)) (wcmatch ch "[0-9.]"))
              (setq numStr (strcat numStr ch))
              (setq i (1+ i))
            )
            (if (> (strlen numStr) 0) (setq fc_val (atof numStr)))
          )
        )
      )
    )
  )

  (while (setq line (read-line fp))
    (setq line (vl-string-trim " \t" line))

    (_try-parse-fc line)

    (cond
      ;; Entrar a bloque NODOS
      ((wcmatch line "*JOINT COORDINATES*") (setq dataMode "NODES"))

      ;; Entrar a bloque MIEMBROS
      ((wcmatch line "*MEMBER INCIDENCES*") (setq dataMode "MEMBERS"))

      ;; Entrar a bloque PROPIEDADES
      ((wcmatch line "*MEMBER PROPERTY*") (setq dataMode "PROPS"))

      ;; Salir de bloque si viene otra cabecera típica
      ((or (wcmatch line "*DEFINE MATERIAL*")
           (wcmatch line "*CONSTANTS*")
           (wcmatch line "*LOAD*"))
       (if (or (= dataMode "NODES") (= dataMode "MEMBERS") (= dataMode "PROPS")) (setq dataMode nil))
      )

      ;; Leer nodos: soporta líneas con "11. 1 0 0 0" o "1 0 0 0"
      ((and (= dataMode "NODES") (> (strlen line) 0))
        (if (numberp (read (substr line 1 1)))
          (progn
            ;; Mantener lógica V13: reemplazo de "." a " " (para soportar "11.")
            (setq strList (read (strcat "(" (vl-string-translate "." " " line) ")")))
            ;; casos:
            ;;  - con enumeración: (11 1 0 0 0) => nodeID = nth 1, x = nth 2
            ;;  - sin enumeración: (1 0 0 0)    => nodeID = nth 0, x = nth 1
            (cond
              ((>= (length strList) 5)
                (setq nID (nth 1 strList))
                (setq nX_cm (nth 2 strList))
              )
              ((= (length strList) 4)
                (setq nID (nth 0 strList))
                (setq nX_cm (nth 1 strList))
              )
              (t (setq nID nil nX_cm nil))
            )
            (if (and (numberp nID) (numberp nX_cm))
              (setq nodeXcm (_put nodeXcm nID nX_cm))
            )
          )
        )
      )

      ;; Leer miembros: "17. 1 1 2" o "1 1 2"
      ((and (= dataMode "MEMBERS") (> (strlen line) 0))
        (if (numberp (read (substr line 1 1)))
          (progn
            (setq strList (read (strcat "(" (vl-string-translate "." " " line) ")")))
            (cond
              ((>= (length strList) 4)
                (setq mID (nth 1 strList))
                (setq nI  (nth 2 strList))
                (setq nJ  (nth 3 strList))
              )
              ((= (length strList) 3)
                (setq mID (nth 0 strList))
                (setq nI  (nth 1 strList))
                (setq nJ  (nth 2 strList))
              )
              (t (setq mID nil nI nil nJ nil))
            )
            (if (and (numberp mID) (numberp nI) (numberp nJ))
              (setq memberNodes (_put memberNodes mID (list nI nJ)))
            )
          )
        )
      )

      ;; Leer propiedades por miembro: "33. 1 PRIS YD 25 ZD 30"
      ((and (= dataMode "PROPS") (> (strlen line) 0) (wcmatch (strcase line) "*PRIS*"))
        (setq cleanLine (clean-string line))
        ;; Mantener estilo: traducir "." a " " para soportar "33."
        (setq tokenList (read (strcat "(" (vl-string-translate "." " " cleanLine) ")")))
        ;; member id
        (setq mID nil t_hcm nil t_bcm nil)
        (cond
          ((and (>= (length tokenList) 2) (numberp (nth 1 tokenList))) (setq mID (nth 1 tokenList)))
          ((and (>= (length tokenList) 1) (numberp (nth 0 tokenList))) (setq mID (nth 0 tokenList)))
        )
        ;; escanear YD / ZD / ZB
        (setq i 0)
        (while (< i (length tokenList))
          (cond
            ((and (eq (nth i tokenList) 'YD) (numberp (nth (1+ i) tokenList))) (setq t_hcm (nth (1+ i) tokenList)))
            ((and (or (eq (nth i tokenList) 'ZD) (eq (nth i tokenList) 'ZB)) (numberp (nth (1+ i) tokenList))) (setq t_bcm (nth (1+ i) tokenList)))
          )
          (setq i (1+ i))
        )
        (if (and (numberp mID) (numberp t_bcm) (numberp t_hcm))
          (setq memberProp (_put memberProp mID (list t_bcm t_hcm)))
        )
      )
    )
  )
  (close fp)
  (setq fp nil)

  ;; ----------------------------------------------------------------------------
  ;; 3) Validaciones y construcción de tramos (A4-A6, J)
  ;; ----------------------------------------------------------------------------
  (if (or (null nodeXcm) (null memberNodes))
    (progn (alert "Abort: No se pudieron leer nodos o miembros desde el ANL.") (exit))
  )
  (if (null memberProp)
    (progn (alert "Abort: No se pudieron leer propiedades por miembro (MEMBER PROPERTY).") (exit))
  )

  ;; Build tramosOrdenados = (list (mID x1_m x2_m L_m b_cm h_cm key))
  (defun _getx (nid / a) (setq a (assoc nid nodeXcm)) (if a (cdr a) nil))

  (setq tramosOrdenados '())
  (foreach mPair memberNodes
    (setq mID (car mPair))
    (setq nI (car (cdr mPair)))
    (setq nJ (cadr (cdr mPair)))
    (setq xI (_getx nI))
    (setq xJ (_getx nJ))
    (setq pSec (assoc mID memberProp))
    (if (and (numberp xI) (numberp xJ) pSec)
      (progn
        (setq t_bcm (car (cdr pSec)))
        (setq t_hcm (cadr (cdr pSec)))
        (setq x1 (min xI xJ))
        (setq x2 (max xI xJ))
        (setq L_cm (- x2 x1))
        (if (<= L_cm 0.0)
          (progn (alert (strcat "Abort: Longitud no válida en miembro " (itoa mID))) (exit))
        )
        (setq key (strcat "b" (rtos t_bcm 2 0) "_h" (rtos t_hcm 2 0)))
        (setq tramosOrdenados
          (cons (list mID (/ x1 100.0) (/ x2 100.0) (/ L_cm 100.0) t_bcm t_hcm key) tramosOrdenados)
        )
      )
    )
  )

  (if (null tramosOrdenados)
    (progn (alert "Abort: No se pudieron construir tramos (miembros sin nodos/properties).") (exit))
  )

  (setq tramosOrdenados (vl-sort tramosOrdenados '(lambda (a b) (< (cadr a) (cadr b)))))

  ;; Verificar continuidad por X (cadena)
  (setq i 0)
  (while (< i (1- (length tramosOrdenados)))
    (setq t1 (nth i tramosOrdenados))
    (setq t2 (nth (1+ i) tramosOrdenados))
    (if (> (abs (- (caddr t1) (cadr t2))) 0.0001)
      (progn
        (alert "Abort: No se puede ordenar la cadena por X (no es continua o hay ramas/gaps).")
        (exit)
      )
    )
    (setq i (1+ i))
  )

  ;; coordList = nodos principales ordenados (id x_cm) por X dentro de la cadena
  ;; (Se usa para ejes y offsets)
  (setq coordList '())
  (foreach tramo tramosOrdenados
    (setq mID (car tramo))
    (setq nPair (assoc mID memberNodes))
    (setq nI (car (cdr nPair)))
    (setq nJ (cadr (cdr nPair)))
    (setq xI (_getx nI))
    (setq xJ (_getx nJ))
    ;; agregar ambos nodos
    (if (and (numberp nI) (numberp xI)) (setq coordList (cons (list nI xI) coordList)))
    (if (and (numberp nJ) (numberp xJ)) (setq coordList (cons (list nJ xJ) coordList)))
  )
  ;; unique by node id
  (defun _uniq-nodes (lst / out seen it)
    (setq out '() seen '())
    (foreach it lst
      (if (not (member (car it) seen))
        (progn (setq seen (cons (car it) seen)) (setq out (cons it out)))
      )
    )
    out
  )
  (setq coordList (_uniq-nodes coordList))
  (setq coordList (vl-sort coordList '(lambda (a b) (< (cadr a) (cadr b)))))

  (setq startX (cadr (nth 0 coordList)))
  (setq endX   (cadr (last coordList)))
  (setq l_total_cm (- endX startX))
  (setq l_total_m (/ l_total_cm 100.0))

  ;; Tipos únicos (máx 2)
  (setq keyList '())
  (foreach tramo tramosOrdenados (setq keyList (cons (nth 6 tramo) keyList)))
  (setq uniqueKeys '())
  (foreach k keyList (if (not (member k uniqueKeys)) (setq uniqueKeys (cons k uniqueKeys))))
  (setq uniqueKeys (reverse uniqueKeys))

  (if (> (length uniqueKeys) 2)
    (progn
      (alert "Abort: Se detectaron 3+ tipos de sección. Máximo soportado: 2.")
      (exit)
    )
  )

  ;; Inicialización preventiva h/b (A6)
  (setq bMax_cm (nth 4 (car tramosOrdenados)))
  (setq hMax_cm (nth 5 (car tramosOrdenados)))

  ;; Calcular min/max b/h por tipos
  (setq hMax_cm 0.0 bMax_cm 0.0 hMin_cm 1e9 bMin_cm 1e9)
  (foreach tramo tramosOrdenados
    (setq t_bcm (nth 4 tramo))
    (setq t_hcm (nth 5 tramo))
    (setq hMax_cm (max hMax_cm t_hcm))
    (setq bMax_cm (max bMax_cm t_bcm))
    (setq hMin_cm (min hMin_cm t_hcm))
    (setq bMin_cm (min bMin_cm t_bcm))
  )

  ;; Resumen consola (K)
  (princ (strcat "\nNodos leidos: " (itoa (length coordList)) " | Miembros: " (itoa (length tramosOrdenados))))
  (princ "\nTramos (mID, L_m, b_cm, h_cm, key):")
  (foreach tramo tramosOrdenados
    (princ (strcat "\n  " (itoa (car tramo)) " | " (rtos (nth 3 tramo) 2 3) " | "
                   (rtos (nth 4 tramo) 2 0) " | " (rtos (nth 5 tramo) 2 0) " | " (nth 6 tramo)))
  )
  (princ (strcat "\nTipos detectados: " (itoa (length uniqueKeys)) " => " (vl-princ-to-string uniqueKeys)))

  ;; ----------------------------------------------------------------------------
  ;; 4) Alineación y punto de origen
  ;; ----------------------------------------------------------------------------
  (initget "Superior Inferior")
  (setq alignChoice (getkword "\nAlineacion de la Trabe? [Superior/Inferior] <Superior>: "))
  (if (null alignChoice) (setq alignChoice "Superior"))

  (setq isAlignSup (= alignChoice "Superior"))
  (setq isAlignInf (= alignChoice "Inferior"))

  ;; Definir altura global de dibujo por peralte máximo
  (setq h_draw_global (* (/ hMax_cm 100.0) 7.0))

  (princ "\nSeleccione punto de origen (Y=0 de la alineacion): ")
  (setq ptOrigin (getpoint))
  (if (not ptOrigin) (exit))

  (setq xOrigin (car ptOrigin))
  (setq yPick   (cadr ptOrigin))

  ;; yAlign = cara alineada (y=0). yOrigin = y inferior global del rectángulo contenedor (compatibilidad V13)
  (if isAlignInf
    (progn
      (setq yAlign yPick)
      (setq yOrigin yPick)
    )
    (progn
      (setq yAlign yPick)                ;; cara superior alineada
      (setq yOrigin (- yPick h_draw_global)) ;; bottom global para mantener V13 (yOrigin = bottom)
    )
  )

  (setq yMin_global yOrigin)
  (setq yMax_global (+ yOrigin h_draw_global))

  ;; ----------------------------------------------------------------------------
  ;; 5) Dibujo encabezados y contenedor (mantener estilo V13, pero h/b variables)
  ;; ----------------------------------------------------------------------------
  (setq widthName (* (strlen filename) 0.28))
  (setq rectLen (+ l_total_m 1.0))

  ;; Textos: h y b (si 2 tipos, mostrar ambos ascendente)
  (if (= (length uniqueKeys) 2)
    (progn
      (command "_.TEXT" "_J" "_TL" (list xOrigin (+ yOrigin 0.33)) 0.25 0 (strcat "h=" (rtos hMin_cm 2 0) "," (rtos hMax_cm 2 0)))
      (command "_.CHPROP" (entlast) "" "Color" 3 "")
      (command "_.TEXT" "_J" "_TL" (list xOrigin (+ yOrigin 0.86)) 0.25 0 (strcat "b=" (rtos bMin_cm 2 0) "," (rtos bMax_cm 2 0)))
      (command "_.CHPROP" (entlast) "" "Color" 3 "")
    )
    (progn
      (command "_.TEXT" "_J" "_TL" (list xOrigin (+ yOrigin 0.33)) 0.25 0 (strcat "h=" (rtos hMax_cm 2 0)))
      (command "_.CHPROP" (entlast) "" "Color" 3 "")
      (command "_.TEXT" "_J" "_TL" (list xOrigin (+ yOrigin 0.86)) 0.25 0 (strcat "b=" (rtos bMax_cm 2 0)))
      (command "_.CHPROP" (entlast) "" "Color" 3 "")
    )
  )

  (command "_.TEXT" "_J" "_TL" (list xOrigin (+ yOrigin h_draw_global)) 0.35 0 filename)
  (command "_.CHPROP" (entlast) "" "Color" 4 "")

  (setq xPad (max (+ widthName 0.3) 2.0))
  (setq p1 (list (+ xOrigin xPad) yOrigin))
  (setq p2 (list (+ (car p1) rectLen) (+ yOrigin h_draw_global)))

  ;; Contenedor invisible/guía (como V13) — se mantiene
  (command "_.RECTANG" p1 p2)
  (setq rectEnt (entlast))
  (command "_.CHPROP" rectEnt "" "Color" 7 "")

  ;; ----------------------------------------------------------------------------
  
;; 6) Contorno longitudinal variable (B3)
;; ----------------------------------------------------------------------------
;; Ejes: de (xBeamStart+0.50) a (xBeamEnd-0.50)
;; Geometría real: de xBeamStart a xBeamEnd (0.50m más allá de ejes)
(setq xBeamStart (car p1))
(setq xBeamEnd   (car p2))
(setq xDrawStart (+ xBeamStart 0.5))           ;; centro eje 1
(setq xDrawEnd   (+ xBeamStart 0.5 l_total_m)) ;; centro último eje

;; Helper: yTop/yBot por tramo (en coordenadas de dibujo)
(defun _tramo-yTop (tr / hdraw)
  (setq hdraw (* (/ (nth 5 tr) 100.0) 7.0))
  (if isAlignInf
    (+ yAlign hdraw)   ;; bottom alineado
    yAlign             ;; top alineado
  )
)
(defun _tramo-yBot (tr / hdraw)
  (setq hdraw (* (/ (nth 5 tr) 100.0) 7.0))
  (if isAlignInf
    yAlign             ;; bottom alineado
    (- yAlign hdraw)   ;; top alineado
  )
)

;; Construir lista de puntos del contorno (polilínea cerrada) - ROBUSTO
(setq ptsContour '())
(setq nTr (length tramosOrdenados))
(setq lastTr (nth (1- nTr) tramosOrdenados))

(if isAlignInf
  (progn
    ;; Cara inferior plana: y = yAlign, cara superior escalonada
    (setq ptsContour (append ptsContour (list (list xBeamStart yAlign) (list xBeamEnd yAlign))))
    (setq ptsContour (append ptsContour (list (list xBeamEnd (_tramo-yTop lastTr)))))

    (setq i (1- nTr))
    (while (>= i 0)
      (setq tr (nth i tramosOrdenados))
      (setq xStart_m (nth 1 tr))
      (setq currY (_tramo-yTop tr))
      (setq xL (if (= i 0) xBeamStart (+ xDrawStart xStart_m)))

      (setq ptsContour (append ptsContour (list (list xL currY))))

      (if (> i 0)
        (progn
          (setq prevY (_tramo-yTop (nth (1- i) tramosOrdenados)))
          (if (> (abs (- prevY currY)) 0.0001)
            (setq ptsContour (append ptsContour (list (list xL prevY))))
          )
        )
      )
      (setq i (1- i))
    )

    ;; Cerrar en cara inferior
    (setq ptsContour (append ptsContour (list (list xBeamStart yAlign))))
  )
  (progn
    ;; Cara superior plana: y = yAlign, cara inferior escalonada
    (setq ptsContour (append ptsContour (list (list xBeamStart yAlign) (list xBeamEnd yAlign))))
    (setq ptsContour (append ptsContour (list (list xBeamEnd (_tramo-yBot lastTr)))))

    (setq i (1- nTr))
    (while (>= i 0)
      (setq tr (nth i tramosOrdenados))
      (setq xStart_m (nth 1 tr))
      (setq currY (_tramo-yBot tr))
      (setq xL (if (= i 0) xBeamStart (+ xDrawStart xStart_m)))

      (setq ptsContour (append ptsContour (list (list xL currY))))

      (if (> i 0)
        (progn
          (setq prevY (_tramo-yBot (nth (1- i) tramosOrdenados)))
          (if (> (abs (- prevY currY)) 0.0001)
            (setq ptsContour (append ptsContour (list (list xL prevY))))
          )
        )
      )
      (setq i (1- i))
    )

    ;; Cerrar en cara superior
    (setq ptsContour (append ptsContour (list (list xBeamStart yAlign))))
  )
)

(princ (strcat "\nOK: Contorno pts=" (itoa (length ptsContour))))

;; Dibujar contorno (PLINE cerrada)
(if (>= (length ptsContour) 4)
  (progn
    (command "_.PLINE")
    (foreach p ptsContour (command p))
    (command "_C")
    (command "_.CHPROP" (entlast) "" "Color" 7 "")
    ;; Quitar rectángulo guía (no debe quedar dibujado)
    (if rectEnt (command "_.ERASE" rectEnt ""))
    (princ "\nOK: Contorno dibujado.")
  )
  (progn
    (alert "Abort: No se pudo construir el contorno (pts insuficientes).")
    (exit)
  )
)

  ;; ----------------------------------------------------------------------------
  ;; 7) Ejes (C1 + C2)
  ;; ----------------------------------------------------------------------------
  (setq axisXList '())
  (setq dimXList '())
  (setq idx 0)
  (setq numNodes (length coordList))

  (foreach node coordList
    (setq nID (car node))
    (setq nX_cm (cadr node))
    (setq nX_m (/ (- nX_cm startX) 100.0))
    (setq drawX (+ (car p1) 0.5 nX_m))
    (setq axisCenter drawX)

    (setq axisXList (append axisXList (list axisCenter)))
    (setq dimXList (append dimXList (list axisCenter)))

    (setq pAxisBot (list drawX (- yMin_global 0.5)))
    (setq pAxisTop (list drawX (+ yMax_global 1.25)))

    (command "_.LINE" pAxisBot pAxisTop "")
    (command "_.CHPROP" (entlast) "" "Color" 1 "Ltype" "CENTER" "Ltscale" 0.3 "")

    (setq centerCircle (list drawX (+ (cadr pAxisTop) 0.275)))
    (command "_.CIRCLE" centerCircle 0.275)
    (command "_.CHPROP" (entlast) "" "Color" 7 "")
    (command "_.TEXT" "_MC" centerCircle 0.22 0 (itoa (1+ idx)))
    (command "_.CHPROP" (entlast) "" "Color" 4 "")

    (setq idx (1+ idx))
  )

  ;; Ejes rojos sin burbuja en cambios de sección
  (setq boundaryXList '())
  (setq i 1)
  (while (< i (length tramosOrdenados))
    (setq tPrev (nth (1- i) tramosOrdenados))
    (setq tCurr (nth i tramosOrdenados))
    (if (/= (nth 6 tPrev) (nth 6 tCurr))
      (setq boundaryXList (cons (nth 2 tPrev) boundaryXList)) ;; xEnd_m de prev
    )
    (setq i (1+ i))
  )
  (setq boundaryXList (reverse boundaryXList))

  (foreach bx boundaryXList
    (setq drawX (+ (car p1) 0.5 bx))
    (command "_.LINE" (list drawX (- yMin_global 0.5)) (list drawX (+ yMax_global 1.25)) "")
    (command "_.CHPROP" (entlast) "" "Color" 1 "Ltype" "CENTER" "Ltscale" 0.3 "")
  )

  ;; Cotas entre ejes (como V13)
  (setq yTopRed (+ yMax_global 1.25))
  (setq yDimLoc (- yTopRed 0.25))
  (setq i 0)
  (repeat (1- (length dimXList))
    (command "_.DIMLINEAR"
             (list (nth i dimXList) yTopRed)
             (list (nth (1+ i) dimXList) yTopRed)
             (list (/ (+ (nth i dimXList) (nth (1+ i) dimXList)) 2.0) yDimLoc))
    (setq i (1+ i))
  )

  ;; ----------------------------------------------------------------------------
  ;; 8) Calcular As_min por tipo (D1) y definir acero base por caras (D2-D3)
  ;; ----------------------------------------------------------------------------
  (setq fy_val 4200.0)

  ;; As_min por key
  (setq asMinByKey '())
  (foreach tramo tramosOrdenados
    (setq t_key (nth 6 tramo))
    (if (not (assoc t_key asMinByKey))
      (progn
        (setq t_bcm (nth 4 tramo))
        (setq t_hcm (nth 5 tramo))
        (setq asMinFormula (* (/ (* 0.8 (sqrt fc_val)) fy_val) t_bcm (- t_hcm 3.0)))
        (setq asMinByKey (cons (cons t_key asMinFormula) asMinByKey))
      )
    )
  )

  ;; As_min_global = max
  (setq asMinGlobal 0.0)
  (foreach a asMinByKey (setq asMinGlobal (max asMinGlobal (cdr a))))

  (princ (strcat "\n\n*** F'c: " (rtos fc_val 2 0) " | AsMinGlobal: " (rtos asMinGlobal 2 3) " ***"))

  ;; Definir caras según alineación
  ;; - Si alineación INFERIOR => cara plana = inferior, escalonada = superior
  ;; - Si alineación SUPERIOR => cara plana = superior, escalonada = inferior
  (setq planoIsBottom isAlignInf)
  (setq escalonIsTop isAlignInf) ;; si plano es bottom, escalonada es top

  ;; -----------------------------
  ;; ACERO CARA PLANA (continuo)
  ;; -----------------------------
  (princ "\n--- ACERO CARA PLANA (continuo) ---")
  (setq varPlan (getstring "\nNumero varilla Cara Plana (ej. 3, 4): "))
  (setq areaBarPlan (obtener-area-varilla varPlan))
  (if (<= areaBarPlan 0.0)
    (progn (alert "Abort: Varilla inválida para cara plana.") (exit))
  )

  (setq qtyPlanRec (fix (+ (/ asMinGlobal areaBarPlan) 0.9999)))
  (princ (strcat "\n-> Sugerido: " (itoa qtyPlanRec) " varillas."))
  (setq numPlan (getint (strcat "\nCantidad definitiva <" (itoa qtyPlanRec) ">: ")))
  (if (null numPlan) (setq numPlan qtyPlanRec))
  (setq asPlan (* numPlan areaBarPlan))

  ;; y del acero plano
  (if planoIsBottom
    (setq ySteelPlan (+ yAlign 0.15))
    (setq ySteelPlan (- yAlign 0.15))
  )

  ;; dibujar acero plano como polilínea continua
  (setq x_ini (+ xBeamStart 0.15))
  (setq x_fin (- xBeamEnd 0.15))
  (if planoIsBottom
    (command "_.PLINE" (list x_ini (+ ySteelPlan 0.30)) (list x_ini ySteelPlan) (list x_fin ySteelPlan) (list x_fin (+ ySteelPlan 0.30)) "")
    (command "_.PLINE" (list x_ini (- ySteelPlan 0.30)) (list x_ini ySteelPlan) (list x_fin ySteelPlan) (list x_fin (- ySteelPlan 0.30)) "")
  )
  (command "_.CHPROP" (entlast) "" "Color" 5 "")
  (command "_.FILLET" "P" (entlast))
  (dibujar-leader-blindado (list (/ (+ x_ini x_fin) 2.0) ySteelPlan) (strcat (itoa numPlan) "#" varPlan) (not planoIsBottom) nil)

  ;; -----------------------------
  ;; ACERO CARA ESCALONADA (wide + extra deep si aplica)
  ;; -----------------------------
  (princ "\nOK: Iniciando acero cara escalonada...")
  (setq varWide nil qtyWideRec nil numWide nil asWide 0.0 ySteelWide nil)
  (setq varDeep nil qtyDeepRec nil numDeep nil asDeepExtra 0.0)

  (if (= (length uniqueKeys) 1)
    (princ "\n(Seccion constante: no hay acero escalonado adicional.)")
    (progn
      ;; Identificar tipo peraltado vs ancho
      ;; criterio: mayor h => peraltada; si empate, mayor b
      (defun _sec-stats (key / tr)
        ;; retorna (b h) del primer tramo con ese key
        (setq tr nil)
        (foreach trTmp tramosOrdenados (if (and (null tr) (= (nth 6 trTmp) key)) (setq tr trTmp)))
        (if (null tr) (progn (alert (strcat "Abort: No se encontró tramo para key " (vl-princ-to-string key))) (exit)))
        (list (nth 4 tr) (nth 5 tr))
      )
      (setq keyA (car uniqueKeys))
      (setq keyB (cadr uniqueKeys))
      (setq stA (_sec-stats keyA))
      (setq stB (_sec-stats keyB))
      (setq bA (car stA) hA (cadr stA))
      (setq bB (car stB) hB (cadr stB))

      (if (or (> hA hB) (and (= hA hB) (> bA bB)))
        (progn (setq keyDeep keyA) (setq keyWide keyB))
        (progn (setq keyDeep keyB) (setq keyWide keyA))
      )

      (princ (strcat "\nSeccion Ancha: " keyWide " | Seccion Peraltada: " keyDeep))

      ;; Acero ancha en cara escalonada (continuo y con Y fija del tramo ancho)
      (princ "\n--- ACERO CARA ESCALONADA: Seccion ANCHA (continuo) ---")
      (setq varWide (getstring "\nNumero varilla (Cara Escalonada - ANCHA): "))
      (setq areaWide (obtener-area-varilla varWide))
      (if (<= areaWide 0.0) (progn (alert "Abort: Varilla inválida (ancha).") (exit)))

      (setq asMinWide (cdr (assoc keyWide asMinByKey)))
      (setq qtyWideRec (fix (+ (/ asMinWide areaWide) 0.9999)))
      (princ (strcat "\n-> Sugerido: " (itoa qtyWideRec) " varillas."))
      (setq numWide (getint (strcat "\nCantidad definitiva <" (itoa qtyWideRec) ">: ")))
      (if (null numWide) (setq numWide qtyWideRec))
      (setq asWide (* numWide areaWide))

      ;; obtener yFace del tramo ancho para fijar Y absoluta
      (setq tramoWide nil)
      (foreach trTmp tramosOrdenados (if (and (null tramoWide) (= (nth 6 trTmp) keyWide)) (setq tramoWide trTmp)))
      (if (null tramoWide) (progn (alert "Abort: No se encontró tramo ancho.") (exit)))

      ;; cara escalonada es opuesta a cara plana
      (if escalonIsTop
        (setq ySteelWide (- (_tramo-yTop tramoWide) 0.15))
        (setq ySteelWide (+ (_tramo-yBot tramoWide) 0.15))
      )

      ;; dibujar continuo en toda longitud a esa Y fija
      (if escalonIsTop
        (command "_.PLINE" (list x_ini (- ySteelWide 0.30)) (list x_ini ySteelWide) (list x_fin ySteelWide) (list x_fin (- ySteelWide 0.30)) "")
        (command "_.PLINE" (list x_ini (+ ySteelWide 0.30)) (list x_ini ySteelWide) (list x_fin ySteelWide) (list x_fin (+ ySteelWide 0.30)) "")
      )
      (command "_.CHPROP" (entlast) "" "Color" 5 "")
      (command "_.FILLET" "P" (entlast))
      (dibujar-leader-blindado (list (/ (+ x_ini x_fin) 2.0) ySteelWide) (strcat (itoa numWide) "#" varWide) escalonIsTop nil)

      ;; Acero extra peraltada SOLO en tramos peraltados (D3.2)
      (setq asMinDeep (cdr (assoc keyDeep asMinByKey)))
      (setq asDeepExtra (max 0.0 (- asMinDeep asMinWide)))

  (princ (strcat "\n*** AsMin Ancha=" (rtos asMinWide 2 3) " | AsMin Peraltada=" (rtos asMinDeep 2 3) " | Extra Peraltada=" (rtos asDeepExtra 2 3) " cm2 ***"))


      (if (> asDeepExtra 0.0001)
        (progn
          (princ "\n--- ACERO CARA ESCALONADA: Extra Seccion PERALTADA (solo en tramos peraltados) ---")
          (setq varDeep (getstring "\nNumero varilla (Extra PERALTADA): "))
          (setq areaDeep (obtener-area-varilla varDeep))
          (if (<= areaDeep 0.0) (progn (alert "Abort: Varilla inválida (peraltada).") (exit)))

          (setq qtyDeepRec (fix (+ (/ asDeepExtra areaDeep) 0.9999)))
          (princ (strcat "\n-> Sugerido: " (itoa qtyDeepRec) " varillas."))
          (setq numDeep (getint (strcat "\nCantidad definitiva <" (itoa qtyDeepRec) ">: ")))
          (if (null numDeep) (setq numDeep qtyDeepRec))

          ;; Dibujar por cada tramo peraltado, con y real del contorno peraltado (escalonado)
          (foreach tramo tramosOrdenados
            (if (= (nth 6 tramo) keyDeep)
              (progn
                (setq t_x1m (nth 1 tramo))
                (setq t_x2m (nth 2 tramo))
                (setq x1 (+ xDrawStart t_x1m 0.15))
                (setq x2 (+ xDrawStart t_x2m -0.15))
                (if escalonIsTop
                  (setq ySteelDeep (- (_tramo-yTop tramo) 0.15))
                  (setq ySteelDeep (+ (_tramo-yBot tramo) 0.15))
                )
                (if escalonIsTop
                  (command "_.PLINE" (list x1 (- ySteelDeep 0.30)) (list x1 ySteelDeep) (list x2 ySteelDeep) (list x2 (- ySteelDeep 0.30)) "")
                  (command "_.PLINE" (list x1 (+ ySteelDeep 0.30)) (list x1 ySteelDeep) (list x2 ySteelDeep) (list x2 (+ ySteelDeep 0.30)) "")
                )
                (command "_.CHPROP" (entlast) "" "Color" 5 "")
                (command "_.FILLET" "P" (entlast))
                (dibujar-leader-blindado (list (/ (+ x1 x2) 2.0) ySteelDeep) (strcat (itoa numDeep) "#" varDeep) escalonIsTop nil)
              )
            )
          )
        )
        (princ "\n(As_min peraltada no excede a As_min ancha: no se requiere acero extra peraltado.)")
      )
    )
  )

  ;; ----------------------------------------------------------------------------
  ;; 9) Lectura de acero requerido (STAAD) — se mantiene (ANSI)
  ;; ----------------------------------------------------------------------------
  (princ "\n2. Procesando Acero (STAAD)...")

  ;; memberOffsetList: offsets por miembro en orden X (usa coordList -> como V13)
  (setq memberOffsetList '())
  (foreach node coordList
    (setq memberOffsetList (append memberOffsetList (list (/ (- (cadr node) startX) 100.0))))
  )
  ;; quitar el último (N-1 offsets = M miembros)
  (if (> (length memberOffsetList) 0)
    (setq memberOffsetList (reverse (cdr (reverse memberOffsetList))))
  )

  (setq dir (vl-filename-directory file))
  (setq tempFile (strcat dir "\\" filename "_TEMP_ANSI.TXT"))
  (setq wsh (vlax-create-object "WScript.Shell"))
  (setq cmd (strcat "powershell -NoProfile -Command \"Get-Content -LiteralPath '" file "' | Set-Content -Encoding Ascii '" tempFile "'\""))
  (vlax-invoke wsh 'Run cmd 0 :vlax-true)
  (vlax-release-object wsh)

  (setq fp (open tempFile "r"))
  (setq globalSteelList '() xOffsetGlobal 0.0)
  (setq isReadingSteel nil savedDist nil astTop 0.0 astBot 0.0)
  (setq mIndex 0)

  (while (setq line (read-line fp))
    (setq cleanLine (clean-string line))
    (if (wcmatch line "*LONGITUDINAL BAR DETAILS*") (setq isReadingSteel T savedDist nil))
    (if (and isReadingSteel (wcmatch line "*LONGITUDINAL BAR LAYOUT*")) (setq isReadingSteel nil))

    (if (wcmatch line "*Member *:*")
      (progn
        (setq mIndex (1+ mIndex))
        (setq savedDist nil)
        (if (and memberOffsetList (<= mIndex (length memberOffsetList)))
          (setq xOffsetGlobal (nth (1- mIndex) memberOffsetList))
          (if (> (length globalSteelList) 0) (setq xOffsetGlobal (car (last globalSteelList))) (setq xOffsetGlobal 0.0))
        )
      )
    )

    (if (and isReadingSteel (> (strlen cleanLine) 5) (not (wcmatch line "*Distance*")) (not (wcmatch line "*-----*")))
      (progn
        (setq tokenList (read (strcat "(" cleanLine ")")))
        (cond
          ((numberp (car tokenList))
            (if savedDist
              (setq globalSteelList (append globalSteelList (list (list (+ xOffsetGlobal savedDist) astTop astBot))))
            )
            (setq savedDist (/ (nth 0 tokenList) 100.0))
            (setq valAst (nth 2 tokenList))
            (if (eq (nth 1 tokenList) 'Top) (setq astTop valAst) (setq astBot valAst))
            (if (eq (nth 1 tokenList) 'Top) (setq astBot 0.0) (setq astTop 0.0))
          )
          ((or (eq (car tokenList) 'Top) (eq (car tokenList) 'Bottom))
            (if savedDist
              (progn
                (setq valAst (nth 1 tokenList))
                (if (eq (car tokenList) 'Top) (setq astTop valAst))
                (if (eq (car tokenList) 'Bottom) (setq astBot valAst))
              )
            )
          )
        )
      )
    )
  )
  (if savedDist (setq globalSteelList (append globalSteelList (list (list (+ xOffsetGlobal savedDist) astTop astBot)))))
  (close fp)
  (setq fp nil)
  (vl-file-delete tempFile)

  (setq globalSteelList (mapcar '(lambda (pt) (list (car pt) (cadr pt) (caddr pt))) globalSteelList))
  (setq maxDemBot 0.0 maxDemTop 0.0)
  (foreach pt globalSteelList
    (setq maxDemBot (max maxDemBot (caddr pt)))
    (setq maxDemTop (max maxDemTop (cadr pt)))
  )

  ;; ----------------------------------------------------------------------------
  ;; 10) Bastones (mantener lógica V13, pero Ld variable 12*ϕ)
  ;; ----------------------------------------------------------------------------
  (setq bastonesInfList '())
  (setq bastonesSupList '())

  ;; Helper: dibujar bastones por tramo (baseArea puede variar por tramo)
  (defun _steel-sublist-in-range (lst x1_m x2_m / out)
    (setq out '())
    (foreach p lst
      (if (and (numberp (car p)) (>= (car p) x1_m) (<= (car p) x2_m))
        (setq out (append out (list p)))
      )
    )
    out
  )

  (defun _baston-draw-zone (x1_d x2_d yBase isTop / yB1 yB2)
    (if isTop
      (progn
        (command "_.PLINE" (list x1_d (- yBase 0.45)) (list x1_d (- yBase 0.15)) (list x2_d (- yBase 0.15)) (list x2_d (- yBase 0.45)) "")
      )
      (progn
        (command "_.PLINE" (list x1_d (+ yBase 0.45)) (list x1_d (+ yBase 0.15)) (list x2_d (+ yBase 0.15)) (list x2_d (+ yBase 0.45)) "")
      )
    )
    (command "_.CHPROP" (entlast) "" "Color" 5 "")
    (command "_.FILLET" "P" (entlast))
  )

  ;; Calcular base area por cara (plana constante; escalonada puede variar por key)
  (setq baseBot 0.0 baseTop 0.0)
  (if planoIsBottom
    (setq baseBot asPlan)
    (setq baseTop asPlan)
  )

  ;; Para cara escalonada:
  ;;  - si hay 2 secciones y acero extra peraltada existe, base en tramos peraltados = asWide + asExtra, en tramos anchos = asWide
  ;;  - si no hay 2 secciones => 0 (se ignora)
  ;; Nota: En términos de STAAD, la demanda "Top" corresponde a cara superior geométrica, y "Bottom" a inferior.
  ;; Mapeo: si escalonada es Top => afecta baseTop por tramo; si escalonada es Bottom => baseBot por tramo.

  ;; ---- Bastones INFERIOR (STAAD Bottom) ----
  (princ "\n--- BASTONES (cara inferior / Bottom STAAD) ---")
  (setq isTopFace nil)

  ;; recorrer por tramos para poder usar base variable si aplica
  (foreach tramo tramosOrdenados
    (setq t_x1m (nth 1 tramo))
    (setq t_x2m (nth 2 tramo))
    (setq t_key (nth 6 tramo))

    ;; base para esta cara en este tramo
    (cond
      (planoIsBottom
        (setq baseAreaThisFace asPlan)
        (setq yBaseFace ySteelPlan)
      )
      (t
        ;; cara inferior es escalonada
        (if (= (length uniqueKeys) 2)
          (progn
            (if (= t_key keyDeep)
              (setq baseAreaThisFace (+ asWide asDeepExtra))
              (setq baseAreaThisFace asWide)
            )
            ;; y base de cara inferior escalonada: depende del contorno real del tramo; acero ancho es fijo y puede no coincidir
            ;; para bastones se usa y del acero ancho fijo si existe; si no, y del contorno
            (cond
              ((and (= t_key keyDeep) ySteelDeep) (setq yBaseFace ySteelDeep))
              (ySteelWide (setq yBaseFace ySteelWide))
              (t (setq yBaseFace (+ (_tramo-yBot tramo) 0.15)))
            )
          )
          (progn
            (setq baseAreaThisFace 0.0)
            (setq yBaseFace (+ (_tramo-yBot tramo) 0.15))
          )
        )
      )
    )

    (setq subSteel (_steel-sublist-in-range globalSteelList t_x1m t_x2m))

    (if (and subSteel (> (apply 'max (mapcar '(lambda (p) (caddr p)) subSteel)) baseAreaThisFace) (> baseAreaThisFace 0.0))
      (progn
        (setq list_zonas (procesar-zonas-v27 subSteel baseAreaThisFace (car p1) nil))
        (foreach zona list_zonas
          (setq zoneStart (car zona))
          (setq zoneEnd   (cadr zona))
          (setq maxReq    (caddr zona))
          (if (> maxReq baseAreaThisFace)
            (progn
              
(setq deficit (- maxReq baseAreaThisFace))
(princ (strcat "\nBaston Inf Req: " (rtos deficit 2 2) " cm2."))
(if (not (_ask-small-deficit deficit))
  (setq deficit nil)
)
(if (and deficit (> deficit 0.0001))
  (progn
    (setq strVarBast (getstring "\nCalibre Baston (Inf): "))
              (setq areaBastOne (obtener-area-varilla strVarBast))
              (if (<= areaBastOne 0.0) (setq areaBastOne 0.71)) ;; fallback #3
              (setq qtyBastRec (fix (+ (/ deficit areaBastOne) 0.9999)))
              (princ (strcat " Sugerido: " (itoa qtyBastRec)))
              (setq numBast (getint (strcat "\nCantidad definitiva <" (itoa qtyBastRec) ">: ")))
              (if (null numBast) (setq numBast qtyBastRec))

              ;; Ld = 12*phi_cm
              (setq phi_cm (obtener-diametro-real strVarBast))
              (setq Ld_m (/ (* 12.0 phi_cm) 100.0))

              (setq pts_fix (calc-puntos-vFINAL zoneStart zoneEnd axisXList Ld_m))
              (setq x_baston_ini (car pts_fix))
              (setq x_baston_fin (cadr pts_fix))

              ;; limitar a tramo y a global
              (setq xMinTr (+ xDrawStart t_x1m 0.15))
              (setq xMaxTr (+ xDrawStart t_x2m -0.15))
              (if (< x_baston_ini xMinTr) (setq x_baston_ini xMinTr))
              (if (> x_baston_fin xMaxTr) (setq x_baston_fin xMaxTr))
              (if (< x_baston_ini x_ini) (setq x_baston_ini x_ini))
              (if (> x_baston_fin x_fin) (setq x_baston_fin x_fin))

              (_baston-draw-zone x_baston_ini x_baston_fin yBaseFace nil)

              ;; cotas a ejes (mantener)
              
(setq _axes (_resolve-baston-axes x_baston_ini x_baston_fin axisXList)
      axis_near_start (car _axes)
      axis_near_end (cadr _axes)
)
(if axis_near_start
                (command "_.DIMLINEAR" (list axis_near_start yOrigin) (list x_baston_ini yOrigin) (list (/ (+ axis_near_start x_baston_ini) 2.0) (- yOrigin 0.4)))
              )
(if axis_near_end
                (command "_.DIMLINEAR" (list x_baston_fin yOrigin) (list axis_near_end yOrigin) (list (/ (+ x_baston_fin axis_near_end) 2.0) (- yOrigin 0.4)))
              )

              (dibujar-leader-blindado (list (/ (+ x_baston_ini x_baston_fin) 2.0) (+ yBaseFace 0.15)) (strcat (itoa numBast) "#" strVarBast) nil nil)
              (setq bastonesInfList (append bastonesInfList (list (list strVarBast numBast))))
                )
              )

            )
          )
        )
      )
    )
  )

  ;; ---- Bastones SUPERIOR (STAAD Top) ----
  (princ "\n--- BASTONES (cara superior / Top STAAD) ---")

  (foreach tramo tramosOrdenados
    (setq t_x1m (nth 1 tramo))
    (setq t_x2m (nth 2 tramo))
    (setq t_key (nth 6 tramo))

    (cond
      ((not planoIsBottom)
        (setq baseAreaThisFace asPlan)
        (setq yBaseFace ySteelPlan)
      )
      (t
        ;; cara superior es escalonada
        (if (= (length uniqueKeys) 2)
          (progn
            (if (= t_key keyDeep)
              (setq baseAreaThisFace (+ asWide asDeepExtra))
              (setq baseAreaThisFace asWide)
            )
            (if ySteelWide
              (setq yBaseFace ySteelWide)
              (setq yBaseFace (- (_tramo-yTop tramo) 0.15))
            )
          )
          (progn
            (setq baseAreaThisFace 0.0)
            (setq yBaseFace (- (_tramo-yTop tramo) 0.15))
          )
        )
      )
    )

    (setq subSteel (_steel-sublist-in-range globalSteelList t_x1m t_x2m))

    (if (and subSteel (> (apply 'max (mapcar '(lambda (p) (cadr p)) subSteel)) baseAreaThisFace) (> baseAreaThisFace 0.0))
      (progn
        (setq list_zonas (procesar-zonas-v27 subSteel baseAreaThisFace (car p1) T))
        (foreach zona list_zonas
          (setq zoneStart (car zona))
          (setq zoneEnd   (cadr zona))
          (setq maxReq    (caddr zona))
          (if (> maxReq baseAreaThisFace)
            (progn
              
(setq deficit (- maxReq baseAreaThisFace))
(princ (strcat "\nBaston Sup Req: " (rtos deficit 2 2) " cm2."))
(if (not (_ask-small-deficit deficit))
  (setq deficit nil)
)
(if (and deficit (> deficit 0.0001))
  (progn
    (setq strVarBast (getstring "\nCalibre Baston (Sup): "))
              (setq areaBastOne (obtener-area-varilla strVarBast))
              (if (<= areaBastOne 0.0) (setq areaBastOne 0.71))
              (setq qtyBastRec (fix (+ (/ deficit areaBastOne) 0.9999)))
              (princ (strcat " Sugerido: " (itoa qtyBastRec)))
              (setq numBast (getint (strcat "\nCantidad definitiva <" (itoa qtyBastRec) ">: ")))
              (if (null numBast) (setq numBast qtyBastRec))

              (setq phi_cm (obtener-diametro-real strVarBast))
              (setq Ld_m (/ (* 12.0 phi_cm) 100.0))

              (setq pts_fix (calc-puntos-vFINAL zoneStart zoneEnd axisXList Ld_m))
              (setq x_baston_ini (car pts_fix))
              (setq x_baston_fin (cadr pts_fix))

              (setq xMinTr (+ xDrawStart t_x1m 0.15))
              (setq xMaxTr (+ xDrawStart t_x2m -0.15))
              (if (< x_baston_ini xMinTr) (setq x_baston_ini xMinTr))
              (if (> x_baston_fin xMaxTr) (setq x_baston_fin xMaxTr))
              (if (< x_baston_ini x_ini) (setq x_baston_ini x_ini))
              (if (> x_baston_fin x_fin) (setq x_baston_fin x_fin))

              (_baston-draw-zone x_baston_ini x_baston_fin yBaseFace T)

              
(setq _axes (_resolve-baston-axes x_baston_ini x_baston_fin axisXList)
      axis_near_start (car _axes)
      axis_near_end (cadr _axes)
)
(if axis_near_start
                (command "_.DIMLINEAR" (list axis_near_start yOrigin) (list x_baston_ini yOrigin) (list (/ (+ axis_near_start x_baston_ini) 2.0) (+ yMax_global 0.4)))
              )
(if axis_near_end
                (command "_.DIMLINEAR" (list x_baston_fin yOrigin) (list axis_near_end yOrigin) (list (/ (+ x_baston_fin axis_near_end) 2.0) (+ yMax_global 0.4)))
              )

              (dibujar-leader-blindado (list (/ (+ x_baston_ini x_baston_fin) 2.0) (- yBaseFace 0.15)) (strcat (itoa numBast) "#" strVarBast) T nil)
              (setq bastonesSupList (append bastonesSupList (list (list strVarBast numBast))))
                )
              )

            )
          )
        )
      )
    )
  )

  ;; ----------------------------------------------------------------------------
  ;; 11) Estribos y contraflecha (idéntico a V13, solo ajuste Y texto si necesario)
  ;; ----------------------------------------------------------------------------
  (princ "\n--- ESTRIBOS ---")
  ;; usar hMax_cm para d_eff como conservador
  (setq d_eff (- hMax_cm 3.0))
  (setq limitEstribos (max 20.0 (/ d_eff 2.0)))

  ;; Si cara escalonada está abajo (alineación superior), texto abajo puede invadir; usar yMin_global
  (setq yTextEstribos (- yMin_global 1.6))

  (command "_.TEXT" "_J" "_ML" (list xOrigin yTextEstribos) 0.25 0 "E#3")
  (command "_.CHPROP" (entlast) "" "Color" 3 "")

  (setq numZonesEstribos (getint "\nNumero de zonas de estribos (1 para uniforme): "))
  (if (null numZonesEstribos) (setq numZonesEstribos 1))

  (setq currentDist 0.0)
  (setq i 1)
  (repeat numZonesEstribos
     (if (= numZonesEstribos 1)
       (setq lenZoneEst l_total_m)
       (setq lenZoneEst (getreal (strcat "\nLongitud Zona " (itoa i) " (m): ")))
     )
     (setq s_estribo (getreal (strcat "Separacion Zona " (itoa i) " (cm): ")))
     (if (> s_estribo limitEstribos)
       (princ (strcat "\n** AVISO: Sep " (rtos s_estribo 2 0) " > Limite " (rtos limitEstribos 2 0) " **"))
     )
     (setq qtyEstribos (+ (fix (+ (/ (* lenZoneEst 100.0) s_estribo) 0.99)) 1))
     (setq txtEstribo (strcat (itoa qtyEstribos) "@" (rtos s_estribo 2 0)))
     (setq pTextEst (list (+ (car p1) 0.5 currentDist (/ lenZoneEst 2.0)) yTextEstribos))
     (command "_.TEXT" "_J" "_MC" pTextEst 0.15 0 txtEstribo)
     (command "_.CHPROP" (entlast) "" "Color" 8 "")
     (setq currentDist (+ currentDist lenZoneEst))
     (setq i (1+ i))
  )

  ;; Contraflecha (igual V13; Y relativo al bottom global)
  (initget "Si No")
  (setq addCamber (getkword "\nAgregar contraflecha? [Si/No] <No>: "))
  (if (= addCamber "Si")
     (progn
        (setq numCamber (getint "\nCuantas contraflechas?: "))
        (repeat numCamber
           (initget "Centro Distancia")
           (setq locCamber (getkword "\nUbicacion? [Centro de Claro / Distancia]: "))
           (if (= locCamber "Centro")
              (progn
                (setq i (getint "Numero de claro (1, 2...): "))
                (setq xCamber (/ (+ (nth (1- i) axisXList) (nth i axisXList)) 2.0))
              )
              (setq xCamber (+ (car p1) 0.5 (getreal "Distancia desde origen (m): ")))
           )
           (setq strCamber (getstring "\nValor Contraflecha (ej. 1.5): "))
           (setq yCamber (- yMin_global 0.65))

           (setq deltaW 0.16)
           (setq gap 0.05)
           (setq textW (* (strlen (strcat "=" strCamber "cm")) 0.12))
           (setq totalW (+ deltaW gap textW))
           (setq xDelta (- xCamber (/ totalW 2.0)))

           (command "_.PLINE" (list xDelta yCamber) (list (- xDelta 0.08) (- yCamber 0.15)) (list (+ xDelta 0.08) (- yCamber 0.15)) "C")
           (command "_.CHPROP" (entlast) "" "Color" 40 "")
           (command "_.TEXT" "_J" "_ML" (list (+ xDelta 0.08 gap) (- yCamber 0.075)) 0.15 0 (strcat "=" strCamber "cm"))
           (command "_.CHPROP" (entlast) "" "Color" 40 "")
        )
     )
  )

  ;; ----------------------------------------------------------------------------
  ;; 12) Sección transversal (H) — máx 2 secciones
  ;; ----------------------------------------------------------------------------
  (princ "\n--- SECCION TRANSVERSAL ---")
  (setq ptSectionOrigin (list (+ (car p2) 1.5) yOrigin))
  (setq xSec (car ptSectionOrigin))
  (setq ySec (cadr ptSectionOrigin))

  ;; Derivar bG/bS y hG/hS si hay 2 secciones (si 1, todo es rectangular)
  (setq bG_cm bMax_cm)
  (setq bS_cm bMin_cm)
  (setq hG_cm hMax_cm)
  (setq hS_cm hMin_cm)

  (setq bG_draw (* (/ bG_cm 100.0) 7.0))
  (setq bS_draw (* (/ bS_cm 100.0) 7.0))
  (setq hG_draw (* (/ hG_cm 100.0) 7.0))
  (setq hS_draw (* (/ hS_cm 100.0) 7.0))

  ;; A-A (siempre): sección "ancha" (rectangular)
  (command "_.TEXT" "_J" "_ML" (list xSec (+ ySec hG_draw 0.6)) 0.18 0 "A-A")
  (command "_.CHPROP" (entlast) "" "Color" 4 "")

  (setq b_draw (* (/ bMax_cm 100.0) 7.0))
  (setq h_draw_sec (* (/ hMin_cm 100.0) 7.0))

  (command "_.RECTANG" "_F" 0 (list xSec ySec) (list (+ xSec bG_draw) (+ ySec hS_draw)))
  (command "_.CHPROP" (entlast) "" "Color" 7 "")

  ;; Si hay 2 secciones, dibujar B-B peraltada compuesta a la derecha
  (if (= (length uniqueKeys) 2)
    (progn
      (setq xSecB (+ xSec bG_draw 1.2))
      (setq ySecB ySec)

      (command "_.TEXT" "_J" "_ML" (list xSecB (+ ySecB hG_draw 0.6)) 0.18 0 "B-B")
      (command "_.CHPROP" (entlast) "" "Color" 4 "")

      ;; Preguntar alineación del alma
      (initget "Izq Der Centro")
      (setq almaAlign (getkword "\nAlineacion del alma en seccion peraltada? [Izq/Der/Centro] <Centro>: "))
      (if (null almaAlign) (setq almaAlign "Centro"))

;; Recalcular dimensiones (por si alguna variable se ensucio antes)
(setq bG_draw (* (/ bG_cm 100.0) 7.0)
      bS_draw (* (/ bS_cm 100.0) 7.0)
      hG_draw (* (/ hG_cm 100.0) 7.0)
      hS_draw (* (/ hS_cm 100.0) 7.0)
)

      (cond
        ((= almaAlign "Izq") (setq almaX0 0.0))
        ((= almaAlign "Der") (setq almaX0 (- bG_draw bS_draw)))
        (t (setq almaX0 (/ (- bG_draw bS_draw) 2.0)))
      )

      (setq yBBMin ySecB)
      (setq yBBMax (+ ySecB hG_draw))
      (princ (strcat "\nB-B: alineacion=" alignChoice " | yMin=" (rtos yBBMin 2 3) " | yMax=" (rtos yBBMax 2 3)))

      (if isAlignInf
        (progn
          (setq bbBranch "escalon-arriba")
          (princ (strcat "\nB-B: rama=" bbBranch))
          ;; Dibujo concreto compuesto (T invertida): base (bG x hS) + alma (bS x (hG-hS))
          (command "_.PLINE"
                   (list xSecB ySecB)
                   (list (+ xSecB bG_draw) ySecB)
                   (list (+ xSecB bG_draw) (+ ySecB hS_draw))
                   (list (+ xSecB almaX0 bS_draw) (+ ySecB hS_draw))
                   (list (+ xSecB almaX0 bS_draw) (+ ySecB hG_draw))
                   (list (+ xSecB almaX0) (+ ySecB hG_draw))
                   (list (+ xSecB almaX0) (+ ySecB hS_draw))
                   (list xSecB (+ ySecB hS_draw))
                   "_C")
        )
        (progn
          (setq bbBranch "escalon-abajo")
          (princ (strcat "\nB-B: rama=" bbBranch))
          (setq yBBStep (+ ySecB (- hG_draw hS_draw)))
          ;; Dibujo concreto compuesto (T normal): base (bG x hS) arriba + alma hacia abajo
          (command "_.PLINE"
                   (list xSecB (+ ySecB hG_draw))
                   (list (+ xSecB bG_draw) (+ ySecB hG_draw))
                   (list (+ xSecB bG_draw) yBBStep)
                   (list (+ xSecB almaX0 bS_draw) yBBStep)
                   (list (+ xSecB almaX0 bS_draw) ySecB)
                   (list (+ xSecB almaX0) ySecB)
                   (list (+ xSecB almaX0) yBBStep)
                   (list xSecB yBBStep)
                   "_C")
        )
      )
      (command "_.CHPROP" (entlast) "" "Color" 7 "")
    )
  )

  ;; (Losa y estribo/ganchos se mantienen SOLO sobre A-A para no romper lógica visual;
  ;;  en B-B el usuario puede revisar contorno. Esto mantiene robustez.)

  ;; Losa (igual V13 sobre A-A)
  (initget "Si No")
  (setq hasLosa (getkword "\nLleva Losa? [Si/No] <No>: "))
  (if (= hasLosa "Si")
    (progn
      (initget "Izq Der Ambos")
      (setq typeLosa (getkword "Lado? [Izq/Der/Ambos]: "))
      (setq capaCompresion (* (/ (getreal "Capa Compresion (cm): ") 100.0) 7.0))
      (setq h_losa (* (/ (getreal "Altura Total Losa (cm): ") 100.0) 7.0))
      (setq alignLosa "Arriba")
      (if (not (equal hS_draw h_losa 0.01))
        (progn
          (initget "Arriba Abajo Centro")
          (setq alignLosa (getkword "Alineacion Losa? [Arriba/Abajo/Centro]: "))
        )
      )

      (setq yLosaStart (+ ySec hS_draw))
      (if (= alignLosa "Abajo") (setq yLosaStart (+ ySec h_losa)))
      (if (= alignLosa "Centro") (setq yLosaStart (+ ySec (/ hS_draw 2.0) (/ h_losa 2.0))))

      (defun draw-break-line (xStart yTop hLosa hCapa isLeft / dir xEnd yCenter ptsHatch)
         (setq dir (if isLeft -1.0 1.0))
         (setq xEnd (+ xStart (* 0.5 dir)))
         (command "_.RECTANG" (list xStart yTop) (list xEnd (- yTop hCapa)))
         (command "_.CHPROP" (entlast) "" "Color" 7 "")
         (setq yCenter (- yTop (/ hLosa 2.0)))
         (setq ptsHatch (list (list xStart (- yTop hCapa)) (list xEnd (- yTop hCapa))
               (list xEnd (+ yCenter 0.075)) (list (- xEnd (* 0.2 dir)) (+ yCenter 0.075))
               (list (- xEnd (* 0.2 dir)) yCenter) (list (+ xEnd (* 0.2 dir)) yCenter)
               (list (+ xEnd (* 0.2 dir)) (- yCenter 0.075)) (list xEnd (- yCenter 0.075))
               (list xEnd (- yTop hLosa)) (list xStart (- yTop hLosa))))
         (command "_.PLINE") (foreach p ptsHatch (command p)) (command "_C")
         (command "_.CHPROP" (entlast) "" "Color" 8 "")
         (command "_.HATCH" "ANSI31" 0.035 0 (entlast) "")
         (command "_.CHPROP" (entlast) "" "Color" 8 "")
         (command "_.PLINE"
                  (list xEnd (+ yTop 0.2)) (list xEnd (+ yCenter 0.075)) (list (- xEnd (* 0.2 dir)) (+ yCenter 0.075))
                  (list (- xEnd (* 0.2 dir)) yCenter) (list (+ xEnd (* 0.2 dir)) yCenter) (list (+ xEnd (* 0.2 dir)) (- yCenter 0.075))
                  (list xEnd (- yCenter 0.075)) (list xEnd (- yTop hLosa 0.2)) "")
         (command "_.CHPROP" (entlast) "" "Color" 7 "")
      )

      (if (or (= typeLosa "Izq") (= typeLosa "Ambos")) (draw-break-line xSec yLosaStart h_losa capaCompresion T))
      (if (or (= typeLosa "Der") (= typeLosa "Ambos")) (draw-break-line (+ xSec bG_draw) yLosaStart h_losa capaCompresion nil))
    )
  )

  ;; Estribo (igual V13 sobre A-A)
  (setq radFillet 0.025)
  (setq rec_draw 0.175)
  (setq p1_st (list (+ xSec rec_draw) (+ ySec rec_draw)))
  (setq p2_st (list (- (+ xSec bG_draw) rec_draw) (- (+ ySec hS_draw) rec_draw)))
  (command "_.RECTANG" "_F" radFillet p1_st p2_st)
  (command "_.CHPROP" (entlast) "" "Color" 3 "")
  (command "_.RECTANG" "_F" 0 (list 0 0) (list 0 0))
  (command "_.ERASE" (entlast) "")

  ;; Ganchos 135 (igual V13, basado en varilla de cara plana como referencia visual)
  (setq rBarTop (/ (* (/ (obtener-diametro-real varPlan) 100.0) 7.0) 2.0))
  (setq xStirrupLeft (car p1_st))
  (setq yStirrupTop (cadr p2_st))
  (setq offsetHookFactor (* rBarTop 1.1))
  (setq centerBarX (+ xStirrupLeft rBarTop))
  (setq pH1 (list (+ centerBarX offsetHookFactor) yStirrupTop))
  (command "_.LINE" pH1 (polar pH1 (* 315.0 (/ pi 180.0)) 0.30) "")
  (command "_.CHPROP" (entlast) "" "Color" 3 "")
  (setq centerBarY (- yStirrupTop rBarTop))
  (setq pH2 (list xStirrupLeft (- centerBarY offsetHookFactor)))
  (command "_.LINE" pH2 (polar pH2 (* 315.0 (/ pi 180.0)) 0.30) "")
  (command "_.CHPROP" (entlast) "" "Color" 3 "")

  ;; ----------------------------------------------------------------------------
  ;; 13) Mostrar acero en sección (mantener lógica V13 simplificada)
  ;; ----------------------------------------------------------------------------
  (defun ocmema--num-str (v) (if (numberp v) (rtos v 2 3) "nil"))
  (defun ocmema--int-str (v) (if (numberp v) (itoa v) "nil"))
  (defun ocmema--list-empty (lst) (if (and lst (> (length lst) 0)) "no" "si"))
  (defun get-critical-bast (bastList / critVar critNum maxD d)
     (setq critVar nil critNum 0 maxD 0.0)
     (foreach b bastList
        (setq d (obtener-diametro-real (car b)))
        (if (> d maxD)
          (progn (setq maxD d) (setq critVar (car b)) (setq critNum (cadr b)))
          (if (and (= d maxD) (> (cadr b) critNum)) (setq critNum (cadr b)))
        )
     )
     (list critVar critNum)
  )

  (setq critBastInf (get-critical-bast bastonesInfList))
  (setq critBastSup (get-critical-bast bastonesSupList))

  (defun process-layer-steel (baseNum baseVar bastNum bastVar isTop / diamBase diamBast yStart dir layoutMode w gap i cx cy)
     (setq diamBase (* (/ (obtener-diametro-real baseVar) 100.0) 7.0))
     (setq diamBast 0.0)
     (if bastVar (setq diamBast (* (/ (obtener-diametro-real bastVar) 100.0) 7.0)))
     (setq yStart (if isTop (- (cadr p2_st) (/ diamBase 2.0)) (+ (cadr p1_st) (/ diamBase 2.0))))
     (setq dir (if isTop -1.0 1.0))
     (initget "Orillas Largo")
     (setq layoutMode (getkword (strcat "\nAcomodo " (if isTop "SUP" "INF") " (Base:" (itoa baseNum) " Bast:" (itoa bastNum) ")? [Orillas/Largo]: ")))
     (if (null layoutMode) (setq layoutMode "Orillas"))

     (if (= layoutMode "Largo")
       (progn
         (setq w (- (car p2_st) (car p1_st) diamBase))
         (setq gap (if (> (+ baseNum bastNum) 1) (/ w (1- (+ baseNum bastNum))) 0.0))
         (setq i 0)
         (repeat (+ baseNum bastNum)
           (setq cx (+ (car p1_st) (/ diamBase 2.0) (* i gap)))
           (command "_.DONUT" 0.0 diamBase (list cx yStart) "")
           (command "_.CHPROP" (entlast) "" "Color" 5 "")
           (setq i (1+ i))
         )
       )
       (progn
         ;; Orillas (simple)
         (setq i 0)
         (repeat baseNum
           (setq cx (if (= (rem i 2) 0) (+ (car p1_st) (/ diamBase 2.0)) (- (car p2_st) (/ diamBase 2.0))))
           (command "_.DONUT" 0.0 diamBase (list cx yStart) "")
           (command "_.CHPROP" (entlast) "" "Color" 5 "")
           (setq i (1+ i))
         )
         ;; Bastones se dibujan como donuts adicionales (aprox)
         (if (and bastVar (> bastNum 0))
           (progn
             (setq i 0)
             (repeat bastNum
               (setq cx (if (= (rem i 2) 0) (+ (car p1_st) (/ diamBast 2.0)) (- (car p2_st) (/ diamBast 2.0))))
               (setq cy (+ yStart (* (+ 1 (fix (/ i 2))) diamBast dir)))
               (command "_.DONUT" 0.0 diamBast (list cx cy) "")
               (command "_.CHPROP" (entlast) "" "Color" 5 "")
               (setq i (1+ i))
             )
           )
         )
       )
     )
  )

  ;; En sección A-A: se muestra acero plano + bastón crítico (aprox)
  (princ
    (strcat
      "\nOCMEMA: A-A acero start | rutina=process-layer-steel"
      " | barras_base=" (ocmema--int-str numPlan)
      " | baston_inf=" (ocmema--int-str (cadr critBastInf))
      " | baston_sup=" (ocmema--int-str (cadr critBastSup))
      " | bG=" (ocmema--num-str bG_draw)
      " | hG=" (ocmema--num-str hG_draw)
      " | bS=" (ocmema--num-str bS_draw)
      " | hS=" (ocmema--num-str hS_draw)
      " | almaX0=" (ocmema--num-str almaX0)
      " | bastonesInfVacia=" (ocmema--list-empty bastonesInfList)
      " | bastonesSupVacia=" (ocmema--list-empty bastonesSupList)
    )
  )
  (process-layer-steel numPlan varPlan (cadr critBastInf) (car critBastInf) nil)
  (process-layer-steel numPlan varPlan (cadr critBastSup) (car critBastSup) T)
  (princ "\nOCMEMA: A-A acero end | rutina=process-layer-steel")

  ;; B-B: no se invoca armado en esta version; solo contorno.
  (princ
    (strcat
      "\nOCMEMA: B-B acero start | rutina=none (solo contorno)"
      " | barras_base=" (ocmema--int-str numPlan)
      " | baston_inf=" (ocmema--int-str (cadr critBastInf))
      " | baston_sup=" (ocmema--int-str (cadr critBastSup))
      " | bG=" (ocmema--num-str bG_draw)
      " | hG=" (ocmema--num-str hG_draw)
      " | bS=" (ocmema--num-str bS_draw)
      " | hS=" (ocmema--num-str hS_draw)
      " | almaX0=" (ocmema--num-str almaX0)
      " | bastonesInfVacia=" (ocmema--list-empty bastonesInfList)
      " | bastonesSupVacia=" (ocmema--list-empty bastonesSupList)
    )
  )
  (princ "\nOCMEMA: B-B acero end | rutina=none (skipped)")

  ;; ----------------------------------------------------------------------------
  ;; 14) Restore vars
  ;; ----------------------------------------------------------------------------
  (setvar "DIMASZ" old_dimasz)
  (setvar "DIMCLRD" old_dimclrd)
  (setvar "DIMTXT" old_dimtxt)
  (setvar "DIMCLRT" old_dimclrt)
  (setvar "DIMSCALE" old_dimscale)
  (setvar "OSMODE" oldOsnap)

  (princ "\nDibujo de Trabe (V_FINAL) completado.")
  (princ)
)
