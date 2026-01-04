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
                                areaWide areaDeep b_per_cm h_per_cm bDeep hDeep bWide hWide
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
                                bastonesWideInfList bastonesWideSupList
                                bastonesStepInfList bastonesStepSupList
                                baseAreaThisFace

                                ;; Estribos / contraflecha
                                d_eff limitEstribos yTextEstribos numZonesEstribos lenZoneEst s_estribo qtyEstribos txtEstribo pTextEst currentDist
                                addCamber numCamber locCamber xCamber yCamber strCamber deltaW gap textW totalW xDelta

                                ;; Sección transversal
                                ptSectionOrigin xSec ySec ySecA ySecB
                                b_draw h_draw_sec
                                bG_cm bS_cm hG_cm hS_cm
                                bG_draw bS_draw hG_draw hS_draw
                                almaAlign almaX0
                                rec_draw radFillet
                                p1_st p2_st p1_st_BB p2_st_BB
                                p1_st_BB_main p2_st_BB_main p1_st_BB_step p2_st_BB_step
                                draw-break-line
                                hasLosa typeLosa typeLosaAA typeLosaBB capaCompresion h_losa alignLosa
                                losaExt sep_base sep_extra hasAnyLosa yLosaStartAA yLosaStartBB
                                yTopBB yBotBB yWide0_A yWide1_A yWide0_B yWide1_B
                                ySlab0 ySlab1 ySlab0B ySlab1B ySlabMid yTopA yTopB yTopGlobal yTextAA yTextBB
                                bbCountBase bbCountInf bbCountSup
                                yBBMin yBBMax yBBStep bbBranch
                                bb-use-anchor bb-align
                                bb-x-left-wide bb-x-right-wide
                                bb-x-inner-left-step bb-x-inner-right-step
                                bb-x-anchor bb-x-anchor-l bb-x-anchor-r
                                stepIsTop numPlan_step varPlan_step
                                baseNum_wide_inf baseVar_wide_inf bastNum_wide_inf bastVar_wide_inf
                                baseNum_wide_sup baseVar_wide_sup bastNum_wide_sup bastVar_wide_sup
                                baseNum_step baseVar_step bastNum_step bastVar_step

                                ;; Ganchos
                                rBarTop xStirrupLeft yStirrupTop yStirrupBot offsetHookFactor centerBarX centerBarY pH1 pH2

                                ;; Bastón crítico / steel layout
                                critBastInf critBastSup
                                critBastWideInf critBastWideSup
                                critBastStepInf critBastStepSup
                                process-layer-steel get-critical-bast

                                ;; Restore vars
                                oldOsnap old_dimasz old_dimclrd old_dimtxt old_dimclrt old_dimscale
                               )

  (vl-load-com)
  (setq ocmema--default-project-dir "C:\\Users\\edgar\\OneDrive - ITESO\\OCMEMA_IE\\01. PROYECTOS\\")

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

  (defun ocmema--num-safe (v) (if (numberp v) v 0.0))
  (defun ocmema--rtos-safe (v) (rtos (ocmema--num-safe v) 2 3))
  

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
  (setq file (getfiled "Seleccionar archivo TRABE (ANL)"
                        (if (vl-file-directory-p ocmema--default-project-dir) ocmema--default-project-dir "")
                        "ANL;TXT;OUT" 4))
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
        (progn
          (setq keyDeep keyA) (setq keyWide keyB)
          (setq bDeep bA hDeep hA)
          (setq bWide bB hWide hB)
        )
        (progn
          (setq keyDeep keyB) (setq keyWide keyA)
          (setq bDeep bB hDeep hB)
          (setq bWide bA hWide hA)
        )
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

      ;; Acero peraltada (D3.2): siempre se solicita en zona peraltada
      (setq b_per_cm (ocmema--num-safe bDeep))
      (setq h_per_cm (- (ocmema--num-safe hDeep) 2.5))
      (if (< h_per_cm 0.0) (setq h_per_cm 0.0))
      (if (or (<= b_per_cm 0.0) (<= h_per_cm 0.0) (null fc_val) (null fy_val))
        (progn
          (princ "\nERROR: Variables peraltadas invalidas")
          (setq asMinDeep 0.0)
        )
        (setq asMinDeep (* (/ (* 0.8 (sqrt fc_val)) fy_val) b_per_cm h_per_cm))
      )
      (princ
        (strcat
          "\nDBG: peraltada | fc=" (ocmema--rtos-safe fc_val)
          " fy=" (ocmema--rtos-safe fy_val)
          " b_per=" (ocmema--rtos-safe b_per_cm)
          " h_per=" (ocmema--rtos-safe h_per_cm)
          " As_min_per=" (ocmema--rtos-safe asMinDeep)
        )
      )

  (princ (strcat "\n*** AsMin Ancha=" (rtos asMinWide 2 3) " | AsMin Peraltada=" (rtos asMinDeep 2 3) " | Extra Peraltada=" (rtos asDeepExtra 2 3) " cm2 ***"))


      (princ "\n--- ACERO CARA ESCALONADA: Seccion PERALTADA (solo en tramos peraltados) ---")
      (setq varDeep (getstring "\nNumero varilla (Seccion PERALTADA): "))
      (setq areaDeep (obtener-area-varilla varDeep))
      (if (<= areaDeep 0.0) (progn (alert "Abort: Varilla inválida (peraltada).") (exit)))

      (setq qtyDeepRec (fix (+ (/ asMinDeep areaDeep) 0.9999)))
      (if (< qtyDeepRec 2) (setq qtyDeepRec 2))
      (princ
        (strcat
          "\nOCMEMA: Peraltada AsMin | fc=" (ocmema--rtos-safe fc_val)
          " fy=" (ocmema--rtos-safe fy_val)
          " b_per=" (ocmema--rtos-safe b_per_cm)
          " h_per=" (ocmema--rtos-safe h_per_cm)
          " As_min_per=" (ocmema--rtos-safe asMinDeep)
          " As_prov=" (ocmema--rtos-safe (* qtyDeepRec areaDeep))
          " n_barras_sugeridas=" (itoa qtyDeepRec)
        )
      )
      (princ (strcat "\n-> Sugerido: " (itoa qtyDeepRec) " varillas."))
      (setq numDeep (getint (strcat "\nCantidad definitiva <" (itoa qtyDeepRec) ">: ")))
      (if (null numDeep) (setq numDeep qtyDeepRec))
      (if (< numDeep 1) (setq numDeep 1))

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
  (setq bastonesWideInfList '())
  (setq bastonesWideSupList '())
  (setq bastonesStepInfList '())
  (setq bastonesStepSupList '())

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
              (if planoIsBottom
                (setq bastonesWideInfList (append bastonesWideInfList (list (list strVarBast numBast))))
                (if (and (= (length uniqueKeys) 2) (= t_key keyDeep))
                  (setq bastonesStepInfList (append bastonesStepInfList (list (list strVarBast numBast))))
                  (setq bastonesWideInfList (append bastonesWideInfList (list (list strVarBast numBast))))
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
              (if (not planoIsBottom)
                (setq bastonesWideSupList (append bastonesWideSupList (list (list strVarBast numBast))))
                (if (and (= (length uniqueKeys) 2) (= t_key keyDeep))
                  (setq bastonesStepSupList (append bastonesStepSupList (list (list strVarBast numBast))))
                  (setq bastonesWideSupList (append bastonesWideSupList (list (list strVarBast numBast))))
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

  ;; ----------------------------------------------------------------------------
  ;; 11) Estribos y contraflecha (idéntico a V13, solo ajuste Y texto si necesario)
  ;; ----------------------------------------------------------------------------
  (princ "\n--- ESTRIBOS ---")
  ;; usar hMax_cm para d_eff como conservador
  (setq d_eff (- hMax_cm 3.0))
  (setq limitEstribos (max 20.0 (/ d_eff 2.0)))

  ;; Si cara escalonada está abajo (alineación superior), texto abajo puede invadir; usar yMin_global
  (setq yTextEstribos (- yMin_global 1.6))

  ;; Ticket 11: sección variable usa 2 estribos -> prefijo "2" en texto
  (command "_.TEXT" "_J" "_ML" (list xOrigin yTextEstribos) 0.25 0 (if (= (length uniqueKeys) 2) "2E#3" "E#3"))
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

  (setq ySecA ySec)
  (if (= (length uniqueKeys) 2)
    (progn
      (setq ySecB ySec)
      (setq yTopBB (+ ySecB hG_draw))
      (setq yBotBB ySecB)
      (if isAlignInf
        (progn
          (setq bbBranch "escalon-arriba")
          (setq ySecA yBotBB)
        )
        (progn
          (setq bbBranch "escalon-abajo")
          (setq ySecA (- yTopBB hS_draw))
        )
      )
      (princ (strcat "\nA-A: ySecA=" (rtos ySecA 2 3) " | rama=" bbBranch))
    )
  )

  ;; A-A (siempre): sección "ancha" (rectangular)

  (setq b_draw (* (/ bMax_cm 100.0) 7.0))
  (setq h_draw_sec (* (/ hMin_cm 100.0) 7.0))

  (command "_.RECTANG" "_F" 0 (list xSec ySecA) (list (+ xSec bG_draw) (+ ySecA hS_draw)))
  (command "_.CHPROP" (entlast) "" "Color" 7 "")

  ;; Losa (preguntar por separado A-A y B-B)
  (setq losaExt 0.5)
  (initget "Ninguna Izq Der Ambos")
  (setq typeLosaAA (getkword "\nLosa A-A? [Ninguna/Izq/Der/Ambos] <Ninguna>: "))
  (if (null typeLosaAA) (setq typeLosaAA "Ninguna"))
  (setq typeLosaBB "Ninguna")
  (if (= (length uniqueKeys) 2)
    (progn
      (initget "Ninguna Izq Der Ambos")
      (setq typeLosaBB (getkword "\nLosa B-B? [Ninguna/Izq/Der/Ambos] <Ninguna>: "))
      (if (null typeLosaBB) (setq typeLosaBB "Ninguna"))
    )
  )
  (setq sep_base 1.2)
  (setq sep_extra 0.0)
  (if (or (= typeLosaAA "Der") (= typeLosaAA "Ambos")) (setq sep_extra (+ sep_extra losaExt)))
  (if (or (= typeLosaBB "Izq") (= typeLosaBB "Ambos")) (setq sep_extra (+ sep_extra losaExt)))
  (setq hasAnyLosa (or (not (= typeLosaAA "Ninguna")) (not (= typeLosaBB "Ninguna"))))
  (if hasAnyLosa
    (progn
      (setq capaCompresion (* (/ (getreal "Capa Compresion (cm): ") 100.0) 7.0))
      (setq h_losa (* (/ (getreal "Altura Total Losa (cm): ") 100.0) 7.0))
      (setq alignLosa "Arriba")
      (if (not (equal hS_draw h_losa 0.01))
        (progn
          (initget "Arriba Abajo Centro")
          (setq alignLosa (getkword "Alineacion Losa? [Arriba/Abajo/Centro]: "))
        )
      )
    )
  )

  ;; Si hay 2 secciones, dibujar B-B peraltada compuesta a la derecha
  (if (= (length uniqueKeys) 2)
    (progn
      (setq xSecB (+ xSec bG_draw sep_base sep_extra))

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

  (setq yWide0_A ySecA)
  (setq yWide1_A (+ ySecA hS_draw))
  (if (= (length uniqueKeys) 2)
    (progn
      (if isAlignInf
        (progn
          (setq yWide0_B ySecB)
          (setq yWide1_B (+ ySecB hS_draw))
        )
        (progn
          (setq yWide0_B (+ ySecB (- hG_draw hS_draw)))
          (setq yWide1_B (+ ySecB hG_draw))
        )
      )
    )
  )
  (setq offsetText 0.6)
  (setq xCenterA (+ xSec (/ bG_draw 2.0)))
  (if (= (length uniqueKeys) 2) (setq xCenterB (+ xSecB (/ bG_draw 2.0))))
  (setq yTopA yWide1_A)
  (setq yTopB yWide1_B)
  (princ
    (strcat
      "\nA-A/B-B: yTopA=" (rtos yTopA 2 3)
      " | yTopB=" (if (numberp yTopB) (rtos yTopB 2 3) "nil")
    )
  )

  ;; (Losa y estribo/ganchos se mantienen SOLO sobre A-A para no romper lógica visual;
  ;;  en B-B el usuario puede revisar contorno. Esto mantiene robustez.)

  ;; Losa (dibujar segun selecciones A-A y B-B)
  (if hasAnyLosa
    (progn
      (if (equal h_losa (- yWide1_A yWide0_A) 0.01)
        (progn
          (setq ySlab0 yWide0_A)
          (setq ySlab1 yWide1_A)
        )
        (progn
          (cond
            ((= alignLosa "Arriba")
              (setq ySlab1 yWide1_A)
              (setq ySlab0 (- yWide1_A h_losa))
            )
            ((= alignLosa "Abajo")
              (setq ySlab0 yWide0_A)
              (setq ySlab1 (+ yWide0_A h_losa))
            )
            (t
              (setq ySlabMid (/ (+ yWide0_A yWide1_A) 2.0))
              (setq ySlab0 (- ySlabMid (/ h_losa 2.0)))
              (setq ySlab1 (+ ySlabMid (/ h_losa 2.0)))
            )
          )
        )
      )
      (setq yLosaStartAA ySlab1)

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

      (if (or (= typeLosaAA "Izq") (= typeLosaAA "Ambos")) (draw-break-line xSec yLosaStartAA h_losa capaCompresion T))
      (if (or (= typeLosaAA "Der") (= typeLosaAA "Ambos")) (draw-break-line (+ xSec bG_draw) yLosaStartAA h_losa capaCompresion nil))

      (if (= (length uniqueKeys) 2)
        (progn
          (if (equal h_losa (- yWide1_B yWide0_B) 0.01)
            (progn
              (setq ySlab0B yWide0_B)
              (setq ySlab1B yWide1_B)
            )
            (progn
              (cond
                ((= alignLosa "Arriba")
                  (setq ySlab1B yWide1_B)
                  (setq ySlab0B (- yWide1_B h_losa))
                )
                ((= alignLosa "Abajo")
                  (setq ySlab0B yWide0_B)
                  (setq ySlab1B (+ yWide0_B h_losa))
                )
                (t
                  (setq ySlabMid (/ (+ yWide0_B yWide1_B) 2.0))
                  (setq ySlab0B (- ySlabMid (/ h_losa 2.0)))
                  (setq ySlab1B (+ ySlabMid (/ h_losa 2.0)))
                )
              )
            )
          )
          (setq yLosaStartBB ySlab1B)
          (if (or (= typeLosaBB "Izq") (= typeLosaBB "Ambos")) (draw-break-line xSecB yLosaStartBB h_losa capaCompresion T))
          (if (or (= typeLosaBB "Der") (= typeLosaBB "Ambos")) (draw-break-line (+ xSecB bG_draw) yLosaStartBB h_losa capaCompresion nil))
        )
      )
    )
  )

  ;; Estribo (igual V13 sobre A-A)
  (setq radFillet 0.025)
  (setq rec_draw 0.175)
  (setq p1_st (list (+ xSec rec_draw) (+ ySecA rec_draw)))
  (setq p2_st (list (- (+ xSec bG_draw) rec_draw) (- (+ ySecA hS_draw) rec_draw)))
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

  (if (= (length uniqueKeys) 1)
    (progn
      (setq yTopGlobal yWide1_A)
      (setq yTextAA (+ yTopGlobal offsetText))
      (command "_.TEXT" "_J" "_MC" (list xCenterA yTextAA) 0.18 0 "A-A")
      (command "_.CHPROP" (entlast) "" "Color" 4 "")
    )
  )

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
  (setq critBastWideInf (get-critical-bast bastonesWideInfList))
  (setq critBastWideSup (get-critical-bast bastonesWideSupList))
  (setq critBastStepInf (get-critical-bast bastonesStepInfList))
  (setq critBastStepSup (get-critical-bast bastonesStepSupList))

  (defun process-layer-steel (baseNum baseVar bastNum bastVar isTop / diamBase diamBast yStart dir layoutMode w gap i cx cy totalBars
                                                               xL xR xC xStepL xStepR xAnchor xAnchorL xAnchorR
                                                               positions positionsPts remaining nStep nWide bestPos bestGap xVal
                                                               nOBase nInner reqAnchors nO nC ly extraLeft extraRight
                                                               levelsIzq levelsDer levelsCen levelsCenR
                                                               ocmema--segment-pos ocmema--max-gap ocmema--merge-unique
                                                               ocmema--ensure-anchor ocmema--draw-xlist ocmema--count-near
                                                               ocmema--sort-num ocmema--sort-insert ocmema--str-val eps)
     (setq diamBase (* (/ (obtener-diametro-real baseVar) 100.0) 7.0))
     (setq diamBast 0.0)
     (if bastVar (setq diamBast (* (/ (obtener-diametro-real bastVar) 100.0) 7.0)))
     (if (null bastVar) (setq bastNum 0))
     (if (null bastNum) (setq bastNum 0))
     (setq yStart (if isTop (- (cadr p2_st) (/ diamBase 2.0)) (+ (cadr p1_st) (/ diamBase 2.0))))
     (setq dir (if isTop -1.0 1.0))
     (initget "Orillas Largo")
     (setq layoutMode (getkword (strcat "\nAcomodo " (if isTop "SUP" "INF") " (Base:" (itoa baseNum) " Bast:" (itoa bastNum) ")? [Orillas/Largo]: ")))
     (if (null layoutMode) (setq layoutMode "Orillas"))

     (defun ocmema--sort-insert (lst val)
       (cond
         ((null lst) (list val))
         ((< val (car lst)) (cons val lst))
         (t (cons (car lst) (ocmema--sort-insert (cdr lst) val)))
       )
     )
     (defun ocmema--sort-num (lst / out x)
       (setq out nil)
       (setq lst (ocmema--as-list lst))
       (foreach x lst
         (setq out (ocmema--sort-insert out x))
       )
       out
     )
     (defun ocmema--segment-pos (a b n / gap j out)
       (setq out nil)
       (if (and (> n 0) (> (- b a) 1e-6))
         (progn
           (setq gap (/ (- b a) (+ n 1.0)))
           (setq j 1)
           (repeat n
             (setq out (cons (+ a (* j gap)) out))
             (setq j (1+ j))
           )
           (setq out (reverse out))
         )
       )
       out
     )
     (defun ocmema--max-gap (lst / s prev maxg)
       (setq maxg 0.0)
       (setq s (ocmema--sort-num (ocmema--as-list lst)))
       (if (> (length s) 1)
         (progn
           (setq prev (car s))
           (foreach v (cdr s)
             (setq maxg (max maxg (- v prev)))
             (setq prev v)
           )
         )
       )
       maxg
     )
     (defun ocmema--merge-unique (lst tol / s out prev v)
       (setq s (ocmema--sort-num (ocmema--as-list lst)))
       (princ
         (strcat
           "\nDBG: merge-unique IN | is-list=" (if (listp s) "T" "nil")
           " | len=" (if (listp s) (itoa (length s)) "nil")
           " | first=" (if (and (listp s) (> (length s) 0)) (ocmema--str-val (car s)) "nil")
         )
       )
       (setq out nil prev nil)
       (foreach v s
         (if (or (null prev) (> (abs (- v prev)) tol))
           (progn
             (setq out (cons v out))
             (setq prev v)
           )
         )
       )
       (setq out (reverse out))
       (princ
         (strcat
           "\nDBG: merge-unique OUT | is-list=" (if (listp out) "T" "nil")
           " | len=" (if (listp out) (itoa (length out)) "nil")
           " | first=" (if (and (listp out) (> (length out) 0)) (ocmema--str-val (car out)) "nil")
         )
       )
       out
     )
     (defun ocmema--ensure-anchor (pos anchor tol / s idx best d)
       (setq s pos)
       (if (and anchor (> (length s) 0))
         (progn
           (setq best nil idx 0 d 1e9)
           (foreach v s
             (if (< (abs (- v anchor)) d) (progn (setq d (abs (- v anchor))) (setq best idx)))
             (setq idx (1+ idx))
           )
           (if (> d tol)
             (setq s (subst anchor (nth best s) s))
           )
         )
       )
       s
     )
     (defun ocmema--str-val (v)
       (cond
         ((numberp v) (rtos v 2 3))
         ((listp v) "<list>")
         ((null v) "nil")
         (t "<obj>")
       )
     )
     (defun ocmema--as-list (v)
       (cond
         ((null v) nil)
         ((listp v) v)
         (t (list v))
       )
     )
     (defun ocmema--count-near (lst target tol / count v)
       (setq count 0)
       (foreach v (ocmema--as-list lst)
         (if (and (numberp v) (<= (abs (- v target)) tol))
           (setq count (1+ count))
         )
       )
       count
     )
     (defun ocmema--ensure-ptlist (lst y / out e)
       (setq out nil)
       (cond
         ((null lst) (setq out nil))
         ((numberp lst)
           (setq out (list (list lst y)))
         )
         ((and (listp lst) (numberp (car lst)))
           (foreach e lst (setq out (cons (list e y) out)))
           (setq out (reverse out))
         )
         ((and (listp lst) (listp (car lst)) (= (length (car lst)) 2))
           (setq out lst)
         )
       )
       out
     )
    (defun ocmema--draw-xlist (xlist y / isNumList isPtList pts firstPt)
       (setq isNumList (and (listp xlist) (numberp (car xlist))))
       (setq isPtList (and (listp xlist) (listp (car xlist)) (= (length (car xlist)) 2)))
       (princ
         (strcat
           "\nOCMEMA: x_list type | num-list=" (if isNumList "T" "nil")
           " | pt-list=" (if isPtList "T" "nil")
           " | x_list=" (ocmema--str-val xlist)
         )
       )
       (setq pts (ocmema--ensure-ptlist xlist y))
       (setq firstPt (if (and pts (> (length pts) 0)) (car pts) nil))
       (princ (strcat "\nOCMEMA: first_pt=" (ocmema--str-val firstPt)))
       (if pts
         (foreach p pts
           (command "_.DONUT" 0.0 diamBase p "")
           (command "_.CHPROP" (entlast) "" "Color" 5 "")
         )
      )
    )
    (defun ocmema--clamp-int (v lo hi)
      (cond
        ((null v) lo)
        ((< v lo) lo)
        ((> v hi) hi)
        (t v)
      )
    )
    (setq eps 1e-4)

     (if bb-use-anchor
       (progn
         (setq xL (+ bb-x-left-wide (/ diamBase 2.0)))
         (setq xR (- bb-x-right-wide (/ diamBase 2.0)))
         (setq xStepL (+ bb-x-inner-left-step (/ diamBase 2.0)))
         (setq xStepR (- bb-x-inner-right-step (/ diamBase 2.0)))
         (setq xAnchor nil xAnchorL nil xAnchorR nil)
         (cond
           ((= bb-align "Izq") (setq xAnchor xStepR))
           ((= bb-align "Der") (setq xAnchor xStepL))
           (t (setq xAnchorL xStepL xAnchorR xStepR))
         )

         (if (= layoutMode "Largo")
           (progn
             (setq totalBarsBase (if (and (numberp baseNum) (> baseNum 0)) baseNum 1))
             (princ
               (strcat
                 "\nDBG: LARGO | baseNum=" (if (numberp baseNum) (itoa baseNum) "nil")
                 " | bastNum=" (if (numberp bastNum) (itoa bastNum) "nil")
                 " | totalBarsBase=" (itoa totalBarsBase)
               )
             )
             (setq positions nil)
             (if (= totalBarsBase 1)
               (setq positions (list (/ (+ xL xR) 2.0)))
               (progn
                 (setq w (- xR xL))
                 (setq gap (if (> totalBarsBase 1) (/ w (1- totalBarsBase)) 0.0))
                 (setq i 0)
                 (repeat totalBarsBase
                   (setq positions (append positions (list (+ xL (* i gap)))))
                   (setq i (1+ i))
                 )
               )
             )
             (cond
               ((= bb-align "Centro")
                 (setq positions (ocmema--ensure-anchor positions xAnchorL eps))
                 (setq positions (ocmema--ensure-anchor positions xAnchorR eps))
               )
               (t
                 (setq positions (ocmema--ensure-anchor positions xAnchor eps))
               )
             )
             (setq positions (ocmema--merge-unique positions eps))
           )
           (progn
             ;; Orillas
             (princ (strcat "\nA-A: helper segment-pos=ocmema--segment-pos"))
             (setq nOBase baseNum)
             (if (> baseNum 2)
               (progn
                 (setq nOBase (getint "\nCuantas barras van en orillas (Total)?: "))
                 (if (or (null nOBase) (< nOBase 2)) (setq nOBase 2))
                 (if (> nOBase baseNum) (setq nOBase baseNum))
               )
             )
             (setq positions (list xL xR))
             (if (< baseNum 2)
               (progn
                 (setq positions (list (/ (+ xL xR) 2.0)))
                 (setq baseNum 1)
               )
             )
             (cond
               ((= bb-align "Centro")
                 (if (< baseNum 4)
                   (progn
                     (princ "\nB-B: Orillas centro -> baseNum ajustado a 4")
                     (setq baseNum 4)
                   )
                 )
               )
             )
             (setq reqAnchors (if (= bb-align "Centro") 2 1))
             (setq nInner (- baseNum nOBase))
             (if (< nInner reqAnchors)
               (progn
                 (setq nOBase (- baseNum reqAnchors))
                 (if (< nOBase 2) (setq nOBase 2))
                 (setq nInner (- baseNum nOBase))
                 (princ "\nB-B: Orillas ajuste para incluir ancla")
               )
             )
             (cond
               ((= bb-align "Centro")
                 (setq positions (append positions (list xAnchorL xAnchorR)))
               )
               (t
                 (if (>= baseNum 3) (setq positions (append positions (list xAnchor))))
               )
             )
             (princ
               (strcat
                 "\nDBG: B-B pre-merge | is-list=" (if (listp positions) "T" "nil")
                 " | len=" (if (listp positions) (itoa (length positions)) "nil")
                 " | first=" (if (and (listp positions) (> (length positions) 0)) (ocmema--str-val (car positions)) "nil")
               )
             )
             (setq positions (ocmema--merge-unique positions eps))
             (setq positions (ocmema--as-list positions))
             (setq remaining (- nInner reqAnchors))
             (if (> remaining 0)
               (progn
                 (if (= bb-align "Centro")
                   (progn
                     (setq positions (append positions (ocmema--segment-pos xL xR remaining)))
                     (setq positions (ocmema--merge-unique positions eps))
                   )
                   (progn
                     (setq bestGap 1e9 bestPos nil)
                     (setq nStep 0)
                     (if (> (- xStepR xStepL) 1e-6)
                       (progn
                         (setq nStep 0)
                         (while (<= nStep remaining)
                           (setq nWide (- remaining nStep))
                             (if (= bb-align "Izq")
                               (setq positions (append (list xL xR xAnchor)
                                             (ocmema--segment-pos xStepL xStepR nStep)
                                             (ocmema--segment-pos xAnchor xR nWide)))
                               (setq positions (append (list xL xR xAnchor)
                                             (ocmema--segment-pos xStepL xStepR nStep)
                                             (ocmema--segment-pos xL xAnchor nWide)))
                             )
                           (setq positions (ocmema--merge-unique positions eps))
                           (setq gap (ocmema--max-gap positions))
                           (if (< gap bestGap) (progn (setq bestGap gap) (setq bestPos positions)))
                           (setq nStep (1+ nStep))
                         )
                       )
                     )
                     (if bestPos (setq positions bestPos))
                   )
                 )
               )
             )
           )
         )

         (if (numberp positions) (setq positions (list positions)))
         (setq positions (ocmema--sort-num positions))
         (princ
           (strcat
             "\nB-B: acero capa=" (if isTop "SUP" "INF")
             " | n_barras=" (itoa baseNum)
             " | modo=" layoutMode
             " | alineacion=" bb-align
             " | x_left_wide=" (rtos bb-x-left-wide 2 3)
             " | x_right_wide=" (rtos bb-x-right-wide 2 3)
             " | x_inner_left_step=" (rtos bb-x-inner-left-step 2 3)
             " | x_inner_right_step=" (rtos bb-x-inner-right-step 2 3)
             (if xAnchor
               (strcat " | x_anchor=" (rtos xAnchor 2 3))
               (strcat " | x_anchor_L=" (rtos xAnchorL 2 3) " | x_anchor_R=" (rtos xAnchorR 2 3))
             )
             " | x_list=" (ocmema--str-val positions)
           )
         )

         (ocmema--draw-xlist positions yStart)

         (if (> bastNum 0)
           (progn
             (princ
               (strcat
                 "\nDBG Bastones: capa=" (if isTop "SUP" "INF")
                 " bastNum=" (itoa bastNum)
                 " bastVar=" (if bastVar (vl-princ-to-string bastVar) "nil")
               )
             )
             (setq nO (ocmema--clamp-int (getint (strcat "\nBastones (" (if isTop "SUP" "INF") "): tienes " (itoa bastNum) ". Cuantos van en orillas (Total) (0-" (itoa bastNum) ")?: ")) 0 bastNum))
             (setq nC (- bastNum nO))
             (princ (strcat "\nDBG Bastones: nO=" (itoa nO) " nC=" (itoa nC)))
             (setq levelsIzq (ocmema--count-near positions xL eps))
             (setq levelsDer (ocmema--count-near positions xR eps))
             (if (= bb-align "Centro")
               (progn
                 (setq levelsCen (ocmema--count-near positions xAnchorL eps))
                 (setq levelsCenR (ocmema--count-near positions xAnchorR eps))
               )
               (setq levelsCen (ocmema--count-near positions xAnchor eps))
             )
             (princ
               (strcat
                 "\nBastones (" (if isTop "SUP" "INF") "): bastNum=" (itoa bastNum)
                 " nO=" (itoa nO) " nC=" (itoa nC)
               )
             )
             (princ
               (strcat
                 "\nLevels start: izq=" (itoa levelsIzq)
                 " der=" (itoa levelsDer)
                 " cen=" (itoa levelsCen)
                 (if (= bb-align "Centro") (strcat " cenR=" (itoa levelsCenR)) "")
               )
             )
             (setq i 0)
             (repeat nO
               (if (= (rem i 2) 0)
                 (progn (setq cx xL) (setq ly levelsIzq) (setq levelsIzq (1+ levelsIzq)))
                 (progn (setq cx xR) (setq ly levelsDer) (setq levelsDer (1+ levelsDer)))
               )
               (setq cy (+ yStart (* ly diamBast dir)))
               (command "_.DONUT" 0.0 diamBast (list cx cy) "")
               (command "_.CHPROP" (entlast) "" "Color" 5 "")
               (setq i (1+ i))
             )
             (if (> nC 0)
               (progn
                 (setq i 0)
                 (repeat nC
                   (if (= bb-align "Centro")
                     (progn
                       (if (= (rem i 2) 0)
                         (progn (setq cx xAnchorL) (setq ly levelsCen) (setq levelsCen (1+ levelsCen)))
                         (progn (setq cx xAnchorR) (setq ly levelsCenR) (setq levelsCenR (1+ levelsCenR)))
                       )
                     )
                     (progn
                       (setq cx xAnchor)
                       (setq ly levelsCen)
                       (setq levelsCen (1+ levelsCen))
                     )
                   )
                   (setq cy (+ yStart (* ly diamBast dir)))
                   (command "_.DONUT" 0.0 diamBast (list cx cy) "")
                   (command "_.CHPROP" (entlast) "" "Color" 5 "")
                   (setq i (1+ i))
                 )
               )
             )
           )
         )
       )
       (progn
         (if (= layoutMode "Largo")
           (progn
             (setq xL (+ (car p1_st) (/ diamBase 2.0)))
             (setq xR (- (car p2_st) (/ diamBase 2.0)))
            (setq xC (/ (+ xL xR) 2.0))
             (setq totalBarsBase (if (and (numberp baseNum) (> baseNum 0)) baseNum 1))
             (princ
               (strcat
                 "\nDBG: LARGO | baseNum=" (if (numberp baseNum) (itoa baseNum) "nil")
                 " | bastNum=" (if (numberp bastNum) (itoa bastNum) "nil")
                 " | totalBarsBase=" (itoa totalBarsBase)
               )
             )
             (setq w (- (car p2_st) (car p1_st) diamBase))
             (setq gap (if (> totalBarsBase 1) (/ w (1- totalBarsBase)) 0.0))
             (setq i 0 positions nil)
             (repeat totalBarsBase
               (setq cx (+ (car p1_st) (/ diamBase 2.0) (* i gap)))
               (setq positions (append positions (list cx)))
               (setq i (1+ i))
             )
             (setq positions (ocmema--as-list positions))
             (princ
               (strcat
                 "\nA-A: xlist | is-list=" (if (listp positions) "T" "nil")
                 " | len=" (itoa (length positions))
                 " | first=" (ocmema--str-val (car positions))
               )
             )
             (ocmema--draw-xlist positions yStart)

             (if (> bastNum 0)
               (progn
                 (princ
                   (strcat
                     "\nDBG Bastones: capa=" (if isTop "SUP" "INF")
                     " bastNum=" (itoa bastNum)
                     " bastVar=" (if bastVar (vl-princ-to-string bastVar) "nil")
                   )
                 )
                 (setq nO (ocmema--clamp-int (getint (strcat "\nBastones (" (if isTop "SUP" "INF") "): tienes " (itoa bastNum) ". Cuantos van en orillas (Total) (0-" (itoa bastNum) ")?: ")) 0 bastNum))
                 (setq nC (- bastNum nO))
                 (princ (strcat "\nDBG Bastones: nO=" (itoa nO) " nC=" (itoa nC)))
                 (setq levelsIzq (ocmema--count-near positions xL eps))
                 (setq levelsDer (ocmema--count-near positions xR eps))
                 (setq levelsCen (ocmema--count-near positions xC eps))
                 (princ
                   (strcat
                     "\nBastones (" (if isTop "SUP" "INF") "): bastNum=" (itoa bastNum)
                     " nO=" (itoa nO) " nC=" (itoa nC)
                   )
                 )
                 (princ
                   (strcat
                     "\nLevels start: izq=" (itoa levelsIzq)
                     " der=" (itoa levelsDer)
                     " cen=" (itoa levelsCen)
                   )
                 )
                 (setq i 0)
                 (repeat nO
                   (if (= (rem i 2) 0)
                     (progn (setq cx xL) (setq ly levelsIzq) (setq levelsIzq (1+ levelsIzq)))
                     (progn (setq cx xR) (setq ly levelsDer) (setq levelsDer (1+ levelsDer)))
                   )
                   (setq cy (+ yStart (* ly diamBast dir)))
                   (command "_.DONUT" 0.0 diamBast (list cx cy) "")
                   (command "_.CHPROP" (entlast) "" "Color" 5 "")
                   (setq i (1+ i))
                 )
                 (if (> nC 0)
                   (progn
                     (setq i 0)
                     (repeat nC
                       (setq cx xC)
                       (setq ly levelsCen)
                       (setq levelsCen (1+ levelsCen))
                       (setq cy (+ yStart (* ly diamBast dir)))
                       (command "_.DONUT" 0.0 diamBast (list cx cy) "")
                       (command "_.CHPROP" (entlast) "" "Color" 5 "")
                       (setq i (1+ i))
                     )
                   )
                 )
               )
             )
           )
           (progn
             ;; Orillas (igual V0)
             (setq levelsIzq 0 levelsDer 0 levelsCen 0)
             (setq xL (+ (car p1_st) (/ diamBase 2.0)))
             (setq xR (- (car p2_st) (/ diamBase 2.0)))
             (setq xC (/ (+ xL xR) 2.0))
             (setq nOBase baseNum)
             (if (> baseNum 2)
               (progn
                 (setq nOBase (getint "\nCuantas barras van en orillas (Total)?: "))
                 (if (or (null nOBase) (< nOBase 2)) (setq nOBase 2))
                 (if (> nOBase baseNum) (setq nOBase baseNum))
               )
             )
             (setq positions nil)
             (setq positionsPts nil)
             (if (>= baseNum 2)
               (progn
                 (setq positions (append positions (list xL xR)))
                 (setq positionsPts (append positionsPts (list (list xL yStart) (list xR yStart))))
                 (setq levelsIzq 1 levelsDer 1)
               )
               (progn
                 (setq positions (append positions (list (/ (+ xL xR) 2.0))))
                 (setq positionsPts (append positionsPts (list (list (/ (+ xL xR) 2.0) yStart))))
               )
             )
             (if (> nOBase 2)
               (progn
                 (setq extraLeft (fix (/ (+ (- nOBase 2) 1) 2)))
                 (setq extraRight (fix (/ (- nOBase 2) 2)))
                 (setq i 0)
                 (repeat extraLeft
                   (setq positions (append positions (list xL)))
                   (setq positionsPts (append positionsPts (list (list xL (+ yStart (* levelsIzq diamBase dir))))))
                   (setq levelsIzq (1+ levelsIzq))
                   (setq i (1+ i))
                 )
                 (setq i 0)
                 (repeat extraRight
                   (setq positions (append positions (list xR)))
                   (setq positionsPts (append positionsPts (list (list xR (+ yStart (* levelsDer diamBase dir))))))
                   (setq levelsDer (1+ levelsDer))
                   (setq i (1+ i))
                 )
               )
             )
             (setq nInner (- baseNum nOBase))
             (if (> nInner 0)
               (progn
                 (if (= nInner 1)
                   (progn
                     (setq positions (append positions (list (/ (+ xL xR) 2.0))))
                     (setq positionsPts (append positionsPts (list (list (/ (+ xL xR) 2.0) yStart))))
                   )
                   (foreach xVal (ocmema--segment-pos xL xR nInner)
                     (setq positions (append positions (list xVal)))
                     (setq positionsPts (append positionsPts (list (list xVal yStart))))
                   )
                 )
               )
             )
             (setq positions (ocmema--as-list positions))
             (princ
               (strcat
                 "\nA-A: xlist | is-list=" (if (listp positions) "T" "nil")
                 " | len=" (itoa (length positions))
                 " | first=" (ocmema--str-val (car positions))
               )
             )
             (ocmema--draw-xlist positionsPts yStart)
             (if (> bastNum 0)
               (progn
                 (princ
                   (strcat
                     "\nDBG Bastones: capa=" (if isTop "SUP" "INF")
                     " bastNum=" (itoa bastNum)
                     " bastVar=" (if bastVar (vl-princ-to-string bastVar) "nil")
                   )
                 )
                 (setq nO (ocmema--clamp-int (getint (strcat "\nBastones (" (if isTop "SUP" "INF") "): tienes " (itoa bastNum) ". Cuantos van en orillas (Total) (0-" (itoa bastNum) ")?: ")) 0 bastNum))
                 (setq nC (- bastNum nO))
                 (princ (strcat "\nDBG Bastones: nO=" (itoa nO) " nC=" (itoa nC)))
                 (setq levelsIzq (ocmema--count-near positions xL eps))
                 (setq levelsDer (ocmema--count-near positions xR eps))
                 (setq levelsCen (ocmema--count-near positions xC eps))
                 (princ
                   (strcat
                     "\nBastones (" (if isTop "SUP" "INF") "): bastNum=" (itoa bastNum)
                     " nO=" (itoa nO) " nC=" (itoa nC)
                   )
                 )
                 (princ
                   (strcat
                     "\nLevels start: izq=" (itoa levelsIzq)
                     " der=" (itoa levelsDer)
                     " cen=" (itoa levelsCen)
                   )
                 )
                 (setq i 0)
                 (repeat nO
                   (if (= (rem i 2) 0)
                     (progn (setq cx xL) (setq ly levelsIzq) (setq levelsIzq (1+ levelsIzq)))
                     (progn (setq cx xR) (setq ly levelsDer) (setq levelsDer (1+ levelsDer)))
                   )
                   (setq cy (+ yStart (* ly diamBast dir)))
                   (command "_.DONUT" 0.0 diamBast (list cx cy) "")
                   (command "_.CHPROP" (entlast) "" "Color" 5 "")
                   (setq i (1+ i))
                 )
                 (if (> nC 0)
                   (progn
                     (setq i 0)
                     (repeat nC
                       (setq cx xC)
                       (setq ly levelsCen)
                       (setq levelsCen (1+ levelsCen))
                       (setq cy (+ yStart (* ly diamBast dir)))
                       (command "_.DONUT" 0.0 diamBast (list cx cy) "")
                       (command "_.CHPROP" (entlast) "" "Color" 5 "")
                       (setq i (1+ i))
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
  (if (= (length uniqueKeys) 2)
    (progn
      (setq bb-use-anchor T)
      (setq bb-align almaAlign)
      (setq bb-x-left-wide (+ xSec rec_draw))
      (setq bb-x-right-wide (- (+ xSec bG_draw) rec_draw))
      (setq bb-x-inner-left-step (+ xSec almaX0 rec_draw))
      (setq bb-x-inner-right-step (- (+ xSec almaX0 bS_draw) rec_draw))
      (princ
        (strcat
          "\nA-A: bb-use-anchor=T | bb-align=" (if bb-align bb-align "nil")
          " | x_stepL=" (rtos bb-x-inner-left-step 2 3)
          " | x_stepR=" (rtos bb-x-inner-right-step 2 3)
        )
      )
    )
    (setq bb-use-anchor nil)
  )
  (setq baseNum_wide_inf numPlan)
  (setq baseVar_wide_inf varPlan)
  (setq bastNum_wide_inf (cadr critBastWideInf))
  (setq bastVar_wide_inf (car critBastWideInf))
  (setq baseNum_wide_sup numPlan)
  (setq baseVar_wide_sup varPlan)
  (setq bastNum_wide_sup (cadr critBastWideSup))
  (setq bastVar_wide_sup (car critBastWideSup))
  (princ
    (strcat
      "\nDBG PACK: WIDE-INF baseNum=" (if (numberp baseNum_wide_inf) (itoa baseNum_wide_inf) "nil")
      " bastNum=" (if (numberp bastNum_wide_inf) (itoa bastNum_wide_inf) "nil")
    )
  )
  (process-layer-steel baseNum_wide_inf baseVar_wide_inf bastNum_wide_inf bastVar_wide_inf nil)
  (princ
    (strcat
      "\nDBG PACK: WIDE-SUP baseNum=" (if (numberp baseNum_wide_sup) (itoa baseNum_wide_sup) "nil")
      " bastNum=" (if (numberp bastNum_wide_sup) (itoa bastNum_wide_sup) "nil")
    )
  )
  (process-layer-steel baseNum_wide_sup baseVar_wide_sup bastNum_wide_sup bastVar_wide_sup T)
  (princ "\nOCMEMA: A-A acero end | rutina=process-layer-steel")

  ;; B-B: dibujar armado con la geometria B-B
  (if (= (length uniqueKeys) 2)
    (progn
      (princ (strcat "\nB-B: estribos | alineacion=" alignChoice))

      ;; Estribo principal (zona ancha)
      (if isAlignInf
        (progn
          ;; zona ancha abajo
          (setq p1_st_BB_main (list (+ xSecB rec_draw) (+ ySecB rec_draw)))
          (setq p2_st_BB_main (list (- (+ xSecB bG_draw) rec_draw) (- (+ ySecB hS_draw) rec_draw)))
        )
        (progn
          ;; zona ancha arriba
          (setq p1_st_BB_main (list (+ xSecB rec_draw) (+ ySecB (- hG_draw hS_draw) rec_draw)))
          (setq p2_st_BB_main (list (- (+ xSecB bG_draw) rec_draw) (- (+ ySecB hG_draw) rec_draw)))
        )
      )
      (princ
        (strcat
          "\nB-B: estribo principal box p1=" (vl-princ-to-string p1_st_BB_main)
          " p2=" (vl-princ-to-string p2_st_BB_main)
        )
      )
      (command "_.RECTANG" "_F" radFillet p1_st_BB_main p2_st_BB_main)
      (command "_.CHPROP" (entlast) "" "Color" 3 "")
      (command "_.RECTANG" "_F" 0 (list 0 0) (list 0 0))
      (command "_.ERASE" (entlast) "")

      ;; Gancho estribo principal (sup-izq)
      (setq rBarTop (/ (* (/ (obtener-diametro-real varPlan) 100.0) 7.0) 2.0))
      (setq xStirrupLeft (car p1_st_BB_main))
      (setq yStirrupTop (cadr p2_st_BB_main))
      (setq offsetHookFactor (* rBarTop 1.1))
      (setq centerBarX (+ xStirrupLeft rBarTop))
      (setq pH1 (list (+ centerBarX offsetHookFactor) yStirrupTop))
      (command "_.LINE" pH1 (polar pH1 (* 315.0 (/ pi 180.0)) 0.30) "")
      (command "_.CHPROP" (entlast) "" "Color" 3 "")
      (setq centerBarY (- yStirrupTop rBarTop))
      (setq pH2 (list xStirrupLeft (- centerBarY offsetHookFactor)))
      (command "_.LINE" pH2 (polar pH2 (* 315.0 (/ pi 180.0)) 0.30) "")
      (command "_.CHPROP" (entlast) "" "Color" 3 "")
      (princ "\nB-B: gancho principal dibujado (sup-izq)")

      ;; Estribo escalonado (zona peraltada): rectangulo completo (bS_draw x hS_draw)
      (if isAlignInf
        (progn
          (setq yBotWide ySecB)
          (setq yTopWide (+ ySecB hS_draw))
        )
        (progn
          (setq yBotWide (+ ySecB (- hG_draw hS_draw)))
          (setq yTopWide (+ ySecB hG_draw))
        )
      )
      (if isAlignInf
        (progn
          ;; zona ancha abajo: estribo escalonado sobresale hacia arriba
          (setq outer_step_x0 (+ xSecB almaX0))
          (setq outer_step_x1 (+ xSecB almaX0 bS_draw))
          (setq outer_step_y0 yBotWide)
          (setq outer_step_y1 (+ yBotWide hG_draw))
        )
        (progn
          ;; zona ancha arriba: estribo escalonado sobresale hacia abajo
          (setq outer_step_x0 (+ xSecB almaX0))
          (setq outer_step_x1 (+ xSecB almaX0 bS_draw))
          (setq outer_step_y1 yTopWide)
          (setq outer_step_y0 (- yTopWide hG_draw))
        )
      )
      (setq p1_st_BB_step (list (+ outer_step_x0 rec_draw) (+ outer_step_y0 rec_draw)))
      (setq p2_st_BB_step (list (- outer_step_x1 rec_draw) (- outer_step_y1 rec_draw)))
      (princ
        (strcat
          "\nB-B: logs | alineacion=" alignChoice
          " | hG=" (rtos hG_draw 2 3)
          " | hS=" (rtos hS_draw 2 3)
          " | wide_y0=" (rtos yBotWide 2 3)
          " | wide_y1=" (rtos yTopWide 2 3)
          " | outer_y0=" (rtos outer_step_y0 2 3)
          " | outer_y1=" (rtos outer_step_y1 2 3)
          " | inner_y0=" (rtos (cadr p1_st_BB_step) 2 3)
          " | inner_y1=" (rtos (cadr p2_st_BB_step) 2 3)
          " | cover=" (rtos rec_draw 2 3)
          " | step_h=" (rtos (- (cadr p2_st_BB_step) (cadr p1_st_BB_step)) 2 3)
        )
      )
      (princ
        (strcat
          "\nB-B: estribo escalonado box p1=" (vl-princ-to-string p1_st_BB_step)
          " p2=" (vl-princ-to-string p2_st_BB_step)
        )
      )
      (command "_.RECTANG" "_F" radFillet p1_st_BB_step p2_st_BB_step)
      (command "_.CHPROP" (entlast) "" "Color" 3 "")
      (command "_.RECTANG" "_F" 0 (list 0 0) (list 0 0))
      (command "_.ERASE" (entlast) "")

      ;; Gancho estribo escalonado (inf-izq)
      (setq xStirrupLeft (car p1_st_BB_step))
      (setq yStirrupBot (cadr p1_st_BB_step))
      (setq offsetHookFactor (* rBarTop 1.1))
      (setq centerBarX (+ xStirrupLeft rBarTop))
      (setq pH1 (list (+ centerBarX offsetHookFactor) yStirrupBot))
      (command "_.LINE" pH1 (polar pH1 (* 45.0 (/ pi 180.0)) 0.30) "")
      (command "_.CHPROP" (entlast) "" "Color" 3 "")
      (setq centerBarY (+ yStirrupBot rBarTop))
      (setq pH2 (list xStirrupLeft (+ centerBarY offsetHookFactor)))
      (command "_.LINE" pH2 (polar pH2 (* 45.0 (/ pi 180.0)) 0.30) "")
      (command "_.CHPROP" (entlast) "" "Color" 3 "")
      (princ "\nB-B: gancho escalonado dibujado (inf-izq)")

      (setq yTopGlobal (max (cadr p2_st_BB_main) (cadr p2_st_BB_step)))
      (setq yTextAA (+ yTopGlobal offsetText))
      (setq yTextBB (+ yTopGlobal offsetText))
      (command "_.TEXT" "_J" "_MC" (list xCenterA yTextAA) 0.18 0 "A-A")
      (command "_.CHPROP" (entlast) "" "Color" 4 "")
      (command "_.TEXT" "_J" "_MC" (list xCenterB yTextBB) 0.18 0 "B-B")
      (command "_.CHPROP" (entlast) "" "Color" 4 "")

      (setq bb-use-anchor T)
      (setq bb-align almaAlign)
      (setq bb-x-left-wide (car p1_st_BB_main))
      (setq bb-x-right-wide (car p2_st_BB_main))
      (setq bb-x-inner-left-step (car p1_st_BB_step))
      (setq bb-x-inner-right-step (car p2_st_BB_step))

      (setq bbCountBase (if (numberp numPlan) numPlan 0))
      (setq bbCountInf (if (numberp (cadr critBastInf)) (cadr critBastInf) 0))
      (setq bbCountSup (if (numberp (cadr critBastSup)) (cadr critBastSup) 0))

      (princ
        (strcat
          "\nOCMEMA: B-B acero start | rutina=process-layer-steel"
          " | barras_base=" (ocmema--int-str numPlan)
          " | baston_inf=" (ocmema--int-str (cadr critBastInf))
          " | baston_sup=" (ocmema--int-str (cadr critBastSup))
          " | conteo_inf=" (itoa (+ bbCountBase bbCountInf))
          " | conteo_sup=" (itoa (+ bbCountBase bbCountSup))
          " | bG=" (ocmema--num-str bG_draw)
          " | hG=" (ocmema--num-str hG_draw)
          " | bS=" (ocmema--num-str bS_draw)
          " | hS=" (ocmema--num-str hS_draw)
          " | almaX0=" (ocmema--num-str almaX0)
          " | bastonesInfVacia=" (ocmema--list-empty bastonesInfList)
          " | bastonesSupVacia=" (ocmema--list-empty bastonesSupList)
        )
      )

      (setq p1_st p1_st_BB_main p2_st p2_st_BB_main)
      (princ
        (strcat
          "\nOCMEMA: B-B acero WIDE start | p1=(" (rtos (car p1_st) 2 3) "," (rtos (cadr p1_st) 2 3) ")"
          " | p2=(" (rtos (car p2_st) 2 3) "," (rtos (cadr p2_st) 2 3) ")"
        )
      )
      (princ
        (strcat
          "\nDBG PACK: WIDE-INF baseNum=" (if (numberp baseNum_wide_inf) (itoa baseNum_wide_inf) "nil")
          " bastNum=" (if (numberp bastNum_wide_inf) (itoa bastNum_wide_inf) "nil")
        )
      )
      (process-layer-steel baseNum_wide_inf baseVar_wide_inf bastNum_wide_inf bastVar_wide_inf nil)
      (princ "\nOCMEMA: B-B acero WIDE process-layer-steel called (INF)")
      (princ
        (strcat
          "\nDBG PACK: WIDE-SUP baseNum=" (if (numberp baseNum_wide_sup) (itoa baseNum_wide_sup) "nil")
          " bastNum=" (if (numberp bastNum_wide_sup) (itoa bastNum_wide_sup) "nil")
        )
      )
      (process-layer-steel baseNum_wide_sup baseVar_wide_sup bastNum_wide_sup bastVar_wide_sup T)
      (princ "\nOCMEMA: B-B acero WIDE process-layer-steel called (SUP)")
      (princ "\nOCMEMA: B-B acero WIDE end | rutina=process-layer-steel")

      (setq p1_st p1_st_BB_step p2_st p2_st_BB_step)
      (setq stepIsTop (= bbBranch "escalon-arriba"))
      (setq numPlan_step (if (numberp numDeep) numDeep 0))
      (setq varPlan_step varDeep)
      (setq baseNum_step numPlan_step)
      (setq baseVar_step varPlan_step)
      (if stepIsTop
        (progn
          (setq bastNum_step (cadr critBastStepSup))
          (setq bastVar_step (car critBastStepSup))
        )
        (progn
          (setq bastNum_step (cadr critBastStepInf))
          (setq bastVar_step (car critBastStepInf))
        )
      )
      (princ
        (strcat
          "\nOCMEMA: B-B step steel | rama=" bbBranch
          " | stepIsTop=" (if stepIsTop "T" "nil")
          " | numPlan_step=" (if (numberp numPlan_step) (itoa numPlan_step) "nil")
          " | varPlan_step=" (if varPlan_step varPlan_step "nil")
          " | p1=(" (rtos (car p1_st) 2 3) "," (rtos (cadr p1_st) 2 3) ")"
          " | p2=(" (rtos (car p2_st) 2 3) "," (rtos (cadr p2_st) 2 3) ")"
        )
      )
      (if (and varPlan_step (numberp numPlan_step) (> numPlan_step 0))
        (progn
          (setq bb-use-anchor nil)
          (princ
            (strcat
              "\nDBG PACK: STEP stepIsTop=" (if stepIsTop "T" "nil")
              " baseNum=" (if (numberp baseNum_step) (itoa baseNum_step) "nil")
              " bastNum=" (if (numberp bastNum_step) (itoa bastNum_step) "nil")
            )
          )
          (process-layer-steel baseNum_step baseVar_step bastNum_step bastVar_step stepIsTop)
          (princ "\nOCMEMA: B-B acero STEP process-layer-steel called")
          (setq bb-use-anchor T)
        )
        (princ "\nOCMEMA: B-B acero STEP skip (sin acero peraltado)")
      )
      (princ "\nOCMEMA: B-B acero STEP end | rutina=process-layer-steel")
      (princ "\nOCMEMA: B-B acero end | rutina=process-layer-steel")
    )
  )

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
