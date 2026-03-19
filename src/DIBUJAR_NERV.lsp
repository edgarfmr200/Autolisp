;; ==============================================================================
;; DIBUJAR_NERV.LSP  (antes GEN_NERV_FINAL_V27)
;; FIX: Offset global por GEOMETRIA (no por ultimo punto del acero)
;; LOGICA: Interpolacion Exacta -> Anclaje 20cm -> Redondeo Distancia a Eje.
;; AJUSTES:
;; 1) Ruta inicial getfiled
;; 2) Redondeo al mas proximo (no al superior)
;; 3) Comando c:DIBUJAR_NERV
;; 4) Cotas superiores a yTop+0.25 y referidas a (axisX,yTop) para no cruzar nervadura
;; 5) En superior: mostrar Req - Min al pedir varillas (en cm2 reales)
;; ==============================================================================

(defun ocmema:nerv:armar-from-anl (anlPath / *error* file dir filename tempFile wsh cmd fp line 
                        coordList YD ZB startX endX l_total_cm l_total_m h_cm b_cm 
                        h_draw rectLen ptOrigin xOrigin yOrigin oldOsnap p1 p2 
                        idx numNodes hasAxis axisName posType drawX extraX 
                        pAxisBot pAxisTop centerCircle axisXList dimXList axisCenter
                        dataMode strList valX nodeID
                        tokenList i len token currentDist xOffsetGlobal 
                        globalSteelList
                        valAstTop valAstBot maxDemBot maxDemTop areaBaseBot areaBaseTop
                        ans numBars strVar row zoneStart zoneEnd req d strVarBast 
                        yBot yTop xDrawStart xDrawEnd 
                        pts pTail pCrank pJoin foundBot j
                        nID nX_m maxDistFound
                        posTypeSteel valAst cleanLine
                        redondear-al-5 get-closest-axis dibujar-leader-blindado
                        obtener-x-interpolada procesar-zonas-v27 calc-puntos-v27
                        obtener-req-en-x
                        x_ini_inf x_fin_inf axis_near_start axis_near_end
                        x_baston_ini x_baston_fin
                        dist_izq dist_der scan_x found
                        p_start p_end p_crank_start p_crank_end
                        usar_minimo len_final_izq len_final_der
                        old_dimasz old_dimclrd old_dimtxt old_dimclrt old_dimscale
                        list_zonas_inf zona list_zonas_sup
                        ;; Offsets por miembro
                        memberOffsetList mIndex
                        ;; Zonas top
                        zonesTop z axisX is_active active_zone
                        x_start x_end cant num maxReq pts_fix
                        ;; Para mostrar req - min en superior
                        reqAxis diffAxis reqAxis_cm2 diffAxis_cm2
                        ;; y de cotas
                        yTopRed yDimLoc yDimSup
                        supportNodes projectItem projectPlant projectPoints
                        axisPairsX axisPairsY axisTolDraw autoAxisFamily
                        autoAxisEnabled autoAxisName hasProjectAxes
                        useModel3dAxis isSupportNode projectPointNodeMap mappedPoint
                      )

  (vl-load-com)

  ;; --- 1. FUNCIONES AUXILIARES MATEMATICAS ---

  ;; (2) Redondeo al múltiplo mas cercano de 0.05 m
  (defun redondear-al-5 (val)
    (* 0.05 (fix (+ (/ val 0.05) 0.5)))
  )

  (defun get-closest-axis (x_pt axis_list / min_dist best_axis axis dist)
    (setq min_dist 100000.0)
    (setq best_axis nil)
    (foreach axis axis_list
      (setq dist (abs (- x_pt axis)))
      (if (< dist min_dist)
        (progn
          (setq min_dist dist)
          (setq best_axis axis)
        )
      )
    )
    best_axis
  )

  ;; FORMULA INTERPOLACION EXACTA Y=MX+B
  (defun obtener-x-interpolada (x1 y1 x2 y2 target)
    (if (equal y2 y1 0.0000001)
        x1 
        (+ x1 (* (- target y1) (/ (- x2 x1) (- y2 y1))))
    )
  )

  ;; Devuelve lista: (xStartInterp xEndInterp maxReqZone) en coordenadas DIBUJO
  (defun procesar-zonas-v27 (steelList limitMin x_origin_draw is_top
                            / idx_val base ptsCorr zonas inZone startXZone endXZone maxReqZone
                              i prev_x prev_req curr_x curr_req first_req last_x)
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

        ;; arranca alto
        (setq first_req (cadr (car ptsCorr)))
        (if (> first_req limitMin)
          (progn
            (setq inZone T)
            (setq startXZone (car (car ptsCorr)))
            (setq maxReqZone first_req)
          )
        )

        (setq i 1)
        (while (< i (length ptsCorr))
          (setq prev_x (car (nth (1- i) ptsCorr)))
          (setq prev_req (cadr (nth (1- i) ptsCorr)))
          (setq curr_x (car (nth i ptsCorr)))
          (setq curr_req (cadr (nth i ptsCorr)))

          ;; ENTRADA: bajo->alto
          (if (and (not inZone) (<= prev_req limitMin) (> curr_req limitMin))
            (progn
              (setq inZone T)
              (setq startXZone (obtener-x-interpolada prev_x prev_req curr_x curr_req limitMin))
              (setq maxReqZone (max prev_req curr_req))
            )
          )

          (if inZone (setq maxReqZone (max maxReqZone curr_req)))

          ;; SALIDA: alto->bajo
          (if (and inZone (> prev_req limitMin) (<= curr_req limitMin))
            (progn
              (setq endXZone (obtener-x-interpolada prev_x prev_req curr_x curr_req limitMin))
              (setq zonas (append zonas (list (list startXZone endXZone maxReqZone))))
              (setq inZone nil startXZone nil endXZone nil maxReqZone 0.0)
            )
          )

          (setq i (1+ i))
        )

        ;; termina alto
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

  ;; Anclaje 20cm -> redondeo distancia a eje (al mas cercano)
  (defun calc-puntos-v27 (x_interp_start x_interp_end axis_list / t_start t_end ax_s ax_e dist_s dist_e round_s round_e final_s final_e tmp)
    (setq t_start (- x_interp_start 0.20))
    (setq t_end   (+ x_interp_end   0.20))

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

    (if (> final_s final_e)
      (progn (setq tmp final_s) (setq final_s final_e) (setq final_e tmp))
    )

    (list final_s final_e)
  )

  ;; (5) Obtener As en un X (coordenada DIBUJO), por interpolación entre puntos
  (defun ocmema--support-ids-from-line (rawLine / toks out i cur nxt tmp)
    (setq out '())
    (if (and rawLine (> (strlen rawLine) 0))
      (progn
        (setq toks (read (strcat "(" (vl-string-translate "." " " rawLine) ")")))
        (setq i 0)
        (while (< i (length toks))
          (setq cur (nth i toks))
          (cond
            ((numberp cur)
              (if (and (< (+ i 2) (length toks))
                       (eq (nth (1+ i) toks) 'TO)
                       (numberp (nth (+ i 2) toks)))
                (progn
                  (setq nxt (nth (+ i 2) toks))
                  (if (> cur nxt)
                    (progn (setq tmp cur) (setq cur nxt) (setq nxt tmp))
                  )
                  (while (<= cur nxt)
                    (if (not (member cur out))
                      (setq out (append out (list cur)))
                    )
                    (setq cur (1+ cur))
                  )
                  (setq i (+ i 3))
                )
                (progn
                  (if (not (member cur out))
                    (setq out (append out (list cur)))
                  )
                  (setq i (1+ i))
                )
              )
            )
            (T (setq i (length toks)))
          )
        )
      )
    )
    out
  )

  (defun ocmema--alist-get-local (key alst / out target item)
    (setq out nil)
    (setq target (strcase (vl-princ-to-string key)))
    (if (listp alst)
      (foreach item alst
        (if (and (listp item)
                 (= (strcase (vl-princ-to-string (car item))) target))
          (setq out (cdr item))
        )
      )
    )
    out
  )

  (defun ocmema--normalize-name-local (s)
    (strcase (vl-string-trim " \t" (if s (vl-princ-to-string s) "")))
  )

  (defun ocmema--find-project-item-local (bucket itemName / proj items norm found item)
    (setq found nil)
    (setq proj (if (and (boundp 'ocmema:*project*) ocmema:*project*) ocmema:*project* nil))
    (setq items (if proj (ocmema--alist-get-local bucket proj) nil))
    (setq norm (ocmema--normalize-name-local itemName))
    (if (listp items)
      (foreach item items
        (if (= (ocmema--normalize-name-local (ocmema--alist-get-local "name" item)) norm)
          (setq found item)
        )
      )
    )
    found
  )

  (defun ocmema--find-plant-local (plantIdx / proj plants found plant)
    (setq found nil)
    (setq proj (if (and (boundp 'ocmema:*project*) ocmema:*project*) ocmema:*project* nil))
    (setq plants (if proj (ocmema--alist-get-local "plants" proj) nil))
    (if (listp plants)
      (foreach plant plants
        (if (= (ocmema--alist-get-local "idx" plant) plantIdx)
          (setq found plant)
        )
      )
    )
    found
  )

  (defun ocmema--meta-model3d-p (meta)
    (and (= (type meta) 'STR)
         (wcmatch (strcase meta) "*SOURCE=MODELO3D*"))
  )

  (defun ocmema--project-draw-unit-factor-local (/ proj u)
    (setq proj (if (and (boundp 'ocmema:*project*) ocmema:*project*) ocmema:*project* nil))
    (setq u (if proj (ocmema--alist-get-local "draw_units" proj) nil))
    (if (not u) (setq u (if proj (ocmema--alist-get-local "units" proj) nil)))
    (setq u (strcase (if u (vl-princ-to-string u) "M")))
    (cond
      ((= u "CM") 1.0)
      ((= u "C") 1.0)
      ((= u "MM") 0.1)
      (T 100.0)
    )
  )

  (defun ocmema--project-scale-local (/ proj sc)
    (setq proj (if (and (boundp 'ocmema:*project*) ocmema:*project*) ocmema:*project* nil))
    (setq sc (if proj (ocmema--alist-get-local "draw_scale_factor" proj) nil))
    (if (not (numberp sc)) (setq sc (if proj (ocmema--alist-get-local "scale" proj) nil)))
    (if (not (numberp sc)) (setq sc 1.0))
    (if (<= sc 0.0) (setq sc 1.0))
    sc
  )

  (defun ocmema--axis-tol-draw (/ sc uf)
    (setq sc (ocmema--project-scale-local))
    (setq uf (ocmema--project-draw-unit-factor-local))
    (/ (* 2.0 sc) uf)
  )

  (defun ocmema--point-x (pt)
    (if (and (listp pt) (numberp (car pt))) (car pt) nil)
  )

  (defun ocmema--point-y (pt)
    (if (and (listp pt) (numberp (cadr pt))) (cadr pt) nil)
  )

  (defun ocmema--axis-family-from-points (pts tol / p0 pN dx dy)
    (setq p0 (if (and (listp pts) (> (length pts) 0)) (car pts) nil))
    (setq pN (if (and (listp pts) (> (length pts) 1)) (last pts) nil))
    (if (and (listp pN) (= (length pN) 1)) (setq pN (car pN)))
    (if (and p0 pN
             (numberp (ocmema--point-x p0)) (numberp (ocmema--point-y p0))
             (numberp (ocmema--point-x pN)) (numberp (ocmema--point-y pN)))
      (progn
        (setq dx (abs (- (ocmema--point-x pN) (ocmema--point-x p0))))
        (setq dy (abs (- (ocmema--point-y pN) (ocmema--point-y p0))))
        (cond
          ((<= dy tol) "X")
          ((<= dx tol) "Y")
          (T nil)
        )
      )
      nil
    )
  )

  (defun ocmema--axis-name-from-coord (coord axisPairs tol / out pair axisVal)
    (setq out nil)
    (if (and (numberp coord) (listp axisPairs))
      (foreach pair axisPairs
        (setq axisVal (cdr pair))
        (if (and (numberp axisVal) (<= (abs (- coord axisVal)) tol))
          (setq out (car pair))
        )
      )
    )
    out
  )

  (defun ocmema--auto-axis-name-for-index (idx pts family axisPairsX axisPairsY tol / pt coord)
    (setq pt (if (and (listp pts) (>= idx 0) (< idx (length pts))) (nth idx pts) nil))
    (cond
      ((or (not pt) (not family)) nil)
      ((= family "X")
        (setq coord (ocmema--point-x pt))
        (ocmema--axis-name-from-coord coord axisPairsX tol)
      )
      ((= family "Y")
        (setq coord (ocmema--point-y pt))
        (ocmema--axis-name-from-coord coord axisPairsY tol)
      )
      (T nil)
    )
  )

  (defun ocmema--draw-delta-to-cm (delta / sc)
    (setq sc (ocmema--project-scale-local))
    (/ (* (abs delta) (ocmema--project-draw-unit-factor-local)) sc)
  )

  (defun ocmema--plant-match-score (plant pts family tol / axes score pt coord)
    (setq score 0)
    (setq axes (if (= family "X")
                 (ocmema--alist-get-local "x_axes" plant)
                 (ocmema--alist-get-local "y_axes" plant)))
    (if (listp pts)
      (foreach pt pts
        (setq coord (if (= family "X") (ocmema--point-x pt) (ocmema--point-y pt)))
        (if (ocmema--axis-name-from-coord coord axes tol)
          (setq score (1+ score))
        )
      )
    )
    score
  )

  (defun ocmema--best-plant-local (pts family tol / proj plants best bestScore score plant)
    (setq best nil)
    (setq bestScore -1)
    (setq proj (if (and (boundp 'ocmema:*project*) ocmema:*project*) ocmema:*project* nil))
    (setq plants (if proj (ocmema--alist-get-local "plants" proj) nil))
    (if (listp plants)
      (foreach plant plants
        (setq score (ocmema--plant-match-score plant pts family tol))
        (if (> score bestScore)
          (progn
            (setq best plant)
            (setq bestScore score)
          )
        )
      )
    )
    (if (> bestScore 0) best nil)
  )

  (defun ocmema--map-points-to-node-indexes (pts family coordList startX / numPts numNodes out baseCoord prevIdx j remaining searchEnd bestIdx bestErr i targetDist nodeDist)
    (cond
      ((or (not (listp pts)) (< (length pts) 2)) nil)
      ((> (length pts) (length coordList)) 'ERR_MORE_POINTS)
      (T
        (setq numPts (length pts))
        (setq numNodes (length coordList))
        (setq out (list (cons 0 0)))
        (setq baseCoord (if (= family "X") (ocmema--point-x (car pts)) (ocmema--point-y (car pts))))
        (setq prevIdx 0)
        (setq j 1)
        (while (< j (1- numPts))
          (setq remaining (- numPts j))
          (setq searchEnd (- numNodes remaining))
          (setq targetDist (ocmema--draw-delta-to-cm (- (if (= family "X")
                                                         (ocmema--point-x (nth j pts))
                                                         (ocmema--point-y (nth j pts)))
                                                      baseCoord)))
          (setq bestIdx nil)
          (setq bestErr nil)
          (setq i (1+ prevIdx))
          (while (<= i searchEnd)
            (setq nodeDist (- (cadr (nth i coordList)) startX))
            (if (or (null bestErr) (< (abs (- nodeDist targetDist)) bestErr))
              (progn
                (setq bestErr (abs (- nodeDist targetDist)))
                (setq bestIdx i)
              )
            )
            (setq i (1+ i))
          )
          (if (null bestIdx)
            (setq j numPts)
            (progn
              (setq out (append out (list (cons bestIdx j))))
              (setq prevIdx bestIdx)
              (setq j (1+ j))
            )
          )
        )
        (if (= (length out) (1- numPts))
          (setq out (append out (list (cons (1- numNodes) (1- numPts)))))
        )
        out
      )
    )
  )

  (defun ocmema--mapped-point-for-node-index (nodeIdx mapping pts / pair pairTmp)
    (setq pair nil)
    (if (listp mapping)
      (foreach pairTmp mapping
        (if (= (car pairTmp) nodeIdx)
          (setq pair pairTmp)
        )
      )
    )
    (if (and pair (listp pts) (>= (cdr pair) 0) (< (cdr pair) (length pts)))
      (nth (cdr pair) pts)
      nil
    )
  )

  (defun obtener-req-en-x (steelList x_target_draw x_origin_draw is_top
                           / idx_val base ptsCorr n i p1 p2 x1 y1 x2 y2)
    (setq idx_val (if is_top 1 2))
    (setq base (+ x_origin_draw 0.5))

    (setq ptsCorr '())
    (foreach pt steelList
      (if (and (numberp (car pt)) (numberp (nth idx_val pt)))
        (setq ptsCorr (append ptsCorr (list (list (+ base (car pt)) (nth idx_val pt)))))
      )
    )
    (setq ptsCorr (vl-sort ptsCorr '(lambda (a b) (< (car a) (car b)))))
    (setq n (length ptsCorr))

    (cond
      ((< n 2) 0.0)
      ((<= x_target_draw (car (car ptsCorr))) (cadr (car ptsCorr)))
      ((>= x_target_draw (car (last ptsCorr))) (cadr (last ptsCorr)))
      (t
        (setq i 1)
        (while (< i n)
          (setq p1 (nth (1- i) ptsCorr))
          (setq p2 (nth i ptsCorr))
          (setq x1 (car p1) y1 (cadr p1))
          (setq x2 (car p2) y2 (cadr p2))
          (if (and (<= x1 x_target_draw) (<= x_target_draw x2))
            (progn
              (setq i n) ;; romper
              (if (equal x2 x1 0.0000001)
                y1
                (+ y1 (* (- x_target_draw x1) (/ (- y2 y1) (- x2 x1)))))
            )
            (setq i (1+ i))
          )
        )
      )
    )
  )

  ;; DIBUJAR LEADER BLINDADO (INTACTO)
  (defun dibujar-leader-blindado (pt1 texto es_top es_final / sgn_y sgn_x pt2 pt3 obj_text obj_leader)
     (setvar "DIMCLRD" 7)
     (setvar "DIMASZ" 0.15)
     (setvar "DIMSCALE" 1.0)

     (setq sgn_y (if es_top -1.0 1.0)) 
     (setq sgn_x (if es_final -1.0 1.0))

     (setq pt2 (list (+ (car pt1) (* 0.20 sgn_x)) (+ (cadr pt1) (* 0.35 sgn_y))))
     (setq pt3 (list (+ (car pt2) (* 0.15 sgn_x)) (cadr pt2)))

     (command "_.LEADER" pt1 pt2 pt3 "" "" "N")
     (if (setq obj_leader (vlax-ename->vla-object (entlast))) (vl-catch-all-apply 'vla-put-Color (list obj_leader 7)))

     (if (< (car pt3) (car pt2)) (command "_.TEXT" "_J" "_MR" pt3 0.15 0 texto) (command "_.TEXT" "_J" "_ML" pt3 0.15 0 texto))
     (setq obj_text (vlax-ename->vla-object (entlast))) (vl-catch-all-apply 'vla-put-Color (list obj_text 40)) 
  )

  (defun clean-string (s) (vl-string-translate "|" " " s))

  (defun *error* (msg)
    (if oldOsnap (setvar "OSMODE" oldOsnap))
    (if (and fp (= (type fp) 'FILE)) (close fp))
    (princ)
  )

  (setq old_dimasz (getvar "DIMASZ")) (setq old_dimclrd (getvar "DIMCLRD"))
  (setq old_dimtxt (getvar "DIMTXT")) (setq old_dimclrt (getvar "DIMCLRT"))
  (setq old_dimscale (getvar "DIMSCALE"))

  (setq oldOsnap (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  (setvar "FILLETRAD" 0.05)

  (if (not (tblsearch "LTYPE" "CENTER")) (command "-LINETYPE" "Load" "CENTER" "acad.lin" ""))

  ;; ==============================================================================
  ;; 2. LECTURA DE DATOS (desde anlPath)
  ;; ==============================================================================
  (setq file anlPath)
  (if (or (not file) (= file ""))
    (progn
      (prompt "\nOCMEMA WARN: ruta ANL vacia.")
      (exit)
    )
  )
  (if (not (findfile file))
    (progn
      (prompt "\nOCMEMA WARN: archivo ANL no encontrado.")
      (exit)
    )
  )

  (setq filename (vl-filename-base file))
  (princ "\n1. Leyendo Geometria...")

  (setq fp (open file "r"))
  (setq coordList nil YD 25.0 ZB 10.0 dataMode nil supportNodes '())

  (while (setq line (read-line fp))
    (setq line (vl-string-trim " \t" line))
    (cond
      ((or (wcmatch line "*JOINT COORDINATES*") (wcmatch line "*COORDENADAS*")) (setq dataMode "NODES"))
      ((and (= dataMode "NODES") (or (wcmatch line "*MEMBER INCIDENCES*") (wcmatch line "*INCIDENCIAS*"))) (setq dataMode nil))
      ((wcmatch line "*SUPPORTS*") (setq dataMode "SUPPORTS"))
      ((and (= dataMode "SUPPORTS") (= line "")) (setq dataMode nil))
      ((and (= dataMode "NODES") (> (strlen line) 0))
        (if (numberp (read (substr line 1 1)))
          (progn
            (setq strList (read (strcat "(" (vl-string-translate "." " " line) ")")))
            (setq valX nil nodeID nil)
            (cond 
              ((>= (length strList) 5) (setq nodeID (nth 1 strList)) (setq valX (nth 2 strList)))
              ((= (length strList) 4) (setq nodeID (nth 0 strList)) (setq valX (nth 1 strList)))
            )
            (if (numberp valX) (setq coordList (append coordList (list (list nodeID valX)))))
          )
        )
      )
      ((and (= dataMode "SUPPORTS") (> (strlen line) 0))
        (foreach nSupp (ocmema--support-ids-from-line line)
          (if (not (member nSupp supportNodes))
            (setq supportNodes (append supportNodes (list nSupp)))
          )
        )
      )
      ((wcmatch line "*MEMBER PROPERTY*") (setq dataMode "PROPS"))
      ((and (= dataMode "PROPS") (wcmatch line "*PRIS*"))
        (if (setq pos (vl-string-search "YD" line)) (setq YD (atof (substr line (+ pos 4) 5))))
        (if (setq pos (vl-string-search "ZB" line)) (setq ZB (atof (substr line (+ pos 4) 5))))
        (if (and (null ZB) (setq pos (vl-string-search "ZD" line))) (setq ZB (atof (substr line (+ pos 4) 5))))
        (setq dataMode nil)
      )
    )
  )
  (close fp)

  (if (null coordList) (progn (alert "No se pudo leer la geometria.") (exit)))
  (setq coordList (vl-sort coordList '(lambda (a b) (< (cadr a) (cadr b)))))
  (setq startX (cadr (nth 0 coordList)))
  (setq endX (cadr (last coordList)))
  (setq l_total_cm (- endX startX))
  (setq l_total_m (/ l_total_cm 100.0))
  (setq h_cm YD b_cm ZB)

  (setq h_draw (* (/ h_cm 100.0) 7.0))
  (setq rectLen (+ l_total_m 1.0))

  ;; Offsets reales por miembro (m) desde geometría
  (setq memberOffsetList '())
  (foreach node coordList
    (setq memberOffsetList (append memberOffsetList (list (/ (- (cadr node) startX) 100.0))))
  )
  (if (> (length memberOffsetList) 0)
    (setq memberOffsetList (reverse (cdr (reverse memberOffsetList))))
  )

  (princ "\n2. Procesando Acero...")
  (setq dir (vl-filename-directory file))
  (setq tempFile (strcat dir "\\" filename "_TEMP_ANSI.TXT"))
  (setq wsh (vlax-create-object "WScript.Shell"))
  (setq cmd (strcat "powershell -NoProfile -Command \"Get-Content -LiteralPath '" file "' | Set-Content -Encoding Ascii '" tempFile "'\""))
  (vlax-invoke wsh 'Run cmd 0 :vlax-true)
  (vlax-release-object wsh)

  (setq fp (open tempFile "r"))
  (setq globalSteelList '() xOffsetGlobal 0.0)
  (setq isReadingSteel nil savedDist nil)
  (setq astTop 0.0 astBot 0.0)
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
          (if (> (length globalSteelList) 0)
            (setq xOffsetGlobal (car (last globalSteelList)))
            (setq xOffsetGlobal 0.0)
          )
        )
      )
    )

    (if (and isReadingSteel (> (strlen cleanLine) 5) (not (wcmatch line "*Distance*")) (not (wcmatch line "*-----*")))
      (progn
        (setq tokenList (read (strcat "(" cleanLine ")")))
        (cond
          ((numberp (car tokenList))
              (if savedDist
                (setq globalSteelList (append globalSteelList (list (list (+ xOffsetGlobal savedDist) astTop astBot)))))
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
  (vl-file-delete tempFile)

  (setq globalSteelList (mapcar '(lambda (pt) (list (car pt) (* (cadr pt) 100.0) (* (caddr pt) 100.0))) globalSteelList))

  (setq maxDemBot 0.0 maxDemTop 0.0)
  (foreach pt globalSteelList (setq maxDemBot (max maxDemBot (caddr pt))) (setq maxDemTop (max maxDemTop (cadr pt))))

  ;; ==============================================================================
  ;; 3. DIBUJO BASE (INTACTO)
  ;; ==============================================================================
  (princ "\nSeleccione punto de origen: ")
  (setq ptOrigin (getpoint))
  (if (not ptOrigin) (exit))
  (setq xOrigin (car ptOrigin) yOrigin (cadr ptOrigin))

  (command "_.TEXT" "_J" "_TL" (list xOrigin (+ yOrigin 0.33)) 0.25 0 (strcat "h=" (rtos h_cm 2 0))) (command "_.CHPROP" (entlast) "" "Color" 3 "")
  (command "_.TEXT" "_J" "_TL" (list xOrigin (+ yOrigin 0.86)) 0.25 0 (strcat "b=" (rtos b_cm 2 0))) (command "_.CHPROP" (entlast) "" "Color" 3 "")
  (command "_.TEXT" "_J" "_TL" (list xOrigin (+ yOrigin h_draw)) 0.35 0 filename) (command "_.CHPROP" (entlast) "" "Color" 4 "")

  (setq p1 (list (+ xOrigin 1.25) yOrigin))
  (setq p2 (list (+ (car p1) rectLen) (+ yOrigin h_draw)))
  (command "_.RECTANG" p1 p2) (command "_.CHPROP" (entlast) "" "Color" 7 "")

  (setq numNodes (length coordList))
  (setq projectItem (ocmema--find-project-item-local "ribs" filename))
  (setq projectPlant nil)
  (setq projectPoints nil)
  (setq axisPairsX nil)
  (setq axisPairsY nil)
  (setq axisTolDraw (ocmema--axis-tol-draw))
  (setq autoAxisFamily nil)
  (setq autoAxisEnabled nil)
  (setq hasProjectAxes nil)
  (setq useModel3dAxis nil)
  (setq projectPointNodeMap nil)
  (if projectItem
    (progn
      (setq useModel3dAxis (ocmema--meta-model3d-p (ocmema--alist-get-local "meta_kv" projectItem)))
      (setq projectPoints (ocmema--alist-get-local "points_raw" projectItem))
      (if (listp projectPoints)
        (setq autoAxisFamily (ocmema--axis-family-from-points projectPoints axisTolDraw))
      )
      (setq projectPlant (ocmema--find-plant-local (ocmema--alist-get-local "plant_idx" projectItem)))
      (if (and (not projectPlant) autoAxisFamily projectPoints)
        (setq projectPlant (ocmema--best-plant-local projectPoints autoAxisFamily axisTolDraw))
      )
      (setq axisPairsX (if projectPlant (ocmema--alist-get-local "x_axes" projectPlant) nil))
      (setq axisPairsY (if projectPlant (ocmema--alist-get-local "y_axes" projectPlant) nil))
      (setq hasProjectAxes (or (and (listp axisPairsX) (> (length axisPairsX) 0))
                               (and (listp axisPairsY) (> (length axisPairsY) 0))))
      (if (and autoAxisFamily projectPoints)
        (progn
          (setq projectPointNodeMap (ocmema--map-points-to-node-indexes projectPoints autoAxisFamily coordList startX))
          (if (= projectPointNodeMap 'ERR_MORE_POINTS)
            (progn
              (setq projectPointNodeMap nil)
              (prompt "\nOCMEMA WARN: Hay mas puntos guardados en TXT que nodos en el ANL. Se desactiva auto-eje.")
            )
          )
        )
      )
      (setq autoAxisEnabled (and useModel3dAxis hasProjectAxes autoAxisFamily projectPoints (listp projectPointNodeMap)))
    )
  )

  ;; --- DIBUJO DE EJES ---
  (setq axisXList '())
  (setq idx 0 dimXList '())

  (foreach node coordList
    (setq nID (car node) nX_m (/ (- (cadr node) startX) 100.0))
    (setq axisName "")
    (setq posType "Centrado")
    (setq drawX 0.0 extraX nil)
    (setq isSupportNode (or (= idx 0) (= idx (1- numNodes)) (member nID supportNodes)))
    (setq mappedPoint (if autoAxisEnabled (ocmema--mapped-point-for-node-index idx projectPointNodeMap projectPoints) nil))
    (setq autoAxisName (if mappedPoint (ocmema--auto-axis-name-for-index (cdr (assoc idx projectPointNodeMap)) projectPoints autoAxisFamily axisPairsX axisPairsY axisTolDraw) nil))
    (cond
      (autoAxisName
        (setq hasAxis "Si")
        (setq axisName autoAxisName)
      )
      ((and useModel3dAxis autoAxisEnabled mappedPoint)
        (setq hasAxis "No")
        (setq axisName "")
      )
      ((and useModel3dAxis autoAxisEnabled)
        (setq hasAxis "No")
        (setq axisName "__NO_RED__")
      )
      (useModel3dAxis
        (princ (strcat "\nNodo " (itoa nID) "..."))
        (initget "Si No") (setq hasAxis (getkword " Eje? [Si/No] <Si>: "))
        (if (null hasAxis) (setq hasAxis "Si"))
        (if (= hasAxis "Si") (setq axisName (getstring " Nombre: ")) (setq axisName ""))
      )
      (isSupportNode
        (princ (strcat "\nNodo " (itoa nID) (if (or (= idx 0) (= idx (1- numNodes))) " (apoyo extremo)..." " (apoyo)...")))
        (initget "Si No") (setq hasAxis (getkword " Eje? [Si/No] <Si>: "))
        (if (null hasAxis) (setq hasAxis "Si"))
        (if (= hasAxis "Si") (setq axisName (getstring " Nombre: ")) (setq axisName ""))
      )
      (T
        (setq hasAxis "No")
        (setq axisName "__NO_RED__")
      )
    )

    (if (and (not (= axisName "__NO_RED__")) (or (= idx 0) (= idx (1- numNodes))) (not autoAxisEnabled) (or isSupportNode useModel3dAxis autoAxisName))
      (progn
        (initget "Orillado Centrado") (setq posType (getkword " Pos? [Orillado/Centrado] <Centrado>: "))
        (if (null posType) (setq posType "Centrado"))
      )
    )
    (cond
      ((= idx 0)
       (if (= posType "Orillado") (progn (setq drawX (+ (car p1) 0.0)) (setq extraX (+ (car p1) 0.5))) (setq drawX (+ (car p1) 0.5)))
      )
      ((= idx (1- numNodes))
       (if (= posType "Orillado") (progn (setq drawX (+ (car p1) rectLen)) (setq extraX (+ (car p1) rectLen -0.5))) (setq drawX (+ (car p1) 0.5 nX_m)))
      )
      (t (setq drawX (+ (car p1) 0.5 nX_m)))
    )

    (setq axisCenter (+ (car p1) 0.5 nX_m))
    (if (not (= axisName "__NO_RED__"))
      (progn
        (setq axisXList (append axisXList (list axisCenter)))
        (setq dimXList (append dimXList (list axisCenter)))
        (setq pAxisBot (list drawX (- yOrigin 0.5)) pAxisTop (list drawX (+ yOrigin h_draw 1.25)))
        (command "_.LINE" pAxisBot pAxisTop "") (command "_.CHPROP" (entlast) "" "Color" 1 "Ltype" "CENTER" "Ltscale" 0.3 "")
        (if extraX (progn (command "_.LINE" (list extraX (- yOrigin 0.5)) (list extraX (+ yOrigin h_draw 1.25)) "") (command "_.CHPROP" (entlast) "" "Color" 1 "Ltype" "CENTER" "Ltscale" 0.3 "")))
        (if (= hasAxis "Si") (progn (setq centerCircle (list drawX (+ (cadr pAxisTop) 0.275))) (command "_.CIRCLE" centerCircle 0.275) (command "_.CHPROP" (entlast) "" "Color" 7 "") (command "_.TEXT" "_MC" centerCircle 0.22 0 axisName) (command "_.CHPROP" (entlast) "" "Color" 4 "")  ))
      )
    )
    (setq idx (1+ idx))
  )

  ;; Cotas generales arriba (como estaban)
  (setvar "DIMASZ" 0.15) (setvar "DIMCLRT" 40) (setvar "DIMTXT" 0.15) (setvar "DIMDEC" 2)
  (setq yTopRed (+ yOrigin h_draw 1.25) yDimLoc (- yTopRed 0.25) i 0)
  (repeat (1- (length dimXList))
     (command "_.DIMLINEAR" (list (nth i dimXList) yTopRed) (list (nth (1+ i) dimXList) yTopRed) (list (/ (+ (nth i dimXList) (nth (1+ i) dimXList)) 2.0) yDimLoc))
     (setq i (1+ i))
  )

  ;; ==============================================================================
  ;; 4. ACERO INFERIOR
  ;; ==============================================================================
  (setq areaBaseBot 71.0)
  (setq numBars 1) (setq strVar "3")
  (princ "\n--- ACERO INFERIOR ---")

  (if (> maxDemBot areaBaseBot)
    (progn
       (setq diff (- maxDemBot areaBaseBot))
       (princ (strcat "\nReq: " (rtos maxDemBot 2 2) " > Min: 71."))
       (if (< diff 50.0)
          (progn (initget "Longitudinal Baston Minimo") (setq ans (getkword " [Longitudinal/Baston/Minimo]: ")))
          (progn (initget "Longitudinal Baston") (setq ans (getkword " [Longitudinal/Baston]: ")))
       )
       (if (= ans "Minimo") (setq ans "No"))
       (if (= ans "Longitudinal")
         (progn (setq numBars (getint " Cant: ")) (setq strVar (getstring " Var: ")) (setq areaBaseBot 9999.0))
       )
    )
    (progn (setq ans "No") (princ "\nMinimo suficiente."))
  )

  (setq yBot (+ yOrigin 0.15))
  (setq x_ini_inf (+ (car p1) 0.15))
  (setq x_fin_inf (- (car p2) 0.15))

  (command "_.PLINE" (list x_ini_inf (+ yBot 0.30)) (list x_ini_inf yBot) (list x_fin_inf yBot) (list x_fin_inf (+ yBot 0.30)) "")
  (command "_.CHPROP" (entlast) "" "Color" 5 "")
  (command "_.FILLET" "P" (entlast))

  (dibujar-leader-blindado (list (/ (+ x_ini_inf x_fin_inf) 2.0) yBot) (strcat (itoa numBars) "#" strVar) nil nil)

  (if (and (> maxDemBot areaBaseBot) (= ans "Baston"))
    (progn
       (setq list_zonas_inf (procesar-zonas-v27 globalSteelList 71.0 (car p1) nil))
       (foreach zona list_zonas_inf
          (setq zoneStart (car zona))
          (setq zoneEnd (cadr zona))
          (setq maxReq (caddr zona))
          (if (> maxReq areaBaseBot)
             (progn
                (setq pts_fix (calc-puntos-v27 zoneStart zoneEnd axisXList))
                (setq x_baston_ini (car pts_fix))
                (setq x_baston_fin (cadr pts_fix))

                (if (< x_baston_ini x_ini_inf) (setq x_baston_ini x_ini_inf))
                (if (> x_baston_fin x_fin_inf) (setq x_baston_fin x_fin_inf))

                (command "_.PLINE"
                         (list x_baston_ini (+ yBot 0.45)) (list x_baston_ini (+ yBot 0.15))
                         (list x_baston_fin (+ yBot 0.15)) (list x_baston_fin (+ yBot 0.45)) "")
                (command "_.CHPROP" (entlast) "" "Color" 5 "")
                (command "_.FILLET" "P" (entlast))

                (setq axis_near_start (get-closest-axis x_baston_ini axisXList))
                (if axis_near_start
                   (command "_.DIMLINEAR" (list axis_near_start yOrigin) (list x_baston_ini yOrigin) (list (/ (+ axis_near_start x_baston_ini) 2.0) (- yOrigin 0.4)))
                )
                (setq axis_near_end (get-closest-axis x_baston_fin axisXList))
                (if axis_near_end
                   (command "_.DIMLINEAR" (list x_baston_fin yOrigin) (list axis_near_end yOrigin) (list (/ (+ x_baston_fin axis_near_end) 2.0) (- yOrigin 0.4)))
                )

                (princ (strcat "\nNuevo Baston Inf (Long " (rtos (- x_baston_fin x_baston_ini) 2 2) "m)"))
                (setq strVarBast (getstring "\nCalibre Varilla Baston (ej. 3): "))
                (if (= strVarBast "") (setq strVarBast "3"))

                (dibujar-leader-blindado (list (/ (+ x_baston_ini x_baston_fin) 2.0) (+ yBot 0.15)) (strcat "1#" strVarBast) nil nil)
             )
          )
       )
    )
  )

  ;; ==============================================================================
  ;; 5. ACERO SUPERIOR
  ;; ==============================================================================
  (princ "\n--- ACERO SUPERIOR ---")
  (setq yTop (+ yOrigin h_draw -0.15))
  (setq yDimSup (+ yTop 0.25))  ;; <-- AQUÍ: cota 0.25 arriba de la cara superior
  (setq areaBaseTop 71.0)

  (setq zonesTop (procesar-zonas-v27 globalSteelList 71.0 (car p1) T))

  (setq idx 0)
  (foreach axisX axisXList
     (setq is_active nil)
     (setq active_zone nil)

     (foreach z zonesTop
        (if (and (>= axisX (car z)) (<= axisX (cadr z)))
           (progn (setq is_active T) (setq active_zone z))
        )
     )

     (if is_active
        (progn
           (setq pts (calc-puntos-v27 (car active_zone) (cadr active_zone) axisXList))
           (setq x_start (car pts))
           (setq x_end (cadr pts))

           ;; Mostrar Req, Min y Dif (en cm2 reales)
           (setq reqAxis (obtener-req-en-x globalSteelList axisX (car p1) T))
           (setq diffAxis (- reqAxis areaBaseTop))
           (if (< diffAxis 0.0) (setq diffAxis 0.0))

           (setq reqAxis_cm2  (/ reqAxis 100.0))
           (setq diffAxis_cm2 (/ diffAxis 100.0))

           (princ
             (strcat
               "\nApoyo " (itoa (1+ idx))
               "  Req=" (rtos reqAxis_cm2 2 3)
               "  Min=0.710"
               "  Dif=" (rtos diffAxis_cm2 2 3)
             )
           )

           (setq cant (getint " Cant: "))
           (setq num  (getint " Num: "))
           (dibujar-leader-blindado (list axisX yTop) (strcat (itoa cant) "#" (itoa num)) T (= idx (1- numNodes)))
        )
        (progn
           (setq x_start (- axisX 0.35))
           (setq x_end (+ axisX 0.35))
           (if (= idx 0) (setq x_start (+ (car p1) 0.15)))
           (if (= idx (1- numNodes)) (setq x_end (- (car p2) 0.15)))
           (dibujar-leader-blindado (list axisX yTop) "1#3" T (= idx (1- numNodes)))
        )
     )

     ;; DIBUJO GEOMETRIA SUPERIOR (PLINE INTACTO) + COTAS SUPERIORES CORREGIDAS
     (cond
        ((= idx 0)
           (setq p_mid (list (+ (car p1) 0.15) yTop))
           (setq p_join (list x_end yTop))
           (setq p_crank (list (+ x_end 0.85) (- yTop 0.6)))
           (setq p_tail (list (+ x_end 0.85 0.25) (- yTop 0.6)))

           (command "_.PLINE" (list (+ (car p1) 0.15) (- yTop 0.30)) p_mid p_join "")
           (command "_.CHPROP" (entlast) "" "Color" 5 "")
           (command "_.FILLET" "P" (entlast))

           (command "_.PLINE" p_join p_crank p_tail "")
           (command "_.CHPROP" (entlast) "" "Color" 5 "")

           ;; Cota horizontal: (axisX,yTop) -> (x_end,yTop), ubicada en yTop+0.25
           (command "_.DIMLINEAR"
                    (list axisX yTop)
                    (list (car p_join) yTop)
                    (list (/ (+ axisX (car p_join)) 2.0) yDimSup))
        )

        ((= idx (1- numNodes))
           (setq p_tail (list (- x_start 0.85 0.25) (- yTop 0.6)))
           (setq p_crank (list (- x_start 0.85) (- yTop 0.6)))
           (setq p_join (list x_start yTop))

           (command "_.PLINE" p_tail p_crank p_join "")
           (command "_.CHPROP" (entlast) "" "Color" 5 "")

           (command "_.PLINE" p_join (list (- (car p2) 0.15) yTop) (list (- (car p2) 0.15) (- yTop 0.30)) "")
           (command "_.CHPROP" (entlast) "" "Color" 5 "")
           (command "_.FILLET" "P" (entlast))

           ;; Cota horizontal: (x_start,yTop) -> (axisX,yTop)
           (command "_.DIMLINEAR"
                    (list (car p_join) yTop)
                    (list axisX yTop)
                    (list (/ (+ (car p_join) axisX) 2.0) yDimSup))
        )

        (t
           (setq p_tail_izq (list (- x_start 0.85 0.25) (- yTop 0.6)))
           (setq p_crank_izq (list (- x_start 0.85) (- yTop 0.6)))
           (setq p_join_izq (list x_start yTop))
           (setq p_join_der (list x_end yTop))
           (setq p_crank_der (list (+ x_end 0.85) (- yTop 0.6)))
           (setq p_tail_der (list (+ x_end 0.85 0.25) (- yTop 0.6)))

           (command "_.PLINE" p_tail_izq p_crank_izq p_join_izq p_join_der p_crank_der p_tail_der "")
           (command "_.CHPROP" (entlast) "" "Color" 5 "")

           ;; IZQ: (x_start,yTop) -> (axisX,yTop)
           (command "_.DIMLINEAR"
                    (list (car p_join_izq) yTop)
                    (list axisX yTop)
                    (list (/ (+ (car p_join_izq) axisX) 2.0) yDimSup))

           ;; DER: (axisX,yTop) -> (x_end,yTop)
           (command "_.DIMLINEAR"
                    (list axisX yTop)
                    (list (car p_join_der) yTop)
                    (list (/ (+ axisX (car p_join_der)) 2.0) yDimSup))
        )
     )

     (setq idx (1+ idx))
  )

  (setvar "DIMASZ" old_dimasz) (setvar "DIMCLRD" old_dimclrd)
  (setvar "DIMTXT" old_dimtxt) (setvar "DIMCLRT" old_dimclrt) (setvar "DIMSCALE" old_dimscale)
  (setvar "OSMODE" oldOsnap)
  (princ "\nGeneracion completada.")
  (princ)
)

;; Wrapper interactivo: mantiene UX y habilita batch desde OCMEMA_PROJECT_IO
(defun c:DIBUJAR_NERV (/ anl)
  (setq anl (getfiled
              "Seleccionar archivo STAAD"
              "C:\\Users\\edgar\\OneDrive - ITESO\\OCMEMA_IE\\01. PROYECTOS\\"
              "ANL;TXT;OUT"
              4
            ))
  (if anl
    (ocmema:nerv:armar-from-anl anl)
  )
  (princ)
)

(princ)

