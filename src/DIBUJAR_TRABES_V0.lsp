;; ==============================================================================
;; DIBUJAR_TRABE_V13.LSP
;; BASE: V12.
;; CORRECCION FINAL GANCHOS:
;;  - Se calculan respecto al CENTRO DE LA VARILLA (no del filete).
;;  - Se aplica un offset de 1.1 * RadioVarilla desde el punto tangente.
;;  - Esto garantiza que el gancho nazca fuera del volumen visual de la dona.
;; ==============================================================================

(defun c:DIBUJAR_TRABE_V13 (/ *error* file dir filename tempFile wsh cmd fp line 
                            coordList YD ZB startX endX l_total_cm l_total_m h_cm b_cm 
                            h_draw rectLen ptOrigin xOrigin yOrigin oldOsnap p1 p2 
                            idx numNodes hasAxis axisName posType drawX extraX 
                            pAxisBot pAxisTop centerCircle axisXList dimXList axisCenter
                            dataMode strList valX nodeID
                            tokenList i len token currentDist xOffsetGlobal 
                            globalSteelList
                            valAstTop valAstBot maxDemBot maxDemTop 
                            ;; Variables Trabe
                            fc_val asMinFormula asBaseTop asBaseBot strVarBaseTop strVarBaseBot
                            numBaseTop numBaseBot areaBarBot areaBarTop qtyRecBot qtyRecTop
                            ans numBars strVar row zoneStart zoneEnd req d strVarBast 
                            yBot yTop xDrawStart xDrawEnd 
                            pts pTail pCrank pJoin foundBot j
                            nID nX_m maxDistFound
                            posTypeSteel valAst cleanLine
                            redondear-al-5 get-closest-axis dibujar-leader-blindado
                            obtener-x-interpolada procesar-zonas-v27 calc-puntos-v27
                            obtener-area-varilla obtener-diametro-real
                            x_ini_inf x_fin_inf axis_near_start axis_near_end
                            x_baston_ini x_baston_fin
                            dist_izq dist_der scan_x found
                            p_start p_end p_crank_start p_crank_end
                            usar_minimo len_final_izq len_final_der
                            old_dimasz old_dimclrd old_dimtxt old_dimclrt old_dimscale
                            list_zonas_inf list_zonas_sup zona
                            memberOffsetList mIndex pos k
                            areaBastOne qtyBastRec numBast deficit
                            bastonesInfList bastonesSupList
                            numZonesEstribos lenZoneEst s_estribo qtyEstribos limitEstribos
                            yTextEstribos txtEstribo pTextEst d_eff
                            addCamber numCamber locCamber xCamber pCamber yCamber strCamber
                            ptSectionOrigin xSec ySec h_losa typeLosa alignLosa capaCompresion
                            rec_draw b_draw h_draw_sec p1_losa p2_losa p3_losa p4_losa
                            draw-stirrup-hooks draw-break-line
                            critBastInf critBastSup
                            process-layer-steel get-critical-bast
                            widthName
                            radFillet pH1 pH2 rBarTop xStirrupLeft yStirrupTop
                            offsetHookFactor centerBarX centerBarY
                            )

  (vl-load-com)

  ;; --- 1. FUNCIONES AUXILIARES ---

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

  ;; --- MOTOR DE ANALISIS (V27) ---
  (defun procesar-zonas-v27 (steelList limitMin x_origin_draw is_top / idx_val base ptsCorr zonas inZone startXZone endXZone maxReqZone i prev_x prev_req curr_x curr_req first_req last_x)
    (setq idx_val (if is_top 1 2)) (setq base (+ x_origin_draw 0.5))
    (setq ptsCorr '())
    (foreach pt steelList (if (and (numberp (car pt)) (numberp (nth idx_val pt))) (setq ptsCorr (append ptsCorr (list (list (+ base (car pt)) (nth idx_val pt)))))))
    (setq ptsCorr (vl-sort ptsCorr '(lambda (a b) (< (car a) (car b)))))
    (setq zonas '())
    (if (< (length ptsCorr) 2) zonas
      (progn
        (setq inZone nil startXZone nil endXZone nil maxReqZone 0.0)
        (setq first_req (cadr (car ptsCorr)))
        (if (> first_req limitMin) (progn (setq inZone T) (setq startXZone (car (car ptsCorr))) (setq maxReqZone first_req)))
        (setq i 1)
        (while (< i (length ptsCorr))
          (setq prev_x (car (nth (1- i) ptsCorr))) (setq prev_req (cadr (nth (1- i) ptsCorr)))
          (setq curr_x (car (nth i ptsCorr))) (setq curr_req (cadr (nth i ptsCorr)))
          (if (and (not inZone) (<= prev_req limitMin) (> curr_req limitMin)) (progn (setq inZone T) (setq startXZone (obtener-x-interpolada prev_x prev_req curr_x curr_req limitMin)) (setq maxReqZone (max prev_req curr_req))))
          (if inZone (setq maxReqZone (max maxReqZone curr_req)))
          (if (and inZone (> prev_req limitMin) (<= curr_req limitMin)) (progn (setq endXZone (obtener-x-interpolada prev_x prev_req curr_x curr_req limitMin)) (setq zonas (append zonas (list (list startXZone endXZone maxReqZone)))) (setq inZone nil startXZone nil endXZone nil maxReqZone 0.0)))
          (setq i (1+ i))
        )
        (if inZone (progn (setq last_x (car (last ptsCorr))) (setq zonas (append zonas (list (list startXZone last_x maxReqZone))))))
        zonas
      )
    )
  )

  (defun calc-puntos-v27 (x_interp_start x_interp_end axis_list / t_start t_end ax_s ax_e dist_s dist_e round_s round_e final_s final_e tmp)
    (setq t_start (- x_interp_start 0.20)) (setq t_end (+ x_interp_end 0.20))
    (setq ax_s (get-closest-axis t_start axis_list))
    (if ax_s (progn (setq dist_s (abs (- t_start ax_s))) (setq round_s (redondear-al-5 dist_s)) (setq final_s (if (< t_start ax_s) (- ax_s round_s) (+ ax_s round_s)))) (setq final_s t_start))
    (setq ax_e (get-closest-axis t_end axis_list))
    (if ax_e (progn (setq dist_e (abs (- t_end ax_e))) (setq round_e (redondear-al-5 dist_e)) (setq final_e (if (< t_end ax_e) (- ax_e round_e) (+ ax_e round_e)))) (setq final_e t_end))
    (if (> final_s final_e) (progn (setq tmp final_s) (setq final_s final_e) (setq final_e tmp)))
    (list final_s final_e)
  )

  (defun dibujar-leader-blindado (pt1 texto es_top es_final / sgn_y sgn_x pt2 pt3 obj_text obj_leader)
     (setvar "DIMCLRD" 7) (setvar "DIMASZ" 0.15) (setvar "DIMSCALE" 1.0)
     (setq sgn_y (if es_top -1.0 1.0)) (setq sgn_x (if es_final -1.0 1.0))
     (setq pt2 (list (+ (car pt1) (* 0.20 sgn_x)) (+ (cadr pt1) (* 0.35 sgn_y))))
     (setq pt3 (list (+ (car pt2) (* 0.15 sgn_x)) (cadr pt2)))
     (command "_.LEADER" pt1 pt2 pt3 "" "" "N")
     (if (setq obj_leader (vlax-ename->vla-object (entlast))) (vl-catch-all-apply 'vla-put-Color (list obj_leader 7)))
     (if (< (car pt3) (car pt2)) (command "_.TEXT" "_J" "_MR" pt3 0.15 0 texto) (command "_.TEXT" "_J" "_ML" pt3 0.15 0 texto))
     (setq obj_text (vlax-ename->vla-object (entlast))) (vl-catch-all-apply 'vla-put-Color (list obj_text 40)) 
  )

  (defun clean-string (s) (vl-string-translate "|" " " s))
  (defun *error* (msg) (if oldOsnap (setvar "OSMODE" oldOsnap)) (if (and fp (= (type fp) 'FILE)) (close fp)) (princ))

  (setq old_dimasz (getvar "DIMASZ")) (setq old_dimclrd (getvar "DIMCLRD")) (setq old_dimtxt (getvar "DIMTXT")) (setq old_dimclrt (getvar "DIMCLRT")) (setq old_dimscale (getvar "DIMSCALE"))
  (setq oldOsnap (getvar "OSMODE")) (setvar "OSMODE" 0) (setvar "CMDECHO" 0) (setvar "FILLETRAD" 0.05)
  (if (not (tblsearch "LTYPE" "CENTER")) (command "-LINETYPE" "Load" "CENTER" "acad.lin" ""))

  ;; ==============================================================================
  ;; 2. LECTURA DE DATOS
  ;; ==============================================================================
  (setq file (getfiled "Seleccionar archivo TRABE (ANL)" "" "ANL;TXT;OUT" 4))
  (if (not file) (exit))
  (setq filename (vl-filename-base file)) (princ "\n1. Leyendo Geometria y Materiales...")
  (setq fp (open file "r"))
  (setq coordList nil YD 25.0 ZB 10.0 dataMode nil fc_val 200.0)

  (while (setq line (read-line fp))
    (setq line (vl-string-trim " \t" line))
    (cond
      ((wcmatch line "* FC *") (setq cleanLine (clean-string line)) (setq tokenList (read (strcat "(" cleanLine ")"))) (setq k 0)
         (while (< k (length tokenList)) (if (and (eq (nth k tokenList) 'FC) (numberp (nth (1+ k) tokenList))) (setq fc_val (nth (1+ k) tokenList))) (setq k (1+ k))))
      ((or (wcmatch line "*JOINT COORDINATES*") (wcmatch line "*COORDENADAS*")) (setq dataMode "NODES"))
      ((and (= dataMode "NODES") (or (wcmatch line "*MEMBER INCIDENCES*") (wcmatch line "*INCIDENCIAS*"))) (setq dataMode nil))
      ((and (= dataMode "NODES") (> (strlen line) 0)) (if (numberp (read (substr line 1 1))) (progn (setq strList (read (strcat "(" (vl-string-translate "." " " line) ")"))) (setq valX nil nodeID nil) (cond ((>= (length strList) 5) (setq nodeID (nth 1 strList)) (setq valX (nth 2 strList))) ((= (length strList) 4) (setq nodeID (nth 0 strList)) (setq valX (nth 1 strList)))) (if (numberp valX) (setq coordList (append coordList (list (list nodeID valX))))))))
      ((wcmatch line "*MEMBER PROPERTY*") (setq dataMode "PROPS"))
      ((and (= dataMode "PROPS") (wcmatch line "*PRIS*")) (if (setq pos (vl-string-search "YD" line)) (setq YD (atof (substr line (+ pos 4) 5)))) (if (setq pos (vl-string-search "ZB" line)) (setq ZB (atof (substr line (+ pos 4) 5))) (if (setq pos (vl-string-search "ZD" line)) (setq ZB (atof (substr line (+ pos 4) 5))))) (setq dataMode nil))
    )
  )
  (close fp)

  (if (null coordList) (progn (alert "No se pudo leer la geometria.") (exit)))
  (setq coordList (vl-sort coordList '(lambda (a b) (< (cadr a) (cadr b)))))
  (setq startX (cadr (nth 0 coordList))) (setq endX (cadr (last coordList)))
  (setq l_total_cm (- endX startX)) (setq l_total_m (/ l_total_cm 100.0)) (setq h_cm YD b_cm ZB)
  (setq h_draw (* (/ h_cm 100.0) 7.0)) (setq rectLen (+ l_total_m 1.0))

  (setq memberOffsetList '())
  (foreach node coordList (setq memberOffsetList (append memberOffsetList (list (/ (- (cadr node) startX) 100.0)))))
  (if (> (length memberOffsetList) 0) (setq memberOffsetList (reverse (cdr (reverse memberOffsetList)))))

  (princ "\n2. Procesando Acero...")
  (setq dir (vl-filename-directory file)) (setq tempFile (strcat dir "\\" filename "_TEMP_ANSI.TXT"))
  (setq wsh (vlax-create-object "WScript.Shell"))
  (setq cmd (strcat "powershell -NoProfile -Command \"Get-Content -LiteralPath '" file "' | Set-Content -Encoding Ascii '" tempFile "'\""))
  (vlax-invoke wsh 'Run cmd 0 :vlax-true) (vlax-release-object wsh)

  (setq fp (open tempFile "r"))
  (setq globalSteelList '() xOffsetGlobal 0.0) (setq isReadingSteel nil savedDist nil) (setq astTop 0.0 astBot 0.0) (setq mIndex 0)

  (while (setq line (read-line fp))
    (setq cleanLine (clean-string line)) 
    (if (wcmatch line "*LONGITUDINAL BAR DETAILS*") (setq isReadingSteel T savedDist nil))
    (if (and isReadingSteel (wcmatch line "*LONGITUDINAL BAR LAYOUT*")) (setq isReadingSteel nil))
    (if (wcmatch line "*Member *:*") (progn (setq mIndex (1+ mIndex)) (setq savedDist nil) (if (and memberOffsetList (<= mIndex (length memberOffsetList))) (setq xOffsetGlobal (nth (1- mIndex) memberOffsetList)) (if (> (length globalSteelList) 0) (setq xOffsetGlobal (car (last globalSteelList))) (setq xOffsetGlobal 0.0)))))
    (if (and isReadingSteel (> (strlen cleanLine) 5) (not (wcmatch line "*Distance*")) (not (wcmatch line "*-----*")))
      (progn (setq tokenList (read (strcat "(" cleanLine ")")))
        (cond ((numberp (car tokenList)) (if savedDist (setq globalSteelList (append globalSteelList (list (list (+ xOffsetGlobal savedDist) astTop astBot))))) (setq savedDist (/ (nth 0 tokenList) 100.0)) (setq valAst (nth 2 tokenList)) (if (eq (nth 1 tokenList) 'Top) (setq astTop valAst) (setq astBot valAst)) (if (eq (nth 1 tokenList) 'Top) (setq astBot 0.0) (setq astTop 0.0)))
              ((or (eq (car tokenList) 'Top) (eq (car tokenList) 'Bottom)) (if savedDist (progn (setq valAst (nth 1 tokenList)) (if (eq (car tokenList) 'Top) (setq astTop valAst)) (if (eq (car tokenList) 'Bottom) (setq astBot valAst))))))))
  )
  (if savedDist (setq globalSteelList (append globalSteelList (list (list (+ xOffsetGlobal savedDist) astTop astBot)))))
  (close fp) (vl-file-delete tempFile)
  (setq globalSteelList (mapcar '(lambda (pt) (list (car pt) (cadr pt) (caddr pt))) globalSteelList))
  (setq maxDemBot 0.0 maxDemTop 0.0) (foreach pt globalSteelList (setq maxDemBot (max maxDemBot (caddr pt))) (setq maxDemTop (max maxDemTop (cadr pt))))

  ;; 3. DIBUJO BASE
  (princ "\nSeleccione punto de origen: ") (setq ptOrigin (getpoint)) (if (not ptOrigin) (exit)) (setq xOrigin (car ptOrigin) yOrigin (cadr ptOrigin))
  
  (command "_.TEXT" "_J" "_TL" (list xOrigin (+ yOrigin 0.33)) 0.25 0 (strcat "h=" (rtos h_cm 2 0))) (command "_.CHPROP" (entlast) "" "Color" 3 "")
  (command "_.TEXT" "_J" "_TL" (list xOrigin (+ yOrigin 0.86)) 0.25 0 (strcat "b=" (rtos b_cm 2 0))) (command "_.CHPROP" (entlast) "" "Color" 3 "")
  (command "_.TEXT" "_J" "_TL" (list xOrigin (+ yOrigin h_draw)) 0.35 0 filename) (command "_.CHPROP" (entlast) "" "Color" 4 "")
  
  (setq widthName (* (strlen filename) 0.28))
  (setq p1 (list (+ xOrigin widthName 0.3) yOrigin)) 
  (setq p2 (list (+ (car p1) rectLen) (+ yOrigin h_draw)))
  (command "_.RECTANG" p1 p2) (command "_.CHPROP" (entlast) "" "Color" 7 "")

  (setq axisXList '()) (setq idx 0 numNodes (length coordList) dimXList '())
  (foreach node coordList
    (setq nID (car node) nX_m (/ (- (cadr node) startX) 100.0))
    (setq hasAxis "Si") (setq axisName (strcat "E-" (itoa (1+ idx)))) 
    (setq drawX 0.0 extraX nil)
    (cond ((= idx 0) (setq drawX (+ (car p1) 0.5))) ((= idx (1- numNodes)) (setq drawX (+ (car p1) 0.5 nX_m))) (t (setq drawX (+ (car p1) 0.5 nX_m))))
    (setq axisCenter (+ (car p1) 0.5 nX_m)) (setq axisXList (append axisXList (list axisCenter))) (setq dimXList (append dimXList (list axisCenter)))
    (setq pAxisBot (list drawX (- yOrigin 0.5)) pAxisTop (list drawX (+ yOrigin h_draw 1.25)))
    (command "_.LINE" pAxisBot pAxisTop "") (command "_.CHPROP" (entlast) "" "Color" 1 "Ltype" "CENTER" "Ltscale" 0.3 "")
    (if extraX (progn (command "_.LINE" (list extraX (- yOrigin 0.5)) (list extraX (+ yOrigin h_draw 1.25)) "") (command "_.CHPROP" (entlast) "" "Color" 1 "Ltype" "CENTER" "Ltscale" 0.3 "")))
    (if (= hasAxis "Si") (progn (setq centerCircle (list drawX (+ (cadr pAxisTop) 0.275))) (command "_.CIRCLE" centerCircle 0.275) (command "_.CHPROP" (entlast) "" "Color" 7 "") (command "_.TEXT" "_MC" centerCircle 0.22 0 (itoa (1+ idx))) (command "_.CHPROP" (entlast) "" "Color" 4 "")))
    (setq idx (1+ idx))
  )
  (setq yTopRed (+ yOrigin h_draw 1.25) yDimLoc (- yTopRed 0.25) i 0)
  (repeat (1- (length dimXList)) (command "_.DIMLINEAR" (list (nth i dimXList) yTopRed) (list (nth (1+ i) dimXList) yTopRed) (list (/ (+ (nth i dimXList) (nth (1+ i) dimXList)) 2.0) yDimLoc)) (setq i (1+ i)))

  (setq asMinFormula (* (/ (* 0.8 (sqrt fc_val)) 4200.0) b_cm (- h_cm 3.0)))
  (princ (strcat "\n\n*** F'c: " (rtos fc_val 2 0) " | As Min (ACI): " (rtos asMinFormula 2 3) " ***"))

  ;; 4. ACERO INFERIOR
  (princ "\n--- ACERO INFERIOR ---") (setq yBot (+ yOrigin 0.15)) (setq x_ini_inf (+ (car p1) 0.15)) (setq x_fin_inf (- (car p2) 0.15))
  (setq strVarBaseBot (getstring "\nNumero varilla Base Inf (ej. 3, 4): ")) (setq areaBarBot (obtener-area-varilla strVarBaseBot))
  (if (> areaBarBot 0.0) (progn (setq qtyRecBot (fix (+ (/ asMinFormula areaBarBot) 0.9999))) (princ (strcat "\n-> Sugerido: " (itoa qtyRecBot) " varillas.")) (setq numBaseBot (getint (strcat "\nCantidad definitiva <" (itoa qtyRecBot) ">: "))) (if (null numBaseBot) (setq numBaseBot qtyRecBot))) (progn (setq numBaseBot 0) (princ "\nVarilla 0.")))
  (setq asBaseBot (* numBaseBot areaBarBot))
  (command "_.PLINE" (list x_ini_inf (+ yBot 0.30)) (list x_ini_inf yBot) (list x_fin_inf yBot) (list x_fin_inf (+ yBot 0.30)) "") (command "_.CHPROP" (entlast) "" "Color" 5 "") (command "_.FILLET" "P" (entlast))
  (dibujar-leader-blindado (list (/ (+ x_ini_inf x_fin_inf) 2.0) yBot) (strcat (itoa numBaseBot) "#" strVarBaseBot) nil nil)

  (setq bastonesInfList '())
  (if (> maxDemBot asBaseBot)
    (progn (setq list_zonas_inf (procesar-zonas-v27 globalSteelList asBaseBot (car p1) nil))
       (foreach zona list_zonas_inf (setq zoneStart (car zona)) (setq zoneEnd (cadr zona)) (setq maxReq (caddr zona))
          (if (> maxReq asBaseBot) (progn (setq pts_fix (calc-puntos-v27 zoneStart zoneEnd axisXList)) (setq x_baston_ini (car pts_fix)) (setq x_baston_fin (cadr pts_fix))
                (if (< x_baston_ini x_ini_inf) (setq x_baston_ini x_ini_inf)) (if (> x_baston_fin x_fin_inf) (setq x_baston_fin x_fin_inf))
                (command "_.PLINE" (list x_baston_ini (+ yBot 0.45)) (list x_baston_ini (+ yBot 0.15)) (list x_baston_fin (+ yBot 0.15)) (list x_baston_fin (+ yBot 0.45)) "") (command "_.CHPROP" (entlast) "" "Color" 5 "") (command "_.FILLET" "P" (entlast))
                (setq axis_near_start (get-closest-axis x_baston_ini axisXList)) (if axis_near_start (command "_.DIMLINEAR" (list axis_near_start yOrigin) (list x_baston_ini yOrigin) (list (/ (+ axis_near_start x_baston_ini) 2.0) (- yOrigin 0.4))))
                (setq axis_near_end (get-closest-axis x_baston_fin axisXList)) (if axis_near_end (command "_.DIMLINEAR" (list x_baston_fin yOrigin) (list axis_near_end yOrigin) (list (/ (+ x_baston_fin axis_near_end) 2.0) (- yOrigin 0.4))))
                (setq deficit (- maxReq asBaseBot)) (princ (strcat "\nBaston Inf Req: " (rtos deficit 2 2) " cm2. Long: " (rtos (- x_baston_fin x_baston_ini) 2 2) "m"))
                (setq strVarBast (getstring "\nCalibre Baston: ")) (setq areaBastOne (obtener-area-varilla strVarBast))
                (if (> areaBastOne 0.0) (progn (setq qtyBastRec (fix (+ (/ deficit areaBastOne) 0.9999))) (princ (strcat " Sugerido: " (itoa qtyBastRec))) (setq numBast (getint "\nCantidad definitiva: ")) (if (null numBast) (setq numBast qtyBastRec))) (setq numBast 1))
                (dibujar-leader-blindado (list (/ (+ x_baston_ini x_baston_fin) 2.0) (+ yBot 0.15)) (strcat (itoa numBast) "#" strVarBast) nil nil)
                (setq bastonesInfList (append bastonesInfList (list (list strVarBast numBast))))
    ))))
    (princ (strcat "\n>>> Acero base cubre demanda Inf."))
  )

  ;; 5. ACERO SUPERIOR
  (princ "\n--- ACERO SUPERIOR ---") (setq yTop (+ yOrigin h_draw -0.15))
  (setq strVarBaseTop (getstring "\nNumero varilla Base Sup (ej. 3, 4): ")) (setq areaBarTop (obtener-area-varilla strVarBaseTop))
  (if (> areaBarTop 0.0) (progn (setq qtyRecTop (fix (+ (/ asMinFormula areaBarTop) 0.9999))) (princ (strcat "\n-> Sugerido: " (itoa qtyRecTop) " varillas.")) (setq numBaseTop (getint (strcat "\nCantidad definitiva <" (itoa qtyRecTop) ">: "))) (if (null numBaseTop) (setq numBaseTop qtyRecTop))) (progn (setq numBaseTop 0) (princ "\nVarilla 0.")))
  (setq asBaseTop (* numBaseTop areaBarTop))
  (command "_.PLINE" (list x_ini_inf (- yTop 0.30)) (list x_ini_inf yTop) (list x_fin_inf yTop) (list x_fin_inf (- yTop 0.30)) "") (command "_.CHPROP" (entlast) "" "Color" 5 "") (command "_.FILLET" "P" (entlast))
  (dibujar-leader-blindado (list (/ (+ x_ini_inf x_fin_inf) 2.0) yTop) (strcat (itoa numBaseTop) "#" strVarBaseTop) T nil)

  (setq bastonesSupList '())
  (if (> maxDemTop asBaseTop)
    (progn (setq list_zonas_sup (procesar-zonas-v27 globalSteelList asBaseTop (car p1) nil))
       (foreach zona list_zonas_sup (setq zoneStart (car zona)) (setq zoneEnd (cadr zona)) (setq maxReq (caddr zona))
          (if (> maxReq asBaseTop) (progn (setq pts_fix (calc-puntos-v27 zoneStart zoneEnd axisXList)) (setq x_baston_ini (car pts_fix)) (setq x_baston_fin (cadr pts_fix))
                (if (< x_baston_ini x_ini_inf) (setq x_baston_ini x_ini_inf)) (if (> x_baston_fin x_fin_inf) (setq x_baston_fin x_fin_inf))
                (command "_.PLINE" (list x_baston_ini (- yTop 0.45)) (list x_baston_ini (- yTop 0.15)) (list x_baston_fin (- yTop 0.15)) (list x_baston_fin (- yTop 0.45)) "") (command "_.CHPROP" (entlast) "" "Color" 5 "") (command "_.FILLET" "P" (entlast))
                (setq axis_near_start (get-closest-axis x_baston_ini axisXList)) (if axis_near_start (command "_.DIMLINEAR" (list axis_near_start yOrigin) (list x_baston_ini yOrigin) (list (/ (+ axis_near_start x_baston_ini) 2.0) (+ yTop 0.4))))
                (setq axis_near_end (get-closest-axis x_baston_fin axisXList)) (if axis_near_end (command "_.DIMLINEAR" (list x_baston_fin yOrigin) (list axis_near_end yOrigin) (list (/ (+ x_baston_fin axis_near_end) 2.0) (+ yTop 0.4))))
                (setq deficit (- maxReq asBaseTop)) (princ (strcat "\nBaston Sup Req: " (rtos deficit 2 2) " cm2. Long: " (rtos (- x_baston_fin x_baston_ini) 2 2) "m"))
                (setq strVarBast (getstring "\nCalibre Baston: ")) (setq areaBastOne (obtener-area-varilla strVarBast))
                (if (> areaBastOne 0.0) (progn (setq qtyBastRec (fix (+ (/ deficit areaBastOne) 0.9999))) (princ (strcat " Sugerido: " (itoa qtyBastRec))) (setq numBast (getint "\nCantidad definitiva: ")) (if (null numBast) (setq numBast qtyBastRec))) (setq numBast 1))
                (dibujar-leader-blindado (list (/ (+ x_baston_ini x_baston_fin) 2.0) (- yTop 0.15)) (strcat (itoa numBast) "#" strVarBast) T nil)
                (setq bastonesSupList (append bastonesSupList (list (list strVarBast numBast))))
    ))))
    (princ (strcat "\n>>> Acero base cubre demanda Sup."))
  )

  ;; 6. ESTRIBOS (CORTANTE)
  (princ "\n--- ESTRIBOS ---")
  (setq d_eff (- h_cm 3.0)) (setq limitEstribos (max 20.0 (/ d_eff 2.0)))
  (setq yTextEstribos (- yOrigin 1.6))
  (command "_.TEXT" "_J" "_ML" (list xOrigin yTextEstribos) 0.25 0 "E#3") (command "_.CHPROP" (entlast) "" "Color" 3 "")
  (setq numZonesEstribos (getint "\nNumero de zonas de estribos (1 para uniforme): ")) (if (null numZonesEstribos) (setq numZonesEstribos 1))
  (setq currentDist 0.0) (setq i 1)
  (repeat numZonesEstribos
     (if (= numZonesEstribos 1) (setq lenZoneEst l_total_m) (setq lenZoneEst (getreal (strcat "\nLongitud Zona " (itoa i) " (m): "))))
     (setq s_estribo (getreal (strcat "Separacion Zona " (itoa i) " (cm): ")))
     (if (> s_estribo limitEstribos) (princ (strcat "\n** AVISO: Sep " (rtos s_estribo 2 0) " > Limite " (rtos limitEstribos 2 0) " **")))
     (setq qtyEstribos (+ (fix (+ (/ (* lenZoneEst 100.0) s_estribo) 0.99)) 1))
     (setq txtEstribo (strcat (itoa qtyEstribos) "@" (rtos s_estribo 2 0)))
     (setq pTextEst (list (+ (car p1) 0.5 currentDist (/ lenZoneEst 2.0)) yTextEstribos))
     (command "_.TEXT" "_J" "_MC" pTextEst 0.15 0 txtEstribo) (command "_.CHPROP" (entlast) "" "Color" 8 "")
     (setq currentDist (+ currentDist lenZoneEst)) (setq i (1+ i))
  )

  ;; 7. CONTRAFLECHA (Correccion Y = -0.65)
  (initget "Si No") (setq addCamber (getkword "\nAgregar contraflecha? [Si/No] <No>: "))
  (if (= addCamber "Si")
     (progn (setq numCamber (getint "\nCuantas contraflechas?: "))
        (repeat numCamber
           (initget "Centro Distancia") (setq locCamber (getkword "\nUbicacion? [Centro de Claro / Distancia]: "))
           (if (= locCamber "Centro")
              (progn (setq i (getint "Numero de claro (1, 2...): ")) (setq xCamber (/ (+ (nth (1- i) axisXList) (nth i axisXList)) 2.0)))
              (setq xCamber (+ (car p1) 0.5 (getreal "Distancia desde origen (m): ")))
           )
           (setq strCamber (getstring "\nValor Contraflecha (ej. 1.5): "))
           (setq yCamber (- yOrigin 0.65)) 
           
           (setq deltaW 0.16) (setq gap 0.05) (setq textW (* (strlen (strcat "=" strCamber "cm")) 0.12))
           (setq totalW (+ deltaW gap textW))
           (setq xDelta (- xCamber (/ totalW 2.0)))
           
           (command "_.PLINE" (list xDelta yCamber) (list (- xDelta 0.08) (- yCamber 0.15)) (list (+ xDelta 0.08) (- yCamber 0.15)) "C")
           (command "_.CHPROP" (entlast) "" "Color" 40 "")
           (command "_.TEXT" "_J" "_ML" (list (+ xDelta 0.08 gap) (- yCamber 0.075)) 0.15 0 (strcat "=" strCamber "cm")) (command "_.CHPROP" (entlast) "" "Color" 40 "")
        )
     )
  )

  ;; 8. SECCION TRANSVERSAL
  (princ "\n--- SECCION TRANSVERSAL ---")
  (setq ptSectionOrigin (list (+ (car p2) 1.5) yOrigin)) 
  (setq xSec (car ptSectionOrigin) ySec (cadr ptSectionOrigin))
  (setq b_draw (* (/ b_cm 100.0) 7.0)) (setq h_draw_sec (* (/ h_cm 100.0) 7.0))
  
  (command "_.RECTANG" "_F" 0 ptSectionOrigin (list (+ xSec b_draw) (+ ySec h_draw_sec))) (command "_.CHPROP" (entlast) "" "Color" 7 "")

  ;; Losa (Zig Zag Recto)
  (initget "Si No") (setq hasLosa (getkword "\nLleva Losa? [Si/No] <No>: "))
  (if (= hasLosa "Si")
    (progn
      (initget "Izq Der Ambos") (setq typeLosa (getkword "Lado? [Izq/Der/Ambos]: "))
      (setq capaCompresion (* (/ (getreal "Capa Compresion (cm): ") 100.0) 7.0))
      (setq h_losa (* (/ (getreal "Altura Total Losa (cm): ") 100.0) 7.0))
      (setq alignLosa "Arriba")
      (if (not (equal h_draw_sec h_losa 0.01)) (progn (initget "Arriba Abajo Centro") (setq alignLosa (getkword "Alineacion Losa? [Arriba/Abajo/Centro]: "))))
      (setq yLosaStart (+ ySec h_draw_sec))
      (if (= alignLosa "Abajo") (setq yLosaStart (+ ySec h_losa)))
      (if (= alignLosa "Centro") (setq yLosaStart (+ ySec (/ h_draw_sec 2.0) (/ h_losa 2.0))))
      
      (defun draw-break-line (xStart yTop hLosa hCapa isLeft / dir xEnd yCenter ptsHatch)
         (setq dir (if isLeft -1.0 1.0)) (setq xEnd (+ xStart (* 0.5 dir)))
         (command "_.RECTANG" (list xStart yTop) (list xEnd (- yTop hCapa))) (command "_.CHPROP" (entlast) "" "Color" 7 "")
         
         (setq yCenter (- yTop (/ hLosa 2.0)))
         (setq ptsHatch (list (list xStart (- yTop hCapa)) (list xEnd (- yTop hCapa)) 
               (list xEnd (+ yCenter 0.075)) (list (- xEnd (* 0.2 dir)) (+ yCenter 0.075))
               (list (- xEnd (* 0.2 dir)) yCenter) (list (+ xEnd (* 0.2 dir)) yCenter)
               (list (+ xEnd (* 0.2 dir)) (- yCenter 0.075)) (list xEnd (- yCenter 0.075))
               (list xEnd (- yTop hLosa)) (list xStart (- yTop hLosa))))
         (command "_.PLINE") (foreach p ptsHatch (command p)) (command "_C")
         (command "_.CHPROP" (entlast) "" "Color" 8 "") (command "_.HATCH" "ANSI31" 0.035 0 (entlast) "") (command "_.CHPROP" (entlast) "" "Color" 8 "")
         
         (command "_.PLINE" (list xEnd (+ yTop 0.2)) (list xEnd (+ yCenter 0.075)) (list (- xEnd (* 0.2 dir)) (+ yCenter 0.075))
            (list (- xEnd (* 0.2 dir)) yCenter) (list (+ xEnd (* 0.2 dir)) yCenter) (list (+ xEnd (* 0.2 dir)) (- yCenter 0.075))
            (list xEnd (- yCenter 0.075)) (list xEnd (- yTop hLosa 0.2)) "") (command "_.CHPROP" (entlast) "" "Color" 7 "")
      )
      (if (or (= typeLosa "Izq") (= typeLosa "Ambos")) (draw-break-line xSec yLosaStart h_losa capaCompresion T))
      (if (or (= typeLosa "Der") (= typeLosa "Ambos")) (draw-break-line (+ xSec b_draw) yLosaStart h_losa capaCompresion nil))
    )
  )

  ;; Estribo (Fillet 0.025 fijo)
  (setq radFillet 0.025)
  (setq rec_draw 0.175)
  (setq p1_st (list (+ xSec rec_draw) (+ ySec rec_draw)))
  (setq p2_st (list (- (+ xSec b_draw) rec_draw) (- (+ ySec h_draw_sec) rec_draw)))
  (command "_.RECTANG" "_F" radFillet p1_st p2_st) (command "_.CHPROP" (entlast) "" "Color" 3 "")
  (command "_.RECTANG" "_F" 0 (list 0 0) (list 0 0)) (command "_.ERASE" (entlast) "")

  ;; Ganchos 135 (OFFSET CORREGIDO 1.1*RadioVARILLA)
  (setq rBarTop (/ (* (/ (obtener-diametro-real strVarBaseTop) 100.0) 7.0) 2.0))
  (setq xStirrupLeft (car p1_st))
  (setq yStirrupTop (cadr p2_st))
  (setq offsetHookFactor (* rBarTop 1.1)) ;; 1.1 * Radio de la Varilla (NO filete)
  
  ;; H1 (Top leg): Start shifted Right by (rBar + offset)
  (setq centerBarX (+ xStirrupLeft rBarTop))
  (setq pH1 (list (+ centerBarX offsetHookFactor) yStirrupTop))
  (command "_.LINE" pH1 (polar pH1 (* 315.0 (/ pi 180.0)) 0.30) "") (command "_.CHPROP" (entlast) "" "Color" 3 "")
  
  ;; H2 (Left leg): Start shifted Down by (rBar + offset)
  (setq centerBarY (- yStirrupTop rBarTop))
  (setq pH2 (list xStirrupLeft (- centerBarY offsetHookFactor)))
  (command "_.LINE" pH2 (polar pH2 (* 315.0 (/ pi 180.0)) 0.30) "") (command "_.CHPROP" (entlast) "" "Color" 3 "")

  ;; LOGICA ACERO
  (defun get-critical-bast (bastList / critVar critNum maxD)
     (setq critVar nil critNum 0 maxD 0.0)
     (foreach b bastList
        (setq d (obtener-diametro-real (car b)))
        (if (> d maxD) (progn (setq maxD d) (setq critVar (car b)) (setq critNum (cadr b)))
           (if (and (= d maxD) (> (cadr b) critNum)) (setq critNum (cadr b)))))
     (list critVar critNum)
  )
  (setq critBastInf (get-critical-bast bastonesInfList))
  (setq critBastSup (get-critical-bast bastonesSupList))
  
  (defun process-layer-steel (baseNum baseVar bastNum bastVar isTop / diamBase diamBast numOrillas numCentro levelsIzq levelsDer levelsCen i cx cy yStart layoutMode)
     (setq diamBase (* (/ (obtener-diametro-real baseVar) 100.0) 7.0))
     (setq diamBast 0.0) (if bastVar (setq diamBast (* (/ (obtener-diametro-real bastVar) 100.0) 7.0)))
     (setq yStart (if isTop (- (cadr p2_st) (/ diamBase 2.0)) (+ (cadr p1_st) (/ diamBase 2.0))))
     (setq dir (if isTop -1.0 1.0))
     (initget "Orillas Largo")
     (setq layoutMode (getkword (strcat "\nAcomodo " (if isTop "SUP" "INF") " (Base:" (itoa baseNum) " Bast:" (itoa bastNum) ")? [Orillas/Largo]: ")))
     
     (if (= layoutMode "Orillas")
        (progn (setq levelsIzq 0 levelsDer 0 levelsCen 0)
           (setq i 0)
           (repeat baseNum
              (if (= (rem i 2) 0) (progn (setq cx (+ (car p1_st) (/ diamBase 2.0))) (setq levelsIzq (1+ levelsIzq))) (progn (setq cx (- (car p2_st) (/ diamBase 2.0))) (setq levelsDer (1+ levelsDer))))
              (setq cy yStart)
              (command "_.DONUT" 0.0 diamBase (list cx cy) "") (command "_.CHPROP" (entlast) "" "Color" 5 "") (setq i (1+ i))
           )
           (if (> bastNum 0) (progn (setq nO (getint "  Cuantos bastones en Orillas (Total)?: ")) (setq nC (- bastNum nO))
                 (setq i 0)
                 (repeat nO
                    (if (= (rem i 2) 0) (progn (setq cx (+ (car p1_st) (/ diamBase 2.0))) (setq ly levelsIzq) (setq levelsIzq (1+ levelsIzq))) (progn (setq cx (- (car p2_st) (/ diamBase 2.0))) (setq ly levelsDer) (setq levelsDer (1+ levelsDer))))
                    (setq cy (+ yStart (* ly diamBast dir)))
                    (command "_.DONUT" 0.0 diamBase (list cx cy) "") (command "_.CHPROP" (entlast) "" "Color" 5 "") (setq i (1+ i))
                 )
                 (if (> nC 0) (repeat nC
                       (setq cx (/ (+ (car p1_st) (car p2_st)) 2.0)) (setq cy (+ yStart (* levelsCen diamBast dir)))
                       (command "_.DONUT" 0.0 diamBast (list cx cy) "") (command "_.CHPROP" (entlast) "" "Color" 5 "") (setq levelsCen (1+ levelsCen))
                 ))
           ))
        )
        (progn (setq w (- (car p2_st) (car p1_st) diamBase)) (setq gap (/ w (1- (+ baseNum bastNum)))) (setq i 0)
           (repeat (+ baseNum bastNum)
              (setq cx (+ (car p1_st) (/ diamBase 2.0) (* i gap)))
              (command "_.DONUT" 0.0 diamBase (list cx yStart) "") (command "_.CHPROP" (entlast) "" "Color" 5 "") (setq i (1+ i))
           )
        )
     )
  )
  
  (process-layer-steel numBaseBot strVarBaseBot (cadr critBastInf) (car critBastInf) nil)
  (process-layer-steel numBaseTop strVarBaseTop (cadr critBastSup) (car critBastSup) T)

  (setvar "DIMASZ" old_dimasz) (setvar "DIMCLRD" old_dimclrd) (setvar "DIMTXT" old_dimtxt) (setvar "DIMCLRT" old_dimclrt) (setvar "DIMSCALE" old_dimscale) (setvar "OSMODE" oldOsnap)
  (princ "\nDibujo de Trabe completado.")
  (princ)
)