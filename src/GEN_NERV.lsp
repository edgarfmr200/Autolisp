(defun c:GEN_NERV (/ *error* file path dirPath fullPath numNerv jobName dateStr fc matName E_mod peralte capa_comp sep_nerv ancho_nerv 
                     nervName numClaros hasCantilever cantPos spanLengths coordList i spanLen 
                     totalLen memberCount supportStr loadCount loadType loadVal d1 d2
                     projName units scale points rawLen dir delta genMode typedLen change dirPref
                     plants plantIdx plantSel plantName useSaved updateSaved newplants pl spanStr meta)

  (vl-load-com)

  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (if file (close file))
    (princ)
  )

  ;; --- CONFIGURACIÓN DE RUTA POR DEFECTO ---
  (setq defaultPath "C:\\Users\\edgar\\OneDrive - ITESO\\OCMEMA_IE\\01. PROYECTOS\\")
  (setq dirPref (if ocmema:*project* (ocmema:pio-assoc-get "dir_ribs_std" ocmema:*project*) nil))
  (if (and dirPref (/= dirPref ""))
    (setq defaultPath (ocmema:proj-ensure-dir-sep dirPref))
  )

  (princ "\n--- DATOS GENERALES DEL PROYECTO ---")
  
  (if ocmema:*rib-single*
    (setq numNerv 1)
    (setq numNerv (getint "\nCantidad de nervaduras a generar: "))
  )
  
  ;; Validación Nombre del Proyecto
  (setq projName "")
  (if ocmema:*project*
    (setq projName (ocmema:pio-assoc-get "project_name" ocmema:*project*))
  )
  (if (or (not projName) (= projName ""))
    (setq projName "OCMEMA")
  )
  (setq jobName (vl-string-translate " " "_" projName))

  (setq dateStr (menucmd "M=$(edtime,$(getvar,date),DD-MON-YY)"))
  (princ (strcat "\nFecha detectada: " dateStr))

  ;; Materiales (concreto)
  (setq fc (if ocmema:*project* (ocmema:pio-assoc-get "fc_kgcm2" ocmema:*project*) nil))
  (setq E_mod (if ocmema:*project* (ocmema:pio-assoc-get "ec_kgcm2" ocmema:*project*) nil))
  (if (and fc E_mod)
    (progn
      (initget "S N")
      (setq change (getkword "\n�Deseas cambiar el concreto? [S/N] <N>: "))
      (if (or (not change) (= change "N"))
        nil
        (progn
          (setq fc (getreal "\nIntroduce el F'c del concreto (kg/cm2): "))
          (setq E_mod (getreal "\nIntroduce el Modulo de Elasticidad E (kg/cm2): "))
          (initget "S N")
          (setq change (getkword "\n�Actualizar estos datos en el proyecto? [S/N] <N>: "))
          (if (and ocmema:*project* change (= change "S"))
            (progn
              (setq ocmema:*project* (ocmema:pio-alist-set "fc_kgcm2" fc ocmema:*project*))
              (setq ocmema:*project* (ocmema:pio-alist-set "ec_kgcm2" E_mod ocmema:*project*))
            )
          )
        )
      )
    )
    (progn
      (setq fc (getreal "\nIntroduce el F'c del concreto (kg/cm2): "))
      (setq E_mod (getreal "\nIntroduce el Modulo de Elasticidad E (kg/cm2): "))
      (if ocmema:*project*
        (progn
          (setq ocmema:*project* (ocmema:pio-alist-set "fc_kgcm2" fc ocmema:*project*))
          (setq ocmema:*project* (ocmema:pio-alist-set "ec_kgcm2" E_mod ocmema:*project*))
        )
      )
    )
  )
  ;; Generamos el nombre del material dinámicamente, ej: fc_250
  (setq matName (strcat "fc_" (rtos fc 2 0)))
  (setq ancho_nerv 10.0) 

  ;; Variable para almacenar la ruta elegida la primera vez
  (setq dirPath nil)

  ;; --- BUCLE PRINCIPAL ---
  (setq n 1)
  (repeat numNerv
    (princ (strcat "\n\n--- CONFIGURANDO NERVADURA " (itoa n) " de " (itoa numNerv) " ---"))
    
    (if ocmema:*rib-force-name*
      (setq nervName ocmema:*rib-force-name*)
      (setq nervName (getstring "\nNombre de la nervadura (ej. N-1): "))
    )

    ;; Seleccion de planta y parametros de losa
    (setq peralte nil)
    (setq capa_comp nil)
    (setq sep_nerv nil)
    (setq YB nil)
    (setq plantIdx nil)
    (setq plantName "")
    (setq plantSel nil)
    (setq plants (if ocmema:*project* (ocmema:pio-assoc-get "plants" ocmema:*project*) nil))
    (if (and plants (> (length plants) 0))
      (progn
        (if (= (length plants) 1)
          (setq plantSel (car plants))
          (progn
            (princ "\nPlantas disponibles:")
            (foreach pl plants
              (princ (strcat "\n" (itoa (ocmema:pio-assoc-get "idx" pl)) " - " (ocmema:pio-assoc-get "name" pl)))
            )
            (setq plantIdx (getint "\nNumero de planta: "))
            (setq plantSel nil)
            (while (not plantSel)
              (foreach pl plants
                (if (= (ocmema:pio-assoc-get "idx" pl) plantIdx) (setq plantSel pl))
              )
              (if (not plantSel)
                (progn
                  (princ "\nPlanta invalida.")
                  (setq plantIdx (getint "\nNumero de planta: "))
                )
              )
            )
          )
        )
        (setq plantIdx (ocmema:pio-assoc-get "idx" plantSel))
        (setq plantName (ocmema:pio-assoc-get "name" plantSel))
        (setq peralte (ocmema:pio-assoc-get "slab_h_total_cm" plantSel))
        (setq capa_comp (ocmema:pio-assoc-get "slab_h_comp_cm" plantSel))
        (setq sep_nerv (ocmema:pio-assoc-get "rib_spacing_cm" plantSel))
        (if (and peralte capa_comp sep_nerv)
          (progn
            (initget "S N")
            (setq useSaved (getkword (strcat "\n�Usar los datos guardados para " plantName "? [S/N] <S>: ")))
            (if (or (not useSaved) (= useSaved "S"))
              nil
              (progn
                (setq peralte nil)
                (while (or (not peralte) (<= peralte 0.0))
                  (setq peralte (getreal "\nPeralte total de la losa (cm) [YD]: "))
                  (if (and peralte (<= peralte 0.0)) (princ "\nValor invalido."))
                )
                (setq capa_comp nil)
                (while (or (not capa_comp) (< capa_comp 0.0) (> capa_comp peralte))
                  (setq capa_comp (getreal "\nAltura de capa de compresion (cm): "))
                  (if (and capa_comp (or (< capa_comp 0.0) (> capa_comp peralte))) (princ "\nValor invalido."))
                )
                (setq sep_nerv nil)
                (while (or (not sep_nerv) (<= sep_nerv 0.0))
                  (setq sep_nerv (getreal "\nSeparacion entre nervaduras (cm) [ZD]: "))
                  (if (and sep_nerv (<= sep_nerv 0.0)) (princ "\nValor invalido."))
                )
                (initget "S N")
                (setq updateSaved (getkword (strcat "\n�Guardar/actualizar estos datos en el proyecto para " plantName "? [S/N] <N>: ")))
                (if (and updateSaved (= updateSaved "S"))
                  (progn
                    (ocmema:proj-set-plant-fields plantIdx
                      (list
                        (cons 'slab_h_total_cm peralte)
                        (cons 'slab_h_comp_cm capa_comp)
                        (cons 'rib_spacing_cm sep_nerv)
                      )
                    )
                    (if ocmema:*debug-io*
                      (ocmema:dbg-io "[DEBUG] after proj-set-plant-fields, about to autosave")
                    )
                    (ocmema:proj-autosave-from "GEN_NERV after slab update")
                  )
                )
              )
            )
          )
          (progn
            (setq peralte nil)
            (while (or (not peralte) (<= peralte 0.0))
              (setq peralte (getreal "\nPeralte total de la losa (cm) [YD]: "))
              (if (and peralte (<= peralte 0.0)) (princ "\nValor invalido."))
            )
            (setq capa_comp nil)
            (while (or (not capa_comp) (< capa_comp 0.0) (> capa_comp peralte))
              (setq capa_comp (getreal "\nAltura de capa de compresion (cm): "))
              (if (and capa_comp (or (< capa_comp 0.0) (> capa_comp peralte))) (princ "\nValor invalido."))
            )
            (setq sep_nerv nil)
            (while (or (not sep_nerv) (<= sep_nerv 0.0))
              (setq sep_nerv (getreal "\nSeparacion entre nervaduras (cm) [ZD]: "))
              (if (and sep_nerv (<= sep_nerv 0.0)) (princ "\nValor invalido."))
            )
            (initget "S N")
            (setq updateSaved (getkword (strcat "\n�Guardar/actualizar estos datos en el proyecto para " plantName "? [S/N] <N>: ")))
            (if (and updateSaved (= updateSaved "S"))
                  (progn
                    (ocmema:proj-set-plant-fields plantIdx
                      (list
                        (cons 'slab_h_total_cm peralte)
                        (cons 'slab_h_comp_cm capa_comp)
                        (cons 'rib_spacing_cm sep_nerv)
                      )
                    )
                    (if ocmema:*debug-io*
                      (ocmema:dbg-io "[DEBUG] after proj-set-plant-fields, about to autosave")
                    )
                    (ocmema:proj-autosave-from "GEN_NERV after slab update")
                  )
                )
          )
        )
      )
      (progn
        (setq peralte nil)
        (while (or (not peralte) (<= peralte 0.0))
          (setq peralte (getreal "\nPeralte total de la losa (cm) [YD]: "))
          (if (and peralte (<= peralte 0.0)) (princ "\nValor invalido."))
        )
        (setq capa_comp nil)
        (while (or (not capa_comp) (< capa_comp 0.0) (> capa_comp peralte))
          (setq capa_comp (getreal "\nAltura de capa de compresion (cm): "))
          (if (and capa_comp (or (< capa_comp 0.0) (> capa_comp peralte))) (princ "\nValor invalido."))
        )
        (setq sep_nerv nil)
        (while (or (not sep_nerv) (<= sep_nerv 0.0))
          (setq sep_nerv (getreal "\nSeparacion entre nervaduras (cm) [ZD]: "))
          (if (and sep_nerv (<= sep_nerv 0.0)) (princ "\nValor invalido."))
        )
      )
    )
    (setq YB (- peralte capa_comp))
    ;; GESTIÓN DE GUARDADO DE ARCHIVO
    ;; GESTIÓN DE GUARDADO DE ARCHIVO
    (if (= n 1)
      (progn
        (setq fullPath (getfiled (strcat "Guardar " nervName) (strcat defaultPath nervName ".std") "std" 1))
        (if fullPath
          (progn
            (setq dirPath (vl-filename-directory fullPath))
            (if ocmema:*project*
              (setq ocmema:*project* (ocmema:pio-alist-set "dir_ribs_std" dirPath ocmema:*project*))
            )
          )
          (progn (princ "\nCancelado por usuario.") (exit))
        )
      )
      (setq fullPath (strcat dirPath "\\" nervName ".std"))
    )

    (if fullPath
      (progn
        (setq file (open fullPath "w"))
        (princ (strcat "\nGenerando archivo en: " fullPath))
        
        ;; 1. HEADER
        (write-line "STAAD PLANE" file)
        (write-line "* --- INFORMACION DEL TRABAJO ---" file)
        (write-line "START JOB INFORMATION" file)
        (write-line (strcat "JOB NAME " jobName) file)
        (write-line (strcat "ENGINEER DATE " dateStr) file)
        (write-line "END JOB INFORMATION" file)
        (write-line "INPUT WIDTH 79" file)
        (write-line "UNIT CM KG" file) 
        
        ;; 2. GEOMETRÍA
        (write-line "* --- GEOMETRIA Y NODOS ---" file)
        (initget "D A")
        (setq genMode (getkword "\nGenerar para [D Dibujo / A Analisis] <D>: "))
        (if (not genMode) (setq genMode "D"))
        (if (= genMode "D")
          (progn
            (initget "H V")
            (setq dir (getkword "\nOCMEMA: Direccion de nervadura [H Horizontal/V Vertical] <H>: "))
            (if (not dir) (setq dir "H"))
            (setq points (ocmema:proj-capture-points))
            (if (not points)
              (progn (princ "\nOCMEMA: Captura cancelada.") (exit))
            )
            (setq numClaros (1- (length points)))
            (setq units (if ocmema:*project* (ocmema:pio-assoc-get "draw_units" ocmema:*project*) nil))
            (setq scale (if ocmema:*project* (ocmema:pio-assoc-get "draw_scale_factor" ocmema:*project*) nil))
            (if (or (not units) (not scale))
              (progn
                (setq delta (if (= dir "V")
                              (abs (- (cadr (cadr points)) (cadr (car points))))
                              (abs (- (car (cadr points)) (car (car points))))
                            )
                )
                (setq rawLen delta)
                (princ (strcat "\nOCMEMA: Claro 1 (sin convertir) = " (rtos rawLen 2 6)))
                (initget "CM M MM")
                (setq units (getkword "\nUnidades [CM/M/MM]: "))
                (if (not units) (exit))
                (setq scale (getreal "\nOCMEMA: Factor de escala (multiplicador) <1.0>: "))
                (if (not scale) (setq scale 1.0))
                (if ocmema:*project*
                  (progn
                    (setq ocmema:*project* (ocmema:pio-alist-set "draw_units" units ocmema:*project*))
                    (setq ocmema:*project* (ocmema:pio-alist-set "draw_scale_factor" scale ocmema:*project*))
                    (ocmema:proj-set-units-scale units scale)
                  )
                )
              )
            )
            (setq spanLengths (list))
            (setq i 0)
            (while (< i numClaros)
              (setq delta (if (= dir "V")
                            (abs (- (cadr (nth (1+ i) points)) (cadr (nth i points))))
                            (abs (- (car (nth (1+ i) points)) (car (nth i points))))
                          )
              )
              (setq spanLen (* delta (ocmema:proj-unit-factor units) scale))
              (setq spanLengths (append spanLengths (list spanLen)))
              (setq i (1+ i))
            )
          )
          (progn
            ;; Analisis: longitudes siempre en cm, sin unidades/escala de dibujo
            (setq dir "")
            (setq points nil)
            (setq numClaros (ocmema:pio-getint-min "\nNumero de claros: " 1))
            (setq spanLengths (list))
            (setq i 1)
            (repeat numClaros
              (setq typedLen (getreal (strcat "\nLongitud claro " (itoa i) " (cm): ")))
              (if (or (not typedLen) (<= typedLen 0.0)) (exit))
              (setq spanLen typedLen)
              (setq spanLengths (append spanLengths (list spanLen)))
              (setq i (1+ i))
            )
          )
        )
        ;; LÓGICA DE VOLADIZOS CORREGIDA
        (setq cantPos "Ninguno")
        
        (if (> numClaros 1)
          (progn
            (initget "Si No")
            (setq hasCantilever (getkword "\nExiste algun voladizo? [Si/No] <No>: "))
            (if (null hasCantilever) (setq hasCantilever "No"))
            
            (if (= hasCantilever "Si")
              (progn
                (initget "Inicio Final")
                (setq cantPos (getkword "\nDonde esta el voladizo? [Inicio/Final]: "))
              )
            )
          )
          ;; Si es 1 claro, no preguntamos y asumimos que no hay voladizo
          (setq hasCantilever "No")
        )

        (write-line "JOINT COORDINATES" file)
        (setq currentX 0.0)
        (write-line (strcat "1 " (rtos currentX 2 2) " 0 0;") file)
        
        (setq nodeIdx 2)
        (foreach len spanLengths
          (setq currentX (+ currentX len))
          (write-line (strcat (itoa nodeIdx) " " (rtos currentX 2 2) " 0 0;") file)
          (setq nodeIdx (1+ nodeIdx))
        )

        (write-line "* --- INCIDENCIAS DE MIEMBROS ---" file)
        (write-line "MEMBER INCIDENCES" file)
        (setq memberCount numClaros)
        (setq i 1)
        (repeat memberCount
          (write-line (strcat (itoa i) " " (itoa i) " " (itoa (1+ i)) ";") file)
          (setq i (1+ i))
        )

        ;; 3. MATERIALES (NOMBRE DINÁMICO)
        (write-line "* --- DEFINICION DE MATERIALES ---" file)
        (write-line "DEFINE MATERIAL START" file)
        ;; Usamos matName (ej. fc_250)
        (write-line (strcat "ISOTROPIC " matName) file) 
        (write-line (strcat "E " (rtos E_mod 2 2)) file)
        (write-line "POISSON 0.17" file)
        (write-line "DENSITY 0.0024" file) 
        (write-line "ALPHA 1e-05" file)
        (write-line "DAMP 0.05" file)
        (write-line (strcat "G " (rtos (/ E_mod 2.34) 2 2)) file)
        (write-line "TYPE CONCRETE" file)
        (write-line "END DEFINE MATERIAL" file)

        ;; 4. PROPIEDADES
        (write-line "* --- PROPIEDADES DE LA NERVADURA ---" file)
        (write-line "MEMBER PROPERTY" file)
        (write-line (strcat "1 TO " (itoa memberCount) " PRIS YD " (rtos peralte 2 2) " ZD " (rtos sep_nerv 2 2) " YB " (rtos YB 2 2) " ZB " (rtos ancho_nerv 2 2)) file)

        ;; 5. CONSTANTES
        (write-line "* --- CONSTANTES Y AGRIETAMIENTO ---" file)
        (write-line "CONSTANTS" file)
        ;; Asignamos el material con nombre dinámico
        (write-line (strcat "MATERIAL " matName " ALL") file)
        (write-line "MEMBER CRACKED" file)
        (write-line (strcat "1 TO " (itoa memberCount) " REDUCTION RIY 0.5") file)

        ;; 6. SOPORTES
        (write-line "* --- CONDICIONES DE APOYO ---" file)
        (write-line "SUPPORTS" file)
        (setq totalNodes (1+ memberCount))
        (cond
          ((= cantPos "Ninguno")
           (write-line (strcat "1 TO " (itoa totalNodes) " PINNED") file)
          )
          ((= cantPos "Inicio")
           (write-line (strcat "2 TO " (itoa totalNodes) " PINNED") file)
          )
          ((= cantPos "Final")
           (write-line (strcat "1 TO " (itoa memberCount) " PINNED") file)
          )
        )

        ;; 7. CARGAS
        (write-line "* --- DEFINICION DE CARGAS ---" file)
        (write-line "LOAD 1 LOADTYPE Dead TITLE CARGA MUERTA" file)
        (write-line "UNIT METER KG" file) 
        (write-line "* CARGAS MUERTAS APLICADAS" file)
        (write-line "MEMBER LOAD" file)
        
        (princ "\n--- CARGAS MUERTAS ---")
        (setq memID 1)
        (repeat memberCount
          (princ (strcat "\nClaro " (itoa memID) " (Muerta):"))
          (setq numLoads (getint "\nCantidad de cargas? (0 para ninguna): "))
          (if (not numLoads) (setq numLoads 0))
          
          (repeat numLoads
            (initget "UC UP P") 
            (setq loadType (getkword "\nTipo? [UC(Completa)/UP(Parcial)/P(Puntual)]: "))
            
            (cond
              ((= loadType "UC")
               (setq loadVal (getreal "\nCarga (kg/m): "))
               (write-line (strcat (itoa memID) " UNI GY -" (rtos loadVal 2 2)) file)
              )
              ((= loadType "UP")
               (setq loadVal (getreal "\nCarga (kg/m): "))
               (setq d1 (getreal "\nDist. Inicio (m): "))
               (setq d2 (getreal "\nDist. Final (m): "))
               (write-line (strcat (itoa memID) " UNI GY -" (rtos loadVal 2 2) " " (rtos d1 2 2) " " (rtos d2 2 2)) file)
              )
              ((= loadType "P")
               (setq loadVal (getreal "\nCarga (kg): "))
               (setq d1 (getreal "\nDistancia (m): "))
               (write-line (strcat (itoa memID) " CON GY -" (rtos loadVal 2 2) " " (rtos d1 2 2)) file)
              )
            )
          )
          (setq memID (1+ memID))
        )

        (write-line "LOAD 2 LOADTYPE Live TITLE CARGA VIVA" file)
        (write-line "* CARGAS VIVAS APLICADAS" file)
        (write-line "MEMBER LOAD" file)
        
        (princ "\n--- CARGAS VIVAS ---")
        (setq memID 1)
        (repeat memberCount
          (princ (strcat "\nClaro " (itoa memID) " (Viva):"))
          (setq numLoads (getint "\nCantidad de cargas? (0 para ninguna): "))
          (if (not numLoads) (setq numLoads 0))
          
          (repeat numLoads
            (initget "UC UP P")
            (setq loadType (getkword "\nTipo? [UC(Completa)/UP(Parcial)/P(Puntual)]: "))
            
            (cond
              ((= loadType "UC")
               (setq loadVal (getreal "\nCarga (kg/m): "))
               (write-line (strcat (itoa memID) " UNI GY -" (rtos loadVal 2 2)) file)
              )
              ((= loadType "UP")
               (setq loadVal (getreal "\nCarga (kg/m): "))
               (setq d1 (getreal "\nDist. Inicio (m): "))
               (setq d2 (getreal "\nDist. Final (m): "))
               (write-line (strcat (itoa memID) " UNI GY -" (rtos loadVal 2 2) " " (rtos d1 2 2) " " (rtos d2 2 2)) file)
              )
              ((= loadType "P")
               (setq loadVal (getreal "\nCarga (kg): "))
               (setq d1 (getreal "\nDistancia (m): "))
               (write-line (strcat (itoa memID) " CON GY -" (rtos loadVal 2 2) " " (rtos d1 2 2)) file)
              )
            )
          )
          (setq memID (1+ memID))
        )

        (write-line "* --- COMBINACIONES DE CARGA ---" file)
        (write-line "LOAD COMB 3 1.2 CM + 1.6 CV" file)
        (write-line "1 1.2 2 1.6" file)
        (write-line "LOAD COMB 4 CM + CV" file)
        (write-line "1 1.0 2 1.0" file)
        (write-line "LOAD COMB 5 1.4 CM" file)
        (write-line "1 1.4" file)

        (write-line "PERFORM ANALYSIS" file)
        
        ;; --- DISEÑO DE CONCRETO ACTUALIZADO ---
        (write-line "UNIT CM KG" file) 
        
        (write-line "* --- PARAMETROS DE DISEÑO DE CONCRETO ---" file)
        (write-line "START CONCRETE DESIGN" file)
        (write-line "CODE ACI" file)
        
        (write-line "* ESPECIFICACIONES DE RECUBRIMIENTO" file)
        (write-line "CLB 2 ALL" file)
        (write-line "CLT 2 ALL" file)
        (write-line "CLS 1.5 ALL" file)
        
        (write-line "* ESPECIFICACIONES DEL CONCRETO (F'c)" file)
        ;; Aquí se usa el valor numérico (ej. 250)
        (write-line (strcat "FC " (rtos fc 2 0) " ALL") file)
        
        (write-line "* ESPECIFICACIONES DEL ACERO (Fy)" file)
        (write-line "FYMAIN 4200 ALL" file)
        (write-line "FYSEC 4200 ALL" file)
        
        (write-line "* LIMITES DE VARILLAS (METRICO 13mm y 16mm)" file)
        (write-line "MINMAIN 13 ALL" file)
        (write-line "MAXMAIN 16 ALL" file)
        
        (write-line "* NIVEL DE DETALLE DEL REPORTE" file)
        (write-line "TRACK 2 ALL" file)
        
        (write-line "DESIGN BEAM ALL" file)
        (write-line "CONCRETE TAKE" file)
        (write-line "END CONCRETE DESIGN" file)
        (write-line "FINISH" file)

        (close file)
        (if ocmema:*project*
          (progn
            (setq spanStr "")
            (foreach len spanLengths
              (setq spanStr (if (= spanStr "") (rtos len 2 6) (strcat spanStr "," (rtos len 2 6))))
            )
            (setq meta (strcat "drawable_plan=" (if points "1" "0") ";span_lengths=" spanStr))
            (ocmema:proj-upsert-rib
              (list
                (cons "name" nervName)
                (cons "plant_idx" (if plantIdx plantIdx 0))
                (cons "dir" dir)
                (cons "spacing" sep_nerv)
                (cons "n_clear" numClaros)
                (cons "n_points" (if points (length points) 0))
                (cons "points_raw" (if points points (list)))
                (cons "drawable_plan" (if points T nil))
                (cons "span_lengths" spanLengths)
                (cons "meta_kv" meta)
              )
            )
            (if ocmema:*debug-io*
              (ocmema:dbg-io "[CALL] autosave from GEN_NERV after upsert rib")
            )
            (ocmema:proj-autosave-from "GEN_NERV after upsert rib")
          )
        )
        (princ (strcat "\nArchivo generado: " nervName))
      )
    )
    (setq n (1+ n))
  )
  (princ "\nProceso terminado.")
  (princ)
)
