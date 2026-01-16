;; ==============================================================================
;; GEN_TRABES_V3.LSP
;; FIX: Signo de Cargas Invertido (Input * -1).
;;      Peso Propio agregado en Carga Muerta.
;; ==============================================================================

(defun c:GEN_TRABES (/ *error* file path dirPath fullPath numNerv jobName dateStr fc matName E_mod 
                        nervName numClaros hasCantilever cantPos spanLengths coordList i spanLen 
                        totalLen memberCount supportStr numLoads loadType loadVal d1 d2 
                        isUniform b h dimList idx dimPair memID currentX nodeIdx totalNodes
                        projName unit points plantIdx plantName)

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

  (princ "\n--- DATOS GENERALES DEL PROYECTO (TRABES) ---")
  
  (setq numNerv (getint "\nCantidad de trabes a generar: "))
  
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

  ;; Materiales
  (setq fc (getreal "\nIntroduce el F'c del concreto (kg/cm2): "))
  (setq matName (strcat "fc_" (rtos fc 2 0)))
  
  (setq E_mod (getreal "\nIntroduce el Modulo de Elasticidad E (kg/cm2): "))

  ;; Variable para almacenar la ruta elegida la primera vez
  (setq dirPath nil)

  ;; --- BUCLE PRINCIPAL (POR TRABE) ---
  (setq n 1)
  (repeat numNerv
    (princ (strcat "\n\n--- CONFIGURANDO TRABE " (itoa n) " de " (itoa numNerv) " ---"))
    
    (setq nervName (getstring "\nNombre de la trabe (ej. T-1): "))
    (if (and (ocmema:proj-beam-name-exists nervName)
             (or (not ocmema:*beam-replace-name*)
                 (/= (ocmema:pio-normalize-name nervName)
                     (ocmema:pio-normalize-name ocmema:*beam-replace-name*))))
      (progn
        (princ "\nOCMEMA: Nombre de trabe duplicado. Elija otro.")
        (setq nervName "")
        (while (= nervName "")
          (setq nervName (getstring "\nNombre de la trabe (ej. T-1): "))
          (if (or (= nervName "") (ocmema:proj-beam-name-exists nervName))
            (progn
              (princ "\nOCMEMA: Nombre de trabe duplicado. Elija otro.")
              (setq nervName "")
            )
          )
        )
      )
    )

    (setq plantIdx (ocmema:pio-getint-min "\nNumero de planta: " 1))
    (if (or (not plantIdx) (> plantIdx (ocmema:pio-assoc-get "n_plants" ocmema:*project*)))
      (progn (princ "\nOCMEMA: Planta invalida.") (exit))
    )
    (setq plantName (ocmema:proj-get-plant-name plantIdx))

    (setq unit (ocmema:proj-get-beam-unit))
    (if (not unit)
      (progn
        (initget "M C MM")
        (setq unit (getkword "\nUnidades de captura [M/C/MM]: "))
        (if (not unit) (exit))
        (ocmema:proj-set-beam-unit unit)
      )
    )

    ;; GESTIÓN DE GUARDADO DE ARCHIVO
    (if (= n 1)
      (progn
        (setq fullPath (getfiled (strcat "Guardar " nervName) (strcat defaultPath nervName ".std") "std" 1))
        (if fullPath
          (setq dirPath (vl-filename-directory fullPath))
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
        
        ;; 2. GEOMETRÍA (LONGITUDES)
        (write-line "* --- GEOMETRIA Y NODOS ---" file)
        (setq points (ocmema:proj-capture-points))
        (if (not points)
          (progn (princ "\nOCMEMA: Captura cancelada.") (exit))
        )
        (setq numClaros (1- (length points)))
        
        ;; Lógica de Voladizos
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
          (setq hasCantilever "No")
        )

        (setq spanLengths '())
        (setq i 0)
        (while (< i numClaros)
          (setq spanLen (* (distance (nth i points) (nth (1+ i) points)) (ocmema:proj-unit-factor unit)))
          (setq spanLengths (append spanLengths (list spanLen)))
          (setq i (1+ i))
        )

        ;; --- DEFINICIÓN DE SECCIONES ---
        (setq dimList '()) 
        
        (if (> numClaros 1)
          (progn
            (initget "Si No")
            (setq isUniform (getkword "\nLa seccion es constante en todos los claros? [Si/No] <Si>: "))
            (if (null isUniform) (setq isUniform "Si"))
          )
          (setq isUniform "Si")
        )

        (if (= isUniform "Si")
          ;; CASO A: Sección Constante
          (progn
            (princ "\n--- Definiendo Seccion Uniforme ---")
            (setq b (getreal "Base (cm): "))
            (setq h (getreal "Peralte (cm): "))
            (repeat numClaros
              (setq dimList (append dimList (list (list b h))))
            )
          )
          ;; CASO B: Sección Variable
          (progn
            (princ "\n--- Definiendo Secciones por Claro ---")
            (setq i 1)
            (repeat numClaros
              (princ (strcat "\nDimensiones Claro " (itoa i) ":"))
              (setq b (getreal " Base (cm): "))
              (setq h (getreal " Peralte (cm): "))
              (setq dimList (append dimList (list (list b h))))
              (setq i (1+ i))
            )
          )
        )

        ;; Escribir Coordenadas
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

        ;; 3. MATERIALES
        (write-line "* --- DEFINICION DE MATERIALES ---" file)
        (write-line "DEFINE MATERIAL START" file)
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
        (write-line "* --- PROPIEDADES DE LA TRABE ---" file)
        (write-line "MEMBER PROPERTY" file)
        
        (setq idx 1)
        (foreach dimPair dimList
          (setq b (car dimPair))
          (setq h (cadr dimPair))
          ;; STAAD: YD=Altura, ZD=Base
          (write-line (strcat (itoa idx) " PRIS YD " (rtos h 2 2) " ZD " (rtos b 2 2)) file)
          (setq idx (1+ idx))
        )

        ;; 5. CONSTANTES
        (write-line "* --- CONSTANTES Y AGRIETAMIENTO ---" file)
        (write-line "CONSTANTS" file)
        (write-line (strcat "MATERIAL " matName " ALL") file)
        (write-line "MEMBER CRACKED" file)
        (write-line (strcat "1 TO " (itoa memberCount) " REDUCTION RIX 0.5 RIY 0.5 RIZ 0.5") file)

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
        
        ;; --- AGREGAR PESO PROPIO ---
        (write-line "SELFWEIGHT Y -1" file)
        
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
               ;; CORRECCION: Invertir signo (Input Positivo -> Negativo, Input Negativo -> Positivo)
               (setq loadVal (- loadVal)) 
               (write-line (strcat (itoa memID) " UNI GY " (rtos loadVal 2 2)) file)
              )
              ((= loadType "UP")
               (setq loadVal (getreal "\nCarga (kg/m): "))
               (setq loadVal (- loadVal))
               (setq d1 (getreal "\nDist. Inicio (m): "))
               (setq d2 (getreal "\nDist. Final (m): "))
               (write-line (strcat (itoa memID) " UNI GY " (rtos loadVal 2 2) " " (rtos d1 2 2) " " (rtos d2 2 2)) file)
              )
              ((= loadType "P")
               (setq loadVal (getreal "\nCarga (kg): "))
               (setq loadVal (- loadVal))
               (setq d1 (getreal "\nDistancia (m): "))
               (write-line (strcat (itoa memID) " CON GY " (rtos loadVal 2 2) " " (rtos d1 2 2)) file)
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
               (setq loadVal (- loadVal))
               (write-line (strcat (itoa memID) " UNI GY " (rtos loadVal 2 2)) file)
              )
              ((= loadType "UP")
               (setq loadVal (getreal "\nCarga (kg/m): "))
               (setq loadVal (- loadVal))
               (setq d1 (getreal "\nDist. Inicio (m): "))
               (setq d2 (getreal "\nDist. Final (m): "))
               (write-line (strcat (itoa memID) " UNI GY " (rtos loadVal 2 2) " " (rtos d1 2 2) " " (rtos d2 2 2)) file)
              )
              ((= loadType "P")
               (setq loadVal (getreal "\nCarga (kg): "))
               (setq loadVal (- loadVal))
               (setq d1 (getreal "\nDistancia (m): "))
               (write-line (strcat (itoa memID) " CON GY " (rtos loadVal 2 2) " " (rtos d1 2 2)) file)
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
        
        ;; --- DISEÑO DE CONCRETO ---
        (write-line "UNIT CM KG" file) 
        
        (write-line "* --- PARAMETROS DE DISEÑO DE CONCRETO ---" file)
        (write-line "START CONCRETE DESIGN" file)
        (write-line "CODE ACI" file)
        
        (write-line "* RECUBRIMIENTOS DE VIGA (2.5 CM)" file)
        (write-line "CLB 2.5 ALL" file)
        (write-line "CLT 2.5 ALL" file)
        (write-line "CLS 2.5 ALL" file)
        
        (write-line "* MATERIALES" file)
        (write-line (strcat "FC " (rtos fc 2 0) " ALL") file)
        (write-line "FYMAIN 4200 ALL" file)
        (write-line "FYSEC 4200 ALL" file)
        
        (write-line "* LIMITES DE VARILLAS" file)
        (write-line "MINMAIN 13 ALL" file)
        (write-line "MAXMAIN 16 ALL" file)
        
        (write-line "TRACK 2 ALL" file)
        
        (write-line "DESIGN BEAM ALL" file)
        (write-line "CONCRETE TAKE" file)
        (write-line "END CONCRETE DESIGN" file)
        (write-line "FINISH" file)

        (close file)
        (if ocmema:*project*
          (progn
            (ocmema:proj-upsert-beam
              (list
                (cons "name" nervName)
                (cons "plant_idx" plantIdx)
                (cons "plant_name" plantName)
                (cons "n_points" (length points))
                (cons "points_raw" points)
                (cons "unit" unit)
                (cons "std_path" fullPath)
              )
            )
            (ocmema:proj-autosave)
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
