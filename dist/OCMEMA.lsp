;; OCMEMA loader
;; Carga archivos desde ../src sin importar desde donde se invoque.

(defun ocmema--loader (/ this dist-dir root-dir src-dir files file full)
  (setq this (findfile "OCMEMA.lsp"))
  (if (not this)
    (prompt "\nOCMEMA: no se pudo ubicar OCMEMA.lsp con findfile.")
    (progn
      (setq dist-dir (vl-filename-directory this))
      (setq root-dir (vl-filename-directory dist-dir))
      (setq src-dir (strcat root-dir "\\src"))
      (setq files
        (list
          "OCMEMA_PROJECT_IO.lsp"
          "DIBUJAR_NERV.lsp"
          "DIBUJAR_TRABES_V0.lsp"
          "DIBUJAR_TRABE_V_FINAL_FIXED10.lsp"
          "GEN_NERV.lsp"
          "GEN_TRABES.lsp"
        )
      )
      (foreach file files
        (setq full (strcat src-dir "\\" file))
        (if (findfile full)
          (progn
            (prompt (strcat "\nOCMEMA: cargando " full " ..."))
            (load full)
            (prompt " OK")
          )
          (prompt (strcat "\nOCMEMA: no se encontro " full))
        )
      )
    )
  )
  (princ)
)

(defun c:OCMEMA (/ choice old_osmode olderr)
  (setq old_osmode (getvar "OSMODE"))
  (setq olderr *error*)
  (defun *error* (msg)
    (if old_osmode (setvar "OSMODE" old_osmode))
    (if olderr (setq *error* olderr))
    (if msg (princ (strcat "\n; ERROR: " msg)))
    (princ)
  )
  (setvar "OSMODE" 0)
  (ocmema--loader)
  (prompt "\nOCMEMA: menu")
  (prompt "\n1) DIBUJAR_TRABES_V0")
  (prompt "\n2) DIBUJAR_TRABE_V_FINAL_FIXED10")
  (prompt "\n3) DIBUJAR_NERV")
  (prompt "\n4) GEN_TRABES")
  (prompt "\n5) GEN_NERV")
  (setq choice (getint "\nOCMEMA: elige una opcion [1-5]: "))
  (cond
    ((= choice 1) (c:DIBUJAR_TRABE_V13))
    ((= choice 2) (c:DIBUJAR_TRABE_V_FINAL))
    ((= choice 3) (c:DIBUJAR_NERV))
    ((= choice 4) (c:GEN_TRABES))
    ((= choice 5) (c:GEN_NERV))
    (T (prompt "\nOCMEMA: opcion invalida."))
  )
  (if old_osmode (setvar "OSMODE" old_osmode))
  (setq *error* olderr)
  (princ)
)

(ocmema--loader)
(princ)
