(defun c:SetPersistentValue ( / val )
  ;; Try to get the stored value from the environment
  (setq val (getenv "MyPersistentValue"))

  ;; If not found, set to default value
  (if (not val)
    (progn
      (setq val "100")  ; default as a string
      (setenv "MyPersistentValue" val) ; store it persistently
    )
  )

  ;; Convert string to integer for use
  (setq val (atoi val))

  (princ (strcat "\nPersistent Value is: " (itoa val)))

  ;; Change and save it for next session
  (setq val (+ val 1))
  (setenv "MyPersistentValue" (itoa val))  ; save back as string

  (princ)
)

(defun c:SetGlobalValue ( / )
  ;; If the global variable hasn't been set, initialize it
  (if (not *MyGlobalValue*)
    (setq *MyGlobalValue* 100)
  )

  (princ (strcat "\nGlobal Value is: " (itoa *MyGlobalValue*)))

  ;; Change it
  (setq *MyGlobalValue* (+ *MyGlobalValue* 1))

  (princ)
)