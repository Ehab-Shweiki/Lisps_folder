(defun c:AddAttrToBlock (/ blkdef attdef inspt)
  (vl-load-com)
  (setq blkname "Window_120x150") ; block name to edit
  (setq blkdef (vla-item (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))) blkname))

  ;; Attribute insertion point
  (setq inspt (vlax-3d-point 0.0 0.0 0.0))

  ;; Create attribute definition object
  (setq attdef
    (vla-addattribute
      blkdef
      1.0                                 ; height (text size)
      acAttributeModePreset               ; mode (Preset = hidden default)
      "HEIGHT"                            ; tag
      "Window Height (m)"                 ; prompt
      "1.50"                              ; default value
      inspt                               ; insertion point
    )
  )

  (vla-put-layer attdef "0")
  (vla-put-textalignment attdef acAlignmentLeft)
  (princ "\n✅ Attribute added to block definition.")

  ;; Run ATTSYNC to update all instances
  (command "._ATTSYNC" "_Name" blkname)
  (princ)
)
