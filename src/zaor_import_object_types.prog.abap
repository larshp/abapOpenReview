*&---------------------------------------------------------------------*
*& Report  ZAOR_IMPORT_OBJECT_TYPES
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zaor_import_object_types.

PARAMETERS: p_delete TYPE sap_bool.

START-OF-SELECTION.

  PERFORM fill_tadir_crossref.

FORM fill_tadir_crossref.
  DATA: ls_tadir_crossref TYPE zaor_ta_crossref,
        lt_tadir_crossref TYPE STANDARD TABLE OF zaor_ta_crossref.

  IF p_delete = abap_true.
    DELETE FROM zaor_ta_crossref.
  ENDIF.

  " Definitions see type group SWBM
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'R3TR'.
  ls_tadir_crossref-obj_class = 'P'.
  ls_tadir_crossref-object = 'PROG'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'LIMU'.
  ls_tadir_crossref-obj_class = 'P'.
  ls_tadir_crossref-object = 'REPS'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'LIMU'.
  ls_tadir_crossref-obj_class = 'PS'.
  ls_tadir_crossref-object = 'DYNP'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'LIMU'.
  ls_tadir_crossref-obj_class = 'NN'.
  ls_tadir_crossref-object = 'MESS'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'LIMU'.
  ls_tadir_crossref-obj_class = 'OC'.
  ls_tadir_crossref-object = 'METH'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'LIMU'.
  ls_tadir_crossref-obj_class = 'OC'.
  ls_tadir_crossref-object = 'CLSD'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'LIMU'.
  ls_tadir_crossref-obj_class = 'OC'.
  ls_tadir_crossref-object = 'CINC'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'LIMU'.
  ls_tadir_crossref-obj_class = 'OC'.
  ls_tadir_crossref-object = 'CPRI'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'LIMU'.
  ls_tadir_crossref-obj_class = 'OC'.
  ls_tadir_crossref-object = 'CPRO'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'LIMU'.
  ls_tadir_crossref-obj_class = 'OC'.
  ls_tadir_crossref-object = 'CPUB'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'LIMU'.
  ls_tadir_crossref-obj_class = 'FF'.
  ls_tadir_crossref-object = 'FUNC'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'R3TR'.
  ls_tadir_crossref-obj_class = 'DT'.
  ls_tadir_crossref-object = 'TABL'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'R3TR'.
  ls_tadir_crossref-obj_class = 'DTF'.
  ls_tadir_crossref-object = 'TABL'.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'R3TR'.
  ls_tadir_crossref-obj_class = 'DS'.
  ls_tadir_crossref-object = 'TABL'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'R3TR'.
  ls_tadir_crossref-obj_class = 'DSF'.
  ls_tadir_crossref-object = 'TABL'.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'LIMU'.
  ls_tadir_crossref-obj_class = 'DS'.
  ls_tadir_crossref-object = 'TABD'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'R3TR'.
  ls_tadir_crossref-obj_class = 'DS'.
  ls_tadir_crossref-object = 'TTYP'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'LIMU'.
  ls_tadir_crossref-obj_class = 'DS'.
  ls_tadir_crossref-object = 'TTYD'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'R3TR'.
  ls_tadir_crossref-obj_class = 'DE'.
  ls_tadir_crossref-object = 'DTEL'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'LIMU'.
  ls_tadir_crossref-obj_class = 'DE'.
  ls_tadir_crossref-object = 'DTED'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'R3TR'.
  ls_tadir_crossref-obj_class = 'DD'.
  ls_tadir_crossref-object = 'DOMA'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'LIMU'.
  ls_tadir_crossref-obj_class = 'DD'.
  ls_tadir_crossref-object = 'DOMD'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'R3TR'.
  ls_tadir_crossref-obj_class = 'DV'.
  ls_tadir_crossref-object = 'VIEW'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'LIMU'.
  ls_tadir_crossref-obj_class = 'DV'.
  ls_tadir_crossref-object = 'VIED'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'R3TR'.
  ls_tadir_crossref-obj_class = 'DVF'.
  ls_tadir_crossref-object = 'VIEW'.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'R3TR'.
  ls_tadir_crossref-obj_class = 'DA'.
  ls_tadir_crossref-object = 'DDLS'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'R3TR'.
  ls_tadir_crossref-obj_class = 'DG'.
  ls_tadir_crossref-object = 'TYPE'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'R3TR'.
  ls_tadir_crossref-obj_class = 'DGT'.
  ls_tadir_crossref-object = 'TYPE'.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'R3TR'.
  ls_tadir_crossref-obj_class = 'VT'.
  ls_tadir_crossref-object = 'XLST'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'R3TR'.
  ls_tadir_crossref-obj_class = 'OC'.
  ls_tadir_crossref-object = 'CLASS'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.
  CLEAR ls_tadir_crossref.
  ls_tadir_crossref-pgmid = 'R3TR'.
  ls_tadir_crossref-obj_class = 'OI'.
  ls_tadir_crossref-object = 'INTF'.
  ls_tadir_crossref-is_primary = abap_true.
  INSERT ls_tadir_crossref INTO TABLE lt_tadir_crossref.

  MODIFY zaor_ta_crossref FROM TABLE lt_tadir_crossref.
ENDFORM.
