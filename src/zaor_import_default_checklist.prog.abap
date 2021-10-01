*&---------------------------------------------------------------------*
*& Report  ZAOR_IMPORT_DEFAULT_CHECKLIST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zaor_import_default_checklist.

START-OF-SELECTION.
  DATA: lt_checklist TYPE STANDARD TABLE OF zaor_checklist,
        lv_item_no   TYPE zaor_checklist_item.
  FIELD-SYMBOLS: <ls_cl> TYPE zaor_checklist.

  SELECT MAX( item ) FROM zaor_checklist
    INTO lv_item_no.
  lv_item_no = lv_item_no + 1.

  APPEND INITIAL LINE TO lt_checklist ASSIGNING <ls_cl>.
  <ls_cl>-item = lv_item_no.
  <ls_cl>-description = 'Dependencies changed in other transport requests?'.
  lv_item_no = lv_item_no + 1.
  APPEND INITIAL LINE TO lt_checklist ASSIGNING <ls_cl>.
  <ls_cl>-item = lv_item_no.
  <ls_cl>-description = 'All associated Customizing-Settings contained in transport request ' &&
    'or are already created manually /via ALE-transfer in target system ' &&
    '(e.g. Characteristics in CT04, Classes in CL02)?'.
  lv_item_no = lv_item_no + 1.
  APPEND INITIAL LINE TO lt_checklist ASSIGNING <ls_cl>.
  <ls_cl>-item = lv_item_no.
  <ls_cl>-description = 'API-Major-Changes (class, function-module etc.) bumped to dependents?'.

  INSERT zaor_checklist FROM TABLE lt_checklist.
