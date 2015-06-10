class ZCL_AOR_DIFF definition
  public
  create public .

public section.

  class-methods DIFF
    importing
      !IV_OBJECT type TROBJTYPE
      !IV_OBJ_NAME type TROBJ_NAME .
protected section.
private section.

  types:
    TY_VRSD_OLD_TT type standard table of VRSD_OLD with default key .

  class-methods DELTA
    importing
      !IT_OLD type STANDARD TABLE
      !IT_NEW type STANDARD TABLE
    returning
      value(RT_DELTA) type VXABAPT255_TAB .
  class-methods VERSION_LIST
    importing
      !IV_OBJECT type TROBJTYPE
      !IV_OBJ_NAME type TROBJ_NAME
    returning
      value(RT_VERSION_LIST) type TY_VRSD_OLD_TT .
ENDCLASS.



CLASS ZCL_AOR_DIFF IMPLEMENTATION.


METHOD delta.

  DATA: lt_trdirtab_old TYPE TABLE OF trdir,
        lt_trdirtab_new TYPE TABLE OF trdir,
        lt_trdir_delta  TYPE TABLE OF xtrdir.


  CALL FUNCTION 'SVRS_COMPUTE_DELTA_REPS'
    EXPORTING
      ignore_case_differences = abap_true
    TABLES
      texttab_old             = it_old
      texttab_new             = it_new
      trdirtab_old            = lt_trdirtab_old
      trdirtab_new            = lt_trdirtab_new
      trdir_delta             = lt_trdir_delta
      text_delta              = rt_delta.

ENDMETHOD.


METHOD diff.

  DATA: lt_repos_tab_new TYPE STANDARD TABLE OF abaptxt255,
        lt_repos_tab_old TYPE STANDARD TABLE OF abaptxt255,
        lt_version_list  TYPE ty_vrsd_old_tt,
        ls_new           LIKE LINE OF lt_version_list,
        ls_old           LIKE LINE OF lt_version_list,
        lt_delta         TYPE STANDARD TABLE OF vxabapt255.


  ASSERT NOT iv_object IS INITIAL.
  ASSERT NOT iv_obj_name IS INITIAL.

  IF iv_object <> 'REPS'.
    BREAK-POINT. " todo
  ENDIF.

  lt_version_list = version_list( iv_object   = iv_object
                                  iv_obj_name = iv_obj_name ).
  READ TABLE lt_version_list INDEX 1 INTO ls_new.
  ASSERT sy-subrc = 0.
  READ TABLE lt_version_list INDEX 2 INTO ls_old.
  ASSERT sy-subrc = 0.

  CALL FUNCTION 'SVRS_GET_VERSION_REPS_40'
    EXPORTING
      object_name           = 'ZLHVP001'
      versno                = ls_new-versno
    TABLES
      repos_tab             = lt_repos_tab_new
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4.
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

  CALL FUNCTION 'SVRS_GET_VERSION_REPS_40'
    EXPORTING
      object_name           = 'ZLHVP001'
      versno                = ls_old-versno
    TABLES
      repos_tab             = lt_repos_tab_old
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4.
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

  lt_delta = delta( it_old = lt_repos_tab_old
                    it_new = lt_repos_tab_new ).
  BREAK-POINT.

ENDMETHOD.


METHOD version_list.

  DATA: lt_lversno_list TYPE STANDARD TABLE OF vrsn,
        lv_vobjname     TYPE vrsd_old-objname,
        lv_vobjtype     TYPE vrsd_old-objtype.


  ASSERT NOT iv_object IS INITIAL.
  ASSERT NOT iv_obj_name IS INITIAL.

  lv_vobjname = iv_obj_name.
  lv_vobjtype = iv_object.

  CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY'
    EXPORTING
      objname      = lv_vobjname
      objtype      = lv_vobjtype
    TABLES
      lversno_list = lt_lversno_list
      version_list = rt_version_list
    EXCEPTIONS
      no_entry     = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

ENDMETHOD.
ENDCLASS.