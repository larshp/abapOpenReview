class ZCL_AOR_DIFF definition
  public
  create public .

public section.

  class-methods DIFF
    importing
      !IS_OBJECT type ZAOR_OBJECT
    returning
      value(RT_DELTA) type VXABAPT255_TAB .
protected section.
private section.

  class-methods DELTA
    importing
      !IT_OLD type STANDARD TABLE
      !IT_NEW type STANDARD TABLE
    returning
      value(RT_DELTA) type VXABAPT255_TAB .
  class-methods GET_METH
    importing
      !IV_OBJECT_NAME type VERSOBJNAM
      !IV_VERSNO type VERSNO
    returning
      value(RT_SOURCE) type ABAPTXT255_TAB .
  class-methods GET_REPS
    importing
      !IV_OBJECT_NAME type VERSOBJNAM
      !IV_VERSNO type VERSNO
    returning
      value(RT_SOURCE) type ABAPTXT255_TAB .
  class-methods RESOLVE
    importing
      !IS_OBJECT type ZAOR_OBJECT
    returning
      value(RT_VRSO) type ZIF_AOR_TYPES=>TY_VRSO_TT .
  class-methods VERSION_LIST
    importing
      !IV_OBJECT type TROBJTYPE
      !IV_OBJ_NAME type TROBJ_NAME
    returning
      value(RT_VERSION_LIST) type VRSD_TAB .
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

  DATA: lt_new          TYPE STANDARD TABLE OF abaptxt255,
        lt_old          TYPE STANDARD TABLE OF abaptxt255,
        lt_version_list TYPE vrsd_tab,
        ls_new          LIKE LINE OF lt_version_list,
        ls_old          LIKE LINE OF lt_version_list,
        lt_vrso         TYPE zif_aor_types=>ty_vrso_tt,
        ls_vrso         LIKE LINE OF lt_vrso.


  ASSERT NOT is_object IS INITIAL.

  lt_vrso = resolve( is_object ).
  IF lt_vrso IS INITIAL.
* non versionable object
    RETURN.
  ENDIF.

  IF is_object-object = 'PROG'.
* todo, somehow show diff for all sub objects
    READ TABLE lt_vrso INTO ls_vrso WITH KEY objtype = 'REPS'.
    ASSERT sy-subrc = 0.
  ELSE.
    ls_vrso = lt_vrso[ 1 ].
  ENDIF.

  lt_version_list = version_list( iv_object   = ls_vrso-objtype
                                  iv_obj_name = CONV #( ls_vrso-objname ) ).
  READ TABLE lt_version_list INDEX 1 INTO ls_new.
  ASSERT sy-subrc = 0.
  READ TABLE lt_version_list INDEX 2 INTO ls_old.

  CASE ls_vrso-objtype.
    WHEN 'REPS'.
      lt_new = get_reps( iv_object_name = ls_vrso-objname
                         iv_versno      = ls_new-versno ).
      IF NOT ls_old IS INITIAL.
        lt_old = get_reps( iv_object_name = ls_vrso-objname
                           iv_versno      = ls_old-versno ).
      ENDIF.
    WHEN 'METH'.
      lt_new = get_meth( iv_object_name = ls_vrso-objname
                         iv_versno      = ls_new-versno ).
      IF NOT ls_old IS INITIAL.
        lt_old = get_meth( iv_object_name = ls_vrso-objname
                           iv_versno      = ls_old-versno ).
      ENDIF.
    WHEN OTHERS.
      BREAK-POINT.
      RETURN.
  ENDCASE.

  rt_delta = delta( it_old = lt_old
                    it_new = lt_new ).

ENDMETHOD.


METHOD get_meth.

  CALL FUNCTION 'SVRS_GET_VERSION_METH_40'
    EXPORTING
      object_name           = iv_object_name
      versno                = iv_versno
    TABLES
      repos_tab             = rt_source
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4.
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

ENDMETHOD.


METHOD get_reps.

  CALL FUNCTION 'SVRS_GET_VERSION_REPS_40'
    EXPORTING
      object_name           = iv_object_name
      versno                = iv_versno
    TABLES
      repos_tab             = rt_source
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4.
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

ENDMETHOD.


METHOD resolve.

  DATA: ls_e071 TYPE e071.


  MOVE-CORRESPONDING is_object TO ls_e071.

  CALL FUNCTION 'SVRS_RESOLVE_E071_OBJ'
    EXPORTING
      e071_obj        = ls_e071
    TABLES
      obj_tab         = rt_vrso
    EXCEPTIONS
      not_versionable = 1
      OTHERS          = 2.

ENDMETHOD.


METHOD version_list.

  DATA: lt_lversno_list TYPE STANDARD TABLE OF vrsn,
        lv_vobjname     TYPE vrsd-objname,
        lv_vobjtype     TYPE vrsd-objtype.


  ASSERT NOT iv_object IS INITIAL.
  ASSERT NOT iv_obj_name IS INITIAL.

  lv_vobjname = iv_obj_name.
  lv_vobjtype = iv_object.

  CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
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