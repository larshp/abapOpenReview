class ZCL_AOR_DIFF definition
  public
  create public .

public section.

  class-methods DIFF
    importing
      !IS_OBJECT type ZAOR_OBJECT
    returning
      value(RT_DIFF) type ZIF_AOR_TYPES=>TY_DIFF_TT .
protected section.
private section.

  class-methods FILTER_VERSIONS
    changing
      !CT_LIST type VRSD_TAB .
  class-methods ADD_NEWLINES
    changing
      !CT_DIFF type ZIF_AOR_TYPES=>TY_DIFF_TT .
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
  class-methods GET_FUNC
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
  class-methods RENDER
    importing
      !IT_OLD type ABAPTXT255_TAB
      !IT_NEW type ABAPTXT255_TAB
      !IT_DELTA type VXABAPT255_TAB
    returning
      value(RT_DIFF) type ZIF_AOR_TYPES=>TY_DIFF_TT .
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


METHOD add_newlines.

  DATA: lv_current TYPE i,
        lv_index   TYPE i,
        lv_new     TYPE i.

  FIELD-SYMBOLS: <ls_diff> LIKE LINE OF ct_diff.


  LOOP AT ct_diff ASSIGNING <ls_diff>.
    lv_index = sy-tabix.

    IF sy-tabix = 1.
      lv_current = <ls_diff>-new.
    ENDIF.
    lv_new = <ls_diff>-new.

    IF NOT lv_new IS INITIAL AND lv_new > lv_current + 1.
      INSERT INITIAL LINE INTO ct_diff INDEX lv_index.
    ENDIF.

    IF NOT lv_new IS INITIAL.
      lv_current = lv_new.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


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
        lv_obj_name     TYPE trobj_name,
        lt_delta        TYPE vxabapt255_tab,
        ls_old          LIKE LINE OF lt_version_list,
        lt_vrso         TYPE zif_aor_types=>ty_vrso_tt,
        ls_vrso         LIKE LINE OF lt_vrso.


  ASSERT NOT is_object IS INITIAL.

  lt_vrso = resolve( is_object ).

  DELETE lt_vrso WHERE objtype = 'DOCU'.

  IF lt_vrso IS INITIAL.
* non versionable object
    RETURN.
  ENDIF.

  ASSERT lines( lt_vrso ) = 1.
  READ TABLE lt_vrso INDEX 1 INTO ls_vrso.

  lv_obj_name = ls_vrso-objname.
  lt_version_list = version_list( iv_object   = ls_vrso-objtype
                                  iv_obj_name = lv_obj_name ).
  filter_versions( CHANGING ct_list = lt_version_list ).
  IF lines( lt_version_list ) = 0.
    RETURN.
  ENDIF.

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
    WHEN 'FUNC'.
      lt_new = get_func( iv_object_name = ls_vrso-objname
                         iv_versno      = ls_new-versno ).
      IF NOT ls_old IS INITIAL.
        lt_old = get_func( iv_object_name = ls_vrso-objname
                           iv_versno      = ls_old-versno ).
      ENDIF.
    WHEN OTHERS.
* todo
      RETURN.
  ENDCASE.

  lt_delta = delta( it_old = lt_old
                    it_new = lt_new ).

  rt_diff = render( it_old   = lt_old
                    it_new   = lt_new
                    it_delta = lt_delta ).

  add_newlines( CHANGING ct_diff = rt_diff ).

ENDMETHOD.


METHOD filter_versions.

  DATA: lv_index      TYPE i,
        lv_trfunction TYPE e070-trfunction.

  FIELD-SYMBOLS: <ls_list> LIKE LINE OF ct_list.


  LOOP AT ct_list ASSIGNING <ls_list>.
    lv_index = sy-tabix.

    IF NOT <ls_list>-korrnum IS INITIAL.
      SELECT SINGLE trfunction
        INTO lv_trfunction
        FROM e070
        WHERE trkorr = <ls_list>-korrnum.
      IF sy-subrc = 0 AND lv_trfunction = 'T'.
* remove transport of copies.
        DELETE ct_list INDEX lv_index.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD get_func.

  CALL FUNCTION 'SVRS_GET_VERSION_FUNC_40'
    EXPORTING
      object_name           = iv_object_name
      versno                = iv_versno
    TABLES
      uincl_tab             = rt_source
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4. "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

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
      OTHERS                = 4. "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

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
      OTHERS                = 4. "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

ENDMETHOD.


METHOD render.

  DATA: lv_diff TYPE i.

  FIELD-SYMBOLS: <ls_diff>  LIKE LINE OF rt_diff,
                 <ls_delta> LIKE LINE OF it_delta,
                 <ls_code>  LIKE LINE OF it_old.


  LOOP AT it_delta ASSIGNING <ls_delta>.

    CASE <ls_delta>-vrsflag.
      WHEN 'I'.
        READ TABLE it_new INDEX <ls_delta>-number + lv_diff ASSIGNING <ls_code>.
        ASSERT sy-subrc = 0.
        APPEND INITIAL LINE TO rt_diff ASSIGNING <ls_diff>.
        <ls_diff>-new   = <ls_delta>-number + lv_diff.
        <ls_diff>-updkz = 'I'.
        <ls_diff>-code  = <ls_code>-line.

        lv_diff = lv_diff + 1.
      WHEN 'D'.
        READ TABLE it_old INDEX <ls_delta>-number ASSIGNING <ls_code>.
        ASSERT sy-subrc = 0.
        APPEND INITIAL LINE TO rt_diff ASSIGNING <ls_diff>.
        <ls_diff>-old   = <ls_delta>-number.
        <ls_diff>-updkz = 'D'.
        <ls_diff>-code  = <ls_code>-line.

        lv_diff = lv_diff - 1.
      WHEN 'U'.
        READ TABLE it_new INDEX <ls_delta>-number + lv_diff ASSIGNING <ls_code>.
        ASSERT sy-subrc = 0.
        APPEND INITIAL LINE TO rt_diff ASSIGNING <ls_diff>.
        <ls_diff>-new   = <ls_delta>-number + lv_diff.
        <ls_diff>-updkz = 'U'.
        <ls_diff>-code  = <ls_code>-line.

        READ TABLE it_old INDEX <ls_delta>-number ASSIGNING <ls_code>.
        ASSERT sy-subrc = 0.
        APPEND INITIAL LINE TO rt_diff ASSIGNING <ls_diff>.
        <ls_diff>-old   = <ls_delta>-number.
        <ls_diff>-updkz = 'U'.
        <ls_diff>-code  = <ls_code>-line.
    ENDCASE.

  ENDLOOP.

  LOOP AT rt_diff ASSIGNING <ls_diff>.
    IF <ls_diff>-new = 0.
      CLEAR <ls_diff>-new.
    ENDIF.
    IF <ls_diff>-old = 0.
      CLEAR <ls_diff>-old.
    ENDIF.
  ENDLOOP.

* todo, merge/rearrange sequential updated lines?

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
      OTHERS          = 2 ##FM_SUBRC_OK. "#EC CI_SUBRC

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
      objname                = lv_vobjname
      objtype                = lv_vobjtype
    TABLES
      lversno_list           = lt_lversno_list
      version_list           = rt_version_list
    EXCEPTIONS
      no_entry               = 1
      communication_failure_ = 2
      system_failure         = 3
      OTHERS                 = 4 ##FM_SUBRC_OK. "#EC CI_SUBRC

ENDMETHOD.
ENDCLASS.