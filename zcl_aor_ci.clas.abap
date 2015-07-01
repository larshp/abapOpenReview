class ZCL_AOR_CI definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_REVIEW type ref to ZCL_AOR_REVIEW .
  methods DELETE .
  methods RESULTS
    importing
      !IV_FILTER type ZAOR_REVIEW-CI_FILTER optional
    returning
      value(RS_INFO) type ZIF_AOR_TYPES=>TY_CI_ST .
  methods RUN
    raising
      ZCX_AOR_ERROR .
protected section.
private section.

  data MO_REVIEW type ref to ZCL_AOR_REVIEW .

  methods OBJECT_TO_INCLUDE
    importing
      !IS_OBJECT type ZAOR_OBJECT
    returning
      value(RV_PROGRAM) type PROGRAMM .
  methods OBJECTSET
    returning
      value(RO_OBJECTSET) type ref to CL_CI_OBJECTSET .
  methods FILTER
    importing
      !IV_FILTER type ZAOR_REVIEW-CI_FILTER optional
    changing
      !CS_INFO type ZIF_AOR_TYPES=>TY_CI_ST .
  methods FILTER_LINES
    changing
      !CS_INFO type ZIF_AOR_TYPES=>TY_CI_ST .
ENDCLASS.



CLASS ZCL_AOR_CI IMPLEMENTATION.


METHOD constructor.

  mo_review = io_review.

ENDMETHOD.


METHOD delete.

  DATA: lo_inspection TYPE REF TO cl_ci_inspection,
        lv_name       TYPE sci_insp,
        lv_objsnam    TYPE sci_objs,
        lo_objectset  TYPE REF TO cl_ci_objectset.


  lv_name = mo_review->header( )-review_id.
  cl_ci_inspection=>get_ref(
    EXPORTING
      p_user          = ''
      p_name          = lv_name
    RECEIVING
      p_ref           = lo_inspection
    EXCEPTIONS
      insp_not_exists = 1
      OTHERS          = 2 ).
  IF sy-subrc = 0.
    lo_inspection->delete( p_mode = 'A' ).
  ENDIF.

  lv_objsnam = mo_review->header( )-review_id.
  cl_ci_objectset=>get_ref(
    EXPORTING
      p_objsnam                 = lv_objsnam
    RECEIVING
      p_ref                     = lo_objectset
    EXCEPTIONS
      missing_parameter         = 1
      objs_not_exists           = 2
      invalid_request           = 3
      object_not_exists         = 4
      object_may_not_be_checked = 5
      no_main_program           = 6
      OTHERS                    = 7 ).
  IF sy-subrc = 0.
    lo_objectset->delete( p_mode = 'A' ).
  ENDIF.

ENDMETHOD.


METHOD filter.

  CASE iv_filter.
    WHEN zif_aor_constants=>c_ci_filter-none.
      RETURN.
    WHEN zif_aor_constants=>c_ci_filter-object.
      BREAK-POINT.
    WHEN zif_aor_constants=>c_ci_filter-include.
      BREAK-POINT.
    WHEN zif_aor_constants=>c_ci_filter-lines.
      filter_lines( CHANGING cs_info = cs_info ).
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.


METHOD filter_lines.

  DATA: lv_include TYPE programm,
        lt_diff    TYPE zif_aor_types=>ty_diff_list_tt,
        lt_results LIKE cs_info-results.

  FIELD-SYMBOLS: <ls_diff>   LIKE LINE OF lt_diff,
                 <ls_result> LIKE LINE OF lt_results,
                 <ls_line>   LIKE LINE OF <ls_diff>-diff.


  lt_diff = mo_review->diff( ).

  lt_results = cs_info-results.
  CLEAR cs_info-results.

  LOOP AT lt_diff ASSIGNING <ls_diff>.
    lv_include = object_to_include( <ls_diff>-object ).
    IF lv_include IS INITIAL.
      CONTINUE.
    ENDIF.
    LOOP AT <ls_diff>-diff ASSIGNING <ls_line>
        WHERE NOT new IS INITIAL.
      LOOP AT lt_results ASSIGNING <ls_result>
          WHERE sobjname = lv_include AND line = <ls_line>-new.
        APPEND <ls_result> TO cs_info-results.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

ENDMETHOD.


METHOD objectset.

  DATA: lt_objects TYPE scit_objs,
        ls_e071    TYPE e071,
        lv_objsnam TYPE sci_objs,
        lv_name    TYPE sci_objs,
        lt_list    TYPE e071_t,
        ls_tadir   TYPE tadir.

  FIELD-SYMBOLS: <ls_object> LIKE LINE OF lt_objects,
                 <ls_review> LIKE LINE OF lt_list.


  CASE mo_review->header( )-base.
    WHEN zif_aor_constants=>c_base-transport.
      cl_ci_objectset=>get_ref(
        EXPORTING
          p_type                    = cl_ci_objectset=>c_0kor
          p_korr                    = mo_review->header( )-review_id
        RECEIVING
          p_ref                     = ro_objectset
        EXCEPTIONS
          missing_parameter         = 1
          objs_not_exists           = 2
          invalid_request           = 3
          object_not_exists         = 4
          object_may_not_be_checked = 5
          no_main_program           = 6
          OTHERS                    = 7 ).                "#EC CI_SUBRC
      ASSERT sy-subrc = 0.
    WHEN zif_aor_constants=>c_base-developer
        OR zif_aor_constants=>c_base-object.
      lv_objsnam = mo_review->header( )-review_id.
      cl_ci_objectset=>get_ref(
        EXPORTING
          p_type                    = cl_ci_objectset=>c_0obj
          p_objsnam                 = lv_objsnam
        RECEIVING
          p_ref                     = ro_objectset
        EXCEPTIONS
          missing_parameter         = 1
          objs_not_exists           = 2
          invalid_request           = 3
          object_not_exists         = 4
          object_may_not_be_checked = 5
          no_main_program           = 6
          OTHERS                    = 7 ).
      IF sy-subrc = 0.
        RETURN.
      ENDIF.
      ASSERT sy-subrc = 2.

* see method cl_wb_object_type=>get_tadir_from_limu
* see class CL_CI_OBJECTSET method MAP_LIMU_TO_R3TR
      lt_list = mo_review->objects_list( ).
      LOOP AT lt_list ASSIGNING <ls_review>.
        IF <ls_review>-pgmid = 'R3TR'
            AND ( <ls_review>-object = 'TABU'
            OR <ls_review>-object = 'SBXL'
            OR <ls_review>-object = 'SBXP' ).
          CONTINUE.
        ENDIF.

        APPEND INITIAL LINE TO lt_objects ASSIGNING <ls_object>.

        IF <ls_review>-pgmid = 'LIMU'.
          MOVE-CORRESPONDING <ls_review> TO ls_e071.
          CALL FUNCTION 'TR_CHECK_TYPE'
            EXPORTING
              wi_e071  = ls_e071
            IMPORTING
              we_tadir = ls_tadir.

          <ls_review>-object   = ls_tadir-object.
          <ls_review>-obj_name = ls_tadir-obj_name.
        ENDIF.

        <ls_object>-objtype = <ls_review>-object.
        <ls_object>-objname = <ls_review>-obj_name.
      ENDLOOP.

      IF lt_objects IS INITIAL.
        RETURN.
      ENDIF.

      lv_name = mo_review->header( )-review_id.
      cl_ci_objectset=>save_from_list(
        EXPORTING
          p_user              = ''
          p_objects           = lt_objects
          p_name              = lv_name
        RECEIVING
          p_ref               = ro_objectset
        EXCEPTIONS
          objs_already_exists = 1
          locked              = 2
          error_in_enqueue    = 3
          not_authorized      = 4
          OTHERS              = 5 ).                      "#EC CI_SUBRC
      ASSERT sy-subrc = 0.

  ENDCASE.

ENDMETHOD.


METHOD object_to_include.

  DATA: ls_mtdkey TYPE seocpdkey.


  CASE is_object-object.
    WHEN 'METH'.
      ls_mtdkey = is_object-obj_name.
      cl_oo_classname_service=>get_method_include(
        EXPORTING
          mtdkey              = ls_mtdkey
        RECEIVING
          result              = rv_program
        EXCEPTIONS
          class_not_existing  = 1
          method_not_existing = 2
          OTHERS              = 3 ).
      IF sy-subrc <> 0.
        BREAK-POINT.
      ENDIF.
    WHEN 'PROG' OR 'REPS'.
      rv_program = is_object-obj_name.
    WHEN 'CINS' OR 'NOTE' OR 'TABU'.
      RETURN.
    WHEN OTHERS.
      BREAK-POINT.
  ENDCASE.

ENDMETHOD.


METHOD results.

  DATA: lv_name   TYPE sci_insp,
        lv_filter TYPE zaor_review-ci_filter,
        lo_ci     TYPE REF TO cl_ci_inspection,
        lo_checkv TYPE REF TO cl_ci_checkvariant.


  lv_name = mo_review->header( )-review_id.

  IF NOT iv_filter IS INITIAL.
    lv_filter = iv_filter.
  ELSE.
    lv_filter = mo_review->header( )-ci_filter.
  ENDIF.

  cl_ci_inspection=>get_ref(
    EXPORTING
      p_user          = ''
      p_name          = lv_name
    RECEIVING
      p_ref           = lo_ci
    EXCEPTIONS
      insp_not_exists = 1
      OTHERS          = 2 ).
  IF sy-subrc = 1.
    RETURN.
  ENDIF.
  ASSERT sy-subrc = 0.

  cl_ci_checkvariant=>get_ref(
    EXPORTING
      p_user            = ''
      p_id              = lo_ci->inspecinf-checkvid
    RECEIVING
      p_ref             = lo_checkv
    EXCEPTIONS
      chkv_not_exists   = 1
      missing_parameter = 2
      OTHERS            = 3 ).                           "#EC CI_SUBRC.
  ASSERT sy-subrc = 0.

  rs_info-chkvinf = lo_checkv->chkvinf.

* make sure SAP note 2043027 is installed
  lo_ci->plain_list(
    IMPORTING
      p_list = rs_info-results ).

  rs_info-header = lo_ci->inspecinf.

  DELETE rs_info-results WHERE objtype = 'STAT'.

  filter( EXPORTING iv_filter = lv_filter
          CHANGING  cs_info   = rs_info ).

ENDMETHOD.


METHOD run.

  DATA: lv_date    TYPE sci_deldat,
        lv_name    TYPE sci_insp,
        lv_text    TYPE sci_text,
        lv_variant TYPE sci_chkv,
        lo_ci      TYPE REF TO cl_ci_inspection,
        lo_objects TYPE REF TO cl_ci_objectset,
        lo_variant TYPE REF TO cl_ci_checkvariant.


  mo_review->check_open( ).

* fetch variant from ATC
  cl_satc_ac_config_access=>load_value(
    EXPORTING
      i_key = 'CI.CHECK.VARIANT'
    IMPORTING
      e_value = lv_variant ).
  IF lv_variant IS INITIAL.
    cl_ci_checkvariant=>get_chkv_alter(
      EXPORTING
        p_checkvname_default = 'DEFAULT'
      IMPORTING
        p_checkvname_new     = lv_variant ).
  ENDIF.

  cl_ci_checkvariant=>get_ref(
    EXPORTING
      p_user            = ''
      p_name            = lv_variant
    RECEIVING
      p_ref             = lo_variant
    EXCEPTIONS
      chkv_not_exists   = 1
      missing_parameter = 2
      OTHERS            = 3 ).                            "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

  lo_objects = objectset( ).
  IF NOT lo_objects IS BOUND.
* no objects valid for code inspection
    RETURN.
  ENDIF.

  lv_name = mo_review->header( )-review_id.
  cl_ci_inspection=>create(
    EXPORTING
      p_user           = ''
      p_name           = lv_name
    RECEIVING
      p_ref            = lo_ci
    EXCEPTIONS
      locked           = 1
      error_in_enqueue = 2
      not_authorized   = 3
      OTHERS           = 4 ).                             "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

  lv_date = sy-datum + 100.
  CONCATENATE 'Review' lv_name
    INTO lv_text SEPARATED BY space ##no_text.

  lo_ci->set( p_chkv    = lo_variant
              p_objs    = lo_objects
              p_text    = lv_text
              p_deldate = lv_date ).

  lo_ci->save( EXCEPTIONS
                 missing_information = 1
                 insp_no_name        = 2
                 not_enqueued        = 3
                 OTHERS              = 4 ).               "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

  lo_ci->run( EXPORTING
                p_howtorun            = 'D'
              EXCEPTIONS
                invalid_check_version = 1
                OTHERS                = 2 ).              "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

ENDMETHOD.
ENDCLASS.