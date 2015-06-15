class ZCL_AOR_REVIEW definition
  public
  final
  create public .

public section.

  methods PDF
    returning
      value(RV_FILE) type STRING .
  methods DELETE .
  methods GET_DESCRIPTION
    returning
      value(RV_TEXT) type AS4TEXT .
  methods CLOSE
    raising
      ZCX_AOR_ERROR .
  methods OBJECTS_LIST
    returning
      value(RT_DATA) type E071_T .
  methods CI_RESULTS
    exporting
      !ES_HEADER type SCIINS_INF
      !ET_RESULTS type SCIT_ALVLIST
      !ES_CHKVINF type SCICHKV_HD .
  methods COMMENT_ADD
    importing
      !IV_TEXT type STRING
      !IV_TOPIC type ZAOR_COMMENT-TOPIC optional
    raising
      ZCX_AOR_ERROR .
  methods COMMENT_CLOSE
    importing
      !IV_TOPIC type ZAOR_COMMENT-TOPIC
    raising
      ZCX_AOR_ERROR .
  methods COMMENT_LIST
    returning
      value(RT_DATA) type ZIF_AOR_TYPES=>TY_COMMENT_TT .
  methods CONSTRUCTOR
    importing
      !IV_REVIEW_ID type ZAOR_REVIEW-REVIEW_ID
    raising
      ZCX_AOR_ERROR .
  methods CI_RUN
    raising
      ZCX_AOR_ERROR .
  methods HEADER
    returning
      value(RS_HEADER) type ZAOR_REVIEW .
protected section.
*"* protected components of class ZCL_AOR_REVIEW
*"* do not include other source files here!!!
private section.

  data MV_REVIEW_ID type ZAOR_REVIEW-REVIEW_ID .
  class-data GV_FOLDER type STRING .

  methods CI_CLEANUP .
  methods OBJECTSET
    returning
      value(RO_OBJECTSET) type ref to CL_CI_OBJECTSET .
  methods CHECK_OPEN
    raising
      ZCX_AOR_ERROR .
  methods CHECK_COMMENTS_CLOSED
    raising
      ZCX_AOR_ERROR .
*"* private components of class ZCL_AOR_REVIEW
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AOR_REVIEW IMPLEMENTATION.


METHOD check_comments_closed.

  DATA: lt_comments TYPE zif_aor_types=>ty_comment_tt.

  FIELD-SYMBOLS: <ls_comment> LIKE LINE OF lt_comments.


  lt_comments = comment_list( ).
  IF lt_comments IS INITIAL.
    RAISE EXCEPTION TYPE zcx_aor_error
      EXPORTING
        textid = zcx_aor_error=>no_comments.
  ENDIF.

  LOOP AT lt_comments ASSIGNING <ls_comment> WHERE closed = abap_true.
    DELETE lt_comments WHERE topic = <ls_comment>-topic.
  ENDLOOP.

  IF NOT lt_comments IS INITIAL.
    RAISE EXCEPTION TYPE zcx_aor_error
      EXPORTING
        textid = zcx_aor_error=>comments_pending.
  ENDIF.

ENDMETHOD.


METHOD check_open.

  DATA: lv_status TYPE zaor_review-status.


  SELECT SINGLE status
    FROM zaor_review INTO lv_status
    WHERE review_id = mv_review_id.                       "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

  IF lv_status = zif_aor_constants=>c_status-closed.
    RAISE EXCEPTION TYPE zcx_aor_error
      EXPORTING
        textid = zcx_aor_error=>review_closed.
  ENDIF.

ENDMETHOD.


METHOD ci_cleanup.

  DATA: lo_inspection TYPE REF TO cl_ci_inspection,
        lo_objectset  TYPE REF TO cl_ci_objectset.


  cl_ci_inspection=>get_ref(
    EXPORTING
      p_user          = ''
      p_name          = CONV #( mv_review_id )
    RECEIVING
      p_ref           = lo_inspection
    EXCEPTIONS
      insp_not_exists = 1
      OTHERS          = 2 ).
  IF sy-subrc = 0.
    lo_inspection->delete( p_mode = 'A' ).
  ENDIF.

  cl_ci_objectset=>get_ref(
    EXPORTING
      p_objsnam                 = CONV #( mv_review_id )
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


METHOD ci_results.

  DATA: lv_name   TYPE sci_insp,
        lo_ci     TYPE REF TO cl_ci_inspection,
        lo_checkv TYPE REF TO cl_ci_checkvariant.


  lv_name = mv_review_id.

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
      OTHERS            = 3 ).
  ASSERT sy-subrc = 0.

  es_chkvinf = lo_checkv->chkvinf.

* make sure SAP note 2043027 is installed
  lo_ci->plain_list(
    IMPORTING
      p_list = et_results ).

  es_header = lo_ci->inspecinf.

  DELETE et_results WHERE objtype = 'STAT'.

ENDMETHOD.


METHOD ci_run.

  DATA: lv_date    TYPE sci_deldat,
        lv_name    TYPE sci_insp,
        lv_text    TYPE sci_text,
        lv_variant TYPE sci_chkv,
        lo_ci      TYPE REF TO cl_ci_inspection,
        lo_objects TYPE REF TO cl_ci_objectset,
        lo_variant TYPE REF TO cl_ci_checkvariant.


  check_open( ).

* fetch variant from ATC
  cl_satc_ac_config_access=>load_value(
    EXPORTING
      i_key = if_satc_ac_config_state_names=>c_config-ci_check_variant
    IMPORTING
      e_value = lv_variant ).

  cl_ci_checkvariant=>get_ref(
    EXPORTING
      p_user            = ''
      p_name            = lv_variant
    RECEIVING
      p_ref             = lo_variant
    EXCEPTIONS
      chkv_not_exists   = 1
      missing_parameter = 2
      OTHERS            = 3 ). "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

  lo_objects = objectset( ).
  IF NOT lo_objects IS BOUND.
* no objects valid for code inspection
    RETURN.
  ENDIF.

  lv_name = mv_review_id.
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
      OTHERS           = 4 ).
  IF sy-subrc <> 0.
* todo
    BREAK-POINT.
  ENDIF.

  lv_date = sy-datum + 100.
  CONCATENATE 'Review' mv_review_id
    INTO lv_text SEPARATED BY space ##NO_TEXT.

  lo_ci->set( EXPORTING
                p_chkv    = lo_variant
                p_objs    = lo_objects
                p_text    = lv_text
                p_deldate = lv_date ).

  lo_ci->save( EXCEPTIONS
                 missing_information = 1
                 insp_no_name        = 2
                 not_enqueued        = 3
                 OTHERS              = 4 ).
  IF sy-subrc <> 0.
* Implement suitable error handling here
    BREAK-POINT.
  ENDIF.

  lo_ci->run( EXPORTING
                p_howtorun            = 'D'
              EXCEPTIONS
                invalid_check_version = 1
                OTHERS                = 2 ).
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

ENDMETHOD.


METHOD close.

  check_comments_closed( ).

  UPDATE zaor_review
    SET status = zif_aor_constants=>c_status-closed
    WHERE review_id = mv_review_id.                       "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

ENDMETHOD.


METHOD comment_add.

  DATA: ls_comment TYPE zaor_comment.


  IF iv_text IS INITIAL.
    RETURN.
  ENDIF.

  check_open( ).

  IF iv_topic IS INITIAL.
    TRY.
        ls_comment-topic = cl_system_uuid=>if_system_uuid_static~create_uuid_c22( ).
      CATCH cx_uuid_error.
        ASSERT 1 = 1 + 1.
    ENDTRY.
  ELSE.
    ls_comment-topic = iv_topic.
  ENDIF.

  ls_comment-review_id = mv_review_id.
  ls_comment-text   = iv_text.
  ls_comment-bname  = sy-uname.
  GET TIME STAMP FIELD ls_comment-timestamp.

  INSERT zaor_comment FROM ls_comment.                    "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

ENDMETHOD.


METHOD comment_close.

  DATA: ls_comment TYPE zaor_comment.


  check_open( ).

  IF iv_topic IS INITIAL.
    ls_comment-topic = cl_system_uuid=>if_system_uuid_static~create_uuid_c22( ).
  ELSE.
    ls_comment-topic = iv_topic.
  ENDIF.
  ls_comment-review_id = mv_review_id.
  ls_comment-text   = 'Ok, closed' ##NO_TEXT.
  ls_comment-bname  = sy-uname.
  ls_comment-closed = abap_true.
  GET TIME STAMP FIELD ls_comment-timestamp.

  INSERT zaor_comment FROM ls_comment.                    "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

ENDMETHOD.


METHOD comment_list.

  SELECT * FROM zaor_comment INTO TABLE rt_data
    WHERE review_id = mv_review_id
    ORDER BY topic ASCENDING timestamp ASCENDING.         "#EC CI_SUBRC

ENDMETHOD.


METHOD constructor.

  SELECT SINGLE review_id FROM zaor_review
    INTO mv_review_id
    WHERE review_id = iv_review_id.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_aor_error
      EXPORTING
        textid = zcx_aor_error=>not_found.
  ENDIF.

ENDMETHOD.


METHOD delete.

  DELETE FROM zaor_review
    WHERE review_id = mv_review_id.                       "#EC CI_SUBRC

  DELETE FROM zaor_review_obj
    WHERE review_id = mv_review_id.                       "#EC CI_SUBRC

  DELETE FROM zaor_comment
    WHERE review_id = mv_review_id.                       "#EC CI_SUBRC

  ci_cleanup( ).

ENDMETHOD.


METHOD get_description.

  DATA: lv_trkorr TYPE trkorr.


  lv_trkorr = mv_review_id(10).

  rv_text = zcl_aor_transport=>get_description( lv_trkorr ).

ENDMETHOD.


METHOD header.

  SELECT SINGLE * FROM zaor_review
    INTO rs_header
    WHERE review_id = mv_review_id.                       "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

ENDMETHOD.


METHOD objectset.

  DATA: lt_objects TYPE scit_objs,
        ls_e071    TYPE e071,
        ls_tadir   TYPE tadir.


  CASE header( )-base.
    WHEN zif_aor_constants=>c_base-transport.
      cl_ci_objectset=>get_ref(
        EXPORTING
          p_type                    = cl_ci_objectset=>c_0kor
          p_korr                    = mv_review_id
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
      cl_ci_objectset=>get_ref(
        EXPORTING
          p_type                    = cl_ci_objectset=>c_0obj
          p_objsnam                 = CONV #( mv_review_id )
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
      ELSEIF sy-subrc <> 2.
        BREAK-POINT.
      ENDIF.

* see method cl_wb_object_type=>get_tadir_from_limu
* see class CL_CI_OBJECTSET method MAP_LIMU_TO_R3TR
      LOOP AT objects_list( ) ASSIGNING FIELD-SYMBOL(<ls_review>).
        IF <ls_review>-pgmid = 'R3TR'
            AND ( <ls_review>-object = 'TABU'
            OR <ls_review>-object = 'SBXL'
            OR <ls_review>-object = 'SBXP' ).
          CONTINUE.
        ENDIF.

        APPEND INITIAL LINE TO lt_objects ASSIGNING FIELD-SYMBOL(<ls_object>).

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

      cl_ci_objectset=>save_from_list(
        EXPORTING
          p_user              = ''
          p_objects           = lt_objects
          p_name              = CONV #( mv_review_id )
        RECEIVING
          p_ref               = ro_objectset
        EXCEPTIONS
          objs_already_exists = 1
          locked              = 2
          error_in_enqueue    = 3
          not_authorized      = 4
          OTHERS              = 5 ).
      IF sy-subrc <> 0.
        BREAK-POINT.
      ENDIF.

  ENDCASE.

ENDMETHOD.


METHOD objects_list.

  CASE header( )-base.
    WHEN zif_aor_constants=>c_base-transport.
      rt_data = zcl_aor_transport=>list_objects( mv_review_id ).
    WHEN OTHERS.
      SELECT * FROM zaor_review_obj
        INTO CORRESPONDING FIELDS OF TABLE rt_data
        WHERE review_id = mv_review_id ##TOO_MANY_ITAB_FIELDS. "#EC CI_SUBRC
      ASSERT sy-subrc = 0.
  ENDCASE.

ENDMETHOD.


METHOD pdf.

  DATA: ls_control TYPE ssfctrlop,
        ls_info    TYPE ssfcrescl,
        lv_size    TYPE i,
        lt_results TYPE scit_alvlist,
        lt_pdf     TYPE STANDARD TABLE OF tline,
        lv_name    TYPE rs38l_fnam.


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZAOR_REVIEW'
    IMPORTING
      fm_name            = lv_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3. "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

  ls_control-no_dialog = abap_true.
  ls_control-getotf    = abap_true.

  ci_results( IMPORTING et_results = lt_results ).

  CALL FUNCTION lv_name
    EXPORTING
      control_parameters = ls_control
      is_header          = header( )
      it_objects         = objects_list( )
      it_comments        = comment_list( )
      it_results         = lt_results
    IMPORTING
      job_output_info    = ls_info
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc = 4.
    RETURN.
  ENDIF.
  ASSERT sy-subrc = 0.

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
    IMPORTING
      bin_filesize          = lv_size
    TABLES
      otf                   = ls_info-otfdata[]
      lines                 = lt_pdf
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      err_bad_otf           = 4
      OTHERS                = 5. "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

  IF gv_folder IS INITIAL.
    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = 'Select folder'
      CHANGING
        selected_folder      = gv_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ) ##NO_TEXT.
    ASSERT sy-subrc = 0.
    IF gv_folder IS INITIAL.
      RETURN.
    ENDIF.
  ENDIF.

  CONCATENATE gv_folder '\' mv_review_id '_' sy-datlo '_' sy-timlo '.pdf'
    INTO rv_file ##NO_TEXT.

  cl_gui_frontend_services=>gui_download(
    EXPORTING
      bin_filesize              = lv_size
      filename                  = rv_file
      filetype                  = 'BIN'
    CHANGING
      data_tab                  = lt_pdf
    EXCEPTIONS
      file_write_error          = 1
      no_batch                  = 2
      gui_refuse_filetransfer   = 3
      invalid_type              = 4
      no_authority              = 5
      unknown_error             = 6
      header_not_allowed        = 7
      separator_not_allowed     = 8
      filesize_not_allowed      = 9
      header_too_long           = 10
      dp_error_create           = 11
      dp_error_send             = 12
      dp_error_write            = 13
      unknown_dp_error          = 14
      access_denied             = 15
      dp_out_of_memory          = 16
      disk_full                 = 17
      dp_timeout                = 18
      file_not_found            = 19
      dataprovider_exception    = 20
      control_flush_error       = 21
      not_supported_by_gui      = 22
      error_no_gui              = 23
      OTHERS                    = 24 ).                   "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

  MESSAGE s006(zabapopenreview) WITH rv_file.

ENDMETHOD.
ENDCLASS.