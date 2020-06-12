class ZCL_AOR_REVIEW definition
  public
  final
  create public .

public section.

  data POS_NEW_CODE_COMMENTS type ZIF_AOR_TYPES=>TY_CODE_COMMENT_TT read-only .

  methods CI
    returning
      value(RO_CI) type ref to ZCL_AOR_CI .
  methods COMMENTS
    returning
      value(RO_COMMENTS) type ref to ZCL_AOR_COMMENTS .
  methods PDF
    returning
      value(RV_FILE) type STRING .
  methods DELETE .
  methods CHECK_OPEN
    raising
      ZCX_AOR_ERROR .
  methods CLOSE
    raising
      ZCX_AOR_ERROR .
  methods OBJECTS_LIST
    returning
      value(RT_DATA) type E071_T .
  methods CONSTRUCTOR
    importing
      !IV_REVIEW_ID type ZAOR_REVIEW-REVIEW_ID
    raising
      ZCX_AOR_ERROR .
  methods HEADER
    returning
      value(RS_HEADER) type ZIF_AOR_TYPES=>TY_HEADER .
  methods DIFF
    returning
      value(RT_DIFF) type ZIF_AOR_TYPES=>TY_DIFF_LIST_TT .
  methods PRE_ADD_CODE_COMMENT
    importing
      value(IV_POSITION) type ZAOR_CODE_COM .
  methods ON_CODE_COMMENT_POSTED
    importing
      value(comment) type zaor_code_com.
  PROTECTED SECTION.
*"* protected components of class ZCL_AOR_REVIEW
*"* do not include other source files here!!!
  PRIVATE SECTION.

    DATA mv_review_id TYPE zaor_review-review_id .
    CLASS-DATA gv_folder TYPE string .

    METHODS fix_newlines
      IMPORTING
        !it_comments       TYPE zif_aor_types=>ty_comment_tt
      RETURNING
        VALUE(rt_comments) TYPE zif_aor_types=>ty_comment_tt .
    METHODS get_description
      RETURNING
        VALUE(rv_text) TYPE as4text .
    METHODS objects_list_limu
      RETURNING
        VALUE(rt_objects) TYPE zaor_object_tt .
*"* private components of class ZCL_AOR_REVIEW
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AOR_REVIEW IMPLEMENTATION.


  METHOD check_open.

    DATA: lv_status TYPE zaor_review-status.


    SELECT SINGLE status
      FROM zaor_review INTO lv_status
      WHERE review_id = mv_review_id.                     "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    IF lv_status = zif_aor_constants=>c_status-closed.
      RAISE EXCEPTION TYPE zcx_aor_error
        EXPORTING
          textid = zcx_aor_error=>review_closed.
    ENDIF.

  ENDMETHOD.


  METHOD ci.

    CREATE OBJECT ro_ci
      EXPORTING
        io_review = me.

  ENDMETHOD.


  METHOD close.

    comments( )->check_all_closed( ).

    UPDATE zaor_review
      SET status = zif_aor_constants=>c_status-closed
      WHERE review_id = mv_review_id.                     "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD comments.

    CREATE OBJECT ro_comments
      EXPORTING
        io_review = me.

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

    ci( )->delete( ).

    DELETE FROM zaor_review
      WHERE review_id = mv_review_id.                     "#EC CI_SUBRC

    DELETE FROM zaor_review_obj
      WHERE review_id = mv_review_id.                     "#EC CI_SUBRC

    DELETE FROM zaor_comment
      WHERE review_id = mv_review_id.                     "#EC CI_SUBRC

  ENDMETHOD.


  METHOD diff.

    DATA: lt_objects TYPE zaor_object_tt,
          lv_trkorr  TYPE trkorr,
          lt_diff    TYPE zif_aor_types=>ty_diff_tt.

    FIELD-SYMBOLS: <ls_diff>   LIKE LINE OF rt_diff,
                   <ls_object> LIKE LINE OF lt_objects.


    lt_objects = objects_list_limu( ).

    LOOP AT lt_objects ASSIGNING <ls_object>.
      lv_trkorr = mv_review_id(10).
      lt_diff = zcl_aor_diff=>diff( iv_trkorr = lv_trkorr
                                    is_object = <ls_object> ).
      IF lines( lt_diff ) = 0.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO rt_diff ASSIGNING <ls_diff>.
      <ls_diff>-object = <ls_object>.
      <ls_diff>-diff   = lt_diff.
    ENDLOOP.

  ENDMETHOD.


  METHOD fix_newlines.

    DATA: lv_index TYPE i,
          lt_lines TYPE TABLE OF string.

    FIELD-SYMBOLS: <lv_line>    LIKE LINE OF lt_lines,
                   <ls_return>  LIKE LINE OF rt_comments,
                   <ls_comment> LIKE LINE OF it_comments.


    LOOP AT it_comments ASSIGNING <ls_comment>.
      SPLIT <ls_comment>-text AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_lines.

      LOOP AT lt_lines ASSIGNING <lv_line>.
        lv_index = sy-tabix.

        APPEND INITIAL LINE TO rt_comments ASSIGNING <ls_return>.
        IF lv_index = 1.
          MOVE-CORRESPONDING <ls_comment> TO <ls_return>.
        ENDIF.
        <ls_return>-topic     = <ls_comment>-topic.
        <ls_return>-timestamp = <ls_comment>-timestamp.
        <ls_return>-text      = <lv_line>.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_description.

    DATA: lv_trkorr TYPE trkorr.


    lv_trkorr = mv_review_id(10).

    rv_text = zcl_aor_transport=>get_description( lv_trkorr ).

  ENDMETHOD.


  METHOD header.

    SELECT SINGLE * FROM zaor_review
      INTO CORRESPONDING FIELDS OF rs_header
      WHERE review_id = mv_review_id.                     "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    rs_header-as4text = get_description( ).

  ENDMETHOD.


  METHOD objects_list.

    CASE header( )-base.
      WHEN zif_aor_constants=>c_base-transport.
        rt_data = zcl_aor_transport=>list_objects( mv_review_id ).
      WHEN OTHERS.
        SELECT * FROM zaor_review_obj
          INTO CORRESPONDING FIELDS OF TABLE rt_data
          WHERE review_id = mv_review_id ##too_many_itab_fields. "#EC CI_SUBRC
        ASSERT sy-subrc = 0.
    ENDCASE.

  ENDMETHOD.


  METHOD objects_list_limu.

    DATA: ls_e071 TYPE e071,
          lt_list TYPE e071_t,
          lt_vrso TYPE zif_aor_types=>ty_vrso_tt.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF rt_objects,
                   <ls_vrso>   LIKE LINE OF lt_vrso,
                   <ls_list>   LIKE LINE OF lt_list.


    lt_list = objects_list( ).

    LOOP AT lt_list ASSIGNING <ls_list>.
      MOVE-CORRESPONDING <ls_list> TO ls_e071.

      CALL FUNCTION 'SVRS_RESOLVE_E071_OBJ'
        EXPORTING
          e071_obj        = ls_e071
        TABLES
          obj_tab         = lt_vrso
        EXCEPTIONS
          not_versionable = 1
          OTHERS          = 2.                              "#EC CI_SUBRC
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      LOOP AT lt_vrso ASSIGNING <ls_vrso>.
        APPEND INITIAL LINE TO rt_objects ASSIGNING <ls_object>.
        <ls_object>-pgmid    = 'LIMU'.
        <ls_object>-object   = <ls_vrso>-objtype.
        <ls_object>-obj_name = <ls_vrso>-objname.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  method ON_CODE_COMMENT_POSTED.

    DELETE TABLE pos_new_code_comments FROM comment.

  endmethod.


  METHOD pdf.

    DATA: ls_control  TYPE ssfctrlop,
          ls_info     TYPE ssfcrescl,
          lv_size     TYPE i,
          lt_pdf      TYPE STANDARD TABLE OF tline,
          lv_name     TYPE rs38l_fnam,
          ls_header   TYPE zif_aor_types=>ty_header,
          lt_objects  TYPE e071_t,
          lt_comments TYPE zif_aor_types=>ty_comment_tt,
          ls_ci       TYPE zif_aor_types=>ty_ci_st,
          lt_diff     TYPE zif_aor_types=>ty_diff_list_tt.


    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = 'ZAOR_REVIEW'
      IMPORTING
        fm_name            = lv_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.                             "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    ls_control-no_dialog = abap_true.
    ls_control-getotf    = abap_true.

    ls_header   = header( ).
    lt_objects  = objects_list( ).
    lt_comments = fix_newlines( comments( )->list( ) ).
    ls_ci       = ci( )->results( ).
    lt_diff     = diff( ).

    CALL FUNCTION lv_name
      EXPORTING
        control_parameters = ls_control
        is_header          = ls_header
        it_objects         = lt_objects
        it_comments        = lt_comments
        is_ci              = ls_ci
        it_diff            = lt_diff
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
        OTHERS                = 5.                          "#EC CI_SUBRC
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
          OTHERS               = 4 ) ##no_text.           "#EC CI_SUBRC
      ASSERT sy-subrc = 0.
      IF gv_folder IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    CONCATENATE gv_folder '\' mv_review_id '_' sy-datlo '_' sy-timlo '.pdf'
      INTO rv_file ##no_text.

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
        OTHERS                    = 24 ).                 "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    MESSAGE s006(zabapopenreview) WITH rv_file.

  ENDMETHOD.


  METHOD pre_add_code_comment.

    TRY.
        iv_position-topic = cl_system_uuid=>if_system_uuid_static~create_uuid_c22( ).
      CATCH cx_uuid_error.
        ASSERT 1 = 0.
    ENDTRY.
    iv_position-review_id = mv_review_id.

    INSERT iv_position INTO TABLE pos_new_code_comments.

  ENDMETHOD.
ENDCLASS.
