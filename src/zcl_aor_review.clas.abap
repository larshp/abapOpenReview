CLASS zcl_aor_review DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA mv_pos_new_code_comments TYPE zif_aor_types=>ty_code_comment_tt READ-ONLY .
    DATA mv_logger TYPE REF TO zif_logger READ-ONLY .

    METHODS ci
      RETURNING
        VALUE(ro_ci) TYPE REF TO zcl_aor_ci .
    METHODS comments
      RETURNING
        VALUE(ro_comments) TYPE REF TO zcl_aor_comments .
    METHODS pdf
      RETURNING
        VALUE(rv_file) TYPE string .
    METHODS delete .
    METHODS check_open
      RAISING
        zcx_aor_error .
    METHODS objects_list
      RETURNING
        VALUE(rt_data) TYPE e071_t .
    METHODS constructor
      IMPORTING
        !iv_review_id TYPE zaor_review-review_id
      RAISING
        zcx_aor_error .
    METHODS header
      RETURNING
        VALUE(rs_header) TYPE zif_aor_types=>ty_header .
    METHODS diff
      RETURNING
        VALUE(rt_diff) TYPE zif_aor_types=>ty_diff_list_tt .
    METHODS pre_add_code_comment
      IMPORTING
        VALUE(iv_position) TYPE zaor_code_com .
    METHODS on_code_comment_posted
      IMPORTING
        VALUE(comment) TYPE zaor_code_com .
    METHODS get_approvals
      RETURNING
        VALUE(rt_approvals) TYPE zif_aor_types=>ty_approvals_tt .
    METHODS approve
      RAISING
        zcx_aor_error .
    METHODS can_approve
      RETURNING
        VALUE(rv_result) TYPE sap_bool .
    METHODS close
      RAISING
        zcx_aor_error .
    METHODS remove_approval.
    METHODS get_status
      RETURNING VALUE(rv_status) TYPE zaor_status.
protected section.
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


  METHOD approve.
    DATA: ls_approval TYPE zaor_approvals.

    comments( )->check_all_closed( ).

    ls_approval-review_id = mv_review_id.
    ls_approval-approved_by = sy-uname.
    GET TIME STAMP FIELD ls_approval-timestamp.

    INSERT zaor_approvals FROM ls_approval.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_aor_error
        EXPORTING
          textid = zcx_aor_error=>already_approved.
    ENDIF.

    UPDATE zaor_review SET status = zif_aor_constants=>c_status-approved
      WHERE review_id = mv_review_id.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD can_approve.

    SELECT COUNT(*) FROM zaor_approvals
      WHERE review_id = mv_review_id AND approved_by = sy-uname.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    rv_result = abap_true.

  ENDMETHOD.


  METHOD check_open.

    IF get_status( ) = zif_aor_constants=>c_status-closed.
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

    SELECT COUNT(*) FROM zaor_config
      WHERE approve_before_transport_req = abap_true.
    IF sy-subrc = 0.
      SELECT COUNT(*) FROM zaor_review
        WHERE review_id = mv_review_id AND status = zif_aor_constants=>c_status-approved.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_aor_error
          EXPORTING
            textid = zcx_aor_error=>approve_before.
      ENDIF.
    ENDIF.

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

    mv_logger = zcl_logger_factory=>create_log( ).

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

    DATA: lt_objects          TYPE zaor_object_tt,
          lv_trkorr           TYPE trkorr,
          lt_diff             TYPE zif_aor_types=>ty_diff_tt,
          lt_enhancement_diff TYPE zif_aor_types=>ty_enh_diff_tt,
          ls_enhanced_object  TYPE zaor_object,
          ls_new_version      TYPE vrsd,
          lr_failure          TYPE REF TO cx_enh_root.

    FIELD-SYMBOLS: <ls_diff>   LIKE LINE OF rt_diff,
                   <ls_object> LIKE LINE OF lt_objects.


    lt_objects = objects_list_limu( ).

    LOOP AT lt_objects ASSIGNING <ls_object>.

      CLEAR: lt_enhancement_diff, ls_enhanced_object, lt_diff.
      lv_trkorr = mv_review_id(10).
      TRY.
          IF <ls_object>-object = 'ENHO'.
            zcl_aor_diff=>enhancement_diff( EXPORTING is_object = <ls_object>
              iv_trkorr = lv_trkorr
              IMPORTING et_diff = lt_enhancement_diff
                es_enhanced_object = ls_enhanced_object
                es_new_version = ls_new_version ).
          ELSE.
            zcl_aor_diff=>diff( EXPORTING iv_trkorr = lv_trkorr
              is_object = <ls_object>
              IMPORTING et_diff = lt_diff es_new_version = ls_new_version ).
          ENDIF.
        CATCH cx_enh_root INTO lr_failure.
          mv_logger->e( obj_to_log = lr_failure ).
      ENDTRY.
      IF lines( lt_diff ) = 0 AND lines( lt_enhancement_diff ) = 0.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO rt_diff ASSIGNING <ls_diff>.
      <ls_diff>-object = <ls_object>.
      <ls_diff>-diff   = lt_diff.
      <ls_diff>-enhanced_object = ls_enhanced_object.
      <ls_diff>-enhancement_diff = lt_enhancement_diff.
      <ls_diff>-last_changed_date = ls_new_version-datum.
      <ls_diff>-last_changed_time = ls_new_version-zeit.

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


  METHOD get_approvals.
    FIELD-SYMBOLS: <ls_approval> TYPE zif_aor_types=>ty_approval_st.

    SELECT * FROM zaor_approvals
      INTO CORRESPONDING FIELDS OF TABLE rt_approvals
      WHERE review_id = mv_review_id.

    LOOP AT rt_approvals ASSIGNING <ls_approval>.
      <ls_approval>-time_formatted =
        zcl_aor_time=>format_timestamp( <ls_approval>-timestamp ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_description.

    DATA: lv_trkorr TYPE trkorr.


    lv_trkorr = mv_review_id(10).

    rv_text = zcl_aor_transport=>get_description( lv_trkorr ).

  ENDMETHOD.


  METHOD get_status.

    SELECT SINGLE status
      FROM zaor_review INTO rv_status
      WHERE review_id = mv_review_id.                     "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

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


  METHOD on_code_comment_posted.

    DELETE TABLE mv_pos_new_code_comments FROM comment.

  ENDMETHOD.


  METHOD pdf.

    DATA: ls_control   TYPE ssfctrlop,
          ls_info      TYPE ssfcrescl,
          lv_size      TYPE i,
          lt_pdf       TYPE STANDARD TABLE OF tline,
          lv_name      TYPE rs38l_fnam,
          ls_header    TYPE zif_aor_types=>ty_header,
          lt_objects   TYPE e071_t,
          lt_comments  TYPE zif_aor_types=>ty_comment_tt,
          ls_ci        TYPE zif_aor_types=>ty_ci_st,
          lt_diff      TYPE zif_aor_types=>ty_diff_list_tt,
          lt_approvals TYPE zif_aor_types=>ty_approvals_tt.


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

    ls_header    = header( ).
    lt_objects   = objects_list( ).
    lt_comments  = fix_newlines( comments( )->list( ) ).
    ls_ci        = ci( )->results( ).
    lt_diff      = diff( ).
    lt_approvals = get_approvals( ).

    CALL FUNCTION lv_name
      EXPORTING
        control_parameters = ls_control
        is_header          = ls_header
        it_objects         = lt_objects
        it_comments        = lt_comments
        is_ci              = ls_ci
        it_diff            = lt_diff
        it_approvals       = lt_approvals
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

    INSERT iv_position INTO TABLE mv_pos_new_code_comments.

  ENDMETHOD.


  METHOD remove_approval.

    DELETE FROM zaor_approvals WHERE review_id = mv_review_id AND approved_by = sy-uname.

  ENDMETHOD.
ENDCLASS.
