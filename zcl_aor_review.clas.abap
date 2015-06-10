class ZCL_AOR_REVIEW definition
  public
  final
  create public .

public section.

  types:
*"* public components of class ZCL_AOR_REVIEW
*"* do not include other source files here!!!
    TY_REVIEW_TT type standard table of zaor_review with default key .
  types:
    TY_comment_TT type standard table of zaor_comment with default key .

  methods PDF .
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
      !ET_RESULTS type SCIT_ALVLIST .
  class-methods OPEN
    importing
      !IV_TRKORR type TRKORR
    returning
      value(RO_REVIEW) type ref to ZCL_AOR_REVIEW
    raising
      ZCX_AOR_ERROR .
  class-methods LIST
    returning
      value(RT_DATA) type TY_REVIEW_TT .
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
      value(RT_DATA) type TY_COMMENT_TT .
  methods CONSTRUCTOR
    importing
      !IV_TRKORR type TRKORR .
  methods CI_RUN
    raising
      ZCX_AOR_ERROR .
  methods GET_TRKORR
    returning
      value(RV_TRKORR) type TRKORR .
protected section.
*"* protected components of class ZCL_AOR_REVIEW
*"* do not include other source files here!!!
private section.

  data MV_TRKORR type ZAOR_REVIEW-TRKORR .

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

  DATA: lt_comments TYPE ty_comment_tt.

  FIELD-SYMBOLS: <ls_comment> LIKE LINE OF lt_comments.


  lt_comments = comment_list( ).

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
    WHERE trkorr = mv_trkorr.                             "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

  IF lv_status = zif_aor_constants=>c_status_closed.
    RAISE EXCEPTION TYPE zcx_aor_error
      EXPORTING
        textid = zcx_aor_error=>review_closed.
  ENDIF.

ENDMETHOD.


METHOD ci_results.

  DATA: lv_name TYPE sci_insp,
        lo_ci   TYPE REF TO cl_ci_inspection.


  lv_name = mv_trkorr.

  cl_ci_inspection=>get_ref(
    EXPORTING
      p_user          = ''
      p_name          = lv_name
    RECEIVING
      p_ref           = lo_ci
    EXCEPTIONS
      insp_not_exists = 1
      OTHERS          = 2 ).
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

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
        lo_ci      TYPE REF TO cl_ci_inspection,
        lo_objects TYPE REF TO cl_ci_objectset,
        lo_variant TYPE REF TO cl_ci_checkvariant.


  check_open( ).

* todo integration with ATC/local defaults?
  cl_ci_checkvariant=>get_ref(
    EXPORTING
      p_user            = ''
      p_name            = 'DEFAULT'
    RECEIVING
      p_ref             = lo_variant
    EXCEPTIONS
      chkv_not_exists   = 1
      missing_parameter = 2
      OTHERS            = 3 ).
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

  cl_ci_objectset=>get_ref(
    EXPORTING
      p_type                    = cl_ci_objectset=>c_0kor
      p_korr                    = mv_trkorr
    RECEIVING
      p_ref                     = lo_objects
    EXCEPTIONS
      missing_parameter         = 1
      objs_not_exists           = 2
      invalid_request           = 3
      object_not_exists         = 4
      object_may_not_be_checked = 5
      no_main_program           = 6
      OTHERS                    = 7 ).
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

  lv_name = mv_trkorr.
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
  CONCATENATE 'Review' mv_trkorr
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
    SET status = zif_aor_constants=>c_status_closed
    WHERE trkorr = mv_trkorr.                             "#EC CI_SUBRC
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

  ls_comment-trkorr = mv_trkorr.
  ls_comment-text   = iv_text.
  ls_comment-bname  = sy-uname.
  GET TIME STAMP FIELD ls_comment-timestamp.

  INSERT zaor_comment FROM ls_comment.                    "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

ENDMETHOD.


METHOD comment_close.

  DATA: ls_comment TYPE zaor_comment.


  IF iv_topic IS INITIAL.
    RETURN.
  ENDIF.

  check_open( ).

  ls_comment-topic  = iv_topic.
  ls_comment-trkorr = mv_trkorr.
  ls_comment-text   = 'Ok, closed' ##NO_TEXT.
  ls_comment-bname  = sy-uname.
  ls_comment-closed = abap_true.
  GET TIME STAMP FIELD ls_comment-timestamp.

  INSERT zaor_comment FROM ls_comment.                    "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

ENDMETHOD.


METHOD comment_list.

  SELECT * FROM zaor_comment INTO TABLE rt_data
    WHERE trkorr = mv_trkorr
    ORDER BY topic ASCENDING timestamp ASCENDING.         "#EC CI_SUBRC

ENDMETHOD.


METHOD constructor.

  DATA: ls_data TYPE zaor_review.


  SELECT SINGLE * FROM zaor_review INTO ls_data
    WHERE trkorr = iv_trkorr.
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

  mv_trkorr = iv_trkorr.

ENDMETHOD.


METHOD delete.

  DATA: lv_delete TYPE sap_bool.


  BREAK-POINT.
  IF lv_delete = abap_false.
    RETURN.
  ENDIF.

  DELETE FROM zaor_review WHERE trkorr = mv_trkorr.
  DELETE FROM zaor_comment WHERE trkorr = mv_trkorr.

ENDMETHOD.


METHOD get_description.

  rv_text = zcl_aor_transport=>get_description( mv_trkorr ).

ENDMETHOD.


METHOD get_trkorr.

  rv_trkorr = mv_trkorr.

ENDMETHOD.


METHOD list.

  SELECT * FROM zaor_review
    INTO TABLE rt_data
    ORDER BY trkorr.                      "#EC CI_NOWHERE "#EC CI_SUBRC

ENDMETHOD.


METHOD objects_list.

  rt_data = zcl_aor_transport=>list_objects( mv_trkorr ).

ENDMETHOD.


METHOD open.

  DATA: ls_review TYPE zaor_review.


  zcl_aor_transport=>validate_open( iv_trkorr ).

  CLEAR ls_review.
  ls_review-trkorr = iv_trkorr.
  ls_review-status = zif_aor_constants=>c_status_open.
  INSERT zaor_review FROM ls_review.
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

  CREATE OBJECT ro_review
    EXPORTING
      iv_trkorr = iv_trkorr.
  ro_review->ci_run( ).

ENDMETHOD.


METHOD pdf.

  BREAK-POINT.

ENDMETHOD.
ENDCLASS.