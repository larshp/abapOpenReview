class ZCL_AOR_REVIEW definition
  public
  final
  create public .

public section.

  types:
*"* public components of class ZCL_AOR_REVIEW
*"* do not include other source files here!!!
    TY_REVIEW_TT type table of zaor_review with default key .
  types:
    TY_comment_TT type table of zaor_comment with default key .

  constants C_STATUS_OPEN type CHAR1 value 'O' ##NO_TEXT.
  constants C_STATUS_CLOSED type CHAR1 value 'C' ##NO_TEXT.

  class-methods CI_RESULTS
    importing
      !IV_TRKORR type TRKORR
    returning
      value(RT_RESULTS) type SCIT_ALVLIST .
  class-methods NEW
    importing
      !IV_TRKORR type TRKORR .
  class-methods LIST
    returning
      value(RT_DATA) type TY_REVIEW_TT .
  class-methods COMMENT_ADD
    importing
      !IV_TRKORR type TRKORR
      !IV_TEXT type STRING
      !IV_TOPIC type ZAOR_COMMENT-TOPIC optional .
  class-methods COMMENT_LIST
    importing
      !IV_TRKORR type TRKORR
    returning
      value(RT_DATA) type TY_COMMENT_TT .
protected section.

  class-methods CI_RUN
    importing
      !IV_TRKORR type TRKORR .
*"* protected components of class ZCL_AOR_REVIEW
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AOR_REVIEW
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AOR_REVIEW IMPLEMENTATION.


METHOD ci_results.

  DATA: lv_name TYPE sci_insp,
        lo_ci   TYPE REF TO cl_ci_inspection.


  lv_name = iv_trkorr.

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
      p_list = rt_results ).

  BREAK-POINT.

ENDMETHOD.


METHOD CI_RUN.

  DATA: lv_date    TYPE sci_deldat,
        lv_name    TYPE sci_insp,
        lv_text    TYPE sci_text,
        lo_ci      TYPE REF TO cl_ci_inspection,
        lo_objects TYPE REF TO cl_ci_objectset,
        lo_variant TYPE REF TO cl_ci_checkvariant.


* todo integration with ATC/local defaults?
  cl_ci_checkvariant=>get_ref(
    EXPORTING
      p_user            = sy-uname
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
      p_korr                    = iv_trkorr
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

  lv_name = iv_trkorr.
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
  CONCATENATE 'Review' iv_trkorr INTO lv_text SEPARATED BY space.

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


METHOD comment_add.

  DATA: ls_comment TYPE zaor_comment.


  zcl_aor_transport=>validate_open( iv_trkorr ).

  IF iv_text IS INITIAL.
    RETURN.
  ENDIF.


  IF iv_topic IS INITIAL.
* todo, this is bad?
    SELECT MAX( topic ) INTO ls_comment-topic
      FROM zaor_comment
      WHERE trkorr = iv_trkorr.
    ls_comment-topic = ls_comment-topic + 1.
  ELSE.
    ls_comment-topic = iv_topic.
  ENDIF.

* todo, this is bad?
  SELECT MAX( id ) INTO ls_comment-id
    FROM zaor_comment WHERE trkorr = iv_trkorr
    AND topic = ls_comment-topic.
  ls_comment-id = ls_comment-id + 1.

  ls_comment-trkorr = iv_trkorr.
  ls_comment-text   = iv_text.
  ls_comment-bname  = sy-uname.
  GET TIME STAMP FIELD ls_comment-timestamp.

  INSERT zaor_comment FROM ls_comment.

ENDMETHOD.


METHOD comment_list.

  SELECT * FROM zaor_comment INTO TABLE rt_data
    WHERE trkorr = iv_trkorr
    ORDER BY topic ASCENDING id ASCENDING.

ENDMETHOD.


METHOD list.

  SELECT * FROM zaor_review INTO TABLE rt_data.

ENDMETHOD.


METHOD new.

  DATA: ls_review TYPE zaor_review.


  zcl_aor_transport=>validate_open( iv_trkorr ).

  ci_run( iv_trkorr ).

  CLEAR ls_review.
  ls_review-trkorr = iv_trkorr.
  ls_review-status = c_status_open.
  INSERT zaor_review FROM ls_review.
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

ENDMETHOD.
ENDCLASS.