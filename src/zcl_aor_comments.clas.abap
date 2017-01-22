CLASS zcl_aor_comments DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !io_review TYPE REF TO zcl_aor_review .
    METHODS add
      IMPORTING
        !iv_text  TYPE string
        !iv_topic TYPE zaor_comment-topic OPTIONAL
      RAISING
        zcx_aor_error .
    METHODS close
      IMPORTING
        !iv_topic TYPE zaor_comment-topic
      RAISING
        zcx_aor_error .
    METHODS check_all_closed
      RAISING
        zcx_aor_error .
    METHODS list
      RETURNING
        VALUE(rt_data) TYPE zif_aor_types=>ty_comment_tt .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_review TYPE REF TO zcl_aor_review .

    METHODS topic_id
      IMPORTING
        !iv_topic       TYPE zaor_comment-topic
      RETURNING
        VALUE(rv_topic) TYPE zaor_comment-topic .
ENDCLASS.



CLASS ZCL_AOR_COMMENTS IMPLEMENTATION.


  METHOD add.

    DATA: ls_comment TYPE zaor_comment.


    IF iv_text IS INITIAL.
      RETURN.
    ENDIF.

    mo_review->check_open( ).

    ls_comment-topic     = topic_id( iv_topic ).
    ls_comment-review_id = mo_review->header( )-review_id.
    ls_comment-text      = iv_text.
    ls_comment-bname     = sy-uname.
    GET TIME STAMP FIELD ls_comment-timestamp.

    INSERT zaor_comment FROM ls_comment.                  "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD check_all_closed.

    DATA: lt_comments TYPE zif_aor_types=>ty_comment_tt.

    FIELD-SYMBOLS: <ls_comment> LIKE LINE OF lt_comments.


    lt_comments = list( ).
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


  METHOD close.

    DATA: ls_comment TYPE zaor_comment.


    mo_review->check_open( ).

    ls_comment-topic     = topic_id( iv_topic ).
    ls_comment-review_id = mo_review->header( )-review_id.
    ls_comment-text      = 'Ok, closed' ##NO_TEXT.
    ls_comment-bname     = sy-uname.
    ls_comment-closed    = abap_true.
    GET TIME STAMP FIELD ls_comment-timestamp.

    INSERT zaor_comment FROM ls_comment.                  "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD constructor.

    mo_review = io_review.

  ENDMETHOD.


  METHOD list.

    DATA: lv_review_id TYPE zaor_review-review_id.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF rt_data.


    lv_review_id = mo_review->header( )-review_id.

    SELECT * FROM zaor_comment
      INTO CORRESPONDING FIELDS OF TABLE rt_data
      WHERE review_id = lv_review_id
      ORDER BY topic ASCENDING timestamp ASCENDING.       "#EC CI_SUBRC

    LOOP AT rt_data ASSIGNING <ls_list>.
      <ls_list>-time_formatted = zcl_aor_time=>format( <ls_list>-timestamp ).
    ENDLOOP.

  ENDMETHOD.


  METHOD topic_id.

    IF iv_topic IS INITIAL.
      TRY.
          rv_topic = cl_system_uuid=>if_system_uuid_static~create_uuid_c22( ).
        CATCH cx_uuid_error.
          ASSERT 1 = 1 + 1.
      ENDTRY.
    ELSE.
      rv_topic = iv_topic.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
