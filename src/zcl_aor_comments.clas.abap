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
        !iv_text               TYPE string
        !iv_topic              TYPE zaor_comment-topic OPTIONAL
      RETURNING
        VALUE(is_code_comment) TYPE sap_bool
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
      IMPORTING
        !iv_with_code_comments TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rt_data)         TYPE zif_aor_types=>ty_comment_tt .
    METHODS delete
      IMPORTING
        !iv_topic TYPE zaor_comment-topic
      RAISING
        zcx_aor_error .
    CLASS-METHODS can_delete
      IMPORTING
        !is_comment       TYPE zif_aor_types=>ty_comment
      RETURNING
        VALUE(can_delete) TYPE abap_bool .
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

    DATA: ls_comment TYPE zaor_comment,
          ls_code_comment TYPE REF TO zaor_code_com.

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

    READ TABLE mo_review->mv_pos_new_code_comments REFERENCE INTO ls_code_comment
      WITH KEY topic = ls_comment-topic.
    IF sy-subrc = 0.
      INSERT zaor_code_com FROM ls_code_comment->*.
      ASSERT sy-subrc = 0.
      mo_review->on_code_comment_posted( ls_code_comment->* ).
    ENDIF.

    SELECT COUNT(*) FROM zaor_code_com WHERE
      review_id = ls_comment-review_id AND topic = ls_comment-topic.
    IF sy-subrc = 0.
      is_code_comment = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD can_delete.

    can_delete = abap_true.

    SELECT COUNT(*) FROM zaor_comment
      WHERE review_id = is_comment-review_id
      AND topic = is_comment-topic AND bname = sy-uname.
    IF sy-subrc <> 0.
      can_delete = abap_false.
    ENDIF.

    SELECT COUNT(*) FROM zaor_comment
      WHERE review_id = is_comment-review_id
      AND topic = is_comment-topic.
    IF sy-dbcnt > 1.
      " conversation exists
      can_delete = abap_false.
    ENDIF.

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

    SELECT COUNT(*) FROM zaor_comment WHERE topic = ls_comment-topic
      AND review_id = ls_comment-review_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_aor_error
        EXPORTING
          textid = zcx_aor_error=>close_new_topic.
    ENDIF.

    INSERT zaor_comment FROM ls_comment.                  "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD constructor.

    mo_review = io_review.

  ENDMETHOD.


  METHOD delete.
    DATA: review_id TYPE zaor_review_id.

    review_id = mo_review->header( )-review_id.

    DELETE FROM zaor_comment WHERE review_id = review_id
      AND topic = iv_topic.

  ENDMETHOD.


  METHOD list.

    DATA: lv_review_id TYPE zaor_review-review_id.
    FIELD-SYMBOLS: <ls_list> LIKE LINE OF rt_data.


    lv_review_id = mo_review->header( )-review_id.

    ##TOO_MANY_ITAB_FIELDS
    SELECT c~review_id c~topic c~timestamp c~text c~bname c~closed
      p~pgmid p~object p~obj_name p~new_line p~old_line
      FROM zaor_comment AS c LEFT OUTER JOIN zaor_code_com AS p
      ON p~review_id = c~review_id AND p~topic = c~topic
      INTO CORRESPONDING FIELDS OF TABLE rt_data
      WHERE c~review_id = lv_review_id
      ORDER BY c~topic ASCENDING c~timestamp ASCENDING.       "#EC CI_SUBRC

    IF iv_with_code_comments = abap_false.
      DELETE rt_data WHERE pgmid IS NOT INITIAL AND object IS NOT INITIAL
        AND obj_name IS NOT INITIAL.
    ENDIF.

    LOOP AT rt_data ASSIGNING <ls_list>.
      <ls_list>-time_formatted = zcl_aor_time=>format_timestamp( <ls_list>-timestamp ).
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
