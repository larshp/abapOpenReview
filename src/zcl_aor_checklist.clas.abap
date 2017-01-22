CLASS zcl_aor_checklist DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_checklist,
        description TYPE zaor_checklist-description.
        INCLUDE     TYPE zaor_checked.
    TYPES: END OF ty_checklist.

    TYPES:
      ty_checklist_tt TYPE STANDARD TABLE OF ty_checklist WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING
        !io_review TYPE REF TO zcl_aor_review.
    METHODS list
      RETURNING
        VALUE(rt_list) TYPE ty_checklist_tt.
    METHODS answer
      IMPORTING
        !iv_item   TYPE zaor_checklist_item
        !iv_answer TYPE zaor_checklist_answer.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_review TYPE REF TO zcl_aor_review .
ENDCLASS.



CLASS ZCL_AOR_CHECKLIST IMPLEMENTATION.


  METHOD answer.

    DATA: ls_answer TYPE zaor_checked.


    ls_answer-review_id = mo_review->header( )-review_id.
    ls_answer-item      = iv_item.
    ls_answer-answer    = iv_answer.
    ls_answer-bname     = sy-uname.
    GET TIME STAMP FIELD ls_answer-timestamp.

    MODIFY zaor_checked FROM ls_answer.

  ENDMETHOD.


  METHOD constructor.

    mo_review = io_review.

  ENDMETHOD.


  METHOD list.

    DATA: lt_checked   TYPE STANDARD TABLE OF zaor_checked WITH DEFAULT KEY,
          lv_review_id TYPE zaor_review-review_id.

    FIELD-SYMBOLS: <ls_list>    LIKE LINE OF rt_list,
                   <ls_checked> LIKE LINE OF lt_checked.


    SELECT * FROM zaor_checklist
      INTO CORRESPONDING FIELDS OF TABLE rt_list.

    lv_review_id = mo_review->header( )-review_id.

* todo, outer join instead?
    SELECT * FROM zaor_checked
      INTO TABLE lt_checked
      WHERE review_id = lv_review_id.

    LOOP AT rt_list ASSIGNING <ls_list>.
      READ TABLE lt_checked ASSIGNING <ls_checked> WITH KEY item = <ls_list>-item.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING <ls_checked> TO <ls_list>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
