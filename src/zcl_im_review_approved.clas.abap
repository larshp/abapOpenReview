CLASS zcl_im_review_approved DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_ex_cts_request_check .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_IM_REVIEW_APPROVED IMPLEMENTATION.


  METHOD if_ex_cts_request_check~check_before_add_objects.
  ENDMETHOD.


  METHOD if_ex_cts_request_check~check_before_changing_owner.
  ENDMETHOD.


  METHOD if_ex_cts_request_check~check_before_creation.
  ENDMETHOD.


  METHOD if_ex_cts_request_check~check_before_release.
    DATA: lv_review_id TYPE zaor_review-review_id,
          lv_status    TYPE zaor_status,
          lo_review    TYPE REF TO zcl_aor_review,
          lo_error     TYPE REF TO zcx_aor_error.

    IF type <> 'K'.
      RETURN.
    ENDIF.

    SELECT COUNT(*) FROM zaor_config WHERE approve_before_transport_req = abap_true.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.

        SELECT SINGLE review_id status FROM zaor_review
          INTO (lv_review_id, lv_status)
          WHERE review_id = request.
        IF sy-subrc <> 0.
          zcl_aor_service=>open( iv_trkorr = request
            iv_base = zif_aor_constants=>c_base-transport
            iv_ci_filter = zif_aor_constants=>c_ci_filter-none ).
          lv_review_id = request.
          lv_status = zif_aor_constants=>c_status-open.
        ENDIF.

        CREATE OBJECT lo_review
          EXPORTING
            iv_review_id = lv_review_id.
        " when transport request contains code diff, review must be approved or closed
        IF lo_review->diff( ) IS NOT INITIAL AND lv_status = zif_aor_constants=>c_status-open.
          MESSAGE s011(zabapopenreview) RAISING cancel.
        ENDIF.
      CATCH zcx_aor_error INTO lo_error.
        MESSAGE s012(zabapopenreview) WITH lo_error->get_text( ) RAISING cancel.
    ENDTRY.


  ENDMETHOD.


  METHOD if_ex_cts_request_check~check_before_release_slin.
  ENDMETHOD.
ENDCLASS.
