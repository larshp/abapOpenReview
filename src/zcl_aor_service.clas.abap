CLASS zcl_aor_service DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS delete_all
      RAISING
        zcx_aor_error .
    CLASS-METHODS list
      RETURNING
        VALUE(rt_data) TYPE zif_aor_types=>ty_review_tt .
    CLASS-METHODS open
      IMPORTING
        !iv_trkorr    TYPE trkorr
        !iv_base      TYPE zaor_review-base
        !iv_ci_filter TYPE zaor_review-ci_filter
      RAISING
        zcx_aor_error .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS ci_run
      IMPORTING
        !iv_review_id TYPE zaor_review-review_id
      RAISING
        zcx_aor_error .
    CLASS-METHODS create
      IMPORTING
        !iv_review_id   TYPE zaor_review-review_id
        !iv_base        TYPE zaor_review-base
        !it_objects     TYPE e071_t OPTIONAL
        !iv_responsible TYPE zaor_review-responsible
        !iv_ci_filter   TYPE zaor_review-ci_filter
        !iv_trkorr      TYPE trkorr .
    CLASS-METHODS open_developer
      IMPORTING
        !iv_trkorr    TYPE trkorr
        !iv_ci_filter TYPE zaor_review-ci_filter
      RAISING
        zcx_aor_error .
    CLASS-METHODS open_object
      IMPORTING
        !iv_trkorr    TYPE trkorr
        !iv_ci_filter TYPE zaor_review-ci_filter
      RAISING
        zcx_aor_error .
    CLASS-METHODS open_transport
      IMPORTING
        !iv_trkorr    TYPE trkorr
        !iv_ci_filter TYPE zaor_review-ci_filter
      RAISING
        zcx_aor_error .
ENDCLASS.



CLASS ZCL_AOR_SERVICE IMPLEMENTATION.


  METHOD ci_run.

    DATA: lo_review TYPE REF TO zcl_aor_review.


    CREATE OBJECT lo_review
      EXPORTING
        iv_review_id = iv_review_id.
    lo_review->ci( )->run( ).

  ENDMETHOD.


  METHOD create.

    DATA: ls_review TYPE zaor_review,
          ls_obj    TYPE zaor_review_obj.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects.


    ASSERT NOT iv_review_id IS INITIAL.
    ASSERT NOT iv_base IS INITIAL.
    ASSERT NOT iv_responsible IS INITIAL.
    ASSERT NOT iv_ci_filter IS INITIAL.
    ASSERT NOT iv_trkorr IS INITIAL.

    ls_review-review_id   = iv_review_id.
    ls_review-status      = zif_aor_constants=>c_status-open.
    ls_review-base        = iv_base.
    ls_review-responsible = iv_responsible.
    ls_review-ci_filter   = iv_ci_filter.
    ls_review-trkorr      = iv_trkorr.
    INSERT zaor_review FROM ls_review.                    "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    LOOP AT it_objects ASSIGNING <ls_object>.
      CLEAR ls_obj.
      MOVE-CORRESPONDING <ls_object> TO ls_obj.
      ls_obj-review_id = iv_review_id.
      INSERT zaor_review_obj FROM ls_obj.                 "#EC CI_SUBRC
      ASSERT sy-subrc = 0.
    ENDLOOP.

  ENDMETHOD.


  METHOD delete_all.

    DATA: lt_reviews TYPE zif_aor_types=>ty_review_tt,
          lo_review  TYPE REF TO zcl_aor_review.

    FIELD-SYMBOLS: <ls_review> LIKE LINE OF lt_reviews.


    lt_reviews = zcl_aor_service=>list( ).

    LOOP AT lt_reviews ASSIGNING <ls_review>.
      CREATE OBJECT lo_review
        EXPORTING
          iv_review_id = <ls_review>-review_id.
      lo_review->delete( ).
    ENDLOOP.

  ENDMETHOD.


  METHOD list.

    SELECT * FROM zaor_review
      INTO TABLE rt_data
      ORDER BY review_id.                 "#EC CI_NOWHERE "#EC CI_SUBRC

  ENDMETHOD.


  METHOD open.
* starts/creates a review. Todo: rename method?

    ASSERT NOT iv_trkorr IS INITIAL.
    ASSERT NOT iv_base IS INITIAL.
    ASSERT NOT iv_ci_filter IS INITIAL.

    zcl_aor_transport=>validate_open( iv_trkorr ).

    CASE iv_base.
      WHEN zif_aor_constants=>c_base-transport.
        open_transport( iv_trkorr = iv_trkorr
                        iv_ci_filter = iv_ci_filter ).
      WHEN zif_aor_constants=>c_base-object.
        open_object( iv_trkorr = iv_trkorr
                     iv_ci_filter = iv_ci_filter ).
      WHEN zif_aor_constants=>c_base-developer.
        open_developer( iv_trkorr = iv_trkorr
                        iv_ci_filter = iv_ci_filter ).
      WHEN OTHERS.
        ASSERT 1 = 1 + 1.
    ENDCASE.

  ENDMETHOD.


  METHOD open_developer.

    BREAK-POINT.

* todo, some way to resolve conflicts, multiple developers for same object
* zif_aor_constants=>c_base-developer

  ENDMETHOD.


  METHOD open_object.

    DATA: lt_objects   TYPE e071_t,
          lt_obj       TYPE e071_t,
          lv_review_id TYPE zaor_review-review_id.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF lt_objects.


    lt_objects = zcl_aor_transport=>list_objects( iv_trkorr ).

    LOOP AT lt_objects ASSIGNING <ls_object>.
      CONCATENATE <ls_object>-trkorr '_' <ls_object>-as4pos INTO lv_review_id.

      CLEAR lt_obj.
      APPEND <ls_object> TO lt_obj.

      create( iv_review_id   = lv_review_id
              iv_base        = zif_aor_constants=>c_base-object
              it_objects     = lt_obj
              iv_responsible = zcl_aor_transport=>get_developer( <ls_object>-trkorr )
              iv_ci_filter   = iv_ci_filter
              iv_trkorr      = iv_trkorr ).

      ci_run( lv_review_id ).

    ENDLOOP.

  ENDMETHOD.


  METHOD open_transport.

    create( iv_review_id   = iv_trkorr
            iv_base        = zif_aor_constants=>c_base-transport
            iv_responsible = zcl_aor_transport=>get_developer( iv_trkorr )
            iv_ci_filter   = iv_ci_filter
            iv_trkorr      = iv_trkorr ).

    ci_run( iv_trkorr ).

  ENDMETHOD.
ENDCLASS.