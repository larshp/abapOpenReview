CLASS zcl_aor_transport DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

*"* public components of class ZCL_AOR_TRANSPORT
*"* do not include other source files here!!!
    CLASS-METHODS get_description
      IMPORTING
        !iv_trkorr     TYPE trkorr
      RETURNING
        VALUE(rv_text) TYPE as4text .
    CLASS-METHODS get_developer
      IMPORTING
        !iv_trkorr     TYPE trkorr
      RETURNING
        VALUE(rv_user) TYPE e070-as4user .
    CLASS-METHODS list_developers
      IMPORTING
        !iv_trkorr TYPE trkorr .
    CLASS-METHODS list_objects
      IMPORTING
        !iv_trkorr     TYPE trkorr
      RETURNING
        VALUE(rt_data) TYPE e071_t .
    CLASS-METHODS list_open
      IMPORTING
        !it_trkorr     TYPE trrngtrkor_tab OPTIONAL
        !iv_as4user    TYPE e070-as4user DEFAULT sy-uname
      RETURNING
        VALUE(rt_data) TYPE zif_aor_types=>ty_transport_tt .
    CLASS-METHODS validate_open
      IMPORTING
        !iv_trkorr TYPE trkorr
      RAISING
        zcx_aor_error .
  PROTECTED SECTION.
*"* protected components of class ZCL_AOR_TRANSPORT
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AOR_TRANSPORT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AOR_TRANSPORT IMPLEMENTATION.


  METHOD get_description.

    SELECT SINGLE as4text INTO rv_text
      FROM e07t
      WHERE trkorr = iv_trkorr
      AND langu = sy-langu.
    IF sy-subrc <> 0.
      SELECT SINGLE as4text INTO rv_text
        FROM e07t
        WHERE trkorr = iv_trkorr
        AND langu = 'E'.                                  "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.


  METHOD get_developer.

    SELECT SINGLE as4user FROM e070
      INTO rv_user
      WHERE trkorr = iv_trkorr.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0 AND NOT rv_user IS INITIAL.

  ENDMETHOD.


  METHOD list_developers.

* select * from e070 where strkorr = iv_trkorr.

* delete adjacent duplicates
* todo

  ENDMETHOD.


  METHOD list_objects.

    DATA: lt_e070 TYPE TABLE OF e070.


    SELECT * FROM e070 INTO TABLE lt_e070
      WHERE strkorr = iv_trkorr.                          "#EC CI_SUBRC
    IF lines( lt_e070 ) = 0.
      RETURN.
    ENDIF.

    SELECT * FROM e071 INTO TABLE rt_data
      FOR ALL ENTRIES IN lt_e070
      WHERE trkorr = lt_e070-trkorr.                      "#EC CI_SUBRC

  ENDMETHOD.


  METHOD list_open.

    DATA: lv_index LIKE sy-tabix.

    FIELD-SYMBOLS: <ls_data> LIKE LINE OF rt_data.


    SELECT * FROM e070 INTO CORRESPONDING FIELDS OF TABLE rt_data
      WHERE as4user  = iv_as4user
      AND trstatus   = 'D'
      AND trfunction = 'K'
      AND strkorr    = ''
      AND trkorr IN it_trkorr ##too_many_itab_fields.     "#EC CI_SUBRC

    LOOP AT rt_data ASSIGNING <ls_data>.
      lv_index = sy-tabix.

      SELECT COUNT(*) FROM zaor_review WHERE
        trkorr = <ls_data>-trkorr.                      "#EC CI_NOFIELD
      IF sy-subrc = 0.
        DELETE rt_data INDEX lv_index.
        CONTINUE. " current loop
      ENDIF.

      <ls_data>-as4text = get_description( <ls_data>-trkorr ).
    ENDLOOP.

  ENDMETHOD.


  METHOD validate_open.

    DATA: ls_e070 TYPE e070.


    SELECT SINGLE * FROM e070
      INTO ls_e070
      WHERE trstatus = 'D'
      AND trfunction = 'K'
      AND strkorr = ''.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_aor_error
        EXPORTING
          textid = zcx_aor_error=>transport_released.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
