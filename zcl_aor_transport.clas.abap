class ZCL_AOR_TRANSPORT definition
  public
  final
  create public .

public section.

*"* public components of class ZCL_AOR_TRANSPORT
*"* do not include other source files here!!!
  class-methods GET_DESCRIPTION
    importing
      !IV_TRKORR type TRKORR
    returning
      value(RV_TEXT) type AS4TEXT .
  class-methods GET_DEVELOPER
    importing
      !IV_TRKORR type TRKORR
    returning
      value(RV_USER) type E070-AS4USER .
  class-methods LIST_DEVELOPERS
    importing
      !IV_TRKORR type TRKORR .
  class-methods LIST_OBJECTS
    importing
      !IV_TRKORR type TRKORR
    returning
      value(RT_DATA) type E071_T .
  class-methods LIST_OPEN
    importing
      !IT_TRKORR type TRRNGTRKOR_TAB optional
    returning
      value(RT_DATA) type ZIF_AOR_TYPES=>TY_TRANSPORT_TT .
  class-methods VALIDATE_OPEN
    importing
      !IV_TRKORR type TRKORR
    raising
      ZCX_AOR_ERROR .
protected section.
*"* protected components of class ZCL_AOR_TRANSPORT
*"* do not include other source files here!!!
private section.
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
      AND langu = 'E'.                                    "#EC CI_SUBRC
  ENDIF.

ENDMETHOD.


METHOD get_developer.

  SELECT SINGLE as4user FROM e070
    INTO rv_user
    WHERE trkorr = iv_trkorr.
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
    WHERE strkorr = iv_trkorr.                            "#EC CI_SUBRC
  IF lines( lt_e070 ) = 0.
    RETURN.
  ENDIF.

  SELECT * FROM e071 INTO TABLE rt_data
    FOR ALL ENTRIES IN lt_e070
    WHERE trkorr = lt_e070-trkorr.                        "#EC CI_SUBRC

ENDMETHOD.


METHOD list_open.

  DATA: lv_index     LIKE sy-tabix,
        lv_review_id TYPE zaor_review-review_id.

  FIELD-SYMBOLS: <ls_data> LIKE LINE OF rt_data.


  SELECT * FROM e070 INTO CORRESPONDING FIELDS OF TABLE rt_data
    WHERE as4user  = sy-uname
    AND trstatus   = 'D'
    AND trfunction = 'K'
    AND strkorr    = ''
    AND trkorr IN it_trkorr ##too_many_itab_fields.       "#EC CI_SUBRC

  LOOP AT rt_data ASSIGNING <ls_data>.
    lv_index = sy-tabix.

    CONCATENATE <ls_data>-trkorr '%' INTO lv_review_id.
    SELECT COUNT(*) FROM zaor_review WHERE
      review_id LIKE lv_review_id.
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
    AND strkorr = '' ##WARN_OK.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_aor_error
      EXPORTING
        textid = zcx_aor_error=>transport_released.
  ENDIF.

ENDMETHOD.
ENDCLASS.