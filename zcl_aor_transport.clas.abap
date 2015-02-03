class ZCL_AOR_TRANSPORT definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_AOR_TRANSPORT
*"* do not include other source files here!!!

  types:
    begin of TY_TRANSPORT .
  include type e070.
  types: as4text type e07t-as4text,
    end of ty_transport .
  types:
    TY_TRANSPORT_tt type standard table of ty_transport with default key .

  class-methods LIST_OPEN
    importing
      !IT_TRKORR type TRRNGTRKOR_TAB optional
    returning
      value(RT_DATA) type ZCL_AOR_TRANSPORT=>TY_TRANSPORT_TT .
protected section.
*"* protected components of class ZCL_AOR_TRANSPORT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AOR_TRANSPORT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AOR_TRANSPORT IMPLEMENTATION.


METHOD list_open.

  DATA: lv_index LIKE sy-tabix.

  FIELD-SYMBOLS: <ls_data> LIKE LINE OF rt_data.


  SELECT * FROM e070 INTO CORRESPONDING FIELDS OF TABLE rt_data
    WHERE as4user = sy-uname
    AND trstatus = 'D'
    AND trfunction = 'K'
    AND strkorr = ''
    AND trkorr IN it_trkorr ##too_many_itab_fields.

  LOOP AT rt_data ASSIGNING <ls_data>.
    lv_index = sy-tabix.

    SELECT COUNT(*) FROM zaor_review WHERE trkorr = <ls_data>-trkorr.
    IF sy-subrc = 0.
      DELETE rt_data INDEX lv_index.
      CONTINUE. " current loop
    ENDIF.

    SELECT SINGLE as4text
      FROM e07t
      INTO <ls_data>-as4text
      WHERE trkorr = <ls_data>-trkorr
      AND langu = sy-langu.
    IF sy-subrc <> 0.
      SELECT SINGLE as4text
        FROM e07t
        INTO <ls_data>-as4text
        WHERE trkorr = <ls_data>-trkorr
        AND langu = 'E'.
    ENDIF.
  ENDLOOP.

ENDMETHOD.
ENDCLASS.