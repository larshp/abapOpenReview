*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_range DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS trkorr
      IMPORTING
        iv_trkorr TYPE trkorr
      RETURNING
        VALUE(rt_trkorr) TYPE trrngtrkor_tab.

ENDCLASS.                    "lcl_range DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_range IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_range IMPLEMENTATION.

  METHOD trkorr.

    FIELD-SYMBOLS: <ls_trkorr> LIKE LINE OF rt_trkorr.


    APPEND INITIAL LINE TO rt_trkorr ASSIGNING <ls_trkorr>.
    <ls_trkorr>-sign = 'I'.
    <ls_trkorr>-option = 'EQ'.
    <ls_trkorr>-low = iv_trkorr.

  ENDMETHOD.                    "trkorr

ENDCLASS.                    "lcl_range IMPLEMENTATION
