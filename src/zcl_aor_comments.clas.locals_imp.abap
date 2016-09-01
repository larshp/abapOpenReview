*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

*----------------------------------------------------------------------*
*       CLASS lcl_time DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_time DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: format
      IMPORTING iv_timestamp   TYPE timestamp
      RETURNING value(rv_text) TYPE string.

ENDCLASS.                    "lcl_time DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_time IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_time IMPLEMENTATION.

  METHOD format.

    DATA: lv_time  LIKE sy-uzeit,
          lv_text  TYPE c LENGTH 15,
          lv_date  LIKE sy-datum,
          lv_ctime TYPE c LENGTH 8,
          lv_cdate TYPE c LENGTH 10.


    lv_text = iv_timestamp.
    lv_date = lv_text.
    lv_time = lv_text+8.

    WRITE lv_date TO lv_cdate.
    WRITE lv_time TO lv_ctime.
    CONCATENATE lv_cdate lv_ctime INTO rv_text SEPARATED BY space.

  ENDMETHOD.                    "format

ENDCLASS.                    "lcl_time IMPLEMENTATION