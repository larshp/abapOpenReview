CLASS zcl_aor_time DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS format
      IMPORTING
        !iv_timestamp  TYPE timestamp
      RETURNING
        VALUE(rv_text) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOR_TIME IMPLEMENTATION.


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

  ENDMETHOD.
ENDCLASS.