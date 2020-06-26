CLASS zcl_aor_time DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS format_timestamp
      IMPORTING
        !iv_timestamp  TYPE timestamp
      RETURNING
        VALUE(rv_text) TYPE string .
    CLASS-METHODS format_date_time
      IMPORTING
        !iv_date       TYPE d
        !iv_time       TYPE t
      RETURNING
        VALUE(rv_text) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOR_TIME IMPLEMENTATION.


  METHOD format_date_time.
    DATA: lv_cdate(10) TYPE c,
          lv_ctime(8)  TYPE c.

    WRITE iv_date TO lv_cdate.
    WRITE iv_time TO lv_ctime.
    CONCATENATE lv_cdate lv_ctime INTO rv_text SEPARATED BY space.

  ENDMETHOD.


  METHOD format_timestamp.

    DATA: lv_time            LIKE sy-uzeit,
          lv_date            LIKE sy-datum,
          lv_ctime           TYPE c LENGTH 8,
          lv_cdate           TYPE c LENGTH 10,
          lv_zone_app_server TYPE ttzcu-tzonesys.

    SELECT SINGLE tzonesys FROM ttzcu INTO lv_zone_app_server.
    CONVERT TIME STAMP iv_timestamp TIME ZONE lv_zone_app_server
      INTO DATE lv_date TIME lv_time.

    WRITE lv_date TO lv_cdate.
    WRITE lv_time TO lv_ctime.
    CONCATENATE lv_cdate lv_ctime INTO rv_text SEPARATED BY space.

  ENDMETHOD.
ENDCLASS.
