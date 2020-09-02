CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_aor_diff DEFINITION LOCAL FRIENDS ltcl_test.

*----------------------------------------------------------------------*
*       CLASS ltcl_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA: mt_old  TYPE abaptxt255_tab,
          mt_new  TYPE abaptxt255_tab,
          mt_diff TYPE zif_aor_types=>ty_diff_tt,
          mt_boundary_lines TYPE zif_aor_types=>ty_boundary_lines_tt.

    METHODS: setup.
    METHODS: run.
    METHODS: check IMPORTING it_diff TYPE zif_aor_types=>ty_diff_tt.

    METHODS: render_001 FOR TESTING,
             render_002 FOR TESTING,
             render_003 FOR TESTING,
             render_004 FOR TESTING,
             render_005 FOR TESTING,
             render_006 FOR TESTING,
             render_007 FOR TESTING,
             render_008 FOR TESTING,
             render_009 FOR TESTING,
             render_010 FOR TESTING.

ENDCLASS.       "ltcl_Test

*----------------------------------------------------------------------*
*       CLASS ltcl_test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test IMPLEMENTATION.

  DEFINE _new.
    append &1 to mt_new.
  END-OF-DEFINITION.

  DEFINE _old.
    append &1 to mt_old.
  END-OF-DEFINITION.

  METHOD setup.
    CLEAR mt_old.
    CLEAR mt_new.
    CLEAR mt_diff.
    CLEAR mt_boundary_lines.
  ENDMETHOD.                    "setup

  METHOD run.

    DATA: lt_delta TYPE vxabapt255_tab.


    lt_delta = zcl_aor_diff=>delta( it_old = mt_old
                                    it_new = mt_new
                                    it_boundary_lines = mt_boundary_lines ).

    mt_diff = zcl_aor_diff=>render( it_old   = mt_old
                                    it_new   = mt_new
                                    it_delta = lt_delta ).

  ENDMETHOD.                    "run

  METHOD check.

    run( ).

    cl_abap_unit_assert=>assert_equals( act = mt_diff
                                        exp = it_diff ).

  ENDMETHOD.                    "check

  METHOD render_001.

    DATA: lt_expected TYPE zif_aor_types=>ty_diff_tt.

    FIELD-SYMBOLS: <ls_expected> LIKE LINE OF lt_expected.

* insert
    _new 'write'.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-new = 1.
    <ls_expected>-code = 'write'.
    <ls_expected>-updkz = 'I'.

    check( lt_expected ).

  ENDMETHOD.                    "render_001

  METHOD render_002.

    DATA: lt_expected TYPE zif_aor_types=>ty_diff_tt.

    FIELD-SYMBOLS: <ls_expected> LIKE LINE OF lt_expected.

* delete
    _old 'write'.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-old = 1.
    <ls_expected>-code = 'write'.
    <ls_expected>-updkz = 'D'.

    check( lt_expected ).

  ENDMETHOD.                    "render_002

  METHOD render_003.

    DATA: lt_expected TYPE zif_aor_types=>ty_diff_tt.

    FIELD-SYMBOLS: <ls_expected> LIKE LINE OF lt_expected.

* update
    _old 'foo'.
    _new 'bar'.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-new = 1.
    <ls_expected>-code = 'bar'.
    <ls_expected>-updkz = 'U'.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-old = 1.
    <ls_expected>-code = 'foo'.
    <ls_expected>-updkz = 'U'.

    check( lt_expected ).

  ENDMETHOD.                    "render_003

  METHOD render_004.

    DATA: lt_expected TYPE zif_aor_types=>ty_diff_tt,
          ls_boundary TYPE zif_aor_types=>ty_boundary_line.

    FIELD-SYMBOLS: <ls_expected> LIKE LINE OF lt_expected.

* delete 2nd line
    _old 'foo'.
    _old 'bar'.

    _new 'foo'.

    ls_boundary-start = 2.
    ls_boundary-end = 2.
    APPEND ls_boundary TO mt_boundary_lines.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-old = 2.
    <ls_expected>-code = 'bar'.
    <ls_expected>-updkz = 'D'.

    check( lt_expected ).

  ENDMETHOD.                    "render_004

  METHOD render_005.

    DATA: lt_expected TYPE zif_aor_types=>ty_diff_tt,
          ls_boundary TYPE zif_aor_types=>ty_boundary_line.

    FIELD-SYMBOLS: <ls_expected> LIKE LINE OF lt_expected.

* update 2nd line
    _old 'foo'.
    _old 'bar'.

    _new 'foo'.
    _new 'bar moo'.

    ls_boundary-start = 2.
    ls_boundary-end = 2.
    APPEND ls_boundary TO mt_boundary_lines.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-new = 2.
    <ls_expected>-code = 'bar moo'.
    <ls_expected>-updkz = 'U'.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-old = 2.
    <ls_expected>-code = 'bar'.
    <ls_expected>-updkz = 'U'.

    check( lt_expected ).

  ENDMETHOD.                    "render_005

  METHOD render_006.

    DATA: lt_expected TYPE zif_aor_types=>ty_diff_tt.

    FIELD-SYMBOLS: <ls_expected> LIKE LINE OF lt_expected.

* insert 2 lines
    _new 'foo'.
    _new 'bar'.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-new = 1.
    <ls_expected>-code = 'foo'.
    <ls_expected>-updkz = 'I'.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-new = 2.
    <ls_expected>-code = 'bar'.
    <ls_expected>-updkz = 'I'.

    check( lt_expected ).

  ENDMETHOD.                    "render_006

  METHOD render_007.

    DATA: lt_expected TYPE zif_aor_types=>ty_diff_tt.

    FIELD-SYMBOLS: <ls_expected> LIKE LINE OF lt_expected.

* insert between 2 lines
    _old 'foo'.
    _old 'bar'.

    _new 'foo'.
    _new 'moo'.
    _new 'bar'.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-new = 2.
    <ls_expected>-code = 'moo'.
    <ls_expected>-updkz = 'I'.

    check( lt_expected ).

  ENDMETHOD.                    "render_007

  METHOD render_008.

    DATA: lt_expected TYPE zif_aor_types=>ty_diff_tt.

    FIELD-SYMBOLS: <ls_expected> LIKE LINE OF lt_expected.

* insert between 2 lines and update
    _old '1'.
    _old '2'.
    _old '3'.
    _old '4'.

    _new '1'.
    _new '2'.
    _new 'inserted'.
    _new '3'.
    _new '4 update'.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-new = 3.
    <ls_expected>-code = 'inserted'.
    <ls_expected>-updkz = 'I'.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-new = 5.
    <ls_expected>-code = '4 update'.
    <ls_expected>-updkz = 'U'.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-old = 4.
    <ls_expected>-code = '4'.
    <ls_expected>-updkz = 'U'.

    check( lt_expected ).

  ENDMETHOD.                    "render_008

  METHOD render_009.

    DATA: lt_expected TYPE zif_aor_types=>ty_diff_tt.

    FIELD-SYMBOLS: <ls_expected> LIKE LINE OF lt_expected.

* delete and update
    _old '1'.
    _old '2'.
    _old '3'.
    _old '4'.

    _new '1'.
    _new '3'.
    _new '4 update'.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-old = 2.
    <ls_expected>-code = '2'.
    <ls_expected>-updkz = 'D'.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-new = 3.
    <ls_expected>-code = '4 update'.
    <ls_expected>-updkz = 'U'.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-old = 4.
    <ls_expected>-code = '4'.
    <ls_expected>-updkz = 'U'.

    check( lt_expected ).

  ENDMETHOD.                    "render_009

  METHOD render_010.

    DATA: lt_expected TYPE zif_aor_types=>ty_diff_tt,
          ls_boundary TYPE zif_aor_types=>ty_boundary_line.

    FIELD-SYMBOLS: <ls_expected> LIKE LINE OF lt_expected.

    _old '1'.
    _old '2'.
    _old '3'.
    _old 'method a'.
    _old 'endmethod'.

    _new '1'.
    _new '3'.
    _new 'method a'.
    _new 'endmethod'.

    ls_boundary-start = 1.
    ls_boundary-end = 3.
    APPEND ls_boundary TO mt_boundary_lines.
    ls_boundary-start = 4.
    ls_boundary-end = 5.
    APPEND ls_boundary TO mt_boundary_lines.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-old = 1.
    <ls_expected>-new = 1.
    <ls_expected>-code = '1'.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-old = 2.
    <ls_expected>-code = '2'.
    <ls_expected>-updkz = 'D'.

    APPEND INITIAL LINE TO lt_expected ASSIGNING <ls_expected>.
    <ls_expected>-old = 3.
    <ls_expected>-new = 3.
    <ls_expected>-code = '3'.

    check( lt_expected ).

  ENDMETHOD.

ENDCLASS.                    "ltcl_test IMPLEMENTATION
