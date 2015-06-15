CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_aor_diff DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA: mt_old  TYPE abaptxt255_tab,
          mt_new  TYPE abaptxt255_tab,
          mt_diff TYPE zif_aor_types=>ty_diff_tt.

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
             render_009 FOR TESTING.

ENDCLASS.       "ltcl_Test

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
  ENDMETHOD.

  METHOD run.

    DATA(lt_delta) = zcl_aor_diff=>delta( it_old = mt_old
                                          it_new = mt_new ).

    mt_diff = zcl_aor_diff=>render( it_old   = mt_old
                                    it_new   = mt_new
                                    it_delta = lt_delta ).

  ENDMETHOD.

  METHOD check.

    run( ).

    cl_abap_unit_assert=>assert_equals( act = mt_diff
                                        exp = it_diff ).

  ENDMETHOD.

  METHOD render_001.

* insert
    _new 'write'.

    check( VALUE #( ( new = 1 code = 'write' updkz = 'I' ) ) ).

  ENDMETHOD.

  METHOD render_002.

* delete
    _old 'write'.

    check( VALUE #( ( old = 1 code = 'write' updkz = 'D' ) ) ).

  ENDMETHOD.

  METHOD render_003.

* update
    _old 'foo'.
    _new 'bar'.

    check( VALUE #( ( new = 1 code = 'bar' updkz = 'U' )
                    ( old = 1 code = 'foo' updkz = 'U' ) ) ).

  ENDMETHOD.

  METHOD render_004.

* delete 2nd line
    _old 'foo'.
    _old 'bar'.

    _new 'foo'.

    check( VALUE #( ( old = 2 code = 'bar' updkz = 'D' ) ) ).

  ENDMETHOD.

  METHOD render_005.

* update 2nd line
    _old 'foo'.
    _old 'bar'.

    _new 'foo'.
    _new 'bar moo'.

    check( VALUE #( ( new = 2 code = 'bar moo' updkz = 'U' )
                    ( old = 2 code = 'bar' updkz = 'U' ) ) ).

  ENDMETHOD.

  METHOD render_006.

* insert 2 lines
    _new 'foo'.
    _new 'bar'.

    check( VALUE #( ( new = 1 code = 'foo' updkz = 'I' )
                    ( new = 2 code = 'bar' updkz = 'I' ) ) ).

  ENDMETHOD.

  METHOD render_007.

* insert between 2 lines
    _old 'foo'.
    _old 'bar'.

    _new 'foo'.
    _new 'moo'.
    _new 'bar'.

    check( VALUE #( ( new = 2 code = 'moo' updkz = 'I' ) ) ).

  ENDMETHOD.

  METHOD render_008.

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

    check( VALUE #( ( new = 3         code = 'inserted' updkz = 'I' )
                    ( new = 5         code = '4 update' updkz = 'U' )
                    (         old = 4 code = '4'        updkz = 'U' )
                  ) ).

  ENDMETHOD.

  METHOD render_009.

* delete and update
    _old '1'.
    _old '2'.
    _old '3'.
    _old '4'.

    _new '1'.
    _new '3'.
    _new '4 update'.

    check( VALUE #( (         old = 2 code = '2'        updkz = 'D' )
                    ( new = 3         code = '4 update' updkz = 'U' )
                    (         old = 4 code = '4'        updkz = 'U' )
                  ) ).

  ENDMETHOD.

ENDCLASS.