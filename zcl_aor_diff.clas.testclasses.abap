
CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
FINAL.

  PRIVATE SECTION.

    METHODS: diff FOR TESTING.
ENDCLASS.       "ltcl_Test


CLASS ltcl_test IMPLEMENTATION.

  METHOD diff.

    zcl_aor_diff=>diff( iv_object   = 'REPS'
                        iv_obj_name = 'ZLHVP001' ).


  ENDMETHOD.

ENDCLASS.