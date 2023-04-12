CLASS zcl_aor_crossref DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.

    METHODS build_objectset
      IMPORTING
        io_review        TYPE REF TO zcl_aor_review
      RETURNING
        VALUE(ro_result) TYPE REF TO cl_ci_objectset
      RAISING
        zcx_aor_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mt_tadir_crossref TYPE STANDARD TABLE OF zaor_ta_crossref.

    METHODS resolve_crossref
      IMPORTING
        is_entity       TYPE e071
      CHANGING
        et_dependencies TYPE scit_objs
      RAISING
        zcx_aor_error.

    METHODS resolve_recursiv
      IMPORTING
        is_entity       TYPE scir_objs
      CHANGING
        et_dependencies TYPE scit_objs
      RAISING
        zcx_aor_error.

    METHODS get_tadir_object_type
      IMPORTING
        is_found         TYPE rsfindlst
      RETURNING
        VALUE(rv_result) TYPE trobjtype
      RAISING
        zcx_aor_error.

    METHODS get_r3tr_entity
      IMPORTING
        is_entity TYPE e071
      RETURNING
        VALUE(rs_result) TYPE scir_objs.

ENDCLASS.



CLASS ZCL_AOR_CROSSREF IMPLEMENTATION.


  METHOD build_objectset.
    DATA: lr_entity       TYPE REF TO e071,
          lo_object_set   TYPE REF TO cl_ci_objectset,
          lt_dependencies TYPE scit_objs,
          lt_list         TYPE e071_t.

    lt_list = io_review->objects_list( ).
    LOOP AT lt_list REFERENCE INTO lr_entity.
      resolve_crossref(
        EXPORTING
          is_entity = lr_entity->*
        CHANGING
          et_dependencies = lt_dependencies ).
    ENDLOOP.
    cl_ci_objectset=>get_ref(
      EXPORTING
        p_user = sy-uname p_objsnam = io_review->header( )-review_id
      RECEIVING
        p_ref = lo_object_set
      EXCEPTIONS
        objs_not_exists = 2 ).
    IF sy-subrc = 0.
      lo_object_set->delete( p_mode = 'A' ).
    ENDIF.
    ro_result = cl_ci_objectset=>save_from_list(
      p_objects = lt_dependencies
      p_name = io_review->header( )-review_id ).
    ro_result->leave_change( ).

  ENDMETHOD.


  METHOD constructor.

    SELECT * FROM zaor_ta_crossref INTO TABLE mt_tadir_crossref.

  ENDMETHOD.


  METHOD get_r3tr_entity.
    DATA:
      ls_r3tr_entity TYPE tadir.

    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071 = is_entity
      IMPORTING
        we_tadir = ls_r3tr_entity.

    rs_result-objtype = ls_r3tr_entity-object.
    rs_result-objname = ls_r3tr_entity-obj_name.

  ENDMETHOD.


  METHOD get_tadir_object_type.
    DATA: lr_tadir_crossref TYPE REF TO zaor_ta_crossref,
          lv_class_type     TYPE seoclstype.

    IF is_found-object_cls = 'OM'.
      SELECT SINGLE clstype FROM seoclass INTO lv_class_type
        WHERE clsname = is_found-encl_objec.
      IF lv_class_type = '1'.
        rv_result = 'INTF'.
      ELSE.
        rv_result = 'CLAS'.
      ENDIF.
      RETURN.
    ENDIF.

    IF is_found-object_cls = 'P'.
      SELECT COUNT(*) FROM trdir
        WHERE name = is_found-object AND subc = cl_abap_parser=>progtype_fugr.
      IF sy-subrc = 0.
        rv_result = 'FUGR'.
        RETURN.
      ENDIF.
    ENDIF.

    READ TABLE mt_tadir_crossref REFERENCE INTO lr_tadir_crossref
      WITH KEY obj_class = is_found-object_cls pgmid = 'R3TR'.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_aor_error.
    ENDIF.

    rv_result = lr_tadir_crossref->*-object.

  ENDMETHOD.


  METHOD resolve_crossref.
    DATA: lr_tadir_crossref TYPE REF TO zaor_ta_crossref,
          ls_to_search      TYPE rsfind,
          lr_found          TYPE REF TO rsfindlst,
          lt_to_search      TYPE STANDARD TABLE OF rsfind,
          lt_found          TYPE STANDARD TABLE OF rsfindlst,
          ls_entity         TYPE scir_objs,
          ##NEEDED
          lv_method_name    TYPE string.

    READ TABLE mt_tadir_crossref REFERENCE INTO lr_tadir_crossref
      WITH KEY pgmid = is_entity-pgmid object = is_entity-object is_primary = abap_true.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF is_entity-pgmid = 'LIMU' AND is_entity-object = 'METH'.
      SPLIT is_entity-obj_name AT space
        INTO ls_to_search-object lv_method_name.
    ELSEIF is_entity-pgmid = 'LIMU' AND is_entity-object = 'CINC'.
      ls_to_search-object = get_r3tr_entity( is_entity )-objname.
    ELSE.
      ls_to_search-object = is_entity-obj_name.
    ENDIF.
    INSERT ls_to_search INTO TABLE lt_to_search.

    INSERT get_r3tr_entity( is_entity ) INTO TABLE et_dependencies.

    CALL FUNCTION 'RS_EU_CROSSREF'
      EXPORTING
        i_find_obj_cls               = lr_tadir_crossref->*-obj_class
        i_answer                     = 'N'
        no_dialog                    = abap_true
        expand_source_in_online_mode = abap_true
        rekursiv                     = abap_true
      TABLES
        i_findstrings                = lt_to_search
        o_founds                     = lt_found
      EXCEPTIONS
        not_found                    = 0.
    LOOP AT lt_found REFERENCE INTO lr_found.
      TRY.
          CLEAR: ls_entity.
          ls_entity-objtype = get_tadir_object_type( lr_found->* ).
          IF lr_found->*-object_cls = 'OM'.
            SPLIT lr_found->*-encl_objec AT space
              INTO ls_entity-objname lv_method_name.
          ELSEIF ls_entity-objtype = 'FUGR'.
            ls_entity-objname = lr_found->*-object+4. " remove SAPL prefix
          ELSE.
            ls_entity-objname = lr_found->*-object.
          ENDIF.
          READ TABLE et_dependencies TRANSPORTING NO FIELDS
            WITH KEY objtype = ls_entity-objtype objname = ls_entity-objname.
          IF sy-subrc = 0.
            " to avoid endless recursion
            CONTINUE.
          ENDIF.
          INSERT ls_entity INTO TABLE et_dependencies.
          resolve_recursiv(
            EXPORTING
              is_entity = ls_entity
            CHANGING
              et_dependencies = et_dependencies ).
          ##NO_HANDLER
        CATCH zcx_aor_error.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD resolve_recursiv.
    DATA:
      ls_transport_entity TYPE e071.

    ls_transport_entity-pgmid = 'R3TR'.
    ls_transport_entity-object = is_entity-objtype.
    ls_transport_entity-obj_name = is_entity-objname.

    resolve_crossref(
      EXPORTING
        is_entity = ls_transport_entity
      CHANGING
        et_dependencies = et_dependencies ).

  ENDMETHOD.
ENDCLASS.
