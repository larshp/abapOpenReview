CLASS zcl_aor_diff DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS diff
      IMPORTING
        !iv_trkorr      TYPE trkorr
        !is_object      TYPE zaor_object
      EXPORTING
        !et_diff        TYPE zif_aor_types=>ty_diff_tt
        !es_new_version TYPE vrsd .
    CLASS-METHODS enhancement_diff
      IMPORTING
        !is_object          TYPE zaor_object
        !iv_trkorr          TYPE trkorr
      EXPORTING
        !et_diff            TYPE zif_aor_types=>ty_enh_diff_tt
        !es_enhanced_object TYPE zaor_object
        !es_new_version     TYPE vrsd
      RAISING
        cx_enh_root .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_search_result,
      tabix_start TYPE i,
      found_start TYPE sap_bool,
      tabix_end TYPE i,
      found_end TYPE sap_bool,
    END OF ty_search_result.

    CLASS-METHODS filter_versions
      IMPORTING
        !iv_trkorr TYPE trkorr
      CHANGING
        !ct_list   TYPE vrsd_tab .
    CLASS-METHODS add_newlines
      CHANGING
        !ct_diff TYPE zif_aor_types=>ty_diff_tt .
    CLASS-METHODS delta
      IMPORTING
        !it_old           TYPE STANDARD TABLE
        !it_new           TYPE STANDARD TABLE
        it_boundary_lines TYPE zif_aor_types=>ty_boundary_lines_tt OPTIONAL
      RETURNING
        VALUE(rt_delta)   TYPE vxabapt255_tab .
    CLASS-METHODS get_meth
      IMPORTING
        !iv_object_name  TYPE versobjnam
        !iv_versno       TYPE versno
      RETURNING
        VALUE(rt_source) TYPE abaptxt255_tab .
    CLASS-METHODS get_func
      IMPORTING
        !iv_object_name  TYPE versobjnam
        !iv_versno       TYPE versno
      RETURNING
        VALUE(rt_source) TYPE abaptxt255_tab .
    CLASS-METHODS get_reps
      IMPORTING
        !iv_object_name  TYPE versobjnam
        !iv_versno       TYPE versno
      RETURNING
        VALUE(rt_source) TYPE abaptxt255_tab .
    CLASS-METHODS get_wdyc_implementation
      IMPORTING
        !iv_object_name   TYPE versobjnam
        !iv_versno        TYPE versno
      EXPORTING
        et_source         TYPE abaptxt255_tab
        et_boundary_lines TYPE zif_aor_types=>ty_boundary_lines_tt.
    CLASS-METHODS render
      IMPORTING
        !it_old        TYPE abaptxt255_tab
        !it_new        TYPE abaptxt255_tab
        !it_delta      TYPE vxabapt255_tab
      RETURNING
        VALUE(rt_diff) TYPE zif_aor_types=>ty_diff_tt .
    CLASS-METHODS resolve
      IMPORTING
        !is_object     TYPE zaor_object
      RETURNING
        VALUE(rt_vrso) TYPE zif_aor_types=>ty_vrso_tt .
    CLASS-METHODS version_list
      IMPORTING
        !iv_object             TYPE trobjtype
        !iv_obj_name           TYPE trobj_name
      RETURNING
        VALUE(rt_version_list) TYPE vrsd_tab .
    CLASS-METHODS render_hook_diff
      IMPORTING
        !ir_version_manager_new TYPE REF TO if_enh_tool
        !ir_version_manager_old TYPE REF TO if_enh_tool
      EXPORTING
        !et_diff                TYPE zif_aor_types=>ty_enh_diff_tt
        !es_enhanced_object     TYPE zaor_object
      RAISING
        cx_enh_root .
    CLASS-METHODS render_class_diff
      IMPORTING
        !ir_version_manager_new TYPE REF TO if_enh_tool
        !ir_version_manager_old TYPE REF TO if_enh_tool
      EXPORTING
        !et_diff                TYPE zif_aor_types=>ty_enh_diff_tt
        !es_enhanced_object     TYPE zaor_object
      RAISING
        cx_enh_root.
ENDCLASS.



CLASS ZCL_AOR_DIFF IMPLEMENTATION.


  METHOD add_newlines.

    DATA: lv_current TYPE i,
          lv_index   TYPE i,
          lv_new     TYPE i.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF ct_diff.


    LOOP AT ct_diff ASSIGNING <ls_diff>.
      lv_index = sy-tabix.

      IF sy-tabix = 1.
        lv_current = <ls_diff>-new.
      ENDIF.
      lv_new = <ls_diff>-new.

      IF NOT lv_new IS INITIAL AND lv_new > lv_current + 1.
        INSERT INITIAL LINE INTO ct_diff INDEX lv_index.
      ENDIF.

      IF NOT lv_new IS INITIAL.
        lv_current = lv_new.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD delta.

    DATA: lt_trdirtab_old  TYPE TABLE OF trdir,
          lt_trdirtab_new  TYPE TABLE OF trdir,
          lt_trdir_delta   TYPE TABLE OF xtrdir,
          ls_line          TYPE vxabapt255,
          ls_boundary_line TYPE REF TO zif_aor_types=>ty_boundary_line,
          ls_search_result TYPE ty_search_result,
          lr_delta         TYPE REF TO vxabapt255.

    CALL FUNCTION 'SVRS_COMPUTE_DELTA_REPS'
      EXPORTING
        ignore_case_differences = abap_true
      TABLES
        texttab_old             = it_old
        texttab_new             = it_new
        trdirtab_old            = lt_trdirtab_old
        trdirtab_new            = lt_trdirtab_new
        trdir_delta             = lt_trdir_delta
        text_delta              = rt_delta.

    LOOP AT it_boundary_lines REFERENCE INTO ls_boundary_line.
      LOOP AT rt_delta TRANSPORTING NO FIELDS
        WHERE number BETWEEN ls_boundary_line->*-start AND ls_boundary_line->*-end.
      ENDLOOP.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE rt_delta REFERENCE INTO lr_delta
        WITH KEY number = ls_boundary_line->*-start BINARY SEARCH.
      ls_search_result-tabix_start = sy-tabix.
      IF sy-subrc = 0 AND lr_delta->*-vrsflag <> 'I'.
        ls_search_result-found_start = abap_true.
      ENDIF.

      READ TABLE rt_delta REFERENCE INTO lr_delta
        WITH KEY number = ls_boundary_line->*-end BINARY SEARCH.
      ls_search_result-tabix_end = sy-tabix.
      IF sy-subrc = 0 AND lr_delta->*-vrsflag <> 'I'.
        ls_search_result-found_end = abap_true.
      ENDIF.

      IF ls_search_result-found_start = abap_false.
        READ TABLE it_old INTO ls_line-line INDEX ls_boundary_line->*-start.
        ls_line-number = sy-tabix.
        INSERT ls_line INTO rt_delta INDEX ls_search_result-tabix_start.
        ls_search_result-tabix_end = ls_search_result-tabix_end + 1.
      ENDIF.
      IF ls_search_result-found_end = abap_false.
        READ TABLE it_old INTO ls_line-line INDEX ls_boundary_line->*-end.
        ls_line-number = sy-tabix.
        INSERT ls_line INTO rt_delta INDEX ls_search_result-tabix_end.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  METHOD diff.

    DATA: lt_new          TYPE STANDARD TABLE OF abaptxt255,
          lt_old          TYPE STANDARD TABLE OF abaptxt255,
          lt_version_list TYPE vrsd_tab,
          lv_obj_name     TYPE trobj_name,
          lt_delta        TYPE vxabapt255_tab,
          ls_old          LIKE LINE OF lt_version_list,
          lt_vrso         TYPE zif_aor_types=>ty_vrso_tt,
          ls_vrso         LIKE LINE OF lt_vrso,
          lt_boundary_lines TYPE zif_aor_types=>ty_boundary_lines_tt.


    ASSERT NOT is_object IS INITIAL.

    lt_vrso = resolve( is_object ).

    DELETE lt_vrso WHERE objtype = 'DOCU'.

    IF lt_vrso IS INITIAL.
* non versionable object
      RETURN.
    ENDIF.

    ASSERT lines( lt_vrso ) = 1.
    READ TABLE lt_vrso INDEX 1 INTO ls_vrso.

    lv_obj_name = ls_vrso-objname.
    lt_version_list = version_list( iv_object   = ls_vrso-objtype
                                    iv_obj_name = lv_obj_name ).
    filter_versions( EXPORTING iv_trkorr = iv_trkorr
                     CHANGING  ct_list   = lt_version_list ).
    IF lines( lt_version_list ) = 0.
      RETURN.
    ENDIF.

    READ TABLE lt_version_list INDEX 1 INTO es_new_version.
    ASSERT sy-subrc = 0.
    READ TABLE lt_version_list INDEX 2 INTO ls_old.

    CASE ls_vrso-objtype.
      WHEN 'REPS'.
        lt_new = get_reps( iv_object_name = ls_vrso-objname
                           iv_versno      = es_new_version-versno ).
        IF NOT ls_old IS INITIAL.
          lt_old = get_reps( iv_object_name = ls_vrso-objname
                             iv_versno      = ls_old-versno ).
        ENDIF.
      WHEN 'METH'.
        lt_new = get_meth( iv_object_name = ls_vrso-objname
                           iv_versno      = es_new_version-versno ).
        IF NOT ls_old IS INITIAL.
          lt_old = get_meth( iv_object_name = ls_vrso-objname
                             iv_versno      = ls_old-versno ).
        ENDIF.
      WHEN 'FUNC'.
        lt_new = get_func( iv_object_name = ls_vrso-objname
                           iv_versno      = es_new_version-versno ).
        IF NOT ls_old IS INITIAL.
          lt_old = get_func( iv_object_name = ls_vrso-objname
                             iv_versno      = ls_old-versno ).
        ENDIF.
      WHEN 'WDYC'.
        get_wdyc_implementation( EXPORTING iv_object_name = ls_vrso-objname
          iv_versno = es_new_version-versno
          IMPORTING et_source = lt_new ).
        IF NOT ls_old IS INITIAL.
          get_wdyc_implementation( EXPORTING
            iv_object_name = ls_vrso-objname iv_versno = ls_old-versno
            IMPORTING et_source = lt_old et_boundary_lines = lt_boundary_lines ).
        ENDIF.
      WHEN OTHERS.
* todo
        RETURN.
    ENDCASE.

    lt_delta = delta( it_old = lt_old
                      it_new = lt_new
                      it_boundary_lines = lt_boundary_lines ).

    et_diff = render( it_old   = lt_old
                      it_new   = lt_new
                      it_delta = lt_delta ).

    add_newlines( CHANGING ct_diff = et_diff ).

  ENDMETHOD.


  METHOD enhancement_diff.
    DATA: lr_version_manager_new TYPE REF TO if_enh_tool,
          lr_version_manager_old TYPE REF TO if_enh_tool,
          lv_enhancement_id      TYPE enhname,
          lv_obj_name            TYPE trobj_name,
          lt_version_list        TYPE vrsd_tab,
          lt_vrso                TYPE zif_aor_types=>ty_vrso_tt,
          ls_vrso                LIKE LINE OF lt_vrso,
          ls_version_old         TYPE REF TO vrsd.

    lv_enhancement_id = is_object-obj_name.

    lt_vrso = resolve( is_object ).
    IF lt_vrso IS INITIAL.
* non versionable object
      RETURN.
    ENDIF.

    ASSERT lines( lt_vrso ) = 1.
    READ TABLE lt_vrso INDEX 1 INTO ls_vrso.

    ASSERT ls_vrso-objtype = 'ENHO'.
    lv_obj_name = ls_vrso-objname.
    lt_version_list = version_list( iv_object   = ls_vrso-objtype
                                    iv_obj_name = lv_obj_name ).
    filter_versions( EXPORTING iv_trkorr = iv_trkorr
                     CHANGING  ct_list   = lt_version_list ).
    IF lines( lt_version_list ) = 0.
      RETURN.
    ENDIF.

    READ TABLE lt_version_list INDEX 1 INTO es_new_version.
    ASSERT sy-subrc = 0.
    lr_version_manager_new = cl_enh_factory=>get_enhancement(
      enhancement_id = lv_enhancement_id
      versno = es_new_version-versno ).
    READ TABLE lt_version_list INDEX 2 REFERENCE INTO ls_version_old.
    IF sy-subrc = 0.
      lr_version_manager_old = cl_enh_factory=>get_enhancement(
        enhancement_id = lv_enhancement_id
        versno = ls_version_old->*-versno ).
    ENDIF.

    CASE lr_version_manager_new->get_tool( ).
      WHEN 'HOOK_IMPL'.
        render_hook_diff( EXPORTING ir_version_manager_new = lr_version_manager_new
          ir_version_manager_old = lr_version_manager_old
          IMPORTING et_diff = et_diff es_enhanced_object = es_enhanced_object ).
      WHEN 'CLASENH'.
        render_class_diff( EXPORTING ir_version_manager_new = lr_version_manager_new
          ir_version_manager_old = lr_version_manager_old
          IMPORTING et_diff = et_diff es_enhanced_object = es_enhanced_object ).
    ENDCASE.

  ENDMETHOD.


  METHOD filter_versions.

    DATA: lv_index      TYPE i,
          lv_found      TYPE abap_bool,
          lv_trfunction TYPE e070-trfunction.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF ct_list.


    LOOP AT ct_list ASSIGNING <ls_list>.
      lv_index = sy-tabix.

      IF <ls_list>-korrnum IS INITIAL.
        lv_found = abap_false.
      ELSEIF <ls_list>-korrnum = iv_trkorr.
        lv_found = abap_true.
      ELSEIF <ls_list>-versno = '00000'.
        SELECT SINGLE COUNT( * ) FROM e070
          WHERE trkorr = <ls_list>-korrnum
          AND strkorr = iv_trkorr.
        IF sy-subrc = 0.
          lv_found = abap_true.
        ENDIF.
      ENDIF.

      IF lv_found = abap_false.
        DELETE ct_list INDEX lv_index.
        CONTINUE.
      ENDIF.

      SELECT SINGLE trfunction
        INTO lv_trfunction
        FROM e070
        WHERE trkorr = <ls_list>-korrnum.
      IF sy-subrc = 0 AND lv_trfunction = 'T'.
* remove transport of copies.
        DELETE ct_list INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_func.

    CALL FUNCTION 'SVRS_GET_VERSION_FUNC_40'
      EXPORTING
        object_name           = iv_object_name
        versno                = iv_versno
      TABLES
        uincl_tab             = rt_source
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS                = 4. "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD get_meth.

    CALL FUNCTION 'SVRS_GET_VERSION_METH_40'
      EXPORTING
        object_name           = iv_object_name
        versno                = iv_versno
      TABLES
        repos_tab             = rt_source
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS                = 4. "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD get_reps.

    CALL FUNCTION 'SVRS_GET_VERSION_REPS_40'
      EXPORTING
        object_name           = iv_object_name
        versno                = iv_versno
      TABLES
        repos_tab             = rt_source
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS                = 4. "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD get_wdyc_implementation.
    DATA: lt_components        TYPE STANDARD TABLE OF wdy_ctlr_compo_vrs,
          lt_components_source TYPE STANDARD TABLE OF wdy_ctlr_compo_source_vrs,
          lr_component         TYPE REF TO wdy_ctlr_compo_vrs,
          lr_component_source     TYPE REF TO wdy_ctlr_compo_source_vrs,
          ls_boundary_line        TYPE zif_aor_types=>ty_boundary_line.

    CALL FUNCTION 'SVRS_GET_VERSION_WDYC_40'
      EXPORTING
        object_name = iv_object_name
        versno      = iv_versno
      TABLES
        ccomp_tab   = lt_components
        ccoms_tab   = lt_components_source.

    LOOP AT lt_components REFERENCE INTO lr_component
      WHERE cmptype = 'CL_WDY_MD_CONTROLLER_METHOD' OR
            cmptype = 'CL_WDY_MD_SUPPLY_FUNCTION'   OR
            cmptype = 'CL_WDY_MD_CTLR_EVENT_HANDLER'.

      CLEAR ls_boundary_line.
      LOOP AT lt_components_source REFERENCE INTO lr_component_source
        WHERE component_name = lr_component->*-component_name
        AND controller_name = lr_component->*-controller_name AND cmpname = lr_component->*-cmpname.

        APPEND lr_component_source->*-source_line TO et_source.
        ls_boundary_line-end = sy-tabix.
        IF lr_component_source->*-line_number = 1.
          ls_boundary_line-start = ls_boundary_line-end.
        ENDIF.

      ENDLOOP.
      IF sy-subrc = 0.
        INSERT ls_boundary_line INTO TABLE et_boundary_lines.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD render.

    DATA: lv_diff TYPE i.

    FIELD-SYMBOLS: <ls_diff>  LIKE LINE OF rt_diff,
                   <ls_delta> LIKE LINE OF it_delta,
                   <ls_code>  LIKE LINE OF it_old.


    LOOP AT it_delta ASSIGNING <ls_delta>.

      CASE <ls_delta>-vrsflag.
        WHEN 'I'.
          READ TABLE it_new INDEX <ls_delta>-number + lv_diff ASSIGNING <ls_code>.
          ASSERT sy-subrc = 0.
          APPEND INITIAL LINE TO rt_diff ASSIGNING <ls_diff>.
          <ls_diff>-new   = <ls_delta>-number + lv_diff.
          <ls_diff>-updkz = 'I'.
          <ls_diff>-code  = <ls_code>-line.

          lv_diff = lv_diff + 1.
        WHEN 'D'.
          READ TABLE it_old INDEX <ls_delta>-number ASSIGNING <ls_code>.
          ASSERT sy-subrc = 0.
          APPEND INITIAL LINE TO rt_diff ASSIGNING <ls_diff>.
          <ls_diff>-old   = <ls_delta>-number.
          <ls_diff>-updkz = 'D'.
          <ls_diff>-code  = <ls_code>-line.

          lv_diff = lv_diff - 1.
        WHEN 'U'.
          READ TABLE it_new INDEX <ls_delta>-number + lv_diff ASSIGNING <ls_code>.
          ASSERT sy-subrc = 0.
          APPEND INITIAL LINE TO rt_diff ASSIGNING <ls_diff>.
          <ls_diff>-new   = <ls_delta>-number + lv_diff.
          <ls_diff>-updkz = 'U'.
          <ls_diff>-code  = <ls_code>-line.

          READ TABLE it_old INDEX <ls_delta>-number ASSIGNING <ls_code>.
          ASSERT sy-subrc = 0.
          APPEND INITIAL LINE TO rt_diff ASSIGNING <ls_diff>.
          <ls_diff>-old   = <ls_delta>-number.
          <ls_diff>-updkz = 'U'.
          <ls_diff>-code  = <ls_code>-line.
        WHEN space.
          APPEND INITIAL LINE TO rt_diff ASSIGNING <ls_diff>.
          <ls_diff>-new   = <ls_delta>-number.
          <ls_diff>-old   = <ls_delta>-number.
          <ls_diff>-code  = <ls_delta>-line.
      ENDCASE.

    ENDLOOP.

    LOOP AT rt_diff ASSIGNING <ls_diff>.
      IF <ls_diff>-new = 0.
        CLEAR <ls_diff>-new.
      ENDIF.
      IF <ls_diff>-old = 0.
        CLEAR <ls_diff>-old.
      ENDIF.
    ENDLOOP.

* todo, merge/rearrange sequential updated lines?

  ENDMETHOD.


  METHOD render_class_diff.
    DATA: lr_version_manager_new   TYPE REF TO cl_enh_tool_class,
          lr_version_manager_old   TYPE REF TO cl_enh_tool_class,
          ls_enhancement_new       TYPE enhclassmethdata,
          ls_enhancement_old       TYPE enhclassmethdata,
          ls_new_method_definition TYPE REF TO enhnewmeth_data,
          ls_new_source            TYPE REF TO seo_method_source,
          lt_new_source_code       TYPE abaptxt255_tab,
          ls_old_method_definition TYPE REF TO enhnewmeth_data,
          ls_old_source            TYPE REF TO seo_method_source,
          lt_old_source_code       TYPE abaptxt255_tab,
          lt_delta                 TYPE vxabapt255_tab,
          lv_class_name             TYPE seoclsname,
          ls_diff_method           TYPE zif_aor_types=>ty_enh_diff_st.

    lr_version_manager_new ?= ir_version_manager_new.
    IF ir_version_manager_old IS BOUND.
      lr_version_manager_old ?= ir_version_manager_old.
    ENDIF.

    es_enhanced_object-pgmid = 'R3TR'.
    es_enhanced_object-object = 'CLAS'.
    lr_version_manager_new->get_class(
      IMPORTING class_name = lv_class_name ).
    es_enhanced_object-obj_name = lv_class_name.

    lr_version_manager_new->get_all_data_for_class( EXPORTING class_name = lv_class_name
      IMPORTING enha_data = ls_enhancement_new ).
    IF lr_version_manager_old IS BOUND.
      lr_version_manager_old->get_all_data_for_class( EXPORTING class_name = lv_class_name
        IMPORTING enha_data = ls_enhancement_old ).
    ENDIF.
    " methods
    LOOP AT ls_enhancement_new-enh_newmethodes REFERENCE INTO ls_new_method_definition.
      CLEAR: lt_new_source_code, lt_old_source_code, lt_delta.
      READ TABLE ls_enhancement_new-enh_methsources REFERENCE INTO ls_new_source
        WITH KEY cpdname = ls_new_method_definition->*-methkey-cmpname.
      ASSERT sy-subrc = 0.
      APPEND LINES OF ls_new_source->*-source TO lt_new_source_code.

      READ TABLE ls_enhancement_old-enh_newmethodes REFERENCE INTO ls_old_method_definition
        WITH KEY methkey = ls_new_method_definition->*-methkey.
      IF sy-subrc = 0.
        READ TABLE ls_enhancement_old-enh_methsources REFERENCE INTO ls_old_source
          WITH KEY cpdname = ls_old_method_definition->*-methkey-cmpname.
        APPEND LINES OF ls_old_source->*-source TO lt_old_source_code.
      ENDIF.

      lt_delta = delta( it_old = lt_old_source_code it_new = lt_new_source_code ).
      IF lt_delta IS NOT INITIAL.
        ls_diff_method-id = lines( et_diff ) + 1.
        ls_diff_method-full_name = |Method { ls_new_method_definition->*-methkey-cmpname }|.
        ls_diff_method-diff = render( it_old = lt_old_source_code it_new = lt_new_source_code
          it_delta = lt_delta ).
        add_newlines( CHANGING ct_diff = ls_diff_method-diff ).
        INSERT ls_diff_method INTO TABLE et_diff.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD render_hook_diff.
    DATA: lr_version_manager_new  TYPE REF TO cl_enh_tool_hook_impl,
          lr_version_manager_old  TYPE REF TO cl_enh_tool_hook_impl,
          ls_hook_header          TYPE enh_hook_admin,
          lt_hook_implementations TYPE enh_hook_impl_it,
          lr_hook_impl_new        TYPE REF TO enh_hook_impl,
          ls_hook_impl_old        TYPE enh_hook_impl,
          lt_source_new           TYPE STANDARD TABLE OF abaptxt255,
          lt_source_old           LIKE lt_source_new,
          lt_delta                TYPE vxabapt255_tab,
          ls_diff_hook            LIKE LINE OF et_diff.

    lr_version_manager_new ?= ir_version_manager_new.
    IF ir_version_manager_old IS BOUND.
      lr_version_manager_old ?= ir_version_manager_old.
    ENDIF.

    ir_version_manager_new->if_enh_object~get_data( IMPORTING data = ls_hook_header ).

    es_enhanced_object-pgmid = ls_hook_header-pgmid.
    es_enhanced_object-object = ls_hook_header-org_obj_type.
    es_enhanced_object-obj_name = ls_hook_header-org_obj_name.

    lt_hook_implementations = lr_version_manager_new->get_hook_impls( ).
    LOOP AT lt_hook_implementations REFERENCE INTO lr_hook_impl_new.

      CLEAR: lt_source_new, lt_source_old, lt_delta.

      APPEND LINES OF lr_hook_impl_new->*-source TO lt_source_new.
      IF ir_version_manager_old IS BOUND.
        READ TABLE lr_version_manager_old->get_hook_impls( ) INTO ls_hook_impl_old
          WITH KEY id = lr_hook_impl_new->*-id.
        IF sy-subrc = 0.
          APPEND LINES OF ls_hook_impl_old-source TO lt_source_old.
        ENDIF.
      ENDIF.
      lt_delta = delta( it_old = lt_source_old it_new = lt_source_new ).
      IF lt_delta IS NOT INITIAL.
        ls_diff_hook-id = lr_hook_impl_new->*-id.
        ls_diff_hook-full_name = lr_hook_impl_new->*-full_name.
        ls_diff_hook-diff = render( it_old = lt_source_old it_new = lt_source_new it_delta = lt_delta ).
        add_newlines( CHANGING ct_diff = ls_diff_hook-diff ).
        INSERT ls_diff_hook INTO TABLE et_diff.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD resolve.

    DATA: ls_e071 TYPE e071.


    MOVE-CORRESPONDING is_object TO ls_e071.

    CALL FUNCTION 'SVRS_RESOLVE_E071_OBJ'
      EXPORTING
        e071_obj        = ls_e071
      TABLES
        obj_tab         = rt_vrso
      EXCEPTIONS
        not_versionable = 1
        OTHERS          = 2 ##FM_SUBRC_OK. "#EC CI_SUBRC

  ENDMETHOD.


  METHOD version_list.

    DATA: lt_lversno_list TYPE STANDARD TABLE OF vrsn,
          lv_vobjname     TYPE vrsd-objname,
          lv_vobjtype     TYPE vrsd-objtype.

    ASSERT NOT iv_object IS INITIAL.
    ASSERT NOT iv_obj_name IS INITIAL.

    lv_vobjname = iv_obj_name.
    lv_vobjtype = iv_object.

    CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
      EXPORTING
        objname                = lv_vobjname
        objtype                = lv_vobjtype
      TABLES
        lversno_list           = lt_lversno_list
        version_list           = rt_version_list
      EXCEPTIONS
        no_entry               = 1
        communication_failure_ = 2
        system_failure         = 3
        OTHERS                 = 4 ##FM_SUBRC_OK. "#EC CI_SUBRC

  ENDMETHOD.
ENDCLASS.
