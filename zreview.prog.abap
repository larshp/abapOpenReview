REPORT zreview.

* See https://github.com/larshp/abapOpenReview

CONSTANTS: gc_version TYPE string VALUE 'v0.1-alpha'.       "#EC NOTEXT

************************************************************************
* abapOpenReview is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 2 of the License, or
* (at your option) any later version.
*
* abapOpenReview is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/>.
************************************************************************

CONSTANTS: gc_newline TYPE abap_char1 VALUE cl_abap_char_utilities=>newline.

DATA: go_review TYPE REF TO zcl_aor_review.

START-OF-SELECTION.
  PERFORM run.

*----------------------------------------------------------------------*
*       CLASS lcl_navigate DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_navigate DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS navigate
      IMPORTING iv_object   TYPE e071-object
                iv_obj_name TYPE e071-obj_name.

ENDCLASS.                    "lcl_navigate DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_navigate IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_navigate IMPLEMENTATION.

  METHOD navigate.

    IF iv_object = 'TABU'.
      MESSAGE s005(zabapopenreview) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = iv_obj_name
        object_type         = iv_object
        in_new_window       = abap_true
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.                          "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.                    "navigate

ENDCLASS.                    "lcl_navigate IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_gui DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS run
      RAISING zcx_aor_error.

    CLASS-METHODS render_header
      IMPORTING iv_onload      TYPE string OPTIONAL
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS render_footer
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS on_event
                  FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING action frame getdata postdata query_table.  "#EC NEEDED

  PRIVATE SECTION.
    CLASS-DATA go_html_viewer TYPE REF TO cl_gui_html_viewer.

    CLASS-METHODS getdata
      IMPORTING iv_field        TYPE string
                iv_getdata      TYPE clike
      RETURNING value(rv_value) TYPE string.

    CLASS-METHODS postdata
      IMPORTING iv_field        TYPE string
                it_postdata     TYPE cnht_post_data_tab
      RETURNING value(rv_value) TYPE string.

    CLASS-METHODS view
      IMPORTING iv_html TYPE string.

    CLASS-METHODS render_css
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS new
      IMPORTING iv_trkorr         TYPE trkorr
      RETURNING value(rv_success) TYPE abap_bool
      RAISING   zcx_aor_error.

ENDCLASS.                    "lcl_gui DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_gui_start DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui_review DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS render
      IMPORTING iv_onload      TYPE string OPTIONAL
      RETURNING value(rv_html) TYPE string
      RAISING   zcx_aor_error.

    CLASS-DATA: gv_filter TYPE zaor_review-ci_filter.

  PRIVATE SECTION.
    CLASS-METHODS add_comment
      IMPORTING iv_topic       TYPE zaor_comment-topic OPTIONAL
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS comments
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS info
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS close_review
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS objects
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS diff
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS code_inspector
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS filter
      IMPORTING iv_filter      TYPE zaor_review-ci_filter
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS shortcuts
      RETURNING value(rv_html) TYPE string.

ENDCLASS.                    "lcl_gui_start DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_gui_review IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui_review IMPLEMENTATION.

  METHOD diff.

    DATA: lv_style TYPE string,
          lt_diff_list TYPE zif_aor_types=>ty_diff_list_tt,
          lt_diff TYPE zif_aor_types=>ty_diff_tt.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF lt_diff,
                   <ls_diff_list> LIKE LINE OF lt_diff_list.


    rv_html = '<a name="diff"></a><h2>Diff</h2><br>'.

    lt_diff_list = go_review->diff( ).

    IF lt_diff_list IS INITIAL.
      rv_html = rv_html && 'Empty'  ##no_text.
    ENDIF.

    LOOP AT lt_diff_list ASSIGNING <ls_diff_list>.
      rv_html = rv_html                &&
        <ls_diff_list>-object-object   &&
        '&nbsp;'                       &&
        <ls_diff_list>-object-obj_name &&
        '<br><br>'.

      lt_diff = <ls_diff_list>-diff.
      IF NOT lt_diff IS INITIAL.
        rv_html = rv_html &&
          '<table border="0">' &&
          '<tr>' &&
          '<td><u>New</u></td>' &&
          '<td><u>Old</u></td>' &&
          '<td><u>Type</u></td>' &&
          '<td><u>Code</u></td>' &&
          '</tr>'.
        LOOP AT lt_diff ASSIGNING <ls_diff>.
          <ls_diff>-code = escape( val    = <ls_diff>-code
                                   format = cl_abap_format=>e_html_attr ).
          IF <ls_diff>-new <> ''.
            lv_style = ' style="background:lightgreen;"'.   "#EC NOTEXT
          ELSE.
            lv_style = ' style="background:lightpink;"'.    "#EC NOTEXT
          ENDIF.
          rv_html = rv_html &&
            '<tr>' &&
            '<td>' && <ls_diff>-new && '&nbsp;</td>' &&
            '<td>' && <ls_diff>-old && '&nbsp;</td>' &&
            '<td>' && <ls_diff>-updkz && '&nbsp;</td>' &&
            '<td' && lv_style && '><pre>' && <ls_diff>-code && '</pre></td>' &&
            '</tr>'.
        ENDLOOP.
        rv_html = rv_html && '</table>'.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "diff

  METHOD render.

    rv_html = lcl_gui=>render_header( iv_onload ) &&
      '<h1>' && go_review->header( )-review_id
      && '&nbsp;-&nbsp;' && go_review->header( )-as4text &&
      '</h1><br>'                           && gc_newline &&
      shortcuts( )                          && gc_newline &&
      '<br><br>'                            && gc_newline &&
      info( )                               && gc_newline &&
      '<br>'                                && gc_newline &&
      objects( )                            && gc_newline &&
      '<br>'                                && gc_newline &&
      code_inspector( )                     && gc_newline &&
      diff( )                               && gc_newline &&
      '<br><br>'                            && gc_newline &&
      comments( )                           && gc_newline &&
      '<br>'                                && gc_newline &&
      close_review( )                       && gc_newline &&
      shortcuts( )                          && gc_newline &&
      lcl_gui=>render_footer( ).

  ENDMETHOD.                    "render

  METHOD info.

    rv_html = '<table>' &&
      '<tr>' &&
      '<td>Responsible:</td>' &&
      '<td>' && go_review->header( )-responsible && '</td>' &&
      '</tr>' &&
      '<tr>' &&
      '<td>Base:</td>' &&
      '<td>' && go_review->header( )-base && '</td>' &&
      '</tr>' &&
      '</table>' ##no_text.

  ENDMETHOD.                    "info

  METHOD shortcuts.

    rv_html = '<a href="sapevent:back">Back</a>&nbsp;'     && gc_newline &&
      '<a href="#objects" class="grey">Objects</a>&nbsp'   && gc_newline &&
      '<a href="#ci" class="grey">Code Inspector</a>&nbsp' && gc_newline &&
      '<a href="#diff" class="grey">Diff</a>&nbsp'         && gc_newline &&
      '<a href="#comments" class="grey">Comments</a>'.

  ENDMETHOD.                    "shortcuts

  METHOD filter.

    STATICS: st_dd07v TYPE STANDARD TABLE OF dd07v.

    DATA: ls_dd07v LIKE LINE OF st_dd07v.


    IF st_dd07v IS INITIAL.
      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname        = 'ZAOR_CI_FILTER'
          text           = abap_true
        TABLES
          dd07v_tab      = st_dd07v
        EXCEPTIONS
          wrong_textflag = 1
          OTHERS         = 2.                             "#EC CI_SUBRC
      ASSERT sy-subrc = 0.
    ENDIF.

    LOOP AT st_dd07v INTO ls_dd07v.
      IF ls_dd07v-domvalue_l = iv_filter.
        rv_html = rv_html &&
          '<a href="sapevent:filter?filter=' &&
          ls_dd07v-domvalue_l && '">' &&
          ls_dd07v-ddtext && '</a>&nbsp'.
      ELSE.
        rv_html = rv_html &&
          '<a href="sapevent:filter?filter=' &&
          ls_dd07v-domvalue_l && '" class="grey">' &&
          ls_dd07v-ddtext && '</a>&nbsp'.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "filter

  METHOD code_inspector.

    DATA: ls_header TYPE zif_aor_types=>ty_header,
          ls_ci TYPE zif_aor_types=>ty_ci_st,
          lv_filter TYPE zaor_review-ci_filter.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF ls_ci-results.


    ls_header = go_review->header( ).
    IF NOT gv_filter IS INITIAL.
      lv_filter = gv_filter.
      CLEAR gv_filter.
    ELSE.
      lv_filter = ls_header-ci_filter.
    ENDIF.
    ls_ci = go_review->ci( )->results( lv_filter ).
    IF ls_ci-header IS INITIAL.
      RETURN.
    ENDIF.

    rv_html = '<a name="ci"></a><h2>Code Inspector</h2>&nbsp;' &&
      '<a href="sapevent:rerun">Rerun</a><br><br>' && gc_newline &&
      '<table>' &&
      '<tr>' &&
      '<td>Name:</td><td>' && ls_ci-header-inspecname && '</td>' &&
      '</tr>' && gc_newline &&
      '<tr>' &&
      '<td>Version:</td><td>' && ls_ci-header-inspecvers && '</td>' &&
      '</tr>' && gc_newline &&
      '<tr>' &&
      '<td>Date:</td><td>' && ls_ci-header-creadate && '</td>' &&
      '</tr>' && gc_newline &&
      '<tr>' &&
      '<td>Check Variant:</td><td>' && ls_ci-chkvinf-checkvname && '</td>' &&
      '</tr>' && gc_newline &&
      '<tr>' &&
      '<td>Filter:</td><td>' && filter( lv_filter ) && '</td>' &&
      '</tr>' && gc_newline &&
      '</table><br>' && gc_newline ##no_text.

    IF NOT ls_ci-results IS INITIAL.
      rv_html = rv_html && '<table border="0">' && gc_newline.
      LOOP AT ls_ci-results ASSIGNING <ls_result>.
        rv_html = rv_html &&
          '<tr>' && gc_newline &&
          '<td>' && <ls_result>-sobjtype && '</td>' && gc_newline &&
          '<td>' && <ls_result>-sobjname && '</td>' && gc_newline &&
          '<td>' && <ls_result>-kind && '</td>' && gc_newline &&
          '<td>' && <ls_result>-line && '</td>' && gc_newline &&
          '<td>' && <ls_result>-text && '</td>' && gc_newline &&
          '</tr>' && gc_newline.
      ENDLOOP.
      rv_html = rv_html && '</table>' && gc_newline.
    ELSE.
      rv_html = rv_html && '<font color="red"><b>APPROVED</b></font>'.
    ENDIF.

    rv_html = rv_html && '<br><br>'.

  ENDMETHOD.                    "code_inspector

  METHOD objects.

    DATA: lt_objects TYPE e071_t.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF lt_objects.


    rv_html = '<a name="objects"></a><h2>Objects</h2><br><br>' && gc_newline.

    lt_objects = go_review->objects_list( ).

    rv_html = rv_html && '<table border="0">' && gc_newline.
    LOOP AT lt_objects ASSIGNING <ls_object>.
      rv_html = rv_html &&
        '<tr>' && gc_newline &&
        '<td>' && <ls_object>-object && '</td>' && gc_newline &&
        '<td>' &&
        '<a href="sapevent:navigate?object=' &&
        <ls_object>-object &&
        '&obj_name=' &&
        <ls_object>-obj_name && '">' &&
        <ls_object>-obj_name &&
        '</a>' &&
        '</td>' && gc_newline &&
        '</tr>' && gc_newline.
    ENDLOOP.
    rv_html = rv_html && '</table>' && gc_newline.

  ENDMETHOD.                    "objects

  METHOD comments.

    CONSTANTS: lc_color TYPE c LENGTH 7 VALUE '#C0C0C0'.

    DATA: lv_color TYPE c LENGTH 7,
          lt_list  TYPE zif_aor_types=>ty_comment_tt.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    lt_list = go_review->comments( )->list( ).

    lv_color = lc_color.

    rv_html = '<a name="comments"></a><h2>Comments</h2><br>' &&
      '<table border="0">' && gc_newline.
    LOOP AT lt_list ASSIGNING <ls_list>.
      AT NEW topic.
        rv_html = rv_html && '<tr bgcolor="' && lv_color && '">' && '<td>'.

        IF lv_color IS INITIAL.
          lv_color = lc_color.
        ELSE.
          CLEAR lv_color.
        ENDIF.
      ENDAT.
      rv_html = rv_html &&
        '<u>' &&
        <ls_list>-bname && '&nbsp;' &&
        <ls_list>-time_formatted && '</u>:&nbsp;' &&
        <ls_list>-text &&
        '<br><br>'.
      AT END OF topic.
        IF <ls_list>-closed = abap_false.
          rv_html = rv_html &&
            add_comment( <ls_list>-topic ) &&
            '</td>' &&
            '</tr>'.
        ENDIF.
      ENDAT.
    ENDLOOP.

    rv_html = rv_html                     &&
      '<tr bgcolor="' && lv_color && '">' &&
      '<td>New topic:'                    && gc_newline &&
      add_comment( )                      &&
      '</td></tr>'                        && gc_newline.

    rv_html = rv_html && '</table>'.

  ENDMETHOD.                    "comments

  METHOD add_comment.

* if you are a developer for the transport, it should not be possible to add root comment?

    rv_html =
      '<table border="0" style="foo"><tr><td colspan="2">'                &&
      '<form method="post" action="sapevent:add_comment">'                && gc_newline &&
      '<input type="hidden" name="topic" value="' && iv_topic && '">'     && gc_newline &&
      '<textarea name="comment" cols="60" rows="3"></textarea></td></tr>' && gc_newline &&
      '<tr><td><input type="submit" value="Add">'                         && gc_newline &&
      '</td><td align="right">'                                           &&
      '<a href="sapevent:close?topic=' && iv_topic && '">Close topic</a>' &&
      '</td></tr></table></form>'                                         && gc_newline.

  ENDMETHOD.                    "new_comment

  METHOD close_review.

    rv_html = '<a href="sapevent:closereview">Close review</a><br><br>'.

  ENDMETHOD.                    "close_review

ENDCLASS.                    "lcl_gui_review IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_gui_review DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui_start DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS render
      RETURNING value(rv_html) TYPE string
      RAISING   zcx_aor_error.

  PRIVATE SECTION.
    CLASS-METHODS render_transports
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS render_reviews
      RETURNING value(rv_html) TYPE string
      RAISING   zcx_aor_error.

    CLASS-METHODS status_description
      IMPORTING iv_status             TYPE zaor_status
      RETURNING value(rv_description) TYPE string.

ENDCLASS.                    "lcl_gui_review DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_gui_review IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui_start IMPLEMENTATION.

  METHOD status_description.

    STATICS: st_dd07v TYPE STANDARD TABLE OF dd07v.

    DATA: ls_dd07v LIKE LINE OF st_dd07v.


    IF st_dd07v IS INITIAL.
      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname        = 'ZAOR_STATUS'
          text           = abap_true
        TABLES
          dd07v_tab      = st_dd07v
        EXCEPTIONS
          wrong_textflag = 1
          OTHERS         = 2.                             "#EC CI_SUBRC
      ASSERT sy-subrc = 0.
    ENDIF.

    READ TABLE st_dd07v INTO ls_dd07v WITH KEY domvalue_l = iv_status.
    ASSERT sy-subrc = 0.
    rv_description = ls_dd07v-ddtext.

  ENDMETHOD.                    "status_description

  METHOD render.

    rv_html = lcl_gui=>render_header( ).

    rv_html = rv_html &&
      '<h1>abapOpenReview</h1>' && gc_newline &&
      '<br><br>'                && gc_newline &&
      '<h2>My Stuff</h2>'       && gc_newline &&
      render_transports( )      && gc_newline &&
      '<br><br>'                && gc_newline &&
      '<h2>All Reviews</h2>'    && gc_newline &&
      render_reviews( )         && gc_newline.

    rv_html = rv_html &&
              lcl_gui=>render_footer( ).

  ENDMETHOD.                    "render

  METHOD render_reviews.

    DATA: lt_list   TYPE zif_aor_types=>ty_review_tt,
          lo_review TYPE REF TO zcl_aor_review.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    lt_list = zcl_aor_service=>list( ).

    rv_html = '<table border="0">' && gc_newline.
    LOOP AT lt_list ASSIGNING <ls_list>.
      CREATE OBJECT lo_review
        EXPORTING
          iv_review_id = <ls_list>-review_id.
      rv_html = rv_html &&
        '<tr>' &&
        '<td>' && <ls_list>-review_id && '</td>' &&
        '<td>' && lo_review->header( )-as4text && '</td>' &&
        '<td>' && status_description( <ls_list>-status ) && '</td>' &&
        '<td>' && <ls_list>-responsible && '</td>' &&
        '<td><a href="sapevent:show?review_id=' && <ls_list>-review_id && '">' &&
        'Show</a></td>' &&
        '<td><a href="sapevent:pdf?review_id=' && <ls_list>-review_id && '">' &&
        'PDF</a></td>' &&
        '<td><a href="sapevent:delete?review_id=' && <ls_list>-review_id && '">' &&
        'Delete</a></td>' &&
        '</tr>' ##no_text.
    ENDLOOP.
    rv_html = rv_html && '</table>'.

  ENDMETHOD.                    "render_reviews

  METHOD render_transports.

    DATA: lt_list TYPE zif_aor_types=>ty_transport_tt.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    lt_list = zcl_aor_transport=>list_open( ).

    rv_html = '<table border="0">' && gc_newline.
    LOOP AT lt_list ASSIGNING <ls_list>.
      rv_html = rv_html &&
        '<tr>' &&
        '<td>' && <ls_list>-trkorr && '</td>' &&
        '<td>' && <ls_list>-as4text && '</td>' &&
        '<td><a href="sapevent:new?trkorr=' && <ls_list>-trkorr && '">Start</a></td>' &&
        '</tr>'.
    ENDLOOP.
    rv_html = rv_html && '</table>'.

  ENDMETHOD.                    "render_transports

ENDCLASS.                    "lcl_gui_review

*----------------------------------------------------------------------*
*       CLASS lcl_gui IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui IMPLEMENTATION.

  METHOD postdata.

    DATA: lv_string TYPE string.


    CONCATENATE LINES OF it_postdata INTO lv_string.

    rv_value = getdata( iv_field   = iv_field
                        iv_getdata = lv_string ).

  ENDMETHOD.                    "postdata

  METHOD getdata.

    DATA: lt_fields TYPE TABLE OF string,
          lv_string LIKE LINE OF lt_fields,
          lv_field  TYPE string,
          lv_value  TYPE string.


    SPLIT iv_getdata AT '&' INTO TABLE lt_fields.

    LOOP AT lt_fields INTO lv_string.
      SPLIT lv_string AT '=' INTO lv_field lv_value.
      IF lv_field = iv_field.
        rv_value = lv_value.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "getdata

  METHOD render_footer.

    rv_html = rv_html &&
      '<br><br><hr><center><h3>abapOpenReview Version:&nbsp;' &&
      gc_version &&
      '&nbsp;</h3></center>'.                               "#EC NOTEXT

    rv_html = rv_html && '</body></html>'.

  ENDMETHOD.                    "render_footer

  METHOD render_header.

    rv_html = '<html>'                                && gc_newline &&
          '<head>'                                    && gc_newline &&
          '<title>abapGit</title>'                    && gc_newline &&
          render_css( )                               && gc_newline &&
          '<meta content="text/html; charset=utf-8">' && gc_newline &&
          '<script>'                                  && gc_newline &&
          'function goBack() {'                       && gc_newline &&
          '  window.history.back();'                  && gc_newline &&
          '}'                                         && gc_newline &&
          '</script>'                                 && gc_newline &&
          '</head>'                                   && gc_newline &&
          '<body onload=''' && iv_onload && '''>'.          "#EC NOTEXT

  ENDMETHOD.                    "render_header

  METHOD render_css.

    rv_html = '<style type="text/css">' && gc_newline &&
          'body, textarea {'            && gc_newline &&    "#EC NOTEXT
          '  font-family: verdana;'     && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a:link {'                    && gc_newline &&    "#EC NOTEXT
          '  color: blue;'              && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a:visited {'                 && gc_newline &&    "#EC NOTEXT
          '  color: blue;'              && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.grey:link {'               && gc_newline &&    "#EC NOTEXT
          '  color: grey;'              && gc_newline &&    "#EC NOTEXT
          '  font-size: smaller;'       && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.grey:visited {'            && gc_newline &&    "#EC NOTEXT
          '  color: grey;'              && gc_newline &&    "#EC NOTEXT
          '  font-size: smaller;'       && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.plain:link {'              && gc_newline &&    "#EC NOTEXT
          '  color: black;'             && gc_newline &&    "#EC NOTEXT
          '  text-decoration: none;'    && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.plain:visited {'           && gc_newline &&    "#EC NOTEXT
          '  color: black;'             && gc_newline &&    "#EC NOTEXT
          '  text-decoration: none;'    && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.white:link {'              && gc_newline &&    "#EC NOTEXT
          '  color: white;'             && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.white:visited {'           && gc_newline &&    "#EC NOTEXT
          '  color: white;'             && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'h1 {'                        && gc_newline &&    "#EC NOTEXT
          '  display: inline;'          && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'h2 {'                        && gc_newline &&    "#EC NOTEXT
          '  display: inline;'          && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'h3 {'                        && gc_newline &&    "#EC NOTEXT
          '  display: inline;'          && gc_newline &&    "#EC NOTEXT
          '  color: grey;'              && gc_newline &&    "#EC NOTEXT
          '  font-weight:normal;'       && gc_newline &&    "#EC NOTEXT
          '  font-size: smaller;'       && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'pre {'                       && gc_newline &&
          '  display: inline;'          && gc_newline &&
          '}'                           && gc_newline &&
          'html {'                              && gc_newline &&
          '  background-color: #E4EEF9;'        && gc_newline &&
          '}'                                   && gc_newline &&
          '</style>'                            && gc_newline.

  ENDMETHOD.                    "render_css

  METHOD view.

    CONSTANTS: lc_length TYPE i VALUE 200.

    DATA: lt_data TYPE TABLE OF text200,
          lv_html TYPE string,
          lv_url  TYPE text200.


    lv_html = iv_html.

    WHILE strlen( lv_html ) > 0.
      IF strlen( lv_html ) < lc_length.
        APPEND lv_html TO lt_data.
        CLEAR lv_html.
      ELSE.
        APPEND lv_html(lc_length) TO lt_data.
        lv_html = lv_html+lc_length.
      ENDIF.
    ENDWHILE.

    go_html_viewer->load_data(
      IMPORTING
        assigned_url = lv_url
      CHANGING
        data_table   = lt_data ).

    go_html_viewer->show_url( lv_url ).

  ENDMETHOD.                    "view

  METHOD run.

    DATA: lt_events TYPE cntl_simple_events,
          ls_event  LIKE LINE OF lt_events.


    CREATE OBJECT go_html_viewer
      EXPORTING
        parent = cl_gui_container=>screen0.

    CLEAR ls_event.
    ls_event-eventid = go_html_viewer->m_id_sapevent.
    ls_event-appl_event = 'x'.
    APPEND ls_event TO lt_events.
    go_html_viewer->set_registered_events( lt_events ).

    SET HANDLER lcl_gui=>on_event FOR go_html_viewer.

    view( lcl_gui_start=>render( ) ).

  ENDMETHOD.                    "run

  METHOD new.

    DATA: lv_returncode TYPE c,
          lv_base       TYPE zaor_review-base,
          lv_ci_filter  TYPE zaor_review-ci_filter,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname    = 'E070'.
    <ls_field>-fieldname  = 'TRKORR'.
    <ls_field>-value      = iv_trkorr.
    <ls_field>-field_attr = '02'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'ZAOR_REVIEW'.
    <ls_field>-fieldname = 'BASE'.
    <ls_field>-value     = zif_aor_constants=>c_base-object.
    <ls_field>-field_obl = abap_true.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'ZAOR_REVIEW'.
    <ls_field>-fieldname = 'CI_FILTER'.
    <ls_field>-value     = zif_aor_constants=>c_ci_filter-lines.
    <ls_field>-field_obl = abap_true.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title = 'New'                                 "#EC NOTEXT
      IMPORTING
        returncode  = lv_returncode
      TABLES
        fields      = lt_fields.
    IF lv_returncode = 'A'.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 2 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    lv_base = <ls_field>-value.

    READ TABLE lt_fields INDEX 3 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    lv_ci_filter = <ls_field>-value.

    zcl_aor_service=>open( iv_trkorr    = iv_trkorr
                           iv_base      = lv_base
                           iv_ci_filter = lv_ci_filter ).

    rv_success = abap_true.

  ENDMETHOD.                    "new

  METHOD on_event.

    DATA: lv_review_id TYPE zaor_review_id,
          lv_topic     TYPE zaor_comment-topic,
          lv_text      TYPE string,
          lv_trkorr    TYPE trkorr,
          lv_object    TYPE e071-object,
          lv_obj_name  TYPE e071-obj_name,
          lx_error     TYPE REF TO zcx_aor_error.


    lv_review_id = getdata( iv_field   = 'review_id'
                            iv_getdata = getdata ) ##no_text.

    TRY.
        CASE action.
          WHEN 'new'.
            lv_trkorr = getdata( iv_field   = 'trkorr'
                                 iv_getdata = getdata ) ##no_text.
            IF new( lv_trkorr ) = abap_true.
              view( lcl_gui_start=>render( ) ).
            ENDIF.
          WHEN 'filter'.
            lcl_gui_review=>gv_filter = getdata( iv_field   = 'filter'
                                                 iv_getdata = getdata ) ##no_text.
            view( lcl_gui_review=>render( 'location.href="#ci";' ) ) ##no_text.
          WHEN 'show'.
            CREATE OBJECT go_review
              EXPORTING
                iv_review_id = lv_review_id.
            view( lcl_gui_review=>render( ) ).
          WHEN 'delete'.
            CREATE OBJECT go_review
              EXPORTING
                iv_review_id = lv_review_id.
            go_review->delete( ).
            CLEAR go_review.
            view( lcl_gui_start=>render( ) ).
          WHEN 'pdf'.
            CREATE OBJECT go_review
              EXPORTING
                iv_review_id = lv_review_id.
            go_review->pdf( ).
          WHEN 'add_comment'.
            lv_topic = postdata( iv_field    = 'topic'
                                 it_postdata = postdata ) ##no_text.
            lv_text = postdata( iv_field    = 'comment'
                                it_postdata = postdata ) ##no_text.
            lv_text = cl_http_utility=>if_http_utility~unescape_url( lv_text ).
            go_review->comments( )->add( iv_topic  = lv_topic
                                         iv_text   = lv_text ) ##no_text.
            view( lcl_gui_review=>render( 'location.href="#comments";' ) ) ##no_text.
          WHEN 'back'.
            view( lcl_gui_start=>render( ) ).
          WHEN 'close'.
            lv_topic = getdata( iv_field   = 'topic'
                                 iv_getdata = getdata ) ##no_text.
            go_review->comments( )->close( lv_topic ).
            view( lcl_gui_review=>render( ) ).
          WHEN 'closereview'.
            go_review->close( ).
            view( lcl_gui_start=>render( ) ).
          WHEN 'navigate'.
            lv_object = getdata( iv_field   = 'object'
                                 iv_getdata = getdata )  ##no_text.
            lv_obj_name = getdata( iv_field   = 'obj_name'
                                   iv_getdata = getdata )  ##no_text.
            lcl_navigate=>navigate( iv_object = lv_object
                                    iv_obj_name = lv_obj_name ).
          WHEN 'rerun'.
            go_review->ci( )->run( ).
            view( lcl_gui_review=>render( ) ).
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_aor_error
              EXPORTING
                textid = zcx_aor_error=>unknown_action.
        ENDCASE.
      CATCH zcx_aor_error INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.                    "on_event

ENDCLASS.                    "lcl_gui IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM run.

  DATA: lx_error TYPE REF TO zcx_aor_error.


  TRY.
      lcl_gui=>run( ).
    CATCH zcx_aor_error INTO lx_error.
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.

  WRITE: / '.'.     " required

ENDFORM.                    "run