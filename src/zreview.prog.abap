REPORT zreview.

* See https://github.com/larshp/abapOpenReview

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

SELECTION-SCREEN BEGIN OF SCREEN 1001.
* dummy for triggering screen
SELECTION-SCREEN END OF SCREEN 1001.

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

    IF iv_object = 'TABU' OR iv_object = 'CDAT'.
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
    ASSERT sy-subrc <> 1 AND sy-subrc <> 3.

  ENDMETHOD.                    "navigate

ENDCLASS.                    "lcl_navigate IMPLEMENTATION

CLASS lcl_util DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      unescape IMPORTING iv_data        TYPE clike
               RETURNING VALUE(rv_data) TYPE string.

ENDCLASS.

CLASS lcl_util IMPLEMENTATION.

  METHOD unescape.
* http://www.obkb.com/dcljr/charstxt.html

* cl_http_utility=>if_http_utility~unescape_url removes newlines
* is there no standard utility to call?

    rv_data = iv_data.

    REPLACE ALL OCCURRENCES OF '%3D' IN rv_data WITH '='.
    REPLACE ALL OCCURRENCES OF '%3F' IN rv_data WITH '?'.
    REPLACE ALL OCCURRENCES OF '%26' IN rv_data WITH '&'.

  ENDMETHOD.

ENDCLASS.

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
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS render_footer
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS on_event
                  FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING action frame getdata postdata query_table.  "#EC NEEDED

  PRIVATE SECTION.
    CLASS-DATA go_html_viewer TYPE REF TO cl_gui_html_viewer.

    CLASS-METHODS getdata
      IMPORTING iv_field        TYPE string
                iv_getdata      TYPE clike
      RETURNING VALUE(rv_value) TYPE string.

    CLASS-METHODS answer
      IMPORTING iv_getdata TYPE clike.

    CLASS-METHODS postdata
      IMPORTING iv_field        TYPE string
                it_postdata     TYPE cnht_post_data_tab
      RETURNING VALUE(rv_value) TYPE string.

    CLASS-METHODS view
      IMPORTING iv_html TYPE string.

    CLASS-METHODS render_css
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS new
      IMPORTING iv_trkorr         TYPE trkorr
      RETURNING VALUE(rv_success) TYPE abap_bool
      RAISING   zcx_aor_error.

    CLASS-METHODS parse_query_table
      IMPORTING query  TYPE cnht_query_table
      EXPORTING result TYPE any.

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
      RETURNING VALUE(rv_html) TYPE string
      RAISING   zcx_aor_error.

    CLASS-DATA: gv_filter TYPE zaor_review-ci_filter.
    CLASS-DATA: gv_code_comments_hidden TYPE sap_bool.

  PRIVATE SECTION.
    CONSTANTS: gc_color_comment TYPE c LENGTH 7 VALUE '#C0C0C0'.

    CLASS-METHODS add_comment
      IMPORTING iv_topic       TYPE zaor_comment-topic OPTIONAL
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS comments_header
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS comments
      IMPORTING it_list          TYPE zif_aor_types=>ty_comment_tt
                iv_add_new_topic TYPE sap_bool
      RETURNING VALUE(rv_html)   TYPE string.

    CLASS-METHODS info
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS checklist
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS close_review
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS objects
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS diff
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS render_diff_with_comments
      IMPORTING object         TYPE zaor_object
                diff           TYPE zif_aor_types=>ty_diff_st
                comments       TYPE zif_aor_types=>ty_comment_tt
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS render_pure_diff
      IMPORTING
                diff           TYPE zif_aor_types=>ty_diff_st
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS code_inspector
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS filter
      IMPORTING iv_filter      TYPE zaor_review-ci_filter
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS shortcuts
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS create_diff_key
      IMPORTING object     TYPE zaor_object
                line       TYPE zif_aor_types=>ty_diff_st
      RETURNING VALUE(key) TYPE string.

    CLASS-METHODS get_comments_for_line
      IMPORTING object                   TYPE zaor_object
                line                     TYPE zif_aor_types=>ty_diff_st
                comments                 TYPE zif_aor_types=>ty_comment_tt
      RETURNING VALUE(comments_on_topic) TYPE zif_aor_types=>ty_comment_tt.

    CLASS-METHODS line_has_new_comment
      IMPORTING object      TYPE zaor_object
                line        TYPE zif_aor_types=>ty_diff_st
      EXPORTING topic       TYPE zaor_topic
                has_comment TYPE sap_bool.

    CLASS-METHODS add_new_comment
      IMPORTING iv_topic       TYPE zaor_topic OPTIONAL
      RETURNING VALUE(rv_html) TYPE string.

ENDCLASS.                    "lcl_gui_start DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_gui_review IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui_review IMPLEMENTATION.

  METHOD diff.

    DATA: lv_style             TYPE string,
          lt_diff_list         TYPE zif_aor_types=>ty_diff_list_tt,
          lt_diff              TYPE zif_aor_types=>ty_diff_tt,
          lt_comments          TYPE zif_aor_types=>ty_comment_tt.

    FIELD-SYMBOLS: <ls_diff>      LIKE LINE OF lt_diff,
                   <ls_diff_list> LIKE LINE OF lt_diff_list.


    rv_html = '<a name="diff"></a><h2>Diff</h2><br>'.
    IF gv_code_comments_hidden = abap_false.
      rv_html = rv_html && '<form method="post" action="sapevent:hide_code_comments">' &&
                '<input type="submit" value="Hide code comments" ></form>'.
    ELSE.
      rv_html = rv_html && '<form method="post" action="sapevent:show_code_comments">' &&
                '<input type="submit" value="Show code comments" ></form>'.
    ENDIF.

    lt_diff_list = go_review->diff( ).
    lt_comments = go_review->comments( )->list( iv_with_code_comments = abap_true ).

    IF lt_diff_list IS INITIAL.
      rv_html = rv_html && 'Empty'  ##no_text.
    ENDIF.

    LOOP AT lt_diff_list ASSIGNING <ls_diff_list>.
      rv_html = rv_html                &&
        <ls_diff_list>-object-object   &&
        '&nbsp;'                       &&
        <ls_diff_list>-object-obj_name &&
        '&nbsp;Last changed:&nbsp;'    &&
        zcl_aor_time=>format_date_time( iv_date = <ls_diff_list>-last_changed_date
          iv_time = <ls_diff_list>-last_changed_time ) &&
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
          IF gv_code_comments_hidden = abap_false.
            rv_html = rv_html && render_diff_with_comments( object = <ls_diff_list>-object
              diff = <ls_diff> comments = lt_comments ).
          ELSE.
            rv_html = rv_html && render_pure_diff( diff = <ls_diff> ).
          ENDIF.
        ENDLOOP.
        rv_html = rv_html && '</table>'.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "diff

  METHOD render_diff_with_comments.
    DATA: lv_style             TYPE string,
          lt_comments_on_topic TYPE zif_aor_types=>ty_comment_tt,
          lv_topic             TYPE zaor_topic,
          lv_has_comment       TYPE sap_bool.

    IF diff IS INITIAL.
      CLEAR lv_style.
    ELSEIF diff-new <> ''.
      lv_style = ' style="background:lightgreen;"'.         "#EC NOTEXT
    ELSE.
      lv_style = ' style="background:lightpink;"'.          "#EC NOTEXT
    ENDIF.
    rv_html = rv_html &&
      '<tr>' &&
      '<td><a href="sapevent:add_comment_on_code?' &&
        create_diff_key( object = object line = diff )
        && '">'
        && diff-new && '&nbsp;</a></td>' &&
      '<td>' && diff-old && '&nbsp;</td>' &&
      '<td>' && diff-updkz && '&nbsp;</td>' &&
      '<td' && lv_style && '><pre>' && diff-code && '</pre></td>' &&
      '</tr>'.
    lt_comments_on_topic = get_comments_for_line( object = object
      comments = comments line = diff ).
    IF lines( lt_comments_on_topic ) > 0.
      rv_html = rv_html && '<tr><td></td><td></td><td></td><td>'
        && comments( it_list = lt_comments_on_topic iv_add_new_topic = abap_false )
        && '</td></tr>' && gc_newline.
    ENDIF.
    line_has_new_comment( EXPORTING object = object line = diff
      IMPORTING topic = lv_topic has_comment = lv_has_comment ).
    IF lv_has_comment = abap_true.
      rv_html = rv_html && '<tr><td></td><td></td><td></td>' && add_new_comment( lv_topic )
        && '</tr>' && gc_newline.
    ENDIF.

  ENDMETHOD.

  METHOD render_pure_diff.
    DATA: lv_style TYPE string.

    IF diff IS INITIAL.
      CLEAR lv_style.
    ELSEIF diff-new <> ''.
      lv_style = ' style="background:lightgreen;"'.         "#EC NOTEXT
    ELSE.
      lv_style = ' style="background:lightpink;"'.          "#EC NOTEXT
    ENDIF.
    rv_html = rv_html &&
      '<tr>' &&
      '<td>' && diff-new && '&nbsp;</a></td>' &&
      '<td>' && diff-old && '&nbsp;</td>' &&
      '<td>' && diff-updkz && '&nbsp;</td>' &&
      '<td' && lv_style && '><pre>' && diff-code && '</pre></td>' &&
      '</tr>'.

  ENDMETHOD.

  METHOD create_diff_key.

    key = |PGMID={ object-pgmid }&OBJECT={ object-object }&OBJ_NAME={ object-obj_name }| &&
      |&NEW_LINE={ line-new }&OLD_LINE={ line-old }|.

  ENDMETHOD.

  METHOD get_comments_for_line.
    DATA: ls_comment TYPE REF TO zif_aor_types=>ty_comment.

    CLEAR comments_on_topic.
    LOOP AT comments REFERENCE INTO ls_comment
      WHERE pgmid = object-pgmid AND object = object-object AND obj_name = object-obj_name
      AND new_line = line-new AND old_line = line-old.

      APPEND ls_comment->* TO comments_on_topic.

    ENDLOOP.

  ENDMETHOD.

  METHOD line_has_new_comment.
    DATA: BEGIN OF ls_line,
            new TYPE zaor_code_com-new_line,
            old TYPE zaor_code_com-new_line,
          END OF ls_line,
          ls_position TYPE REF TO zaor_code_com.

    CLEAR: topic, has_comment.
    ls_line-new = line-new.
    ls_line-old = line-old.
    READ TABLE go_review->mv_pos_new_code_comments REFERENCE INTO ls_position
      WITH TABLE KEY pgmid = object-pgmid object = object-object obj_name = object-obj_name
      new_line = ls_line-new old_line = ls_line-old.
    IF sy-subrc = 0.
      topic = ls_position->*-topic.
      has_comment = abap_true.
    ENDIF.

  ENDMETHOD.

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
      checklist( )                          && gc_newline &&
      '<br>'                                && gc_newline &&
      code_inspector( )                     && gc_newline &&
      diff( )                               && gc_newline &&
      '<br><br>'                            && gc_newline &&
      comments_header( )                    && gc_newline &&
      comments( it_list = go_review->comments( )->list( ) iv_add_new_topic = abap_true ) && gc_newline &&
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
          ls_ci     TYPE zif_aor_types=>ty_ci_st,
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

  METHOD checklist.

    DATA: lo_checklist TYPE REF TO zcl_aor_checklist,
          lv_info      TYPE string,
          lt_list      TYPE zcl_aor_checklist=>ty_checklist_tt.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    CREATE OBJECT lo_checklist
      EXPORTING
        io_review = go_review.

    lt_list = lo_checklist->list( ).

    IF lines( lt_list ) = 0.
      RETURN.
    ENDIF.

    rv_html = '<a name="checklist"></a><h2>Checklist</h2><br><br>' && gc_newline.

    rv_html = rv_html && '<table border="0">' && gc_newline.
    LOOP AT lt_list ASSIGNING <ls_list>.

      IF <ls_list>-answer IS INITIAL.
        lv_info = '<a href="sapevent:answer?item=' && <ls_list>-item && '&answer=Yes' && '">Yes</a>&nbsp;' &&
                  '<a href="sapevent:answer?item=' && <ls_list>-item && '&answer=No' && '">No</a>'.
      ELSE.
        lv_info = <ls_list>-answer &&
          '&nbsp;' &&
          <ls_list>-bname &&
          '&nbsp;' &&
          zcl_aor_time=>format_timestamp( <ls_list>-timestamp ).
      ENDIF.

      rv_html = rv_html &&
        '<tr>' && gc_newline &&
        '<td>' && <ls_list>-description && '</td>' && gc_newline &&
        '<td>' &&
        lv_info &&
        '</td>' && gc_newline &&
        '</tr>' && gc_newline.
    ENDLOOP.
    rv_html = rv_html && '</table>' && gc_newline.

  ENDMETHOD.

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

  METHOD comments_header.

    rv_html = '<a name="comments"></a><h2>Comments</h2><br>'.

  ENDMETHOD.

  METHOD comments.
    DATA: lv_color(7)     TYPE c,
          ls_list         TYPE zif_aor_types=>ty_comment,
          lv_topic_closed TYPE sap_bool.

    rv_html = '<table border="0">'.
    LOOP AT it_list INTO ls_list.
      AT NEW topic.
        rv_html = rv_html && '<tr bgcolor="' && lv_color && '">' && '<td>'.

        IF lv_color IS INITIAL.
          lv_color = gc_color_comment.
        ELSE.
          CLEAR lv_color.
        ENDIF.
        IF zcl_aor_comments=>can_delete( ls_list ) = abap_true.
          rv_html = rv_html &&
            |<a href="sapevent:delete_comment?topic={ ls_list-topic }">Delete</a>&nbsp;|.
        ENDIF.
      ENDAT.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
        IN ls_list-text WITH '<br>'.
      rv_html = rv_html &&
        '<u>' &&
        ls_list-bname && '&nbsp;' &&
        ls_list-time_formatted && '</u>:&nbsp;' &&
        ls_list-text &&
        '<br><br>'.

      " Buffer it: in the AT-block "closed" just contains a star
      lv_topic_closed = ls_list-closed.
      AT END OF topic.
        IF lv_topic_closed = abap_false.
          rv_html = rv_html &&
            add_comment( ls_list-topic ) &&
            '</td>' &&
            '</tr>'.
        ENDIF.
      ENDAT.
    ENDLOOP.

    IF iv_add_new_topic = abap_true.
      rv_html = rv_html &&
        '<tr>' && gc_newline &&
        add_new_comment( ) &&
        '</tr>' && gc_newline.
    ENDIF.

    rv_html = rv_html && '</table>'.

  ENDMETHOD.                    "comments

  METHOD add_new_comment.

    rv_html =
      '<td bgcolor="' && gc_color_comment && '">' &&
      'New topic:'                                && gc_newline &&
      add_comment( iv_topic )                     &&
      '</td>'.

  ENDMETHOD.

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
      RETURNING VALUE(rv_html) TYPE string
      RAISING   zcx_aor_error.

  PRIVATE SECTION.
    CLASS-METHODS render_transports
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS render_reviews
      RETURNING VALUE(rv_html) TYPE string
      RAISING   zcx_aor_error.

    CLASS-METHODS status_description
      IMPORTING iv_status             TYPE zaor_status
      RETURNING VALUE(rv_description) TYPE string.

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
      '<br><br><hr><center><h3>abapOpenReview</h3></center>'. "#EC NOTEXT

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

    SET HANDLER on_event FOR go_html_viewer.

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
    <ls_field>-value     = zif_aor_constants=>c_base-transport.
    <ls_field>-field_obl = abap_true.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'ZAOR_REVIEW'.
    <ls_field>-fieldname = 'CI_FILTER'.
    <ls_field>-value     = zif_aor_constants=>c_ci_filter-none.
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

  METHOD answer.

    DATA: lo_checklist TYPE REF TO zcl_aor_checklist,
          lv_item      TYPE zaor_checklist_item,
          lv_answer    TYPE zaor_checklist_answer.


    lv_item = getdata( iv_field   = 'item'
                       iv_getdata = iv_getdata ) ##no_text.

    lv_answer = getdata( iv_field   = 'answer'
                         iv_getdata = iv_getdata ) ##no_text.

    CREATE OBJECT lo_checklist
      EXPORTING
        io_review = go_review.

    lo_checklist->answer(  iv_item   = lv_item
                           iv_answer = lv_answer ).

  ENDMETHOD.

  METHOD on_event.

    DATA: lv_review_id             TYPE zaor_review_id,
          lv_topic                 TYPE zaor_comment-topic,
          lv_text                  TYPE string,
          lv_trkorr                TYPE trkorr,
          lv_object                TYPE e071-object,
          lv_obj_name              TYPE e071-obj_name,
          ls_position_code_comment TYPE zaor_code_com,
          lv_is_code_comment       TYPE sap_bool,
          lx_error                 TYPE REF TO zcx_aor_error.


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
          WHEN 'answer'.
            answer( getdata ).
            view( lcl_gui_review=>render( 'location.href="#checklist";' ) ) ##no_text.
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
            lv_text = lcl_util=>unescape( lv_text ).
            lv_is_code_comment = go_review->comments( )->add( iv_topic  = lv_topic
                                         iv_text   = lv_text ) ##no_text.
            IF lv_is_code_comment = abap_false.
              view( lcl_gui_review=>render( 'location.href="#comments";' ) ) ##no_text.
            ELSE.
              view( lcl_gui_review=>render( 'location.href="#diff";' ) ) ##no_text.
            ENDIF.
          WHEN 'add_comment_on_code'.
            parse_query_table( EXPORTING query = query_table
              IMPORTING result = ls_position_code_comment ).
            go_review->pre_add_code_comment( iv_position = ls_position_code_comment ).
            view( lcl_gui_review=>render( 'location.href="#diff";' ) ) ##no_text.
          WHEN 'back'.
            view( lcl_gui_start=>render( ) ).
          WHEN 'close'.
            lv_topic = getdata( iv_field   = 'topic'
                                 iv_getdata = getdata ) ##no_text.
            go_review->comments( )->close( lv_topic ).
            view( lcl_gui_review=>render( 'location.href="#comments"' ) ).
          WHEN 'delete_comment'.
            lv_topic = getdata( iv_field   = 'topic'
                                 iv_getdata = getdata ) ##no_text.
            go_review->comments( )->delete( lv_topic ).
            view( lcl_gui_review=>render( 'location.href="#comments"' ) ).
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
          WHEN 'hide_code_comments'.
            lcl_gui_review=>gv_code_comments_hidden = abap_true.
            view( lcl_gui_review=>render( 'location.href="#diff";' ) ) ##no_text.
          WHEN 'show_code_comments'.
            lcl_gui_review=>gv_code_comments_hidden = abap_false.
            view( lcl_gui_review=>render( 'location.href="#diff";' ) ) ##no_text.
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_aor_error
              EXPORTING
                textid = zcx_aor_error=>unknown_action.
        ENDCASE.
      CATCH zcx_aor_error INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.                    "on_event

  METHOD parse_query_table.
    DATA: lo_descriptor TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS: <ls_component> TYPE abap_compdescr,
                   <ls_query_var> TYPE w3query,
                   <lv_value>     TYPE any.

    lo_descriptor ?= cl_abap_datadescr=>describe_by_data( result ).

    LOOP AT lo_descriptor->components ASSIGNING <ls_component>.
      READ TABLE query ASSIGNING <ls_query_var>
        WITH KEY name = <ls_component>-name.
      IF sy-subrc = 0.
        ASSIGN COMPONENT <ls_component>-name OF STRUCTURE result TO <lv_value>.
        <lv_value> = <ls_query_var>-value.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

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

  CALL SELECTION-SCREEN 1001. " trigger screen

ENDFORM.                    "run
