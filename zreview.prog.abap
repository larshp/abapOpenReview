REPORT zreview.
* todo license

CONSTANTS: gc_version TYPE string VALUE 'v0.1-alpha',       "#EC NOTEXT
           gc_newline TYPE abap_char1 VALUE cl_abap_char_utilities=>newline.

DEFINE _raise.
  raise exception type lcx_exception
    exporting
      iv_text = &1.                                         "#EC NOTEXT
END-OF-DEFINITION.

START-OF-SELECTION.
  PERFORM run.

*----------------------------------------------------------------------*
*       CLASS CX_LOCAL_EXCEPTION DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_exception DEFINITION INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.
    DATA mv_text TYPE string.

    METHODS constructor
      IMPORTING iv_text TYPE string.

ENDCLASS.                    "CX_LOCAL_EXCEPTION DEFINITION

*----------------------------------------------------------------------*
*       CLASS CX_LOCAL_EXCEPTION IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_exception IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mv_text = iv_text.
  ENDMETHOD.                    "CONSTRUCTOR

ENDCLASS.                    "lcx_exception IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_actions DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_actions DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS new
      RAISING lcx_exception.

ENDCLASS.                    "lcl_actions DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_actions IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_actions IMPLEMENTATION.

  METHOD new.
    BREAK-POINT.
  ENDMETHOD.                    "new

ENDCLASS.                    "lcl_actions IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_gui DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS run
      RAISING lcx_exception.

    CLASS-METHODS on_event
      FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING action frame getdata postdata query_table.  "#EC NEEDED

  PRIVATE SECTION.
    CLASS-DATA go_html_viewer TYPE REF TO cl_gui_html_viewer.

    CLASS-METHODS getdata
      IMPORTING iv_field TYPE string
                iv_getdata TYPE clike
      RETURNING value(rv_value) TYPE string.

    CLASS-METHODS view
      IMPORTING iv_html TYPE string.

    CLASS-METHODS render
      RETURNING value(rv_html) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS render_header
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS render_footer
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS render_css
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS render_transports
      RETURNING value(rv_html) TYPE string.

ENDCLASS.                    "lcl_gui DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_gui IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui IMPLEMENTATION.

  METHOD getdata.
    BREAK-POINT.
  ENDMETHOD.                    "getdata

  METHOD render.

    rv_html = render_header( ).

    rv_html = rv_html &&
      '<h1>abapOpenReview</h1>' && gc_newline &&
      '<br><br>'                && gc_newline &&
      '<h2>My Stuff</h2>'       && gc_newline &&
      render_transports( )      && gc_newline &&
      '<br><br>'                && gc_newline &&
      '<h2>All Reviews</h2>'    && gc_newline.

    rv_html = rv_html &&
              render_footer( ).

  ENDMETHOD.                    "render

  METHOD render_transports.

    DATA: lt_list TYPE zcl_aor_transport=>ty_transport_tt.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    lt_list = zcl_aor_transport=>list( ).

    rv_html = '<table border="1">' && gc_newline.
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

  METHOD render_footer.

    rv_html = rv_html &&
      '<br><br><hr><center><h3>abapOpenReview Version:&nbsp;' &&
      gc_version &&
      '&nbsp;</h3></center>'.                               "#EC NOTEXT

    rv_html = rv_html && '</body></html>'.

  ENDMETHOD.                    "render_footer

  METHOD render_header.

    rv_html = '<html>'
          && gc_newline &&
          '<head>'
          && gc_newline &&
          '<title>abapGit</title>'
          && gc_newline &&
          render_css( )
          && gc_newline &&
          '<meta http-equiv="content-type" content="text/html; charset=utf-8">'
          && gc_newline &&
          '<script>'
          && gc_newline &&
          'function goBack() {'
          && gc_newline &&
          '  window.history.back();'
          && gc_newline &&
          '}'
          && gc_newline &&
          '</script>'
          && gc_newline &&
          '</head>'
          && gc_newline &&
          '<body>'.                                         "#EC NOTEXT

  ENDMETHOD.                    "render_header

  METHOD render_css.

    rv_html = '<style type="text/css">' && gc_newline &&
          'body {'                      && gc_newline &&    "#EC NOTEXT
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
          '</style>'                    && gc_newline.

  ENDMETHOD.                    "render_css

  METHOD view.

    DATA: lt_data TYPE TABLE OF text200,
          lv_html TYPE string,
          lv_url  TYPE text200.


    lv_html = iv_html.

    WHILE strlen( lv_html ) > 0.
      IF strlen( lv_html ) < 200.
        APPEND lv_html TO lt_data.
        CLEAR lv_html.
      ELSE.
        APPEND lv_html(200) TO lt_data.
        lv_html = lv_html+200.
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

    view( render( ) ).

  ENDMETHOD.                    "run

  METHOD on_event.

    DATA: lv_trkorr    TYPE trkorr,
          lx_exception TYPE REF TO lcx_exception.


    TRY.
        CASE action.
          WHEN 'new'.
            lv_trkorr = getdata( iv_field   = 'trkorr'
                                 iv_getdata = getdata ).
            zcl_aor_review=>new( lv_trkorr ).
          WHEN OTHERS.
            _raise 'Unknown action'.
        ENDCASE.
      CATCH lcx_exception INTO lx_exception.
        MESSAGE lx_exception->mv_text TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.                    "on_event

ENDCLASS.                    "lcl_gui IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM run.

  DATA: lx_exception TYPE REF TO lcx_exception.

* todo, check client settings? must be development client

  TRY.
      lcl_gui=>run( ).
    CATCH lcx_exception INTO lx_exception.
      MESSAGE lx_exception->mv_text TYPE 'E'.
  ENDTRY.

  WRITE: / '.'.     " required

ENDFORM.                    "run