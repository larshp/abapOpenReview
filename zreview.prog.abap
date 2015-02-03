REPORT zreview.
* todo license?

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
*       CLASS lcl_time DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_time DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS: format
      IMPORTING iv_timestamp TYPE timestamp
      RETURNING value(rv_text) TYPE string.

ENDCLASS.                    "lcl_time DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_time IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_time IMPLEMENTATION.

  METHOD format.

    DATA: lv_time  LIKE sy-uzeit,
          lv_text  TYPE c LENGTH 15,
          lv_date  LIKE sy-datum,
          lv_ctime TYPE c LENGTH 8,
          lv_cdate TYPE C LENGTH 10.


    lv_text = iv_timestamp.
    lv_date = lv_text.
    lv_time = lv_text+8.

    WRITE lv_date TO lv_cdate.
    WRITE lv_time TO lv_ctime.
    CONCATENATE lv_cdate lv_ctime INTO rv_text SEPARATED BY space.

  ENDMETHOD.                    "format

ENDCLASS.                    "lcl_time IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_gui DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS run
      RAISING lcx_exception.

    CLASS-METHODS render_header
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS render_footer
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS on_event
      FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING action frame getdata postdata query_table.  "#EC NEEDED

  PRIVATE SECTION.
    CLASS-DATA go_html_viewer TYPE REF TO cl_gui_html_viewer.

    CLASS-METHODS getdata
      IMPORTING iv_field TYPE string
                iv_getdata TYPE clike
      RETURNING value(rv_value) TYPE string.

    CLASS-METHODS postdata
      IMPORTING iv_field TYPE string
                it_postdata TYPE cnht_post_data_tab
      RETURNING value(rv_value) TYPE string.

    CLASS-METHODS view
      IMPORTING iv_html TYPE string.

    CLASS-METHODS render_css
      RETURNING value(rv_html) TYPE string.

ENDCLASS.                    "lcl_gui DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_gui_start DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui_review DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS render
      IMPORTING iv_trkorr TYPE trkorr
      RETURNING value(rv_html) TYPE string
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-DATA: gv_trkorr TYPE trkorr.

    CLASS-METHODS add_comment
      IMPORTING iv_topic TYPE zaor_comment-topic OPTIONAL
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS comments
      RETURNING value(rv_html) TYPE string.

ENDCLASS.                    "lcl_gui_start DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_gui_review IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui_review IMPLEMENTATION.

  METHOD render.

    gv_trkorr = iv_trkorr.

    rv_html = lcl_gui=>render_header( ).

    rv_html = rv_html &&
      '<h1>Review&nbsp;' && iv_trkorr && '</h1>'     && gc_newline &&
      '&nbsp;<a href="javascript:goBack()">Back</a>' && gc_newline &&
      '<br><br>todo, list objects in transport<br>'  && gc_newline &&
      'todo, code inspector results<br><br>'         && gc_newline &&
      comments( )                                    && gc_newline &&
      add_comment( ) && gc_newline &&
      '<a href="sapevent:close&trkorr=' && iv_trkorr && '">Close review </a>'.

    rv_html = rv_html && lcl_gui=>render_footer( ).

  ENDMETHOD.                    "render

  METHOD comments.

    DATA: lt_list TYPE zcl_aor_review=>ty_comment_tt.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    lt_list = zcl_aor_review=>comment_list( gv_trkorr ).

    rv_html = '<table border="1">' && gc_newline.
    LOOP AT lt_list ASSIGNING <ls_list>.
      AT NEW topic.
        rv_html = rv_html && '<tr>' && '<td>'.
      ENDAT.
      rv_html = rv_html &&
        <ls_list>-bname && '&nbsp;' &&
        lcl_time=>format( <ls_list>-timestamp ) && ':&nbsp;' &&
        <ls_list>-text &&
        '<br><br>'.
      AT END OF topic.
        rv_html = rv_html && add_comment( <ls_list>-topic ) && '</td>' && '</tr>'.
      ENDAT.
    ENDLOOP.
    rv_html = rv_html && '</table>'.

  ENDMETHOD.                    "comments

  METHOD add_comment.

* if you are a developer for the transport, it should not be possible to add root comment

    rv_html =
      '<form method="post" action="sapevent:add_comment">' && gc_newline &&
      '<input type="hidden" name="trkorr" value="' && gv_trkorr && '">' &&
      '<input type="hidden" name="topic" value="' && iv_topic && '">' &&
      '<textarea name="comment"></textarea><br>'           && gc_newline &&
      '<input type="submit" value="Add">'                  && gc_newline &&
      '</form>'                                            && gc_newline.

  ENDMETHOD.                    "new_comment

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
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS render_transports
      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS render_reviews
      RETURNING value(rv_html) TYPE string.

ENDCLASS.                    "lcl_gui_review DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_gui_review IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui_start IMPLEMENTATION.

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

    DATA: lt_list TYPE zcl_aor_review=>ty_review_tt.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    lt_list = zcl_aor_review=>list( ).

    rv_html = '<table border="1">' && gc_newline.
    LOOP AT lt_list ASSIGNING <ls_list>.
      rv_html = rv_html &&
        '<tr>' &&
        '<td>' && <ls_list>-trkorr && '</td>' &&
        '<td>' && <ls_list>-status && '</td>' &&
        '<td><a href="sapevent:show?trkorr=' && <ls_list>-trkorr && '">Show</a></td>' &&
        '</tr>'.
    ENDLOOP.
    rv_html = rv_html && '</table>'.

  ENDMETHOD.                    "render_reviews

  METHOD render_transports.

    DATA: lt_list TYPE zcl_aor_transport=>ty_transport_tt.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    lt_list = zcl_aor_transport=>list_open( ).

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
          '<body>'.                                         "#EC NOTEXT

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

    view( lcl_gui_start=>render( ) ).

  ENDMETHOD.                    "run

  METHOD on_event.

    DATA: lv_trkorr    TYPE trkorr,
          lv_topic     TYPE zaor_comment-topic,
          lv_text      TYPE string,
          lx_exception TYPE REF TO lcx_exception.


    TRY.
        CASE action.
          WHEN 'new'.
            lv_trkorr = getdata( iv_field   = 'trkorr'
                                 iv_getdata = getdata ).
            zcl_aor_review=>new( lv_trkorr ).
            view( lcl_gui_start=>render( ) ).
          WHEN 'show'.
            lv_trkorr = getdata( iv_field   = 'trkorr'
                                 iv_getdata = getdata ).
            view( lcl_gui_review=>render( lv_trkorr ) ).
          WHEN 'add_comment'.
            lv_trkorr = postdata( iv_field    = 'trkorr'
                                  it_postdata = postdata ).
            lv_topic = postdata( iv_field    = 'topic'
                                 it_postdata = postdata ).
            lv_text = postdata( iv_field    = 'comment'
                                it_postdata = postdata ).
            zcl_aor_review=>comment_add( iv_trkorr = lv_trkorr
                                         iv_topic  = lv_topic
                                         iv_text   = lv_text ).
            view( lcl_gui_review=>render( lv_trkorr ) ).
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

* todo, check client settings? must be development client?

  TRY.
      lcl_gui=>run( ).
    CATCH lcx_exception INTO lx_exception.
      MESSAGE lx_exception->mv_text TYPE 'E'.
  ENDTRY.

  WRITE: / '.'.     " required

ENDFORM.                    "run