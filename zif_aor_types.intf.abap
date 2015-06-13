INTERFACE zif_aor_types
  PUBLIC .

  TYPES: ty_vrso_tt TYPE STANDARD TABLE OF vrso WITH DEFAULT KEY.
  TYPES: ty_review_tt TYPE STANDARD TABLE OF zaor_review WITH DEFAULT KEY .
  TYPES: ty_comment_tt TYPE STANDARD TABLE OF zaor_comment WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_transport .
          INCLUDE TYPE e070.
  TYPES: as4text TYPE e07t-as4text,
         END OF ty_transport .
  TYPES:
    ty_transport_tt TYPE STANDARD TABLE OF ty_transport WITH DEFAULT KEY .

  TYPES: BEGIN OF ty_diff_st,
           new   TYPE c LENGTH 6,
           old   TYPE c LENGTH 6,
           updkz TYPE c LENGTH 1,
           code  TYPE text255,
         END OF ty_diff_st.
  TYPES: ty_diff_tt TYPE STANDARD TABLE OF ty_diff_st WITH DEFAULT KEY.

ENDINTERFACE.