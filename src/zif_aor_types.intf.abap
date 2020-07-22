INTERFACE zif_aor_types
  PUBLIC .

  TYPES:
    BEGIN OF ty_comment.
          INCLUDE TYPE zaor_comment.
          INCLUDE TYPE zaor_object.
  TYPES: time_formatted TYPE string,
         new_line       TYPE zaor_code_com-new_line,
         old_line       TYPE zaor_code_com-old_line,
         END OF ty_comment.

  TYPES: ty_vrso_tt TYPE STANDARD TABLE OF vrso WITH DEFAULT KEY.
  TYPES: ty_review_tt TYPE STANDARD TABLE OF zaor_review WITH DEFAULT KEY.
  TYPES: ty_comment_tt TYPE STANDARD TABLE OF ty_comment WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_transport.
          INCLUDE TYPE e070.
  TYPES: as4text TYPE e07t-as4text,
         END OF ty_transport.
  TYPES:
    ty_transport_tt TYPE STANDARD TABLE OF ty_transport WITH DEFAULT KEY .
  TYPES:
    ty_code_comment_tt TYPE HASHED TABLE OF zaor_code_com WITH UNIQUE KEY
      pgmid object obj_name new_line old_line.

  TYPES: BEGIN OF ty_diff_st,
           new   TYPE c LENGTH 6,
           old   TYPE c LENGTH 6,
           updkz TYPE c LENGTH 1,
           code  TYPE text255,
         END OF ty_diff_st.
  TYPES: ty_diff_tt TYPE STANDARD TABLE OF ty_diff_st WITH DEFAULT KEY.
  TYPES: BEGIN OF ty_enh_diff_st,
    type TYPE enhtooltype,
    id TYPE i,
    full_name TYPE string,
    diff TYPE ty_diff_tt,
  END OF ty_enh_diff_st.
  TYPES: ty_enh_diff_tt TYPE STANDARD TABLE OF ty_enh_diff_st
    WITH NON-UNIQUE KEY id.

  TYPES: BEGIN OF ty_ci_st,
           header  TYPE sciins_inf,
           results TYPE scit_alvlist,
           chkvinf TYPE scichkv_hd,
         END OF ty_ci_st.

  TYPES: BEGIN OF ty_diff_list_st,
           object            TYPE zaor_object,
           last_changed_date TYPE d,
           last_changed_time TYPE t,
           diff              TYPE ty_diff_tt,
           enhanced_object   TYPE zaor_object,
           enhancement_diff  TYPE ty_enh_diff_tt,
         END OF ty_diff_list_st.
  TYPES: ty_diff_list_tt TYPE STANDARD TABLE OF ty_diff_list_st WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_header.
          INCLUDE TYPE zaor_review.
  TYPES: as4text TYPE e07t-as4text,
         END OF ty_header.

ENDINTERFACE.
