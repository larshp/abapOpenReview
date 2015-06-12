interface ZIF_AOR_TYPES
  public .

  types: TY_VRSO_TT type standard table of vrso with default key.
  types: TY_REVIEW_TT type standard table of zaor_review with default key .
  types: TY_comment_TT type standard table of zaor_comment with default key .
  types:
    begin of TY_TRANSPORT .
  include type e070.
  types: as4text type e07t-as4text,
    end of ty_transport .
  types:
    TY_TRANSPORT_tt type standard table of ty_transport with default key .

endinterface.