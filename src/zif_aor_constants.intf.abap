INTERFACE zif_aor_constants
  PUBLIC.

  CONSTANTS:
    BEGIN OF c_status,
      open   TYPE zaor_status VALUE 'O' ##NO_TEXT,
      closed TYPE zaor_status VALUE 'C' ##NO_TEXT,
    END OF c_status.

  CONSTANTS:
    BEGIN OF c_base,
      transport TYPE zaor_base VALUE 'T',
      developer TYPE zaor_base VALUE 'D',
      object    TYPE zaor_base VALUE 'O',
    END OF c_base.

  CONSTANTS:
    BEGIN OF c_ci_filter,
      none    TYPE zaor_ci_filter VALUE 'N',
      object  TYPE zaor_ci_filter VALUE 'O',
      include TYPE zaor_ci_filter VALUE 'I',
      lines   TYPE zaor_ci_filter VALUE 'L',
    END OF c_ci_filter.

ENDINTERFACE.