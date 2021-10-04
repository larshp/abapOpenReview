"Name: \FU:TR_RELEASE_REQUEST\SE:END\EI
ENHANCEMENT 0 ZAOR_REVIEW_CLOSING.
  IF lv_subrc = 0.
    UPDATE zaor_review SET status = zif_aor_constants=>c_status-closed
      WHERE trkorr = iv_trkorr.
  ENDIF.
ENDENHANCEMENT.
