*&---------------------------------------------------------------------*
*& Report  ZAGSSR_SAVE_SEC_NOTES_HIST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zagssr_save_sec_notes_hist.

data:
  it_sname type agsnote_system_id,
  it_stype type agsnote_system_type.

selection-screen begin of block param_selection.
  select-options: t_sname for it_sname.
  select-options: t_stype for it_stype.
selection-screen end of block param_selection.

start-of-selection.

  call method zcl_sys_rec_data_api=>save_notes_stat_snapshot(
    exporting
      it_systems    = t_sname[]
      it_system_types = t_stype[]
      ip_note_types = 'S' ).