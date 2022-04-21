class zcl_sys_rec_data_api definition
  public
  final
  create public .

  public section.

    types:
      tt_system_id type range of ags_sr_s_system-system_id .
    types:
      tt_system_types type range of ttdtg_agssrsn-system_type .
    types:
      begin of ty_sys_stat,
        status   type agsnote_status_id,
        priority type agsnote_priority_id,
        count    type int4,
      end of ty_sys_stat .
    types:
      tt_sys_stat type standard table of ty_sys_stat .

    types:
      begin of ty_notes_snapshot,
        system_name   type  agsnote_system_name,
        system_type   type    agsnote_system_type,
        record_date   type  dats,
        record_time   type  tims,
        note_status   type  string,
        prio1_scount  type   int4,
        prio2_scount  type   int4,
        prio3_scount  type   int4,
        prio4_scount  type int4,
        prio5_scount  type   int4,
        prio6_scount  type   int4,
        calendar_week type  numc2,
        calendar_year type    numc4,
        note_type	    type agsnote_note_typs,
      end of ty_notes_snapshot .

types:
      tt_notes_snapshot type standard table of  ty_notes_snapshot .


*    types:
*      tt_notes_snapshot type standard table of  zagssr_sysstath .



    types:
      begin of ty_status,
        status type agsnote_status_id,
      end of ty_status .
    types:
      tt_status type standard table of ty_status .
    types:
      tt_status_selopt type range of agssr_status-status_id .

    class-methods sum_day_stat_by_all_statuses
      importing
        !it_notes_snapshot       type tt_notes_snapshot
      exporting
        value(et_notes_snapshot) type tt_notes_snapshot .
    class-methods sum_week_stat_by_all_statuses
      importing
        !it_notes_snapshot       type tt_notes_snapshot
      exporting
        value(et_notes_snapshot) type tt_notes_snapshot .
    class-methods get_notes_stat_snapshot
      importing
        !it_systems                 type tt_system_id
        !ip_note_types              type agsnote_note_typs
        !ip_explicit_type_selection type char1 optional
        !it_status                  type tt_status_selopt optional
      exporting
        !et_notes_snapshot          type tt_notes_snapshot .
    class-methods get_stat_snapshot_hist_by_day
      importing
        !it_systems                 type tt_system_id
        !ip_note_types              type agsnote_note_typs
        !ip_explicit_type_selection type char1 optional
        !it_status                  type tt_status_selopt optional
      exporting
        !et_notes_snapshot          type tt_notes_snapshot .
    class-methods get_stat_snapshot_hist_by_week
      importing
        !it_systems                 type tt_system_id
        !ip_note_types              type agsnote_note_typs
        !ip_explicit_type_selection type char1 optional
        !it_status                  type tt_status_selopt optional
      exporting
        !et_notes_snapshot          type tt_notes_snapshot .
    class-methods save_notes_stat_snapshot
      importing
        !it_systems                 type tt_system_id optional
        !ip_status                  type agsnote_status_id optional
        !ip_note_types              type agsnote_note_typs optional
        !ip_explicit_type_selection type char1 optional
        !it_system_types            type tt_system_types optional .
protected section.
private section.

  class-methods GET_ALL_SR_CONF_SYSTEMS
    importing
      !IT_SYSTEM_TYPES type TT_SYSTEM_TYPES optional
    returning
      value(ET_SYSTEMS) type TT_SYSTEM_ID .
ENDCLASS.



CLASS ZCL_SYS_REC_DATA_API IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SYS_REC_DATA_API=>GET_ALL_SR_CONF_SYSTEMS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SYSTEM_TYPES                TYPE        TT_SYSTEM_TYPES(optional)
* | [<-()] ET_SYSTEMS                     TYPE        TT_SYSTEM_ID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_all_sr_conf_systems.

    data:
      lt_agssr_confsys        type standard table of agssr_confsys,
      wa_system               like line of et_systems.

    select
      system_name
      system_type
    into corresponding fields of table lt_agssr_confsys
    from agssr_confsys.

    loop at lt_agssr_confsys assigning field-symbol(<ls_agssr_confsys>) where system_type in it_system_types.

      wa_system-sign = 'I'.
      wa_system-option = 'EQ'.
      concatenate <ls_agssr_confsys>-system_name '~' <ls_agssr_confsys>-system_type into wa_system-low.

      append wa_system to et_systems.

    endloop. "loop at lt_agssr_confsys ASSIGNING FIELD-SYMBOL(<ls_agssr_confsys>)


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SYS_REC_DATA_API=>GET_NOTES_STAT_SNAPSHOT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SYSTEMS                     TYPE        TT_SYSTEM_ID
* | [--->] IP_NOTE_TYPES                  TYPE        AGSNOTE_NOTE_TYPS
* | [--->] IP_EXPLICIT_TYPE_SELECTION     TYPE        CHAR1(optional)
* | [--->] IT_STATUS                      TYPE        TT_STATUS_SELOPT(optional)
* | [<---] ET_NOTES_SNAPSHOT              TYPE        TT_NOTES_SNAPSHOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_notes_stat_snapshot.

    data:
      lt_status_filter         type range of agsnote_status_id,
      lt_systems               type tt_system_id,
      lv_note_types_expression type string,
      lt_sys_stat              type tt_sys_stat,
      lv_system_id             type agsnote_system_id,
      lv_system_name           type agsnote_system_name,
      lv_current_year_and_week type scal-week,
      lv_current_week          type numc2,
      lv_current_year          type numc4,
      lv_system_type           type agsnote_system_type,
      wa_notes_snapshot        LIKE LINE OF et_notes_snapshot,

      lv_priority_field_name   type char20.

    data: lo_descr      type ref to cl_abap_tabledescr,
          lo_type       type ref to cl_abap_datadescr,
          lo_struct     type ref to cl_abap_structdescr,
          lt_components type cl_abap_structdescr=>component_table.

    field-symbols : <lv_value>     type any,
                    <ls_component> like line of lt_components.

    " Getting year and calendar week

    call function 'DATE_GET_WEEK'
      exporting
        date = sy-datum
      importing
        week = lv_current_year_and_week.

    lv_current_year = lv_current_year_and_week+0(4).
    lv_current_week = lv_current_year_and_week+4(2).


    if it_systems is not initial.

      lt_systems  = it_systems.

    else.

      lt_systems  = get_all_sr_conf_systems( ).

    endif. " if it_systems is not initial       .

    " Building expression to search notes by type

    if ip_explicit_type_selection <> 'X'.

      concatenate '%' ip_note_types '%' into lv_note_types_expression.

    endif. " if ip_explicit_type_selection <> 'X'

    " Preparing table of fields names

    lo_descr ?= cl_abap_typedescr=>describe_by_data( et_notes_snapshot ).
    lo_type = lo_descr->get_table_line_type( ).
    lo_struct ?= cl_abap_typedescr=>describe_by_name( lo_type->absolute_name ).
    lt_components = lo_struct->get_components( ).

    loop at lt_systems assigning field-symbol(<ls_system>).
      clear:
        lv_system_id,
        lv_system_name,
        lv_system_type,
        lt_sys_stat.

      lv_system_id = <ls_system>-low.
      lv_system_name = substring_before( val = lv_system_id sub = '~' ).
      lv_system_type = substring_after( val = lv_system_id sub = '~' ).

      if ( lv_system_name is initial ) and ( lv_system_type is initial ) .
        continue.
      endif.

      if ip_explicit_type_selection <> 'X'.

        select last_status priority count(*) into table lt_sys_stat  from ttdtg_agssrsn
          where
            system_name = lv_system_name and
            system_type = lv_system_type and
            "last_status in lt_status_filter and
            last_status in it_status and
            note_types like lv_note_types_expression
            group by last_status priority.
      else.

        select last_status priority count(*) into table lt_sys_stat  from ttdtg_agssrsn
          where
            system_name = lv_system_name and
            system_type = lv_system_type and
            last_status in it_status and
            note_types = ip_note_types
            group by last_status priority.

      endif. "  if ip_explicit_type_selection <> 'X'

      if sy-subrc <> 0.
        continue.
      endif.

      " Calculating sum and preparing result

      clear wa_notes_snapshot.

      wa_notes_snapshot-system_type = lv_system_type.
      wa_notes_snapshot-system_name = lv_system_name.
      wa_notes_snapshot-note_type = ip_note_types.
      wa_notes_snapshot-calendar_week = lv_current_week.
      wa_notes_snapshot-calendar_year = lv_current_year.
      wa_notes_snapshot-record_date = sy-datum.

      loop at lt_sys_stat assigning field-symbol(<ls_sys_stat>).

        clear lv_priority_field_name.

        concatenate 'PRIO' <ls_sys_stat>-priority  '_SCOUNT' into lv_priority_field_name.

        if <ls_component>  is assigned.
          unassign <ls_component> .
        endif.

        read table lt_components with table key name = lv_priority_field_name assigning <ls_component> .

        if <ls_component> is assigned.

          assign component <ls_component>-name of structure   wa_notes_snapshot to <lv_value>.

          if <lv_value> is assigned.

            if <lv_value> is initial.
              <lv_value> = <ls_sys_stat>-count.
            else.
              <lv_value> = <lv_value>  + <ls_sys_stat>-count.
            endif.


          endif. " if <lv_value> is assigned

        endif. " if <ls_component> is assigned

        unassign:
          <ls_component>,
          <lv_value>.

      endloop. " loop at lt_sys_stat assigning field-symbol(<ls_sys_stat>)

      append wa_notes_snapshot to et_notes_snapshot.

    endloop. " loop at lt_systems assigning field-symbol(<ls_system>)


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SYS_REC_DATA_API=>GET_STAT_SNAPSHOT_HIST_BY_DAY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SYSTEMS                     TYPE        TT_SYSTEM_ID
* | [--->] IP_NOTE_TYPES                  TYPE        AGSNOTE_NOTE_TYPS
* | [--->] IP_EXPLICIT_TYPE_SELECTION     TYPE        CHAR1(optional)
* | [--->] IT_STATUS                      TYPE        TT_STATUS_SELOPT(optional)
* | [<---] ET_NOTES_SNAPSHOT              TYPE        TT_NOTES_SNAPSHOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_STAT_SNAPSHOT_HIST_BY_DAY.


    data:
      lv_system_name           type agsnote_system_name,
      lv_system_type           type agsnote_system_type,
      lv_system_id             type agsnote_system_id,
      lv_note_types_expression type string,
      lt_notes_snapshot        type tt_notes_snapshot,
      wa_notes_snapshot        like LINE OF lt_notes_snapshot,
      lt_systems               type tt_system_id.

    if it_systems is not initial.

      lt_systems  = it_systems.

    else.

      lt_systems  = get_all_sr_conf_systems( ).

    endif. " if it_systems is not initial

    loop at lt_systems assigning field-symbol(<ls_system>).

      clear:
        lv_system_id,
        lv_system_name,
        lv_system_type,
        lt_notes_snapshot.

      lv_system_id = <ls_system>-low.
      lv_system_name = substring_before( val = lv_system_id sub = '~' ).
      lv_system_type = substring_after( val = lv_system_id sub = '~' ).

      " Building expression to search notes by type

      if ip_explicit_type_selection <> 'X'.

        concatenate '%' ip_note_types '%' into lv_note_types_expression.

      endif. " if ip_explicit_type_selection <> 'X'

      if ip_explicit_type_selection <> 'X'.

        select
          system_name
          system_type
          record_date
          record_time
          note_status
          prio1_scount
          prio2_scount
          prio3_scount
          prio4_scount
          prio5_scount
          prio6_scount
          calendar_week
          calendar_year
          note_type
       from zagssr_sysstath
          into corresponding fields of table lt_notes_snapshot
          where
            system_name = lv_system_name and
            system_type = lv_system_type and
            note_type like lv_note_types_expression and
            note_status in it_status.

      else.

        select
          system_name
          system_type
          record_date
          record_time
          note_status
          prio1_scount
          prio2_scount
          prio3_scount
          prio4_scount
          prio5_scount
          prio6_scount
          calendar_week
          calendar_year
          note_type
       from zagssr_sysstath
            into corresponding fields of table lt_notes_snapshot
            where
              system_name = lv_system_name and
              system_type = lv_system_type and
              note_type = lv_note_types_expression and
              note_status in it_status.

      endif. " if ip_explicit_type_selection <> 'X'

    " Removing possible weekly duplicates

    sort lt_notes_snapshot by  note_status  record_date DESCENDING record_time DESCENDING.

    delete adjacent duplicates from lt_notes_snapshot comparing record_date.

      loop at lt_notes_snapshot assigning field-symbol(<ls_notes_snapshot>).

        clear wa_notes_snapshot.

        wa_notes_snapshot = <ls_notes_snapshot>.

        append wa_notes_snapshot to et_notes_snapshot.

      endloop. " loop at lt_notes_snapshot assigning field-symbol(<ls_notes_snapshot>)

    endloop. " loop at lt_systems assigning field-symbol(<ls_system>)

    sort et_notes_snapshot by  record_date DESCENDING record_time DESCENDING.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SYS_REC_DATA_API=>GET_STAT_SNAPSHOT_HIST_BY_WEEK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SYSTEMS                     TYPE        TT_SYSTEM_ID
* | [--->] IP_NOTE_TYPES                  TYPE        AGSNOTE_NOTE_TYPS
* | [--->] IP_EXPLICIT_TYPE_SELECTION     TYPE        CHAR1(optional)
* | [--->] IT_STATUS                      TYPE        TT_STATUS_SELOPT(optional)
* | [<---] ET_NOTES_SNAPSHOT              TYPE        TT_NOTES_SNAPSHOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_stat_snapshot_hist_by_week.


    data:
      lv_system_name           type agsnote_system_name,
      lv_system_type           type agsnote_system_type,
      lv_system_id             type agsnote_system_id,
      lv_note_types_expression type string,
      lt_notes_snapshot        type tt_notes_snapshot,
      wa_notes_snapshot        like line of lt_notes_snapshot,
      lt_systems               type tt_system_id.

    if it_systems is not initial.

      lt_systems  = it_systems.

    else.

      lt_systems  = get_all_sr_conf_systems( ).

    endif. " if it_systems is not initial

    loop at lt_systems assigning field-symbol(<ls_system>).

      clear:
        lv_system_id,
        lv_system_name,
        lv_system_type,
        lt_notes_snapshot.

      lv_system_id = <ls_system>-low.
      lv_system_name = substring_before( val = lv_system_id sub = '~' ).
      lv_system_type = substring_after( val = lv_system_id sub = '~' ).

      " Building expression to search notes by type

      if ip_explicit_type_selection <> 'X'.

        concatenate '%' ip_note_types '%' into lv_note_types_expression.

      endif. " if ip_explicit_type_selection <> 'X'

      if ip_explicit_type_selection <> 'X'.

        select
          system_name
          system_type
          record_date
          record_time
          note_status
          prio1_scount
          prio2_scount
          prio3_scount
          prio4_scount
          prio5_scount
          prio6_scount
          calendar_week
          calendar_year
          note_type
       from zagssr_sysstath
          into corresponding fields of table lt_notes_snapshot
          where
            system_name = lv_system_name and
            system_type = lv_system_type and
            note_type like lv_note_types_expression and
            note_status in it_status.

      else.

        select
          system_name
          system_type
          record_date
          record_time
          note_status
          prio1_scount
          prio2_scount
          prio3_scount
          prio4_scount
          prio5_scount
          prio6_scount
          calendar_week
          calendar_year
          note_type
       from zagssr_sysstath
            into corresponding fields of table lt_notes_snapshot
            where
              system_name = lv_system_name and
              system_type = lv_system_type and
              note_type = lv_note_types_expression and
              note_status in it_status.

      endif. " if ip_explicit_type_selection <> 'X'

      " Removing possible weekly duplicates

      sort lt_notes_snapshot by  note_status  record_date descending record_time descending.
      delete adjacent duplicates from lt_notes_snapshot comparing note_status calendar_year calendar_week.

      loop at lt_notes_snapshot assigning field-symbol(<ls_notes_snapshot>).

        clear wa_notes_snapshot.

        wa_notes_snapshot = <ls_notes_snapshot>.

        append wa_notes_snapshot to et_notes_snapshot.

      endloop. " loop at lt_notes_snapshot assigning field-symbol(<ls_notes_snapshot>)

    endloop. " loop at lt_systems assigning field-symbol(<ls_system>)

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SYS_REC_DATA_API=>SAVE_NOTES_STAT_SNAPSHOT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SYSTEMS                     TYPE        TT_SYSTEM_ID(optional)
* | [--->] IP_STATUS                      TYPE        AGSNOTE_STATUS_ID(optional)
* | [--->] IP_NOTE_TYPES                  TYPE        AGSNOTE_NOTE_TYPS(optional)
* | [--->] IP_EXPLICIT_TYPE_SELECTION     TYPE        CHAR1(optional)
* | [--->] IT_SYSTEM_TYPES                TYPE        TT_SYSTEM_TYPES(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method save_notes_stat_snapshot.

    data:
      lt_sys_stat              type tt_sys_stat,
      lt_status                type tt_status,
      wa_status                type ty_status,
      wa_zagssr_sysstath       type zagssr_sysstath,
      lt_zagssr_sysstath       type standard table of zagssr_sysstath,
      lv_current_year_and_week type scal-week,
      lv_current_week          type numc2,
      lv_system_name           type agsnote_system_name,
      lv_system_type           type agsnote_system_type,
      lv_system_id             type agsnote_system_id,
      lv_current_year          type numc4,
      lv_priority_field_name   type char20,
      lv_note_types_expression type string,
      lt_status_filter         type range of agsnote_status_id,
      wa_status_filter         like line of lt_status_filter,
      lt_systems               type tt_system_id.

    " Data for dynamic structures

    data: lo_descr      type ref to cl_abap_tabledescr,
          lo_type       type ref to cl_abap_datadescr,
          lo_struct     type ref to cl_abap_structdescr,
          lt_components type cl_abap_structdescr=>component_table.

    field-symbols : <lv_value>     type any,
                    <ls_component> like line of lt_components,
                    <ls_sys_stat>  like line of lt_sys_stat.


    if ip_status is not initial.

      wa_status_filter-sign = 'I'.
      wa_status_filter-option = 'EQ'.
      wa_status_filter-low = ip_status.
      append wa_status_filter to lt_status_filter.

    endif. " if ip_status is not initial

    if it_systems is not initial.

      lt_systems  = it_systems.

    else.

      " Selecting all available systems by system type filter

      lt_systems  = get_all_sr_conf_systems( it_system_types ).


    endif. " if it_systems is not initial

    " Preparing table of fields names

    lo_descr ?= cl_abap_typedescr=>describe_by_data( lt_zagssr_sysstath ).
    lo_type = lo_descr->get_table_line_type( ).
    lo_struct ?= cl_abap_typedescr=>describe_by_name( lo_type->absolute_name ).
    lt_components = lo_struct->get_components( ).

    " Getting year and calendar week

    call function 'DATE_GET_WEEK'
      exporting
        date = sy-datum
      importing
        week = lv_current_year_and_week.

    lv_current_year = lv_current_year_and_week+0(4).
    lv_current_week = lv_current_year_and_week+4(2).

    " Building expression to search notes by type

    if ip_explicit_type_selection <> 'X'.

      concatenate '%' ip_note_types '%' into lv_note_types_expression.

    endif. " if ip_explicit_type_selection <> 'X'

    loop at lt_systems assigning field-symbol(<ls_system>).

      clear:
        lv_system_id,
        lv_system_name,
        lv_system_type,
        lt_sys_stat,
        lt_status.

      lv_system_id = <ls_system>-low.
      lv_system_name = substring_before( val = lv_system_id sub = '~' ).
      lv_system_type = substring_after( val = lv_system_id sub = '~' ).

      if ( lv_system_name is initial ) and ( lv_system_type is initial ) .
        continue.
      endif.

      if ip_explicit_type_selection <> 'X'.

        select last_status priority count(*) into table lt_sys_stat  from ttdtg_agssrsn
          where
            system_name = lv_system_name and
            system_type = lv_system_type and
            last_status in lt_status_filter and
            note_types like lv_note_types_expression
            group by last_status priority.
      else.

        select last_status priority count(*) into table lt_sys_stat  from ttdtg_agssrsn
          where
            system_name = lv_system_name and
            system_type = lv_system_type and
            last_status in lt_status_filter and
            note_types = ip_note_types
            group by last_status priority.

      endif. "  if ip_explicit_type_selection <> 'X'

      if sy-subrc <> 0.
        continue.
      endif.

      " Selecting all statuses for the system

      if <ls_sys_stat>  is assigned.
        unassign <ls_sys_stat> .
      endif.

      loop at lt_sys_stat assigning <ls_sys_stat>.

        clear wa_status.

        wa_status = <ls_sys_stat>-status.

        append wa_status to lt_status.

      endloop. " loop at lt_sys_stat assigning <ls_sys_stat>

      sort lt_status.
      delete adjacent duplicates from lt_status.

      if <ls_sys_stat>  is assigned.
        unassign <ls_sys_stat> .
      endif.

      loop at lt_status assigning field-symbol(<ls_status>).
        clear wa_zagssr_sysstath.

        " Adding informational fields

        wa_zagssr_sysstath-system_name = lv_system_name.
        wa_zagssr_sysstath-system_type = lv_system_type.
        wa_zagssr_sysstath-record_date = sy-datum.
        wa_zagssr_sysstath-record_time = sy-uzeit.
        wa_zagssr_sysstath-calendar_year =  lv_current_year.
        wa_zagssr_sysstath-calendar_week = lv_current_week.
        wa_zagssr_sysstath-note_type = ip_note_types.
        wa_zagssr_sysstath-note_status = <ls_status>-status.

        loop at lt_sys_stat assigning <ls_sys_stat> where status = <ls_status>-status.

          clear lv_priority_field_name.

          concatenate 'PRIO' <ls_sys_stat>-priority  '_SCOUNT' into lv_priority_field_name.

          if <ls_component>  is assigned.
            unassign <ls_component> .
          endif.

          read table lt_components with table key name = lv_priority_field_name assigning <ls_component> .

          if <ls_component> is assigned.

            assign component <ls_component>-name of structure  wa_zagssr_sysstath to <lv_value>.

            if <lv_value> is assigned.
              <lv_value> = <ls_sys_stat>-count.

            endif. " if <lv_value> is assigned

          endif. " if <ls_component> is assigned

          unassign:
            <ls_component>,
            <lv_value>.

        endloop. " loop at lt_sys_stat assigning field-symbol(<ls_sys_stat>)

        insert zagssr_sysstath from wa_zagssr_sysstath.

      endloop. " loop at lt_status ASSIGNING FIELD-SYMBOL(<ls_status>)

    endloop. "loop at lt_systems assigning field-symbol(<ls_system>)


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SYS_REC_DATA_API=>SUM_DAY_STAT_BY_ALL_STATUSES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_NOTES_SNAPSHOT              TYPE        TT_NOTES_SNAPSHOT
* | [<---] ET_NOTES_SNAPSHOT              TYPE        TT_NOTES_SNAPSHOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method sum_day_stat_by_all_statuses.

    types:
      begin of ty_systems_and_types,
        system_name type agsnote_system_name,
        system_type type agsnote_system_type,
      end of ty_systems_and_types.

    data:
      lt_systems_and_types     type standard table of ty_systems_and_types,
      wa_systems_and_types     type ty_systems_and_types,
      lv_calendar_week         type  numc2,
      wa_notes_snapshot        like line of et_notes_snapshot,
      lt_system_notes_snapshot type tt_notes_snapshot,
      wa_system_notes_snapshot like line of lt_system_notes_snapshot,
      lv_current_date         type  sy-datum.

    field-symbols:
      <ls_notes_snapshot>        like line of it_notes_snapshot,
      <ls_system_notes_snapshot> like line of lt_system_notes_snapshot.

    " Selecting all systems

    if <ls_notes_snapshot>  is assigned.
      unassign <ls_notes_snapshot>.
    endif.

    loop at it_notes_snapshot assigning <ls_notes_snapshot>.

      clear wa_systems_and_types.

      wa_systems_and_types-system_name = <ls_notes_snapshot>-system_name.
      wa_systems_and_types-system_type = <ls_notes_snapshot>-system_type.

      append wa_systems_and_types to lt_systems_and_types.

    endloop. " loop at it_notes_snapshot assigning <ls_notes_snapshot>

    sort lt_systems_and_types.
    delete adjacent duplicates from lt_systems_and_types.

    loop at lt_systems_and_types assigning field-symbol(<ls_systems_and_types>).

      if <ls_notes_snapshot>  is assigned.
        unassign <ls_notes_snapshot>.
      endif.

      clear:
       lt_system_notes_snapshot,
       lv_calendar_week,
       lv_current_date.

        loop at it_notes_snapshot assigning <ls_notes_snapshot>
                where system_type = <ls_systems_and_types>-system_type and
                system_name = <ls_systems_and_types>-system_name.

          clear wa_system_notes_snapshot.

          wa_system_notes_snapshot = <ls_notes_snapshot>.

          append wa_system_notes_snapshot to lt_system_notes_snapshot.


        endloop. " loop at it_notes_snapshot assigning <ls_notes_snapshot>

        if wa_notes_snapshot is not initial.

          append wa_notes_snapshot to et_notes_snapshot.

        endif.

        clear wa_notes_snapshot.

        wa_notes_snapshot-system_type = <ls_systems_and_types>-system_type.
        wa_notes_snapshot-system_name = <ls_systems_and_types>-system_name.
        wa_notes_snapshot-note_type = <ls_notes_snapshot>-note_type.

        loop at lt_system_notes_snapshot assigning <ls_system_notes_snapshot>.

          if ( lv_current_date is not initial ).

            if ( <ls_system_notes_snapshot>-record_date eq lv_current_date ).

              wa_notes_snapshot-prio1_scount = wa_notes_snapshot-prio1_scount + <ls_system_notes_snapshot>-prio1_scount.
              wa_notes_snapshot-prio2_scount = wa_notes_snapshot-prio2_scount + <ls_system_notes_snapshot>-prio2_scount.
              wa_notes_snapshot-prio3_scount = wa_notes_snapshot-prio3_scount + <ls_system_notes_snapshot>-prio3_scount.
              wa_notes_snapshot-prio4_scount = wa_notes_snapshot-prio4_scount + <ls_system_notes_snapshot>-prio4_scount.
              wa_notes_snapshot-prio5_scount = wa_notes_snapshot-prio5_scount + <ls_system_notes_snapshot>-prio5_scount.
              wa_notes_snapshot-prio6_scount = wa_notes_snapshot-prio6_scount + <ls_system_notes_snapshot>-prio6_scount.

              wa_notes_snapshot-calendar_week =  <ls_system_notes_snapshot>-calendar_week.
              wa_notes_snapshot-record_date =  <ls_system_notes_snapshot>-record_date.

              if wa_notes_snapshot-note_status is initial.
                wa_notes_snapshot-note_status = <ls_system_notes_snapshot>-note_status.
              else.
                concatenate wa_notes_snapshot-note_status <ls_system_notes_snapshot>-note_status into wa_notes_snapshot-note_status separated by ','.
              endif.

            else.

              " Starting new day cycle

              append wa_notes_snapshot to et_notes_snapshot.

              lv_current_date = <ls_system_notes_snapshot>-record_date.

              wa_notes_snapshot-calendar_week =  <ls_system_notes_snapshot>-calendar_week.
              wa_notes_snapshot-calendar_year =  <ls_system_notes_snapshot>-calendar_year.
              wa_notes_snapshot-record_date =  <ls_system_notes_snapshot>-record_date.
              wa_notes_snapshot-prio1_scount = <ls_system_notes_snapshot>-prio1_scount.
              wa_notes_snapshot-prio2_scount = <ls_system_notes_snapshot>-prio2_scount.
              wa_notes_snapshot-prio3_scount = <ls_system_notes_snapshot>-prio3_scount.
              wa_notes_snapshot-prio4_scount = <ls_system_notes_snapshot>-prio4_scount.
              wa_notes_snapshot-prio5_scount = <ls_system_notes_snapshot>-prio5_scount.
              wa_notes_snapshot-prio6_scount = <ls_system_notes_snapshot>-prio6_scount.
              wa_notes_snapshot-note_status = <ls_system_notes_snapshot>-note_status.

              wa_notes_snapshot-calendar_week =  <ls_system_notes_snapshot>-calendar_week.

            endif. " if ( <ls_system_notes_snapshot>-calendar_week eq lv_calendar_week )

          else.

            " Very first record

            wa_notes_snapshot-prio1_scount = <ls_system_notes_snapshot>-prio1_scount.
            wa_notes_snapshot-prio2_scount = <ls_system_notes_snapshot>-prio2_scount.
            wa_notes_snapshot-prio3_scount = <ls_system_notes_snapshot>-prio3_scount.
            wa_notes_snapshot-prio4_scount = <ls_system_notes_snapshot>-prio4_scount.
            wa_notes_snapshot-prio5_scount = <ls_system_notes_snapshot>-prio5_scount.
            wa_notes_snapshot-prio6_scount = <ls_system_notes_snapshot>-prio6_scount.

            wa_notes_snapshot-calendar_week =  <ls_system_notes_snapshot>-calendar_week.
            wa_notes_snapshot-calendar_year =  <ls_system_notes_snapshot>-calendar_year.
            wa_notes_snapshot-record_date =  <ls_system_notes_snapshot>-record_date.

            wa_notes_snapshot-note_status = <ls_system_notes_snapshot>-note_status.

            lv_current_date = <ls_system_notes_snapshot>-record_date.

          endif. "  if ( lv_calendar_week is not initial )

        endloop.  " loop at lt_system_notes_snapshot assigning <ls_system_notes_snapshot>


      endloop. " loop at lt_systems_and_types ASSIGNING FIELD-SYMBOL(<ls_systems_and_types>)

      if wa_notes_snapshot is not initial.

        append wa_notes_snapshot to et_notes_snapshot.

      endif.


    endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SYS_REC_DATA_API=>SUM_WEEK_STAT_BY_ALL_STATUSES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_NOTES_SNAPSHOT              TYPE        TT_NOTES_SNAPSHOT
* | [<---] ET_NOTES_SNAPSHOT              TYPE        TT_NOTES_SNAPSHOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method SUM_WEEK_STAT_BY_ALL_STATUSES.

    types:
      begin of ty_systems_and_types,
        system_name type agsnote_system_name,
        system_type type agsnote_system_type,
      end of ty_systems_and_types.

    data:
      lt_systems_and_types     type standard table of ty_systems_and_types,
      wa_systems_and_types     type ty_systems_and_types,
      lv_calendar_week         type  numc2,
      wa_notes_snapshot        like line of et_notes_snapshot,
      lt_system_notes_snapshot type tt_notes_snapshot,
      wa_system_notes_snapshot like line of lt_system_notes_snapshot.

    field-symbols:
      <ls_notes_snapshot>        like line of it_notes_snapshot,
      <ls_system_notes_snapshot> like line of lt_system_notes_snapshot.

    " Selecting all systems

    if <ls_notes_snapshot>  is assigned.
      unassign <ls_notes_snapshot>.
    endif.

    loop at it_notes_snapshot assigning <ls_notes_snapshot>.

      clear wa_systems_and_types.

      wa_systems_and_types-system_name = <ls_notes_snapshot>-system_name.
      wa_systems_and_types-system_type = <ls_notes_snapshot>-system_type.

      append wa_systems_and_types to lt_systems_and_types.

    endloop. " loop at it_notes_snapshot assigning <ls_notes_snapshot>

    sort lt_systems_and_types.
    delete adjacent duplicates from lt_systems_and_types.

    loop at lt_systems_and_types assigning field-symbol(<ls_systems_and_types>).

      if <ls_notes_snapshot>  is assigned.
        unassign <ls_notes_snapshot>.
      endif.

      clear:
       lt_system_notes_snapshot,
       lv_calendar_week.

      loop at it_notes_snapshot assigning <ls_notes_snapshot>
              where system_type = <ls_systems_and_types>-system_type and
              system_name = <ls_systems_and_types>-system_name.
        clear wa_system_notes_snapshot.

        wa_system_notes_snapshot = <ls_notes_snapshot>.

        append wa_system_notes_snapshot to lt_system_notes_snapshot.


      endloop.

      sort lt_system_notes_snapshot by calendar_year calendar_week.

      if wa_notes_snapshot is not initial.

        append wa_notes_snapshot to et_notes_snapshot.

      endif.

      clear wa_notes_snapshot.


      wa_notes_snapshot-system_type = <ls_systems_and_types>-system_type.
      wa_notes_snapshot-system_name = <ls_systems_and_types>-system_name.
      wa_notes_snapshot-note_type = <ls_notes_snapshot>-note_type.

      loop at lt_system_notes_snapshot assigning <ls_system_notes_snapshot>.

        if ( lv_calendar_week is not initial ).

          if ( <ls_system_notes_snapshot>-calendar_week eq lv_calendar_week ).

            wa_notes_snapshot-prio1_scount = wa_notes_snapshot-prio1_scount + <ls_system_notes_snapshot>-prio1_scount.
            wa_notes_snapshot-prio2_scount = wa_notes_snapshot-prio2_scount + <ls_system_notes_snapshot>-prio2_scount.
            wa_notes_snapshot-prio3_scount = wa_notes_snapshot-prio3_scount + <ls_system_notes_snapshot>-prio3_scount.
            wa_notes_snapshot-prio4_scount = wa_notes_snapshot-prio4_scount + <ls_system_notes_snapshot>-prio4_scount.
            wa_notes_snapshot-prio5_scount = wa_notes_snapshot-prio5_scount + <ls_system_notes_snapshot>-prio5_scount.
            wa_notes_snapshot-prio6_scount = wa_notes_snapshot-prio6_scount + <ls_system_notes_snapshot>-prio6_scount.

            wa_notes_snapshot-calendar_week =  <ls_system_notes_snapshot>-calendar_week.

            wa_notes_snapshot-record_date =  <ls_system_notes_snapshot>-record_date.

            if wa_notes_snapshot-note_status is initial.
              wa_notes_snapshot-note_status = <ls_system_notes_snapshot>-note_status.
            else.
              CONCATENATE wa_notes_snapshot-note_status <ls_system_notes_snapshot>-note_status INTO wa_notes_snapshot-note_status SEPARATED BY ','.
            endif.

          else.

            wa_notes_snapshot-calendar_week =  lv_calendar_week.

            append wa_notes_snapshot to et_notes_snapshot.

            lv_calendar_week = <ls_system_notes_snapshot>-calendar_week.

            wa_notes_snapshot-calendar_week =  <ls_system_notes_snapshot>-calendar_week.
            wa_notes_snapshot-calendar_year =  <ls_system_notes_snapshot>-calendar_year.
            wa_notes_snapshot-record_date =  <ls_system_notes_snapshot>-record_date.

            wa_notes_snapshot-prio1_scount = <ls_system_notes_snapshot>-prio1_scount.
            wa_notes_snapshot-prio2_scount = <ls_system_notes_snapshot>-prio2_scount.
            wa_notes_snapshot-prio3_scount = <ls_system_notes_snapshot>-prio3_scount.
            wa_notes_snapshot-prio4_scount = <ls_system_notes_snapshot>-prio4_scount.
            wa_notes_snapshot-prio5_scount = <ls_system_notes_snapshot>-prio5_scount.
            wa_notes_snapshot-prio6_scount = <ls_system_notes_snapshot>-prio6_scount.

            wa_notes_snapshot-note_status = <ls_system_notes_snapshot>-note_status.


          endif. " if ( <ls_system_notes_snapshot>-calendar_week eq lv_calendar_week )

        else.

          wa_notes_snapshot-prio1_scount = <ls_system_notes_snapshot>-prio1_scount.
          wa_notes_snapshot-prio2_scount = <ls_system_notes_snapshot>-prio2_scount.
          wa_notes_snapshot-prio3_scount = <ls_system_notes_snapshot>-prio3_scount.
          wa_notes_snapshot-prio4_scount = <ls_system_notes_snapshot>-prio4_scount.
          wa_notes_snapshot-prio5_scount = <ls_system_notes_snapshot>-prio5_scount.
          wa_notes_snapshot-prio6_scount = <ls_system_notes_snapshot>-prio6_scount.

          wa_notes_snapshot-calendar_week =  <ls_system_notes_snapshot>-calendar_week.
          wa_notes_snapshot-calendar_year =  <ls_system_notes_snapshot>-calendar_year.
          wa_notes_snapshot-record_date =  <ls_system_notes_snapshot>-record_date.

          wa_notes_snapshot-note_status = <ls_system_notes_snapshot>-note_status.

          lv_calendar_week = <ls_system_notes_snapshot>-calendar_week.

        endif. "  if ( lv_calendar_week is not initial )

      endloop.  " loop at lt_system_notes_snapshot assigning <ls_system_notes_snapshot>


    endloop. " loop at lt_systems_and_types ASSIGNING FIELD-SYMBOL(<ls_systems_and_types>)

    if wa_notes_snapshot is not initial.

      append wa_notes_snapshot to et_notes_snapshot.

    endif.


  endmethod.
ENDCLASS.