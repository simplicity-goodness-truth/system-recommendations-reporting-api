class ZCL_ZSYSREC_API_DPC_EXT definition
  public
  inheriting from ZCL_ZSYSREC_API_DPC
  create public .

public section.

  types:
    tt_system_id type range of ags_sr_s_system-system_id .
  types:
    tt_status_selopt type range of agssr_status-status_id .
  types:
    tt_agssr_sysstath type standard table of zagssr_sysstath .
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
protected section.

  methods SECNOTESACTUALST_GET_ENTITYSET
    redefinition .
  methods SECNOTESDAYHIS01_GET_ENTITYSET
    redefinition .
  methods SECNOTESDAYHISTS_GET_ENTITYSET
    redefinition .
  methods SECNOTESWEEKHI01_GET_ENTITYSET
    redefinition .
  methods SECNOTESWEEKHIST_GET_ENTITYSET
    redefinition .
private section.

  class-methods FILL_STAT_ENTITY_SET
    importing
      !IT_AGSSR_SYSSTATH type TT_NOTES_SNAPSHOT
    exporting
      value(ET_ENTITYSET) type ZCL_ZSYSREC_API_MPC=>TT_SECNOTESACTUALSTATUS .
  class-methods GET_DEFAULT_STATUS_FILTERING
    returning
      value(ET_STATUSES) type TT_STATUS_SELOPT .
  class-methods GET_STATUS_TEXT
    importing
      !IP_STATUS type STRING
    returning
      value(EP_TEXT) type STRING .
  class-methods GET_FILTER_VALUE
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
      !IP_PROPERTY type STRING
    returning
      value(EP_VALUE) type STRING .
  class-methods GET_FILTER_SELECT_OPTIONS
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
      !IP_PROPERTY type STRING
    returning
      value(ET_SELECT_OPTIONS) type /IWBEP/T_COD_SELECT_OPTIONS .
  class-methods CONVERT_DATE_FORMAT
    importing
      !IP_DATE type SY-DATUM
    returning
      value(EP_RESULT) type CHAR20 .
ENDCLASS.



CLASS ZCL_ZSYSREC_API_DPC_EXT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ZSYSREC_API_DPC_EXT=>CONVERT_DATE_FORMAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_DATE                        TYPE        SY-DATUM
* | [<-()] EP_RESULT                      TYPE        CHAR20
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CONVERT_DATE_FORMAT.

    data: lv_year    type char4,
          lv_month   type char2,
          lv_day     type char2.

    lv_year = ip_date+0(4).
    lv_month = ip_date+4(2).
    lv_day  = ip_date+6(2).

    concatenate lv_day '.' lv_month '.' lv_year into ep_result.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ZSYSREC_API_DPC_EXT=>FILL_STAT_ENTITY_SET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_AGSSR_SYSSTATH              TYPE        TT_NOTES_SNAPSHOT
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZSYSREC_API_MPC=>TT_SECNOTESACTUALSTATUS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method fill_stat_entity_set.

    data:
      ls_entityset            like line of et_entityset,
      lv_week_start_date      type sy-datum,
      lv_week_start_date_char type char10,
      lv_week_end_date        type sy-datum,
      lv_week_end_date_char   type char10,
      lv_week_of_year         type kweek.


    loop at it_agssr_sysstath assigning field-symbol(<ls_zagssr_sysstath>).

      clear ls_entityset.

     ls_entityset-priority1notescount = <ls_zagssr_sysstath>-prio1_scount.
      ls_entityset-priority2notescount = <ls_zagssr_sysstath>-prio2_scount.
      ls_entityset-priority3notescount = <ls_zagssr_sysstath>-prio3_scount.
      ls_entityset-priority4notescount = <ls_zagssr_sysstath>-prio4_scount.
      ls_entityset-priority5notescount = <ls_zagssr_sysstath>-prio5_scount.
      ls_entityset-priority6notescount = <ls_zagssr_sysstath>-prio6_scount.
      ls_entityset-notestype = <ls_zagssr_sysstath>-note_type.
      ls_entityset-notesstatus = <ls_zagssr_sysstath>-note_status.
      ls_entityset-calendarweek = <ls_zagssr_sysstath>-calendar_week.
      ls_entityset-year = <ls_zagssr_sysstath>-calendar_year.
      ls_entityset-systemtype = <ls_zagssr_sysstath>-system_type.
      ls_entityset-systemname = <ls_zagssr_sysstath>-system_name.
      ls_entityset-date = convert_date_format( <ls_zagssr_sysstath>-record_date ).


      ls_entityset-allprioritiessum =
        <ls_zagssr_sysstath>-prio1_scount + <ls_zagssr_sysstath>-prio2_scount +
        <ls_zagssr_sysstath>-prio3_scount + <ls_zagssr_sysstath>-prio4_scount +
        <ls_zagssr_sysstath>-prio5_scount + <ls_zagssr_sysstath>-prio6_scount.

      " Adding calendar week dates

      concatenate <ls_zagssr_sysstath>-calendar_year <ls_zagssr_sysstath>-calendar_week    into lv_week_of_year.

      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week = lv_week_of_year
        importing
          date = lv_week_start_date.

      lv_week_end_date = lv_week_start_date + 7.

      lv_week_start_date_char = convert_date_format( lv_week_start_date ).
      lv_week_end_date_char = convert_date_format( lv_week_end_date ).

      concatenate lv_week_start_date_char '-' lv_week_end_date_char into  ls_entityset-calendarweekperiod separated by space.

      " Adding status text

      if <ls_zagssr_sysstath>-note_status is not initial.

        ls_entityset-notesstatustext = get_status_text( <ls_zagssr_sysstath>-note_status ).

      endif. " if <ls_zagssr_sysstath>-note_status is not initial

      append ls_entityset to et_entityset.

    endloop. " loop at it_zagssr_sysstath assigning field-symbol(<ls_zagssr_sysstath>)


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ZSYSREC_API_DPC_EXT=>GET_DEFAULT_STATUS_FILTERING
* +-------------------------------------------------------------------------------------------------+
* | [<-()] ET_STATUSES                    TYPE        TT_STATUS_SELOPT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_default_status_filtering.

    select
      stat_sign
      stat_option
      stat_low
      stat_high
    into table et_statuses from zsr_api_stat_flt.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ZSYSREC_API_DPC_EXT=>GET_FILTER_SELECT_OPTIONS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET
* | [--->] IP_PROPERTY                    TYPE        STRING
* | [<-()] ET_SELECT_OPTIONS              TYPE        /IWBEP/T_COD_SELECT_OPTIONS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_filter_select_options.

    data(it_filter_so) = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    if line_exists( it_filter_so[ property = ip_property ] ).

      et_select_options = it_filter_so[ property = ip_property ]-select_options.

    endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ZSYSREC_API_DPC_EXT=>GET_FILTER_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET
* | [--->] IP_PROPERTY                    TYPE        STRING
* | [<-()] EP_VALUE                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_filter_value.

    data  rg_filter_so     type /iwbep/t_cod_select_options.

    data(it_filter_so) = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    if line_exists( it_filter_so[ property = ip_property ] ).

      rg_filter_so = it_filter_so[ property = ip_property ]-select_options.
      loop at rg_filter_so assigning field-symbol(<rs_filter_so>).
        ep_value = <rs_filter_so>-low.
      endloop.

    endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ZSYSREC_API_DPC_EXT=>GET_STATUS_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_STATUS                      TYPE        STRING
* | [<-()] EP_TEXT                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_status_text.


    types:
      begin of ty_status,
        status type agsnote_status_id,
      end of ty_status.

    data:
      lt_status      type standard table of ty_status,
      lv_status_text type string.


    split ip_status at ',' into table lt_status.

    loop at lt_status assigning field-symbol(<ls_status>).

      clear lv_status_text.

      case <ls_status>-status.

        when 'SAPINITSTA'.
          lv_status_text = 'Начальный (требует анализа)'.

        when others.

          select single status_text into lv_status_text
             from agssr_status where status_id = <ls_status>-status and
            langu = 'RU'.

      endcase. " case <ls_status>-status



      if ( sy-subrc = 0 ) and ( lv_status_text is not INITIAL ).
        if ep_text is initial.
          ep_text = lv_status_text.
        else.
          concatenate ep_text lv_status_text into ep_text separated by ','.
        endif. " if ep_text is INITIAL
      endif. " if sy-subrc = 0

    endloop. " loop at lt_status assigning field-symbol(<ls_status>)

*    search ip_status for ','.
*
*    if sy-subrc <> 0.
*
*      case ip_status.
*
*        when 'SAPINITSTA'.
*
*          ep_text = 'Для анализа'.
*
*        when others.
*
*          select single status_text into ep_text
*            from agssr_status where status_id = ip_status and
*            langu = 'RU'.
*
*      endcase.
*
*    endif. " if sy-subrc <> 0


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZSYSREC_API_DPC_EXT->SECNOTESACTUALST_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZSYSREC_API_MPC=>TT_SECNOTESACTUALSTATUS
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method secnotesactualst_get_entityset.

    data:

      lt_zagssr_sysstath     type tt_notes_snapshot,
      lt_filter_system_name  type /iwbep/t_cod_select_options,
      lt_filter_notes_status type /iwbep/t_cod_select_options,
      lv_filter_notes_type   type agsnote_note_typs,
      lt_systems             type tt_system_id,
      wa_system              like line of lt_systems,
      lt_statuses            type tt_status_selopt,
      wa_status              like line of lt_statuses.

    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Getting filters
    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    lt_filter_system_name = get_filter_select_options( io_tech_request_context  = io_tech_request_context
                                            ip_property = 'SYSTEMNAME' ).

    lt_filter_notes_status = get_filter_select_options( io_tech_request_context  = io_tech_request_context
                                    ip_property = 'NOTESSTATUS' ).

    lv_filter_notes_type = get_filter_value( io_tech_request_context  = io_tech_request_context
                                        ip_property = 'NOTESTYPE' ).

    loop at lt_filter_system_name assigning field-symbol(<ls_filter_system_name>).

      wa_system-sign = <ls_filter_system_name>-sign.
      wa_system-option = <ls_filter_system_name>-option.
      wa_system-low = <ls_filter_system_name>-low.

      append wa_system to lt_systems.

    endloop. " loop at lt_filter_system_name assigning field-symbol(<ls_filter_system_name>)

    if lt_filter_notes_status is not initial.

      loop at lt_filter_notes_status assigning field-symbol(<ls_filter_notes_status>).

        wa_status-sign = <ls_filter_notes_status>-sign.
        wa_status-option = <ls_filter_notes_status>-option.
        wa_status-low = <ls_filter_notes_status>-low.

        append wa_status to lt_statuses.

      endloop. " loop at lt_filter_system_name assigning field-symbol(<ls_filter_system_name>)

    else.

      lt_statuses = get_default_status_filtering( ).

    endif. " if lt_filter_notes_status is not initial


    if lv_filter_notes_type is initial.
      lv_filter_notes_type = 'S'.
    endif.

    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Data processing
    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^


    call method zcl_sys_rec_data_api=>get_notes_stat_snapshot
      exporting
        it_systems        = lt_systems
        ip_note_types     = lv_filter_notes_type
        it_status         = lt_statuses
      importing
        et_notes_snapshot = lt_zagssr_sysstath.

    fill_stat_entity_set(
      exporting
        it_agssr_sysstath =  lt_zagssr_sysstath
      importing
        et_entityset = et_entityset ).





  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZSYSREC_API_DPC_EXT->SECNOTESDAYHIS01_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZSYSREC_API_MPC=>TT_SECNOTESDAYHISTSPLITBYSTATU
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method SECNOTESDAYHIS01_GET_ENTITYSET.

data:

      lt_zagssr_sysstath     type tt_notes_snapshot,
      lt_filter_system_name  type /iwbep/t_cod_select_options,
      lt_filter_notes_status type /iwbep/t_cod_select_options,
      lv_filter_notes_type   type agsnote_note_typs,
      lt_systems             type tt_system_id,
      wa_system              like line of lt_systems,
      lt_statuses            type tt_status_selopt,
      wa_status              like line of lt_statuses.


    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Getting filters
    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^


    lt_filter_system_name = get_filter_select_options( io_tech_request_context  = io_tech_request_context
                                            ip_property = 'SYSTEMNAME' ).

    lt_filter_notes_status = get_filter_select_options( io_tech_request_context  = io_tech_request_context
                                    ip_property = 'NOTESSTATUS' ).

    lv_filter_notes_type = get_filter_value( io_tech_request_context  = io_tech_request_context
                                        ip_property = 'NOTESTYPE' ).

    loop at lt_filter_system_name assigning field-symbol(<ls_filter_system_name>).

      wa_system-sign = <ls_filter_system_name>-sign.
      wa_system-option = <ls_filter_system_name>-option.
      wa_system-low = <ls_filter_system_name>-low.

      append wa_system to lt_systems.

    endloop. " loop at lt_filter_system_name assigning field-symbol(<ls_filter_system_name>)

    if lt_filter_notes_status is not initial.

      loop at lt_filter_notes_status assigning field-symbol(<ls_filter_notes_status>).

        wa_status-sign = <ls_filter_notes_status>-sign.
        wa_status-option = <ls_filter_notes_status>-option.
        wa_status-low = <ls_filter_notes_status>-low.

        append wa_status to lt_statuses.

      endloop. " loop at lt_filter_system_name assigning field-symbol(<ls_filter_system_name>)

    else.

      lt_statuses = get_default_status_filtering( ).

    endif. " if lt_filter_notes_status is not initial


    if lv_filter_notes_type is initial.
      lv_filter_notes_type = 'S'.
    endif.

    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Data processing
    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^


    call method zcl_sys_rec_data_api=>get_stat_snapshot_hist_by_day
      exporting
        it_systems        = lt_systems
        ip_note_types     = lv_filter_notes_type
        it_status         = lt_statuses
      importing
        et_notes_snapshot = lt_zagssr_sysstath.

    fill_stat_entity_set(
      exporting
        it_agssr_sysstath =  lt_zagssr_sysstath
      importing
        et_entityset = et_entityset ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZSYSREC_API_DPC_EXT->SECNOTESDAYHISTS_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZSYSREC_API_MPC=>TT_SECNOTESDAYHISTSUMBYALLSTAT
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method secnotesdayhists_get_entityset.

    data:

      lt_zagssr_sysstath     type tt_notes_snapshot,
      lt_zagssr_sysstath_sum type tt_notes_snapshot,
      lt_filter_system_name  type /iwbep/t_cod_select_options,
      lt_filter_notes_status type /iwbep/t_cod_select_options,
      lv_filter_notes_type   type agsnote_note_typs,
      lt_systems             type tt_system_id,
      wa_system              like line of lt_systems,
      lt_statuses            type tt_status_selopt,
      wa_status              like line of lt_statuses.


    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Getting filters
    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^


    lt_filter_system_name = get_filter_select_options( io_tech_request_context  = io_tech_request_context
                                            ip_property = 'SYSTEMNAME' ).

    lt_filter_notes_status = get_filter_select_options( io_tech_request_context  = io_tech_request_context
                                    ip_property = 'NOTESSTATUS' ).

    lv_filter_notes_type = get_filter_value( io_tech_request_context  = io_tech_request_context
                                        ip_property = 'NOTESTYPE' ).

    loop at lt_filter_system_name assigning field-symbol(<ls_filter_system_name>).

      wa_system-sign = <ls_filter_system_name>-sign.
      wa_system-option = <ls_filter_system_name>-option.
      wa_system-low = <ls_filter_system_name>-low.

      append wa_system to lt_systems.

    endloop. " loop at lt_filter_system_name assigning field-symbol(<ls_filter_system_name>)

    if lt_filter_notes_status is not initial.

      loop at lt_filter_notes_status assigning field-symbol(<ls_filter_notes_status>).

        wa_status-sign = <ls_filter_notes_status>-sign.
        wa_status-option = <ls_filter_notes_status>-option.
        wa_status-low = <ls_filter_notes_status>-low.

        append wa_status to lt_statuses.

      endloop. " loop at lt_filter_system_name assigning field-symbol(<ls_filter_system_name>)

    else.

      lt_statuses = get_default_status_filtering( ).

    endif. " if lt_filter_notes_status is not initial


    if lv_filter_notes_type is initial.
      lv_filter_notes_type = 'S'.
    endif.

    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Data processing
    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^


    call method zcl_sys_rec_data_api=>get_stat_snapshot_hist_by_day
      exporting
        it_systems        = lt_systems
        ip_note_types     = lv_filter_notes_type
        it_status         = lt_statuses
      importing
        et_notes_snapshot = lt_zagssr_sysstath.

    zcl_sys_rec_data_api=>sum_day_stat_by_all_statuses(
      exporting
         it_notes_snapshot = lt_zagssr_sysstath
      importing
        et_notes_snapshot =  lt_zagssr_sysstath_sum ).


    fill_stat_entity_set(
      exporting
        it_agssr_sysstath =  lt_zagssr_sysstath_sum
      importing
        et_entityset = et_entityset ).



  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZSYSREC_API_DPC_EXT->SECNOTESWEEKHI01_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZSYSREC_API_MPC=>TT_SECNOTESWEEKHISTSPLITBYSTAT
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method SECNOTESWEEKHI01_GET_ENTITYSET.

    data:
      lt_zagssr_sysstath     type tt_notes_snapshot,
      lt_filter_system_name  type /iwbep/t_cod_select_options,
      lt_filter_notes_status type /iwbep/t_cod_select_options,
      lv_filter_notes_type   type agsnote_note_typs,
      lt_systems             type tt_system_id,
      wa_system              like line of lt_systems,
      lt_statuses            type tt_status_selopt,
      wa_status              like line of lt_statuses.

    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Getting filters
    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    lt_filter_system_name = get_filter_select_options( io_tech_request_context  = io_tech_request_context
                                            ip_property = 'SYSTEMNAME' ).

    lt_filter_notes_status = get_filter_select_options( io_tech_request_context  = io_tech_request_context
                                    ip_property = 'NOTESSTATUS' ).

    lv_filter_notes_type = get_filter_value( io_tech_request_context  = io_tech_request_context
                                        ip_property = 'NOTESTYPE' ).

    loop at lt_filter_system_name assigning field-symbol(<ls_filter_system_name>).

      wa_system-sign = <ls_filter_system_name>-sign.
      wa_system-option = <ls_filter_system_name>-option.
      wa_system-low = <ls_filter_system_name>-low.

      append wa_system to lt_systems.

    endloop. " loop at lt_filter_system_name assigning field-symbol(<ls_filter_system_name>)

    if lt_filter_notes_status is not initial.

      loop at lt_filter_notes_status assigning field-symbol(<ls_filter_notes_status>).

        wa_status-sign = <ls_filter_notes_status>-sign.
        wa_status-option = <ls_filter_notes_status>-option.
        wa_status-low = <ls_filter_notes_status>-low.

        append wa_status to lt_statuses.

      endloop. " loop at lt_filter_system_name assigning field-symbol(<ls_filter_system_name>)

    else.

      lt_statuses = get_default_status_filtering( ).

    endif. " if lt_filter_notes_status is not initial

    if lv_filter_notes_type is initial.
      lv_filter_notes_type = 'S'.
    endif.

    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Data processing
    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    call method zcl_sys_rec_data_api=>get_stat_snapshot_hist_by_week
      exporting
        it_systems        = lt_systems
        ip_note_types     = lv_filter_notes_type
        it_status         = lt_statuses
      importing
        et_notes_snapshot = lt_zagssr_sysstath.


    fill_stat_entity_set(
      exporting
        it_agssr_sysstath =  lt_zagssr_sysstath
      importing
        et_entityset = et_entityset ).


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZSYSREC_API_DPC_EXT->SECNOTESWEEKHIST_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZSYSREC_API_MPC=>TT_SECNOTESWEEKHISTSUMBYALLSTA
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method secnotesweekhist_get_entityset.

    data:

      lt_zagssr_sysstath     type tt_notes_snapshot,
      lt_zagssr_sysstath_sum type tt_notes_snapshot,
      lt_filter_system_name  type /iwbep/t_cod_select_options,
      lt_filter_notes_status type /iwbep/t_cod_select_options,
      lv_filter_notes_type   type agsnote_note_typs,
      lt_systems             type tt_system_id,
      wa_system              like line of lt_systems,
      lt_statuses            type tt_status_selopt,
      wa_status              like line of lt_statuses.

    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Getting filters
    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    lt_filter_system_name = get_filter_select_options( io_tech_request_context  = io_tech_request_context
                                            ip_property = 'SYSTEMNAME' ).

    lt_filter_notes_status = get_filter_select_options( io_tech_request_context  = io_tech_request_context
                                    ip_property = 'NOTESSTATUS' ).

    lv_filter_notes_type = get_filter_value( io_tech_request_context  = io_tech_request_context
                                        ip_property = 'NOTESTYPE' ).

    loop at lt_filter_system_name assigning field-symbol(<ls_filter_system_name>).

      wa_system-sign = <ls_filter_system_name>-sign.
      wa_system-option = <ls_filter_system_name>-option.
      wa_system-low = <ls_filter_system_name>-low.

      append wa_system to lt_systems.

    endloop. " loop at lt_filter_system_name assigning field-symbol(<ls_filter_system_name>)

    if lt_filter_notes_status is not initial.

      loop at lt_filter_notes_status assigning field-symbol(<ls_filter_notes_status>).

        wa_status-sign = <ls_filter_notes_status>-sign.
        wa_status-option = <ls_filter_notes_status>-option.
        wa_status-low = <ls_filter_notes_status>-low.

        append wa_status to lt_statuses.

      endloop. " loop at lt_filter_system_name assigning field-symbol(<ls_filter_system_name>)

    else.

      lt_statuses = get_default_status_filtering( ).

    endif. " if lt_filter_notes_status is not initial

    if lv_filter_notes_type is initial.
      lv_filter_notes_type = 'S'.
    endif.

    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Data processing
    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    call method zcl_sys_rec_data_api=>get_stat_snapshot_hist_by_week
      exporting
        it_systems        = lt_systems
        ip_note_types     = lv_filter_notes_type
        it_status         = lt_statuses
      importing
        et_notes_snapshot = lt_zagssr_sysstath.

    zcl_sys_rec_data_api=>sum_week_stat_by_all_statuses(
       exporting
         it_notes_snapshot = lt_zagssr_sysstath
       importing
         et_notes_snapshot =  lt_zagssr_sysstath_sum ).

    fill_stat_entity_set(
      exporting
        it_agssr_sysstath =  lt_zagssr_sysstath_sum
      importing
        et_entityset = et_entityset ).

  endmethod.
ENDCLASS.