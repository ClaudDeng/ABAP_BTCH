class zcl_ba_background_job definition
  public
  final
  create public .

  public section.

    interfaces zif_ba_batch_process_node .

    aliases s_exe_rt_info
      for zif_ba_batch_process_node~s_exe_rt_info .
    aliases s_start_params
      for zif_ba_batch_process_node~s_start_params .
    aliases get_exe_status
      for zif_ba_batch_process_node~get_exe_status .
    aliases start
      for zif_ba_batch_process_node~start .
    aliases enum_exe_status
      for zif_ba_batch_process_node~enum_exe_status .
    aliases ty_exe_rt_info
      for zif_ba_batch_process_node~ty_exe_rt_info .
    aliases ty_start_params
      for zif_ba_batch_process_node~ty_start_params .


    types:
      begin of ty_cycle_params,
        prddays   type tbtcjob-prddays,
        prdhours  type tbtcjob-prdhours,
        prdmins   type tbtcjob-prdmins,
        prdmonths type tbtcjob-prdmonths,
        prdweeks  type tbtcjob-prdweeks,
      end  of ty_cycle_params .
    types:
      begin of ty_pre_job_params,
        predjob_checkstat type tbtcstrt-checkstat,
        pred_jobcount     type tbtcjob-jobcount,
        pred_jobname      type tbtcjob-jobname,
      end  of ty_pre_job_params .
    types:
      begin of ty_exe_job_info,
        prog_name      type btcjob,
        variant        type  raldb-variant,
        wait_until_end type c length 1,
        exe_uname      type sy-uname,
        job_status     type tbtcjob-status,
        job_message    type bapiret2-message,
      end  of ty_exe_job_info .
    types: ty_t_exe_params type table of rsparamsl_255.

    data v_job_name type btcjob .
    data v_job_count type btcjobcnt .
    data s_pre_job_params type ty_pre_job_params .
    data s_cycle_params type ty_cycle_params .
    data s_exe_job_info type ty_exe_job_info .
    data t_job_log type table of tbtc5 .
    data t_exe_params type ty_t_exe_params .
    data o_check_class type ref to zif_ba_batch_process_check .
    data o_appl_log type ref to zcl_ba_appl_log.
    methods constructor
      importing
        !iv_job_name type btcjob
        io_appl_log  type ref to zcl_ba_appl_log.
    methods set_exe_prog_and_params
      importing
        !iv_prog_name      type btcjob
        !iv_variant        type raldb-variant
        !iv_wait_until_end type c default ''
        !iv_uname          type tbtcjob-authcknam default sy-uname
        !is_pre_job_params type ty_pre_job_params optional
        !is_cycle_params   type ty_cycle_params optional
        it_params_info     type ty_t_exe_params
        !io_check_class    type ref to zif_ba_batch_process_check optional
      raising
        zcx_ba_batch_process .
  protected section.
  private section.
    methods job_open
      raising
        zcx_ba_batch_process .
    methods job_close
      importing
        !is_start_params   type ty_start_params
        !is_pre_job_params type ty_pre_job_params
        !is_cycle_params   type ty_cycle_params
      raising
        zcx_ba_batch_process .
    methods job_submit
      raising
        zcx_ba_batch_process .
    methods job_status_read
      returning value(ev_status) type tbtcjob-status
      raising
                zcx_ba_batch_process .
    methods job_log_read
      raising
        zcx_ba_batch_process .
    methods print_execute_appl_log
      raising
        zcx_ba_batch_process .
endclass.



class zcl_ba_background_job implementation.


  method constructor.
    v_job_name =  iv_job_name.
    o_appl_log = io_appl_log.
  endmethod.


  method job_close.

    call function 'JOB_CLOSE'
      exporting
        jobcount             = v_job_count
        jobname              = v_job_name
        laststrtdt           = is_start_params-laststrtdt
        laststrttm           = is_start_params-laststrttm
        prddays              = is_cycle_params-prddays
        prdhours             = is_cycle_params-prdhours
        prdmins              = is_cycle_params-prdmins
        prdmonths            = is_cycle_params-prdmonths
        prdweeks             = is_cycle_params-prdweeks
        predjob_checkstat    = is_pre_job_params-predjob_checkstat
        pred_jobcount        = is_pre_job_params-pred_jobcount
        pred_jobname         = is_pre_job_params-pred_jobname
        sdlstrtdt            = is_start_params-sdlstrtdt
        sdlstrttm            = is_start_params-sdlstrttm
        strtimmed            = is_start_params-strtimmed
      exceptions
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        invalid_target       = 8
        invalid_time_zone    = 9
        others               = 10.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno into data(lv_message)
                    with sy-msgv1  sy-msgv2  sy-msgv3  sy-msgv4.
      raise exception type zcx_ba_batch_process
        exporting
          textid     = zcx_ba_batch_process=>error
          error_info = lv_message.
    endif.
  endmethod.


  method job_log_read.
    refresh t_job_log.
    call function 'BP_JOBLOG_READ'
      exporting
        client                = sy-mandt
        jobcount              = v_job_count
*       JOBLOG                = ' '
        jobname               = v_job_name
*       LINES                 =
*       DIRECTION             =
      tables
        joblogtbl             = t_job_log
      exceptions
        cant_read_joblog      = 1
        jobcount_missing      = 2
        joblog_does_not_exist = 3
        joblog_is_empty       = 4
        joblog_name_missing   = 5
        jobname_missing       = 6
        job_does_not_exist    = 7
        others                = 8.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno into data(lv_message)
                    with sy-msgv1  sy-msgv2  sy-msgv3  sy-msgv4.
      raise exception type zcx_ba_batch_process
        exporting
          textid     = zcx_ba_batch_process=>error
          error_info = lv_message.
    endif.

  endmethod.


  method job_open.
    call function 'JOB_OPEN'
      exporting
        jobname          = v_job_name
      importing
        jobcount         = v_job_count
      exceptions
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        others           = 4.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno into data(lv_message)
                    with sy-msgv1  sy-msgv2  sy-msgv3  sy-msgv4.
      raise exception type zcx_ba_batch_process
        exporting
          textid     = zcx_ba_batch_process=>error
          error_info = lv_message.
    endif.
  endmethod.


  method job_status_read.

*CONSTANTS:
*  btc_running       TYPE tbtco-status VALUE 'R',
*  btc_ready         TYPE tbtco-status VALUE 'Y',
*  btc_scheduled     TYPE tbtco-status VALUE 'P',
*  btc_intercepted   TYPE btcstatus VALUE btc_scheduled,
*  btc_released      TYPE tbtco-status VALUE 'S',
*  btc_aborted       TYPE tbtco-status VALUE 'A',
*  btc_finished      TYPE tbtco-status VALUE 'F',
*  btc_put_active    TYPE tbtco-status VALUE 'Z',
*  btc_unknown_state TYPE tbtco-status VALUE 'X'.

    call function 'BP_JOB_STATUS_GET'
      exporting
        jobcount                   = v_job_count
        jobname                    = v_job_name
        read_only_status           = 'X'
      importing
        status                     = ev_status
*       HAS_CHILD                  =
      exceptions
        job_doesnt_exist           = 1
        unknown_error              = 2
        parent_child_inconsistency = 3
        others                     = 4.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno into data(lv_message)
                    with sy-msgv1  sy-msgv2  sy-msgv3  sy-msgv4.
      raise exception type zcx_ba_batch_process
        exporting
          textid     = zcx_ba_batch_process=>error
          error_info = lv_message.
    endif.
  endmethod.


  method job_submit.
    submit s_exe_rt-prog_name
             using selection-set s_exe_job_info-variant
             with selection-table t_exe_params
             user  s_exe_job_info-exe_uname
             via job v_job_name number v_job_count
             and return.


*    catch zcx_ba_background_job. " ZCX_BA_BACKGROUND_JOB

*      call function 'JOB_SUBMIT'
*        exporting
**         ARCPARAMS               =
*          authcknam               = s_exe_info-job_uname
*          jobcount                = v_job_count
*          jobname                 = v_job_name
*          language                = sy-langu
**         PRIPARAMS               = ' '
*          report                  = s_exe_info-prog_name
*          variant                 = s_exe_info-variant
**     IMPORTING
**         STEP_NUMBER             =
*        exceptions
*          bad_priparams           = 1
*          bad_xpgflags            = 2
*          invalid_jobdata         = 3
*          jobname_missing         = 4
*          job_notex               = 5
*          job_submit_failed       = 6
*          lock_failed             = 7
*          program_missing         = 8
*          prog_abap_and_extpg_set = 9
*          others                  = 10.
*      if sy-subrc <> 0.
*        message id sy-msgid type sy-msgty number sy-msgno into lv_message
*                      with sy-msgv1  sy-msgv2  sy-msgv3  sy-msgv4.
*        raise exception type zcx_ba_background_job
*          exporting
*            textid     = zcx_ba_background_job=>error
*            error_info = lv_message.
*      endif.

  endmethod.


  method set_exe_prog_and_params.

    if iv_prog_name is initial.
      raise exception type zcx_ba_batch_process
        exporting
          textid     = zcx_ba_batch_process=>error
          error_info = '程序名必须输入。'.
    endif.
    s_exe_job_info-prog_name = iv_prog_name.
    s_exe_job_info-variant = iv_variant.
    s_exe_job_info-wait_until_end = iv_wait_until_end.
    s_exe_job_info-exe_uname = s_exe_rt_info-uname = iv_uname.
    s_pre_job_params = is_pre_job_params.
    s_cycle_params = is_cycle_params.
    o_check_class = io_check_class.
    t_exe_params = it_params_info.
  endmethod.


  method zif_ba_batch_process_node~get_exe_status.


    me->print_execute_appl_log( ).

    s_exe_job_info-job_status = me->job_status_read( ).
    me->job_log_read( ).
*  btc_running       TYPE tbtco-status VALUE 'R',
*  btc_ready         TYPE tbtco-status VALUE 'Y',
*  btc_scheduled     TYPE tbtco-status VALUE 'P',
*  btc_intercepted   TYPE btcstatus VALUE btc_scheduled,
*  btc_released      TYPE tbtco-status VALUE 'S',
*  btc_aborted       TYPE tbtco-status VALUE 'A',
*  btc_finished      TYPE tbtco-status VALUE 'F',
*  btc_put_active    TYPE tbtco-status VALUE 'Z',
*  btc_unknown_state TYPE tbtco-status VALUE 'X'.
    case s_exe_job_info-job_status .
      when 'F'.
        s_exe_job_info-job_message = '后台任务执行完成'.
        rv_status = zif_ba_batch_process_node=>finished.
        get time.
        s_exe_rt_info-edate = sy-datum.
        s_exe_rt_info-etime = sy-uzeit.
        if o_check_class is not initial.
          s_exe_rt_info-rt_subrc = o_check_class->check(
                 exporting
                   iv_date_from = s_exe_rt_info-bdate
                   iv_time_from = s_exe_rt_info-btime
                   iv_date_to   = s_exe_rt_info-edate
                   iv_time_to   = s_exe_rt_info-etime
                 importing
                   ev_mesg_txt = s_exe_rt_info-rt_message
                 ).
        endif.

      when 'R'.
        s_exe_job_info-job_message = '后台任务执行中'.
        rv_status = zif_ba_batch_process_node=>running.
      when others.
        s_exe_job_info-job_message = '后台任务执行中断'.
        rv_status = zif_ba_batch_process_node=>intercepted.
    endcase.

  endmethod.


  method zif_ba_batch_process_node~start.
    get time.
    s_exe_rt_info-bdate = sy-datum.
    s_exe_rt_info-btime = sy-uzeit.

    me->print_execute_appl_log( ).

    me->job_open( ).
    me->job_submit( ).
    me->job_close(
      exporting
        is_start_params   = is_start_params
        is_pre_job_params = s_pre_job_params
        is_cycle_params   = s_cycle_params
    ).


    if s_exe_job_info-wait_until_end is not initial.
      "While 的判断条件为True，执行while内的内容并循环。判断条件为False时，退出循环
      while me->get_exe_status(    ) = zif_ba_batch_process_node=>running.
        wait up to 1 seconds.
      endwhile.
    else.
      me->get_exe_status(    ).
    endif.
  endmethod.


  method print_execute_appl_log.

    data: lt_cstack_tab type sys_callst.
    field-symbols: <fs> type any.
    call function 'SYSTEM_CALLSTACK'
      importing
        et_callstack = lt_cstack_tab. " internal table
    if lines( lt_cstack_tab ) ge 2.
      message s000(00) with |执行作业( { v_job_name  })的{  lt_cstack_tab[ 2 ]-eventname  }方法。|.
      me->o_appl_log->add_log(     ).
*      catch zcx_ba_batch_process. " ZCX_BA_BACKGROUND_JOB
    endif.
* l_cstack_tab 里就是abap的调用堆栈
  endmethod.

endclass.
