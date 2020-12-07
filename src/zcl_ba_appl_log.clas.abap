class zcl_ba_appl_log definition
  public
  final
  create public .

  public section.
    interfaces zif_ba_batch_process_check .

    data: v_appl_obj    type balhdr-object,
          v_appl_subobj type balhdr-subobject,
          v_user_id     type sy-uname,
          v_ext_number  type balhdr-extnumber,
          v_log_handle  type balloghndl,
          v_read_only   type char1.
    data s_log_head type bal_s_log.
    data t_header_data type table of balhdr.
    data t_header_parameters type table of balhdrp.
    data t_messages type table of balm.
    data t_message_parameters type table of balmp.
    data t_contexts type table of balc.
    data t_exceptions type table of bal_s_exception.
    constants c_cust_table type baltabname value 'ZSBA_APPL_LOG_CUST'.

    methods constructor
      importing
        iv_appl_obj    type  balhdr-object
        iv_appl_subobj type  balhdr-subobject
        iv_user_id     type  sy-uname default sy-uname
        iv_ext_number  type balhdr-extnumber
        iv_read_only   type c
      raising
        zcx_ba_batch_process .
    methods add_log
      importing
        is_message       type bapiret2 optional
        iv_problem_class type balprobcl optional
      raising
        zcx_ba_batch_process .
    methods display_log
      raising
        zcx_ba_batch_process .
    methods save_log
      raising
        zcx_ba_batch_process .
  protected section.
  private section.
endclass.



class zcl_ba_appl_log implementation.


  method add_log.
    data: lv_cust_context type zsba_appl_log_cust,
          lv_timestamp    type timestamp.
    data: ls_log_msg type bal_s_msg.
    if is_message is not initial.
      message id is_message-id type is_message-type number is_message-number into data(lv_message)
           with is_message-message_v1  is_message-message_v2  is_message-message_v3  is_message-message_v4.
    else.
      message id sy-msgid type sy-msgty number sy-msgno into lv_message
         with sy-msgv1  sy-msgv2  sy-msgv3  sy-msgv4.
    endif.

    if v_log_handle is initial.
      raise exception type zcx_ba_batch_process
        exporting
          textid     = zcx_ba_batch_process=>error
          error_info = '应用日志未初始化，无法增加日志。'.
    endif.

    ls_log_msg-msgid = sy-msgid.
    ls_log_msg-msgty = sy-msgty.
    ls_log_msg-msgno =  sy-msgno.
    ls_log_msg-msgv1 = sy-msgv1.
    ls_log_msg-msgv2 = sy-msgv2.
    ls_log_msg-msgv3 = sy-msgv3.
    ls_log_msg-msgv4 = sy-msgv4.
    ls_log_msg-probclass = iv_problem_class.


    get time stamp field lv_timestamp.

    lv_cust_context-exe_date = sy-datum.
    lv_cust_context-exe_time = sy-uzeit.
    lv_cust_context-exe_timestamp  = lv_timestamp.

    ls_log_msg-context-tabname = c_cust_table.
    ls_log_msg-context-value   = lv_cust_context.

    call function 'BAL_LOG_MSG_ADD'
      exporting
        i_log_handle     = v_log_handle
        i_s_msg          = ls_log_msg
*     IMPORTING
*       E_S_MSG_HANDLE   =
*       E_MSG_WAS_LOGGED =
*       E_MSG_WAS_DISPLAYED       =
      exceptions
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        others           = 4.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno into lv_message
                    with sy-msgv1  sy-msgv2  sy-msgv3  sy-msgv4.
      raise exception type zcx_ba_batch_process
        exporting
          textid     = zcx_ba_batch_process=>error
          error_info = lv_message.
    endif.
  endmethod.


  method constructor.
    s_log_head-object = v_appl_obj = iv_appl_obj.
    s_log_head-subobject =  v_appl_subobj = iv_appl_subobj.
    s_log_head-aluser =  v_user_id = iv_user_id.
    s_log_head-extnumber = v_ext_number = iv_ext_number.
    v_read_only = iv_read_only.

    if v_read_only is initial.
      call function 'BAL_LOG_CREATE'
        exporting
          i_s_log                 = s_log_head
        importing
          e_log_handle            = v_log_handle
        exceptions
          log_header_inconsistent = 1
          others                  = 2.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno into data(lv_message)
                      with sy-msgv1  sy-msgv2  sy-msgv3  sy-msgv4.
        raise exception type zcx_ba_batch_process
          exporting
            textid     = zcx_ba_batch_process=>error
            error_info = lv_message.
      endif.

    endif.






  endmethod.


  method display_log.
    data:
      ls_display_profile type bal_s_prof,
      ls_fcat            type bal_s_fcat.
    if v_log_handle is initial.
      raise exception type zcx_ba_batch_process
        exporting
          textid     = zcx_ba_batch_process=>error
          error_info = '应用日志未初始化，无法显示日志。'.
    endif.

    me->save_log( ).
*   catch zcx_ba_batch_process. " ZCX_BA_BACKGROUND_JOB
* get standard display profile
    call function 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      importing
        e_s_display_profile = ls_display_profile
      exceptions
        others              = 1.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno into data(lv_message)
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      raise exception type zcx_ba_batch_process
        exporting
          textid     = zcx_ba_batch_process=>error
          error_info = lv_message.
    endif.
* add passenger ID to message table
    ls_fcat-ref_table = c_cust_table.
    ls_fcat-ref_field = 'EXE_DATE'.
    ls_fcat-col_pos   = 100.
    ls_fcat-ref_field = 'EXE_TIME'.
    ls_fcat-col_pos   = 101.
    ls_fcat-ref_field = 'EXE_TIMESTAMP'.
    ls_fcat-col_pos   = 102.

    append ls_fcat to ls_display_profile-mess_fcat.
    clear ls_fcat.

* for display variants add report id
    ls_display_profile-disvariant-report = sy-repid.
* when you use also other ALV lists in your report,
* please specify a handle to distinguish between the display
* variants of these different lists, e.g:
    ls_display_profile-disvariant-handle = 'LOG'.

* show log file with modified output profile
* - we specify the display profile since we created our own
* - we do not specify any filter (like I_S_LOG_FILTER, ...,
*   I_T_MSG_HANDLE) since we want to display all messages available

    data:  lt_log_handle type bal_t_logh.

    append me->v_log_handle to lt_log_handle.

    call function 'BAL_DSP_LOG_DISPLAY'
      exporting
*       I_S_LOG_FILTER     =
*       I_T_LOG_CONTEXT_FILTER =
*       I_S_MSG_FILTER     =
*       I_T_MSG_CONTEXT_FILTER =
        i_t_log_handle     = lt_log_handle
*       I_T_MSG_HANDLE     =
        is_display_profile = ls_display_profile
      exceptions
        others             = 1.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno into lv_message
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      raise exception type zcx_ba_batch_process
        exporting
          textid     = zcx_ba_batch_process=>error
          error_info = lv_message.
    endif.
  endmethod.


  method save_log.
    data:  lt_log_handle type bal_t_logh.

    if v_log_handle is initial.
      raise exception type zcx_ba_batch_process
        exporting
          textid     = zcx_ba_batch_process=>error
          error_info = '应用日志未初始化，无法保存日志。'.
    endif.
    append me->v_log_handle to lt_log_handle.
    call function 'BAL_DB_SAVE'
      exporting
        i_client         = sy-mandt
        i_in_update_task = 'X '
        i_save_all       = 'X'
        i_t_log_handle   = lt_log_handle
*       I_2TH_CONNECTION = ' '
*       I_2TH_CONNECT_COMMIT       = ' '
*       I_LINK2JOB       = 'X'
*     IMPORTING
*       E_NEW_LOGNUMBERS =
*       E_SECOND_CONNECTION        =
      exceptions
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        others           = 4.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno into data(lv_message)
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      raise exception type zcx_ba_batch_process
        exporting
          textid     = zcx_ba_batch_process=>error
          error_info = lv_message.
    endif.

  endmethod.


  method zif_ba_batch_process_check~check.
    data: ls_message type  balm.
    call function 'APPL_LOG_READ_DB'
      exporting
        object             = v_appl_obj
        subobject          = v_appl_subobj
        external_number    = v_ext_number
        date_from          = iv_date_from
        date_to            = iv_date_to
        time_from          = iv_time_from
        time_to            = iv_time_to
*       LOG_CLASS          = '4'
*       PROGRAM_NAME       = '*'
*       TRANSACTION_CODE   = '*'
        user_id            = v_user_id
*       MODE               = '+'
*       PUT_INTO_MEMORY    = ' '
*     IMPORTING
*       NUMBER_OF_LOGS     =
      tables
        header_data        = t_header_data
        header_parameters  = t_header_parameters
        messages           = t_messages
        message_parameters = t_message_parameters
        contexts           = t_contexts
        t_exceptions       = t_exceptions.

    if line_exists( t_messages[ msgty = 'E' ] )
        or line_exists( t_messages[ msgty = 'A' ] )  .
      ls_message = t_messages[ msgty = 'E' ] .
      if ls_message is initial .
        ls_message = t_messages[ msgty = 'A' ] .
      endif.
      message id ls_message-msgid type ls_message-msgty number ls_message-msgno into data(lv_message)
            with ls_message-msgv1  ls_message-msgv2  ls_message-msgv3  ls_message-msgv4.
      ev_subrc = 'E'.
      ev_mesg_txt = |任务执行失败。{  lv_message } |.
    else.
      ev_subrc = 'S'.
      ev_mesg_txt = '任务执行成功。'.
    endif.

  endmethod.
endclass.
