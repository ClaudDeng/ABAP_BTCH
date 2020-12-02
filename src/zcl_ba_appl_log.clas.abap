class zcl_ba_appl_log definition
  public
  final
  create public .

  public section.
    interfaces zif_ba_batch_process_check .

    data: v_appl_obj    type balhdr-object,
          v_appl_subobj type balhdr-subobject,
          v_user_id     type sy-uname.
    data t_header_data type table of balhdr.
    data t_header_parameters type table of balhdrp.
    data t_messages type table of balm.
    data t_message_parameters type table of balmp.
    data t_contexts type table of balc.
    data t_exceptions type table of bal_s_exception.

    methods constructor
      importing
        iv_appl_obj    type  balhdr-object
        iv_appl_subobj type  balhdr-subobject
        iv_user_id     type  sy-uname default sy-uname .
  protected section.
  private section.
endclass.



class zcl_ba_appl_log implementation.


  method constructor.
    v_appl_obj = iv_appl_obj.
    v_appl_subobj = iv_appl_subobj.
    v_user_id = iv_user_id.
  endmethod.


  method zif_ba_batch_process_check~check.
    data: ls_message type  balm.
    call function 'APPL_LOG_READ_DB'
      exporting
        object             = v_appl_obj
        subobject          = v_appl_subobj
*       EXTERNAL_NUMBER    = ' '
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
