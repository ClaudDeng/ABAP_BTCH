interface zif_ba_batch_process_check
  public .



  methods check
    importing
      iv_date_from    type sy-datum
      iv_time_from    type sy-uzeit
      iv_date_to      type sy-datum
      iv_time_to      type sy-uzeit
    exporting
      ev_mesg_txt     type bapiret2-message
    returning
      value(ev_subrc) type char25.

endinterface.
