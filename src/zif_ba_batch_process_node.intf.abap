interface zif_ba_batch_process_node
  public .

  types: begin of ty_start_params,
           strtimmed  type btch0000-char1,
           sdlstrtdt  type tbtcjob-sdlstrtdt,  "Start Date of Background Job
           sdlstrttm  type tbtcjob-sdlstrttm, "Time of Start Date of Background Job
           laststrtdt type tbtcjob-laststrtdt, "No Start After: Date
           laststrttm type tbtcjob-laststrttm, "No Start After: Time
         end  of ty_start_params.
  types:
    begin of ty_exe_rt_info,
      uname      type tbtcjob-authcknam,
      bdate      type sy-datum,
      btime      type sy-uzeit,
      edate      type sy-datum,
      etime      type sy-uzeit,
      rt_subrc   type char25,
      rt_message type bapiret2-message,
    end  of ty_exe_rt_info .
  types:
    begin of enum enum_exe_status,
      running,   "执行中
      finished,  "完成
      intercepted,  "中断
    end of enum enum_exe_status .

  data s_exe_rt_info type ty_exe_rt_info .
  data s_start_params type ty_start_params.

  methods start
    importing
      is_start_params type ty_start_params
    raising
      zcx_ba_batch_process .

  methods get_exe_status
    exporting
      es_exe_rt_info   type ty_exe_rt_info
    returning
      value(rv_status) type enum_exe_status
    raising
      zcx_ba_batch_process .
endinterface.
