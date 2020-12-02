class ZCX_BA_BATCH_PROCESS definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  constants ERROR type SOTR_CONC value '000C291565CB1EEB8CF91840FD141738' ##NO_TEXT.
  data ERROR_INFO type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !ERROR_INFO type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_BA_BATCH_PROCESS IMPLEMENTATION.


  method CONSTRUCTOR ##ADT_SUPPRESS_GENERATION.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->ERROR_INFO = ERROR_INFO .
  endmethod.
ENDCLASS.
