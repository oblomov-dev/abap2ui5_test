CLASS zztest99_cl_001 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS test.
ENDCLASS.



CLASS ZZTEST99_CL_001 IMPLEMENTATION.


  METHOD test.

*    DATA x TYPE REF TO cx_sy_itab_line_not_found.
    RAISE EXCEPTION hlp=>x_factory( ).

  ENDMETHOD.
ENDCLASS.
