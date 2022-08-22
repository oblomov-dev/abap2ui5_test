*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class hlx DEFINITION INHERITING FROM cx_no_check.
endclass.

CLASS hlp DEFINITION.


  PUBLIC SECTION.

    CLASS-METHODS x_factory
      RETURNING VALUE(result) TYPE REF TO cx_no_check.
    "cx_demo_t100_d

ENDCLASS.

CLASS hlp IMPLEMENTATION.

  METHOD x_factory.

  ENDMETHOD.

ENDCLASS.
