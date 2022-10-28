CLASS zcl_2ui5_http DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_2ui5_http IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.

    DATA(lv_method) = request->get_method( ).
    DATA(lv_body)   = request->get_text(  ).
    DATA(lt_url_param) = VALUE if_web_http_request=>name_value_pairs(
         FOR row IN request->get_form_fields( ) (
                    name  = to_upper( row-name )
                    value = to_upper( row-value )
     ) ).

    DATA(lv_status) = 200.

    DATA(lv_resp) = SWITCH #( lv_method
        WHEN 'GET'  THEN lcl_2ui5_backend=>main_index_html( lt_url_param )
        WHEN 'POST' THEN lcl_2ui5_backend=>main_roundtrip( lv_body )
      ).

    response->set_status( lv_status ).
    response->set_text( lv_resp ).

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    FINAL(test) = '1243'.

    DATA(lo_test) = NEW lcl_test( ).
    DATA(lr_assign) = REF #(  lo_test->mv_test2 ).

    DATA(lo_descr) = CAST cl_abap_classdescr( cl_abap_objectdescr=>describe_by_object_ref(
         p_object_ref         = lo_test
         ) ).

    DATA(lt_attri) = lo_descr->attributes.

    LOOP AT lt_attri REFERENCE INTO DATA(lr_attri)
    WHERE visibility = cl_abap_classdescr=>public.
      DATA(lv_name) = lr_attri->name.
      DATA(lr_assign_gen) = REF #( lo_test->(lr_attri->name) ).
      EXIT.
    ENDLOOP.

    IF lr_assign = lr_assign_gen.
        data(lv_name_save) = lv_name.
    ENDIF.

   data(lo_descr_ref) = cl_abap_refdescr=>describe_by_data_ref(
      EXPORTING
        p_data_ref           = lr_assign
*      RECEIVING
*        p_descr_ref          =
*      EXCEPTIONS
*        reference_is_initial = 1
*        others               = 2
    ).
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


  ENDMETHOD.

ENDCLASS.
