CLASS zcl_2ui5_http DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .
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

    data(lv_resp) = SWITCH #( lv_method
        WHEN 'GET'  THEN lcl_2ui5_backend=>main_index_html( lt_url_param )
        WHEN 'POST' THEN lcl_2ui5_backend=>main_roundtrip( lv_body )
      ).

    response->set_status( lv_status ).
    response->set_text( lv_resp ).

  ENDMETHOD.

ENDCLASS.
