class Z2UI5_cl_HTTP_CLOUD definition
  public
  create public .

public section.

  interfaces IF_HTTP_SERVICE_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS Z2UI5_cl_HTTP_CLOUD IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.

    data(lt_header) = request->get_header_fields( ).
    data(lt_param)  = request->get_form_fields( ).
    data(lv_method) = request->get_method( ).
    DATA(lv_body)   = request->get_text( ).


    DATA(lv_resp) = SWITCH #( lv_method
        WHEN 'GET'  THEN z2ui5_cl_http_handler=>main_index_html( )
        WHEN 'POST' THEN z2ui5_cl_http_handler=>main_roundtrip( value #(
                t_header = lt_header
                t_param  = lt_param
                o_body   = zcl_2ui5_hlp_tree_json=>factory( lv_body )
         ) )
      ).

    response->set_status( 200 ).
    response->set_text( lv_resp ).

  ENDMETHOD.

ENDCLASS.
