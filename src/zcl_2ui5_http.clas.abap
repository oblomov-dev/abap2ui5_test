class ZCL_2UI5_HTTP definition
  public
  create public .

public section.

  interfaces IF_HTTP_SERVICE_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_2UI5_HTTP IMPLEMENTATION.


  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.

    DATA(lv_method) = request->get_method( ).
    DATA(lv_body)   = request->get_text(  ).
    DATA(lt_fields) = VALUE if_web_http_request=>name_value_pairs(
         FOR row IN request->get_form_fields( ) (
                    name  = to_upper( row-name )
                    value = to_upper( row-value )
     ) ).

    DATA(lv_resp) = ``.
    DATA(lv_status) = 200.

    CASE lv_method.
      WHEN 'GET'.
        lv_resp = NEW lcl_cntrl_frontend_app( )->load( ).
      WHEN 'POST'.
        lv_resp = lcl_cntrl_backend_http=>roundtrip( lv_body ).
    ENDCASE.


    response->set_status( lv_status ).
    response->set_text( lv_resp ).

  endmethod.

ENDCLASS.
