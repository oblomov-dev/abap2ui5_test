CLASS lcl_View DEFINITION DEFERRED.

class hlp DEFINITION INHERITING FROM zcl_2ui5_hlp_utility.
endclass.

INTERFACE zif_2ui5_screen.

  METHODS begin_of
    IMPORTING
              name            TYPE string OPTIONAL
    RETURNING VALUE(r_result) TYPE REF TO zif_2ui5_screen.

  METHODS end_of
    IMPORTING
              name            TYPE string OPTIONAL
    RETURNING VALUE(r_result) TYPE REF TO zif_2ui5_screen.

  METHODS button
    IMPORTING
              text            TYPE REF TO data OPTIONAL
    RETURNING VALUE(r_result) TYPE REF TO zif_2ui5_screen.

  METHODS text
    IMPORTING
              text            TYPE string OPTIONAL
    RETURNING VALUE(r_result) TYPE REF TO zif_2ui5_screen.

ENDINTERFACE.

CLASS lcl_screen DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF s_control,
        name    TYPE string,
        t_attri TYPE if_web_http_request=>name_value_pairs,
        value   TYPE REF TO data,
      END OF s_control.

    TYPES:
      BEGIN OF s_screen,
        name      TYPE string,
        t_content TYPE STANDARD TABLE OF s_control WITH EMPTY KEY,
      END OF s_screen.

    DATA mt_screen TYPE STANDARD TABLE OF s_screen.
    DATA mr_screen_actual TYPE REF TO s_screen.

    DATA mv_xml_view TYPE string.
    DATA mv_view_model TYPE string.

    METHODS set_back_2_front
      RETURNING
        VALUE(r_result) TYPE string.

    INTERFACES zif_2ui5_screen.
  PRIVATE SECTION.
    DATA mv_actual_screen TYPE string.

ENDCLASS.

CLASS lcl_screen IMPLEMENTATION.

  METHOD zif_2ui5_screen~begin_of.

    INSERT VALUE #(
        name = name
     ) INTO TABLE mt_screen.

    mr_screen_actual = REF #( mt_screen[ lines( mt_screen ) ] ).


  ENDMETHOD.

  METHOD zif_2ui5_screen~button.

    INSERT VALUE #(
      name  = 'BUTTON'
      value = REF #( text->* )
     ) INTO TABLE mr_screen_actual->t_content.

*endcase.

  ENDMETHOD.

  METHOD zif_2ui5_screen~end_of.

  ENDMETHOD.

  METHOD zif_2ui5_screen~text.

    INSERT VALUE #(
      name  = 'TEXT'
      t_attri = VALUE #( ( name = 'TEXT' value = text ) )
     ) INTO TABLE mr_screen_actual->t_content.

  ENDMETHOD.

  METHOD set_back_2_front.

*    IF mt_xml_abap IS NOT INITIAL.
*      r_result = REDUCE #( INIT result = `` FOR row IN mt_xml_abap NEXT result &&= row )   .
*    ELSE.
*      r_result = mv_xml_abap.
*      RETURN.
*    ENDIF.

*    DATA(lo_abap) = zcl_2ui5_hlp_tree_xml=>factory( ).
    DATA(lo_ui5)  = zcl_2ui5_hlp_tree_xml=>factory( ).

    "attribute
    lo_ui5->mv_name = 'View'.
    lo_ui5->mv_namespace = 'mvc'.
    lo_ui5->set_attribute( n = `controllerName` v = `MyController` ).

*    xml_ui5_new( lo_abap ).


    r_result = `<mvc:View controllerName='MyController' xmlns:mvc='sap.ui.core.mvc' displayBlock="true"` && |\n|  &&
                         `    xmlns="sap.m">   <Page id="page" title="{i18n>title}">` && |\n|  &&
                         `        <content>   `.
    "    <Button type="Back" press="onPress" text="das ist ein test" />` && |\n|  &&



    DATA(ls_screen) = mt_screen[ name = mv_actual_screen ].

    LOOP AT ls_screen-t_content INTO DATA(ls_element).

      CASE ls_element-name.

        WHEN `BUTTON`.
          r_result = r_result && ` <Button text='buchen' press='onEvent({ "test" : "tttt" })' />  `.

        WHEN `TEXT`.


      ENDCASE.

    ENDLOOP.

    r_result = r_result && `        </content>` && |\n|  &&
                         `    </Page>` && |\n|  &&
                         `</mvc:View>`.


    "  r_result = lo_ui5->write( ).
    "  r_result = replace( val = r_result sub = '&quot;' with = '"' occ = 0 ).

  ENDMETHOD.

ENDCLASS.

INTERFACE zif_2ui5_frontend.

  METHODS call_screen
    IMPORTING
      name TYPE string.

*    get_event

ENDINTERFACE.

INTERFACE zif_2ui5_selection_screen.
  INTERFACES if_serializable_object.

  METHODS render
    IMPORTING
      screen TYPE REF TO zif_2ui5_screen.

  METHODS roundtrip
    IMPORTING
      frontend TYPE REF TO zif_2ui5_frontend OPTIONAL
      view     TYPE REF TO lcl_view.

ENDINTERFACE.



CLASS lcl_app_error DEFINITION.

  PUBLIC SECTION.

    INTERFACES zif_2ui5_selection_screen.

    DATA mx TYPE REF TO cx_root.

    DATA mv_data TYPE string.
    DATA mv_prev TYPE string.
    DATA mv_count TYPE string.
    DATA mv_value TYPE string.

    DATA mv_error_app_input TYPE string.
    DATA mv_error_classname TYPE string.

    DATA mv_descr TYPE string.
    TYPES:
      BEGIN OF ty_S_tab,
        name  TYPE string,
        color TYPE string,
        value TYPE string,
      END OF ty_S_tab.

    DATA mt_tab TYPE STANDARD TABLE OF ty_S_tab WITH EMPTY KEY.

    DATA mv_check_visible TYPE abap_bool.

    DATA:
      BEGIN OF ms_screen,
        BEGIN OF s_msg_stripe,
          visible TYPE abap_bool,
          text    TYPE string,
        END OF s_msg_stripe,
        app_name          TYPE string,
        check_initialized TYPE abap_bool,
      END OF ms_screen.
  PROTECTED SECTION.

    METHODS ui_on_init
      IMPORTING
        i_view TYPE REF TO lcl_view.

    METHODS ui_on_event
      IMPORTING
        i_view TYPE REF TO lcl_view.
    METHODS ui_set_screen
      IMPORTING
        i_view TYPE REF TO lcl_view.


ENDCLASS.




CLASS lcl_view DEFINITION.

  PUBLIC SECTION.

    INTERFACES if_serializable_object.

    CONSTANTS:
      BEGIN OF s_event_kind,
        ucomm        TYPE string VALUE 'UCOMM',
        init         TYPE string VALUE 'INIT',
        value_change TYPE string VALUE 'VALUE_CHANGE',
      END OF s_event_kind.

    TYPES:
      BEGIN OF ty_s_event,
        kind  TYPE string,
        value TYPE string,
      END OF ty_S_event.

    DATA mv_counter TYPE c LENGTH 4.

    TYPES:
      BEGIN OF ty_S_binding,
        json TYPE string,
        abap TYPE string,
      END OF ty_S_binding.

    DATA mt_binding TYPE STANDARD TABLE OF ty_s_binding WITH EMPTY KEY.
    DATA mt_xml_abap TYPE string_table.
    DATA mv_xml_abap TYPE string.
    DATA mt_xml_ui5 TYPE string_table.
    DATA ms_event TYPE ty_s_event.
    DATA mo_new_app TYPE REF TO object.

    DATA mr_body TYPE REF TO data.

    METHODS get_event
      IMPORTING
        val             TYPE string
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS body_get
      IMPORTING
        val             TYPE string
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS get_xml_ui5
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS render_header.
    METHODS render_footer.
    METHODS render_button
      IMPORTING
        text    TYPE string
        ucomm   TYPE string
        active  TYPE abap_bool DEFAULT abap_true
        visible TYPE string DEFAULT 'true'.

    METHODS render_input
      IMPORTING
        " text           TYPE string
        value   TYPE string
        active  TYPE abap_bool DEFAULT abap_true
        visible TYPE any DEFAULT abap_true.

    METHODS render_view
      IMPORTING
        name TYPE string.

    METHODS get_id
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS constructor
      IMPORTING
        iv_body TYPE string.

    METHODS db_read
      IMPORTING
        name            TYPE string
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS set_app
      IMPORTING
        obj TYPE REF TO object OPTIONAL
        val TYPE string OPTIONAL.

    METHODS set_go_back.

    DATA mv_new_app TYPE string.
    DATA mv_check_go_back TYPE abap_bool.

  PROTECTED SECTION.

    METHODS xml_ui5_head
      IMPORTING
        io_abap TYPE REF TO zcl_2ui5_hlp_tree_xml
      CHANGING
        co_ui5  TYPE REF TO zcl_2ui5_hlp_tree_xml.
    METHODS xml_ui5_new
      IMPORTING
        check_no_bind TYPE abap_bool DEFAULT abap_false
        i_lo_abap     TYPE REF TO zcl_2ui5_hlp_tree_xml.

ENDCLASS.

CLASS lcl_view IMPLEMENTATION.

  METHOD constructor.

    /ui2/cl_json=>deserialize(
        EXPORTING
           json  = iv_body
        CHANGING
           data  = mr_body
       ).

  ENDMETHOD.


  METHOD render_button.

*    INSERT  ` <Button visible='` && visible && `' text='` && text && `' press='onEvent({ "value" : "` && ucomm && `", "kind" : "` && s_event_kind-ucomm && `" })' /> `
    INSERT  ` <Button text='buchen' press='onEvent({ "test" : "tttt" })' />  `
        INTO TABLE mt_xml_abap.
  ENDMETHOD.

  METHOD render_input.

*    DATA(ls_bind) = VALUE ty_s_binding(
*        json = 'K' && get_id( )
*        abap = to_upper( shift_left( val = shift_left( shift_right( val = shift_right( value ) sub = `}` ) ) sub = '{' ) )
*     ).
*
*    INSERT ls_bind INTO TABLE mt_binding.
*
*    INSERT   `<Input value='{/` && ls_bind-json && `}' /> `
*        INTO TABLE mt_xml_abap.

    INSERT   `<Input value='` && value  && `' visible='` && visible  && `' /> `
      INTO TABLE mt_xml_abap.

  ENDMETHOD.

  METHOD render_footer.
    INSERT `</Page>  </Shell>   </mvc:View> `
       INTO TABLE mt_xml_abap.
  ENDMETHOD.

  METHOD render_header.

    INSERT  `<mvc:View controllerName='MyController'  xmlns:mvc='sap.ui.core.mvc' xmlns='sap.m' > <Shell> <Page> <Title text='Orders of ALFKI'/> `
      INTO TABLE mt_xml_abap.

  ENDMETHOD.

  METHOD get_id.

    mv_counter = CONV string( CONV i( mv_counter ) + 1 ).
    r_result = CONV #( mv_counter ).

  ENDMETHOD.


  METHOD get_event.
    TRY.

        IF mr_body IS NOT BOUND OR val IS INITIAL.
          RETURN.
        ENDIF.

        DATA(lv_val) = to_upper( val ).
        r_result = mr_body->('OEVENT')->(lv_val)->*.

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD get_xml_ui5.

    IF mt_xml_abap IS NOT INITIAL.
      r_result = REDUCE #( INIT result = `` FOR row IN mt_xml_abap NEXT result &&= row )   .
    ELSE.
      r_result = mv_xml_abap.
      RETURN.
    ENDIF.

    DATA(lo_abap) = zcl_2ui5_hlp_tree_xml=>factory( iv_xml = r_result ).
    DATA(lo_ui5)  = zcl_2ui5_hlp_tree_xml=>factory( iv_xml = `` ).

    lo_abap->mv_name = 'View'.
    lo_abap->mv_namespace = 'mvc'.
    lo_abap->set_attribute( n = `controllerName` v = `MyController` ).

    xml_ui5_new( lo_abap ).

    r_result = lo_abap->write( ).
    r_result = replace( val = r_result sub = '&quot;' with = '"' occ = 0 ).

  ENDMETHOD.

  METHOD db_read.

    "   DATA(ls_result) = zcl_test85_tree_helper=>db_view_read( iv_screen = name ).
    "  r_result = ls_result-vfragment_xml.

  ENDMETHOD.

  METHOD render_view.

    mv_xml_abap = db_read( name ).

  ENDMETHOD.

  METHOD xml_ui5_head.

*    co_ui5->add_name(
*        n        = `View`
*        p        = `mvc`
*    ).
*
*    DATA(lt_attri) = io_abap->get_attribute_all(  ).
*    DELETE lt_attri WHERE name = `controllerName`.
*
*    co_ui5 = REDUCE #( INIT result2 = co_ui5 FOR row2 IN lt_attri NEXT result2 =
*       co_ui5->add_attri(
*        n        = row2-name
*        v        = row2-value
*        p        = row2-prefix
*       ) ).
*
*    co_ui5->add_attri(
*        n        = `controllerName`
*        v        = `MyController`
*    ).

  ENDMETHOD.

  METHOD body_get.
    TRY.

        SPLIT val AT '->' INTO TABLE DATA(lt_attri).

        DATA(lo_obj) = REDUCE #( INIT result = mr_body
            FOR row IN lt_attri NEXT result = mr_body->(row)
           ).

        r_result = lo_obj->*.

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD set_app.

    mv_new_app = val.
    mo_new_app = obj.

  ENDMETHOD.

  METHOD set_go_back.
    mv_check_go_back = abaP_true.
  ENDMETHOD.


  METHOD xml_ui5_new.

    LOOP AT i_lo_abap->get_child_all( ) INTO DATA(lo_abap_child).

      IF lo_abap_child->get_name( ) = 'ColumnListItem' OR check_no_bind = abaP_true.
        DATA(lv_no_bind) = abap_true.
      ELSE.
        lv_no_bind = abap_false.
      ENDIF.

      LOOP AT lo_abap_child->mt_attributes REFERENCE INTO DATA(lr_attri).

        IF lv_no_bind = abap_false.
          IF strlen( lr_attri->value ) > 2.
            IF lr_attri->value(2) = `{=`.

              SPLIT lr_attri->value+1 AT `{` INTO TABLE DATA(lt_split).
              DELETE lt_split INDEX 1.

              LOOP AT lt_split INTO DATA(lv_split2).

                mt_binding =  COND #( LET ls_bind2 = VALUE ty_s_binding(
                                            json = 'K' && get_id( )
                                            abap = segment( val = lv_split2 sep = `}` index = 1 )
                        ) IN WHEN line_exists( mt_binding[ abap = ls_bind2-abap ]  )
                                THEN mt_binding
                                ELSE VALUE #( BASE mt_binding ( ls_bind2 ) ) ).

              ENDLOOP.

              lr_attri->value = REDUCE #( INIT result = lr_attri->value FOR ls_bind2 IN mt_binding NEXT result =
                COND #( WHEN lr_attri->name = 'test'
                             THEN replace( val = result sub = '{' && ls_bind2-abap && '}' with = '{meta>/' && ls_bind2-json && '}' occ = 0 )
                             ELSE replace( val = result sub = '{' && ls_bind2-abap && '}' with = '{/' && ls_bind2-json && '}' occ = 0 ) ) ).


            ELSEIF lr_attri->value(1) = `{`.
              DATA(lv_name) = segment( val = segment( val = lr_attri->value sep = `{` index = 2 ) sep = `}` index = 1 ).
              TRY.
                  DATA(ls_bind) = mt_binding[ abap = lv_name ].

                CATCH cx_sy_itab_line_not_found.
                  ls_bind = VALUE #(
                       abap = lv_name
                       json =  'K' && get_id( )
                   ).
                  INSERT ls_bind INTO TABLE mt_binding.
              ENDTRY.
              lr_attri->value = replace( val = lr_attri->value sub = ls_bind-abap with = '/' && ls_bind-json ).
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.

      xml_ui5_new( lo_abap_child ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_2ui5_backend DEFINITION.
  PUBLIC SECTION.

    INTERFACES if_serializable_object.
    INTERFACES if_abap_parallel.
    DATA mv_xml TYPE string.
    DATA mo_view TYPE REF TO lcl_view.

    DATA mi_app TYPE REF TO zif_2ui5_selection_screen.

    DATA:
      BEGIN OF ms_config,
        id      TYPE string,
        id_prev TYPE string,
      END OF ms_config.

    CLASS-METHODS main_index_html
      IMPORTING
        url_params      TYPE if_web_http_request=>name_value_pairs OPTIONAL
      RETURNING
        VALUE(r_result) TYPE string.

    CLASS-METHODS main_roundtrip
      IMPORTING
        iv_body        TYPE string
      RETURNING
        VALUE(rv_resp) TYPE string.

    METHODS db_save.

    METHODS db_load
      IMPORTING
        id              TYPE string
      RETURNING
        VALUE(r_result) TYPE REF TO lcl_2ui5_backend.

  PROTECTED SECTION.

    METHODS execute_init
      IMPORTING
        iv_body         TYPE string
      RETURNING
        VALUE(ro_model) TYPE REF TO lcl_2ui5_backend.
    METHODS execute_roundtrip.
    METHODS execute_finish
      RETURNING
        VALUE(rv_resp) TYPE string.
    METHODS init_prev.
    METHODS init_app.
    METHODS go_app_change.
    METHODS go_restart.
    METHODS go_back
      RETURNING VALUE(r_result) TYPE REF TO lcl_2ui5_backend.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_2ui5_backend IMPLEMENTATION.

  METHOD main_roundtrip.

    DATA(lo_model) = NEW lcl_2ui5_backend(  ).

    lo_model->execute_init( iv_body ).

    lo_model->execute_roundtrip(  ).

    IF lo_model->mo_view->mv_new_app IS NOT INITIAL OR lo_model->mo_view->mo_new_app IS BOUND.
      lo_model->go_app_change(  ).
      lo_model->execute_roundtrip(  ).

    ELSEIF lo_model->mo_view->mv_check_go_back = abap_true.
      lo_model = lo_model->go_back( ).
    ENDIF.

    rv_resp = lo_model->execute_finish( ).

  ENDMETHOD.

  METHOD db_load.

*    SELECT SINGLE FROM zzt_lktest_001
*        FIELDS
*            *
*       WHERE id = @id
*      INTO @DATA(ls_model).
*
*    r_result = CAST #( hlp=>trans_xml_2_object( ls_model-data ) ).

  ENDMETHOD.

  METHOD db_save.

*    MODIFY zzt_lktest_001 FROM @( VALUE #(
*      id   = ms_config-id
*      data = hlp=>trans_object_2_xml( me )
*      ) ).
*
*    COMMIT WORK.

  ENDMETHOD.


  METHOD execute_init.

    mo_view = NEW #( iv_body ).

    ms_config-id = hlp=>get( VALUE #( uuid = abap_true ) )-uuid.
    ms_config-id_prev = mo_view->body_get( 'ID' ).

    IF ms_config-id_prev IS INITIAL.
      init_app( ).
    ELSE.
      init_prev( ).
    ENDIF.

  ENDMETHOD.


  METHOD execute_finish.

    DATA(lo_model) = NEW zcl_2ui5_hlp_tree_json( )->add_attribute(
     n           = 'vView'
     v           = mo_view->get_xml_ui5( ) "REDUCE #( INIT result = `` FOR row IN mo_view->mt_xml_abap NEXT result &&= row )
     )->add_attribute_object( name = 'vModel' ).


    lo_model->add_attribute( n = 'id' v = ms_config-id ).


    DATA(lo_object) = CAST object(  mi_app ).

    LOOP AT mo_view->mt_binding REFERENCE INTO DATA(lr_bind).

      TRY.
          DATA(lo_ref) = REF #( lo_object->(lr_bind->abap) ).

          lo_model->add_attribute( n = lr_bind->json v = hlp=>hlp_get_abap_as_json( lo_ref->* ) apos_active = abap_false ).

        CATCH cx_root.
      ENDTRY.
    ENDLOOP.

    rv_resp = lo_model->get_root( )->write_result( ).

    CLEAR me->mo_view->mr_body.
    NEW cl_abap_parallel( )->fork_inst( VALUE #( ( me ) ) ).

    "db_save( ).

  ENDMETHOD.


  METHOD execute_roundtrip.
    TRY.

        ROLLBACK WORK.
        mi_app->roundtrip( mo_view ).
        ROLLBACK WORK.

      CATCH cx_root INTO DATA(lx).
        DATA(lo_app_error) = NEW lcl_app_error(  ).
        lo_app_error->mx = lx.
        lo_app_error->mv_error_classname = cl_abap_classdescr=>get_class_name( mi_app  ).

        mi_app = lo_app_error.
        ROLLBACK WORK.
        mi_app->roundtrip( mo_view ).
        ROLLBACK WORK.

    ENDTRY.
  ENDMETHOD.


  METHOD init_prev.

    DATA(lo_prev) = COND #( WHEN mo_view->mr_body IS BOUND
        THEN db_load( mo_view->mr_body->('ID')->* ) ).

    DATA(lo_obj) = CAST object( lo_prev->mi_app ).

    LOOP AT lo_prev->mo_view->mt_binding INTO DATA(ls_bind).

      hlp=>hlp_get_json_as_abap(
        EXPORTING
          i_mo_app_row2_abap = mo_view->mr_body->(ls_bind-json)
        CHANGING
          co_data            = lo_obj->(ls_bind-abap)
      ).

    ENDLOOP.

    mi_app = CAST #( lo_obj ).

  ENDMETHOD.


  METHOD init_app.

    DATA(lv_window_search) = CONV string( mo_view->mr_body->('WINDOW')->('SEARCH')->* ).

    lv_window_search = shift_left( val = lv_window_search sub = `?` ).

    SPLIT lv_window_search AT '&' INTO TABLE DATA(lt_split).
    DATA(lt_fields) = VALUE if_web_http_request=>name_value_pairs(
        FOR row IN lt_split (
             name = to_upper( segment( val = row sep = '=' index = 1  ) )
             value = to_upper( segment( val = row sep = '=' index = 2  ) )
     )  ).


    DATA(lv_app) = VALUE #( lt_fields[ name = `APP` ]-value DEFAULT `LCL_APP_SYSTEM` ).

    TRY.
        CREATE OBJECT mi_app TYPE (lv_app).
      CATCH cx_root.
        CREATE OBJECT mi_app TYPE ('lcl_app_start').
        mi_app->('mv_error_app_input') = lv_app.
    ENDTRY.
  ENDMETHOD.


  METHOD go_app_change.

    TRY.
        IF mo_view->mo_new_app IS BOUND.
          mi_app = CAST #( mo_view->mo_new_app ).
        ELSE.
          DATA(lv_new_app) = to_upper( mo_view->mv_new_app ).
          CREATE OBJECT mi_app TYPE (lv_new_app).
        ENDIF.

      CATCH cx_root.
        " CREATE OBJECT mi_app TYPE ('lcl_app_start').
        "  mi_app->('mv_error_app_input') = lv_app.
    ENDTRY.

  ENDMETHOD.

  METHOD go_back.

    DATA(lo_prev) = db_load( ms_config-id_prev ).
    r_result = db_load( lo_prev->ms_config-id_prev ).

  ENDMETHOD.

  METHOD go_restart.

  ENDMETHOD.

  METHOD if_abap_parallel~do.

    db_save(  ).

  ENDMETHOD.

  METHOD main_index_html.


    r_result = `<html>` && |\n|  &&
               `<head>` && |\n|  &&
               `    <meta charset="utf-8">` && |\n|  &&
               `    <title>Fiori-Abap Painter</title>` && |\n|  &&
               `    <script src="https://ui5.sap.com/resources/sap-ui-core.js" id="sap-ui-bootstrap" data-sap-ui-theme="sap_fiori_3"` && |\n|  &&
               `        data-sap-ui-libs="sap.m" data-sap-ui-bindingSyntax="complex" data-sap-ui-compatVersion="edge"` && |\n|  &&
               `        data-sap-ui-preload="async">` && |\n|  &&
               `     </script>` && |\n|  &&
               `  </head><body class="sapUiBody">  <div id="content"></div></body></html>` && |\n|  &&
               `    <script>` && |\n|  &&
               `        sap.ui.getCore().attachInit(function () {` && |\n|  &&
               `            "use strict";` && |\n|  &&
               `            sap.ui.define([` && |\n|  &&
               `                "sap/ui/core/mvc/Controller",` && |\n|  &&
               `                "sap/ui/model/odata/v2/ODataModel",` && |\n|  &&
               `                "sap/ui/model/json/JSONModel",` && |\n|  &&
               `            ], function (Controller, ODataModel, JSONModel) {` && |\n|  &&
               `                "use strict";` && |\n|  &&
               `                return Controller.extend("MyController", {` && |\n|  &&
               `                    onEventBackend: function (oEvent) {` && |\n|  &&
               `                        this.oBody = this.oView.getModel().oData;` && |\n|  &&
               `                        this.oBody.oEvent = oEvent;` && |\n|  &&
               `                        this.Roundtrip();` && |\n|  &&
               `                    },` && |\n|  &&
               `                    Roundtrip: function () {` && |\n|  &&
               `                        this.oView.destroy();` && |\n|  &&
               `                        var xhr = new XMLHttpRequest();` && |\n|  &&
               `                        var url = window.location.pathname;` && |\n|  &&
*               `                        url = "https://port8080-workspaces-ws-sbx9h.us10.trial.applicationstudio.cloud.sap/sap/bc/http/sap/z2ui5_http";` && |\n|  &&
               `                        xhr.open("POST", url, true);` && |\n|  &&
               `                        if (!this.oBody) {` && |\n|  &&
               `                            ;` && |\n|  &&
               `                            this.oBody = {};` && |\n|  &&
               `                            this.oBody.window = window.location;` && |\n|  &&
               `                        }` && |\n|  &&
               `                        xhr.onload = function (that) {` && |\n|  &&
               `                            var oResponse = JSON.parse(that.target.response);` && |\n|  &&
               `                            console.log(oResponse.vModel);` && |\n|  &&
               `                            console.log(oResponse.vView);` && |\n|  &&
               `                            var oModel = new JSONModel(oResponse.vModel);` && |\n|  &&
               `                            var oView = new sap.ui.core.mvc.XMLView.create({` && |\n|  &&
               `                                viewContent: oResponse.vView,` && |\n|  &&
               `                                definition: oResponse.vView,` && |\n|  &&
               `                                preprocessors: {` && |\n|  &&
               `                                    xml: {` && |\n|  &&
               `                                        models: { meta: oModel }` && |\n|  &&
               `                                    }` && |\n|  &&
               `                                },` && |\n|  &&
               `                            }).then(oView => {` && |\n|  &&
               `                                oView.setModel(oModel);` && |\n|  &&
               `                                oView.placeAt("content");` && |\n|  &&
               `                            });` && |\n|  &&
               `                        }.bind(this);` && |\n|  &&
               `                        console.log(this.oBody);` && |\n|  &&
               `                        xhr.send(JSON.stringify(this.oBody));` && |\n|  &&
               `                    },` && |\n|  &&
               `                });` && |\n|  &&
               `            });` && |\n|  &&
               `         //   const myTimeout = setTimeout(() => {` && |\n|  &&
               `                var oView = sap.ui.xmlview({ viewContent: "<mvc:View controllerName='MyController' xmlns:mvc='sap.ui.core.mvc' />" });` && |\n|  &&
               `                oView.getController().Roundtrip();` && |\n|  &&
               `        //   }, 1000);` && |\n|  &&
               `        });` && |\n|  &&
               |\n|  &&
               `    </script>` && |\n|  &&
               |\n|  &&
               `</html>`.



  ENDMETHOD.

ENDCLASS.


CLASS lcl_app_error IMPLEMENTATION.

  METHOD zif_2ui5_selection_screen~roundtrip.

*    ui_on_init( view ).
*    ui_on_event( view ).
*    ui_set_screen( view ).



  ENDMETHOD.


  METHOD ui_on_event.

    CASE i_view->get_event( 'EVENT_TYPE' ).

      WHEN 'button'.

        CASE i_view->get_event( 'UCOMM' ).

          WHEN 'BACK'.
            i_view->set_go_back(  ).

          WHEN 'RESTART'.
            i_view->set_app( val = mv_error_classname ).

        ENDCASE.
    ENDCASE.

  ENDMETHOD.


  METHOD ui_set_screen.

*    ms_screen-app_name = 'LCL_APP_ERROR'.
*    ms_screen-s_msg_stripe-text = 'Kein URL Parameter übergeben. Bitte Klassennamen übergeben, z.B. app=zcl_test'.
*    ms_screen-s_msg_stripe-visible = abap_true.
    mv_descr = mx->get_text(  ).
    i_view->mv_xml_abap = i_view->db_read( 'test_test8' ).

  ENDMETHOD.

  METHOD ui_on_init.

    IF ms_screen-check_initialized = abap_true.
      RETURN.
    ENDIF.

    ms_screen-check_initialized = abap_true.

  ENDMETHOD.

  METHOD zif_2ui5_selection_screen~render.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_app_demo DEFINITION.

  PUBLIC SECTION.

    INTERFACES zif_2ui5_selection_screen.

    DATA mv_xml TYPE string.
ENDCLASS.

CLASS lcl_app_demo IMPLEMENTATION.

  METHOD zif_2ui5_selection_screen~roundtrip.

    view->mv_xml_abap = view->db_read( 'LCL_APP_DEMO' ).
    " mv_xml =  view->db_read( 'LCL' ).

  ENDMETHOD.

  METHOD zif_2ui5_selection_screen~render.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_view_editor DEFINITION.

  PUBLIC SECTION.

    INTERFACES zif_2ui5_selection_screen.
    DATA mv_xml TYPE string.

ENDCLASS.

CLASS lcl_view_editor IMPLEMENTATION.

  METHOD zif_2ui5_selection_screen~roundtrip.

*    view->mv_xml_abap = view->db_read( 'test_name3' ).
*    mv_xml =  view->db_read( 'test_name3' ).
*
*    CASE view->get_event( 'ucomm' ).
*
*      WHEN 'GO'.
*
*        DATA(lo_json) = NEW zstc77_cl_json_writer( ).
*
*        lo_json->add_attribute( n = `SCREEN_ID` v = `LCL_APP_DEMO` ).
*        DATA(lv_json_head) = lo_json->write_result( ).
*
*        DATA(ls_screen) = VALUE zcl_ui_builder=>ty_s_view_designtime(
*           head_json     = lv_json_head
*           vfragment_xml = mv_xml
*         ).
*
*
*        zcl_test85_tree_helper=>db_view_save( ls_screen ).
*
*    ENDCASE.

  ENDMETHOD.

  METHOD zif_2ui5_selection_screen~render.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_app_system DEFINITION.

  PUBLIC SECTION.

    INTERFACES zif_2ui5_selection_screen.

    CONSTANTS:
      BEGIN OF cs_app,
        no_url_param TYPE string VALUE '1',
        exception    TYPE string VALUE '2',
      END OF cs_app.

    DATA mv_app TYPE string.

    DATA mv_data TYPE string.
    DATA mv_prev TYPE string.
    DATA mv_count TYPE string.
    DATA mv_value TYPE string.

    DATA mv_error_app_input TYPE string.

    TYPES:
      BEGIN OF ty_S_tab,
        name  TYPE string,
        color TYPE string,
        value TYPE string,
      END OF ty_S_tab.

    DATA mt_tab TYPE STANDARD TABLE OF ty_S_tab WITH EMPTY KEY.

    DATA mv_check_visible TYPE abap_bool.

    DATA:
      BEGIN OF ms_screen,
        BEGIN OF s_msg_stripe,
          visible TYPE abap_bool,
          text    TYPE string,
        END OF s_msg_stripe,
        app_name          TYPE string,
        check_initialized TYPE abap_bool,
      END OF ms_screen.

    DATA mv_button_text TYPE string VALUE 'Das ist ein Buttontext'.

  PROTECTED SECTION.

    METHODS ui_on_init
      IMPORTING
        i_view TYPE REF TO lcl_view.

    METHODS ui_on_event
      IMPORTING
        i_view TYPE REF TO lcl_view.
    METHODS ui_set_screen
      IMPORTING
        i_view   TYPE REF TO lcl_view
        i_screen TYPE REF TO zif_2ui5_selection_screen.


ENDCLASS.

CLASS lcl_app_system IMPLEMENTATION.

  METHOD zif_2ui5_selection_screen~roundtrip.

    ui_on_init( view ).
    ui_on_event( view ).
*    ui_set_screen( view ).


    frontend->call_screen( 'NO_URL_INFO' ).

  ENDMETHOD.


  METHOD ui_on_event.

    CASE i_view->get_event( 'EVENT_TYPE' ).

      WHEN 'button'.

        CASE i_view->get_event( 'UCOMM' ).

          WHEN 'WEITER' OR 'post'.


          WHEN 'GO'.
            i_view->set_app( val = ms_screen-app_name ).
            " RAISE EXCEPTION NEW hlp( ).


        ENDCASE.
    ENDCASE.

  ENDMETHOD.


  METHOD ui_set_screen.

*    ms_screen-app_name = 'LCL_APP_demo'.
*    ms_screen-s_msg_stripe-text = 'Kein URL Parameter übergeben. Bitte Klassennamen übergeben, z.B. app=zcl_test'.
*    ms_screen-s_msg_stripe-visible = abap_true.
*    i_view->mv_xml_abap = i_view->db_read( 'test_test7' ).

*case mv_app.

*when cs_app-no_url_param.
    i_view->mv_xml_abap = `<mvc:View controllerName='MyController' xmlns:mvc='sap.ui.core.mvc' displayBlock="true"` && |\n|  &&
                          `    xmlns="sap.m">   <Page id="page" title="{i18n>title}">` && |\n|  &&
                          `        <content>     <Button type="Back" press="onPress" text="das ist ein test" />` && |\n|  &&
                          `        </content>` && |\n|  &&
                          `    </Page>` && |\n|  &&
                          `</mvc:View>`.
*endcase.

  ENDMETHOD.

  METHOD ui_on_init.

    IF ms_screen-check_initialized = abap_true.
      RETURN.
    ENDIF.

    ms_screen-check_initialized = abap_true.

  ENDMETHOD.

  METHOD zif_2ui5_selection_screen~render.

    screen->begin_of( 'NO_URL_INFO'
        )->text( 'Bitte Klasse in den URL Parametern angeben'
        )->button( text = REF #( mv_button_text ) ).
    screen->end_of( ).

    screen->begin_of( '0200'
        )->text( 'Das ist ein Text'
        )->button( text = REF #( mv_button_text ) ).
    screen->end_of( ).


  ENDMETHOD.

ENDCLASS.



CLASS lcl_salesorder_crudq DEFINITION.
  PUBLIC SECTION.

    METHODS query_by_generic_range
      IMPORTING
        it_filter TYPE cl_abap_range=>ds_frange_t.

    METHODS query_by_lgort
      IMPORTING
        it_filter TYPE cl_abap_range=>ds_frange_t.

    METHODS read.

    METHODS update_lgort.

    METHODS delete.

    METHODS create.

    METHODS _clear_buffer.

  PROTECTED SECTION.

    DATA mt_vbak TYPE string_table.
    DATA mt_vbap TYPE string_table.

ENDCLASS.

CLASS lcl_salesorder_crudq IMPLEMENTATION.

  METHOD query_by_generic_range.

  ENDMETHOD.

  METHOD read.

  ENDMETHOD.

  METHOD update_lgort.

  ENDMETHOD.

  METHOD query_by_lgort.

  ENDMETHOD.

  METHOD delete.

  ENDMETHOD.

  METHOD create.

  ENDMETHOD.

  METHOD _clear_buffer.

  ENDMETHOD.

ENDCLASS.
