CLASS lcl_View DEFINITION DEFERRED.

INTERFACE zif_http_painter.
  INTERFACES if_serializable_object.

  METHODS roundtrip
    IMPORTING
      view TYPE REF TO lcl_view.

ENDINTERFACE.

CLASS zcl_http_hlp DEFINITION
CREATE PUBLIC INHERITING FROM cx_no_check.

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg.

    CONSTANTS:
      BEGIN OF cs,
        BEGIN OF s,
          sign LIKE cl_abap_range=>sign VALUE cl_abap_range=>sign,
          optn LIKE cl_abap_range=>option VALUE cl_abap_range=>option,
        END OF s,
      END OF cs.

    TYPES:
      BEGIN OF ty,
        BEGIN OF s,
          BEGIN OF msg,
            id TYPE string,
            ty TYPE string,
            no TYPE string,
            v1 TYPE string,
            v2 TYPE string,
            v3 TYPE string,
            v4 TYPE string,
          END OF msg,
          BEGIN OF msg_result,
            message  TYPE string,
            is_error TYPE abap_bool,
            type     TYPE abap_bool,
            t_bapi   TYPE bapirettab,
            s_bapi   TYPE LINE OF bapirettab,
          END OF msg_result,
          BEGIN OF get,
            username    TYPE abap_bool,
            timestampl  TYPE abap_bool,
            uuid        TYPE abap_bool,
            user_tech   TYPE abap_bool,
            utc_current TYPE abap_bool,
          END OF get,
          BEGIN OF get_result,
            username    TYPE string,
            user_tech   TYPE string,
            uuid        TYPE string,
            timestampl  TYPE timestampl,
            utc_current TYPE utcl,
            val         TYPE string,
            o           TYPE REF TO zcl_http_hlp,
          END OF get_result,
        END OF s,
        BEGIN OF t,
          range  TYPE cl_abap_range=>ds_trange,
          selopt TYPE cl_abap_range=>ds_selopt_t,
        END OF t,
        BEGIN OF o,
          me TYPE REF TO zcl_http_hlp,
        END OF o,
      END OF ty.

    DATA:
      BEGIN OF ms_log,
        t_log TYPE STANDARD TABLE OF ty-s-msg_result WITH EMPTY KEY,
      END OF ms_log.

    DATA:
      BEGIN OF ms_error,
        x_root TYPE REF TO cx_root,
        "   x_me   type ty-o_me,
        uuid   TYPE string,
        text   TYPE string,
        s_msg  TYPE ty-s-msg_result,
        o_log  TYPE ty-o-me,
      END OF ms_error.

    CLASS-DATA x TYPE REF TO cx_root.

    METHODS constructor
      IMPORTING
        val      TYPE any OPTIONAL
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous LIKE previous OPTIONAL
          PREFERRED PARAMETER val.

    METHODS get_text REDEFINITION.

    CLASS-METHODS log_factory
      RETURNING
        VALUE(result) TYPE ty-o-me.

    METHODS log
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE ty-o-me.

    METHODS log_sy
      RETURNING
        VALUE(result) TYPE ty-o-me.


    CLASS-METHODS msg
      IMPORTING
        val             TYPE any OPTIONAL
        msg             TYPE ty-s-msg OPTIONAL
          PREFERRED PARAMETER val
      RETURNING
        VALUE(r_result) TYPE ty-s-msg_result.

    CLASS-METHODS x_factory_by
      IMPORTING
        x_root        TYPE REF TO cx_root
      RETURNING
        VALUE(result) TYPE ty-o-me.

    METHODS x_db_save
      RETURNING
        VALUE(r_result) TYPE string.

    CLASS-METHODS x_factory_by_db
      IMPORTING
        iv_guid         TYPE string
      RETURNING
        VALUE(r_result) TYPE ty-o-me. "REF TO zstc77_cx.

    CLASS-METHODS db_save_root
      IMPORTING
        ix_root       TYPE REF TO cx_root
      RETURNING
        VALUE(r_uuid) TYPE string.

    CLASS-METHODS db_factory_root
      IMPORTING
        iv_uuid         TYPE string
      RETURNING
        VALUE(r_result) TYPE REF TO cx_root.

    CLASS-METHODS mail.

    CLASS-METHODS get
      IMPORTING
        val             TYPE ty-s-get OPTIONAL
      RETURNING
        VALUE(r_result) TYPE ty-s-get_result.


    CLASS-METHODS get_data_by_json
      IMPORTING
        val             TYPE string
      exporting
        VALUE(r_result) TYPE data.

    CLASS-METHODS trans_json_2_data
      IMPORTING
        iv_json   TYPE clike
        iv_result TYPE REF TO data.

    CLASS-METHODS trans_data_2_json
      IMPORTING
        data            TYPE data
      RETURNING
        VALUE(r_result) TYPE string.

    CLASS-METHODS trans_xml_2_object
      IMPORTING
        xml           TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO if_serializable_object.

    CLASS-METHODS trans_xml_2_any_multi
      IMPORTING
        xml     TYPE clike
      EXPORTING
        object1 TYPE REF TO if_serializable_object
        object2 TYPE REF TO if_serializable_object
        data1   TYPE REF TO data
        data2   TYPE REF TO data.

    CLASS-METHODS trans_any_2_xml_multi
      IMPORTING
        object1       TYPE REF TO if_serializable_object OPTIONAL
        object2       TYPE REF TO if_serializable_object OPTIONAL
        data1         TYPE REF TO data OPTIONAL
        data2         TYPE REF TO data OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS trans_object_2_xml
      IMPORTING
        object        TYPE REF TO if_serializable_object
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS trans_any_2_json
      IMPORTING
        any           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS action_parallel
      IMPORTING
        it_parallel   TYPE cl_abap_parallel=>t_in_inst_tab
      RETURNING
        VALUE(result) TYPE cl_abap_parallel=>t_out_inst_tab.

    CLASS-METHODS conv_string_2_XSTRING
      IMPORTING
        iv_string       TYPE clike
      RETURNING
        VALUE(r_result) TYPE xstring.

    CLASS-METHODS conv_xstring_2_string
      IMPORTING
        iv_xstring      TYPE xstring
      RETURNING
        VALUE(r_result) TYPE string.

    CLASS-METHODS assign
      IMPORTING
        object          TYPE REF TO object
        attri_name      TYPE clike
      RETURNING
        VALUE(r_result) TYPE REF TO data.
    CLASS-METHODS get_data_by_xml
      IMPORTING
        iv_data         TYPE string
      EXPORTING
        VALUE(r_result) TYPE data.
    CLASS-METHODS get_xml_by_data
      IMPORTING
        is_any          TYPE data
      RETURNING
        VALUE(r_result) TYPE string.
    CLASS-METHODS x_get_details
      IMPORTING
        val             TYPE REF TO cx_root
      RETURNING
        VALUE(r_result) TYPE string.

    CLASS-METHODS get_param_user
      IMPORTING
        val             TYPE clike
        VALUE(user)     TYPE clike OPTIONAL
      RETURNING
        VALUE(r_result) TYPE string.

    CLASS-METHODS get_memory_user
      IMPORTING
        id              TYPE clike
        VALUE(user)     TYPE clike OPTIONAL
      RETURNING
        VALUE(r_result) TYPE string.

    CLASS-METHODS set_memory_user
      IMPORTING
        val             TYPE clike
        id              TYPE clike
        VALUE(user)     TYPE clike OPTIONAL
      RETURNING
        VALUE(r_result) TYPE string.

    CLASS-METHODS mime_db_read
      IMPORTING
                iv_id           TYPE string
      RETURNING VALUE(r_result) TYPE string.

    CLASS-METHODS mime_db_save
      IMPORTING
        iv_name1 TYPE string OPTIONAL
        iv_name2 TYPE string OPTIONAL
        iv_name3 TYPE string OPTIONAL
        iv_type  TYPE string
        iv_descr TYPE string OPTIONAL
        iv_data  TYPE string.

ENDCLASS.



CLASS ZCL_HTTP_HLP IMPLEMENTATION.





  METHOD mime_db_save.


*    MODIFY zstc87_t_001 FROM @( VALUE #(
*            id = hlp=>get( VALUE #( uuid = abap_true ) )-val
*            name1 = iv_name1
*            name2 = iv_name2
*            name3 = iv_name3
*            type = iv_type
*            descr = iv_descr
*            data = iv_data
*         )  ).
*
*    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD mime_db_read.

*    SELECT SINGLE FROM zstc87_t_001
*        FIELDS data
*        WHERE id = @( CONV #( iv_id ) )
*       INTO @r_result.

  ENDMETHOD.


  METHOD get_param_user.

*    DATA(lv_user) = COND string( WHEN user IS NOT SUPPLIED THEN hlp=>get( VALUE #( user_tech = 'X' ) )-val ELSE user ).
*
*    SELECT SINGLE FROM zstc88_t_002
*    FIELDS
*        value
*    WHERE uname = @lv_user AND
*        name = @val
*          INTO @r_result.


  ENDMETHOD.


  METHOD get_memory_user.

*    DATA(lv_user) = COND string( WHEN user IS NOT SUPPLIED THEN hlp=>get( VALUE #( user_tech = 'X' ) )-val ELSE user ).
*
*    SELECT SINGLE FROM zstc88_t_003
*    FIELDS
*        value
*    WHERE uname = @lv_user AND
*            id = @id
*          INTO @r_result.

  ENDMETHOD.


  METHOD set_memory_user.

*    DATA(lv_user) = COND string( WHEN user IS NOT SUPPLIED THEN hlp=>get( VALUE #( user_tech = 'X' ) )-val ELSE user ).
*
*    MODIFY zstc88_t_003 FROM @( VALUE #(  id = id uname = lv_user value = val ) ).
*    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD x_get_details.

    DATA(lx) = val.
    DATA(lt_text) = VALUE string_table(   ).
    WHILE lx IS BOUND.

      INSERT lx->get_text(  ) INTO TABLE lt_text.

      lx = lx->previous.

    ENDWHILE.

    DELETE ADJACENT DUPLICATES FROM lt_text COMPARING table_line.

    r_result = REDUCE #( INIT result = || FOR row IN lt_text NEXT result &&= row && | / | ).

  ENDMETHOD.


  METHOD get_xml_by_data.

    CALL TRANSFORMATION id
    SOURCE data = is_any "i_result->ms_db-data
    RESULT XML r_result.

  ENDMETHOD.


  METHOD get_data_by_xml.

    CALL TRANSFORMATION id
        SOURCE XML iv_data
        RESULT data = r_result.

  ENDMETHOD.


  METHOD assign.

    DATA(lv_attri) = to_upper( attri_name ).
    DATA(lr_data) = REF #( object->(attri_name) ).
    CREATE DATA r_result LIKE lr_data->*.
    r_result->* = lr_data->*.

  ENDMETHOD.


  METHOD get_data_by_json.

    IF val IS INITIAL.
      RETURN.
    ENDIF.



    " Convert JSON to post structure
    xco_cp_json=>data->from_string( val )->apply(
      VALUE #( ( xco_cp_json=>transformation->camel_case_to_underscore ) )
      )->write_to( r_result ).

  ENDMETHOD.


  METHOD trans_json_2_data.

    IF iv_json IS INITIAL.
      RETURN.
    ENDIF.

    " Convert JSON to post structure
    xco_cp_json=>data->from_string( iv_json )->apply(
      VALUE #( ( xco_cp_json=>transformation->camel_case_to_underscore ) )
      )->write_to( iv_result ).

  ENDMETHOD.


  METHOD trans_data_2_json.

    " Convert input post to JSON
    DATA(json_post) = xco_cp_json=>data->from_abap( data )->apply(
      VALUE #( ( xco_cp_json=>transformation->underscore_to_camel_case ) ) )->to_string(  ).

    r_result = json_post.

  ENDMETHOD.


  METHOD x_factory_by.

    result = NEW #(  ).
    result->ms_error-x_root = x_root.
    result->ms_error-text = x_root->get_text( ).

  ENDMETHOD.


  METHOD trans_xml_2_object.

    CALL TRANSFORMATION id
       SOURCE XML xml
       RESULT data = result.

  ENDMETHOD.


  METHOD trans_object_2_xml.

    CALL TRANSFORMATION id
       SOURCE data = object "i_result->ms_db-data
       RESULT XML result.
    "i_result->mi_object.


  ENDMETHOD.


  METHOD conv_string_2_XSTRING.

    " TRY.
    DATA(l_convout2) = cl_abap_conv_codepage=>create_out(
        codepage = 'UTF-8' ).
    r_result = l_convout2->convert( source = iv_string ).
*      CATCH cx_sy_conversion_codepage.(

    "  CATCH cx_root.
    " message 'Conversion failure' type 'E'.
    "ENDTRY.

  ENDMETHOD.


  METHOD conv_XSTRING_2_string.

    " TRY.
    DATA(l_conv_in) = cl_abap_conv_codepage=>create_in(
        codepage = 'UTF-8' ).

    r_result = l_conv_in->convert( source = iv_xstring ).
*      CATCH cx_sy_conversion_codepage.(

    "  CATCH cx_root.
    " message 'Conversion failure' type 'E'.
    "ENDTRY.

  ENDMETHOD.


  METHOD trans_any_2_json.

    result = /ui2/cl_json=>serialize( any ).

  ENDMETHOD.


  METHOD action_parallel.

    NEW cl_abap_parallel( )->run_inst(
       EXPORTING
          p_in_tab = it_parallel "value #( ( lo_update )  )
       IMPORTING
          p_out_tab = result ). "DATA(l_out_tab) ).

  ENDMETHOD.


  METHOD trans_any_2_xml_multi.

    CALL TRANSFORMATION id
       SOURCE
        obj1  = object1
        obj2  = object2
        data1 = data1
        data2 = data2 "i_result->ms_db-data
       RESULT
       XML result.
    "i_result->mi_object.

  ENDMETHOD.


  METHOD trans_xml_2_any_multi.

    CALL TRANSFORMATION id
       SOURCE XML xml
       RESULT
        obj1  = object1
        obj2  = object2
        data1 = data1
        data2 = data2.

  ENDMETHOD.


  METHOD mail.

*    data(lo_mail) = cl_bcs_mail_message=>create_instance( ).
*
*    lo_mail->set_subject( `das ist ein subject` ).
*    lo_mail->set_sender( `lk@status-c.com` ).
*    lo_mail->add_recipient(
*        iv_address = `lars.kaldewey@status-c.com`
**        iv_copy    = TO
*    ).

    "data(lo_body) = new cl_bcs_mail_bodypart( ).

    " cl_bcs_mail_bodypart=>

    " lo_mail->set_main( io_main =  ).
*    CATCH cx_bcs_mail.

    " lo_mail->set_main( io_main = new #( )->  ).
*    CATCH cx_bcs_mail.
*    lo_mail->send(
*    IMPORTING
*       et_status      = data(lv_status)
*       ev_mail_status = data(lv_mail_status)
*    ).

  ENDMETHOD.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).
    .
*"    super->constructor( previous = cond #( when val is INSTANCE OF cx_root then val else previous ) ).

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.


    ms_error-s_msg = msg( val ).
    "text  = txt.


    TRY.
        ms_error-x_root ?= val.
      CATCH cx_root.
    ENDTRY.

    TRY.
        ms_error-uuid = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD x_db_save.

*    INSERT ztt_test77_log FROM @(
*        VALUE #(
*        id = ms_error-uuid  "hlp=>get_uuid( )
*        message = get_text( )
*        xml_data = trans_object_2_xml( me )
*        )
*    ).

  ENDMETHOD.


  METHOD x_factory_by_db.

*    SELECT SINGLE FROM ztt_test77_log
*        FIELDS
*        *
*      WHERE id = @iv_GUID
*      INTO @DATA(ls_db).
*
*    r_result = CAST #( trans_xml_2_object( ls_db-xml_data ) ).
*

  ENDMETHOD.


  METHOD get_text.

    IF ms_error-x_root IS NOT INITIAL.
      result = ms_error-x_root->get_text(  ).
      result = COND #( WHEN result IS INITIAL THEN 'Es ist ein unbekannter Fehler aufgetreten' ELSE result ).
      RETURN.
    ENDIF.

    IF ms_error-s_msg-message IS NOT INITIAL.
      result = ms_error-s_msg-message.
      result = COND #( WHEN result IS INITIAL THEN 'Es ist ein unbekannter Fehler aufgetreten' ELSE result ).
      RETURN.
    ENDIF.

    IF if_t100_message~t100key-msgid IS NOT INITIAL.

      MESSAGE ID if_t100_message~t100key-msgid TYPE `I` NUMBER if_t100_message~t100key-msgno
         INTO ms_error-text.
      result = COND #( WHEN result IS INITIAL THEN 'Es ist ein unbekannter Fehler aufgetreten' ELSE result ).
      RETURN.
    ENDIF.


  ENDMETHOD.


  METHOD db_save_root.

*    r_uuid = lcl_help=>get_uuid( ).
*
*    INSERT ztt_test77_log FROM @(
*        VALUE #(
*        id = r_uuid
*        message = ix_root->get_text( )
*        xml_data = trans_object_2_xml( ix_root )
*        )
*    ).

  ENDMETHOD.


  METHOD db_factory_root.

*    SELECT SINGLE FROM ztt_test77_log
*        FIELDS
*        *
*      WHERE id = @iv_uuid
*      INTO @DATA(ls_db).
*
*    r_result = CAST #( trans_xml_2_object( ls_db-xml_data ) ).

  ENDMETHOD.


  METHOD get.

*
*    CASE abap_true.
*
*      WHEN val-timestampl.
*        r_result-timestampl = lcl_help=>get_timestampl( ).
*        r_result-val = CONV #( r_result-timestampl ).
*      WHEN val-user_tech.
*        r_result-user_tech = lcl_help=>get_user_tech( ).
*        r_result-val = CONV #( r_result-user_tech ).
*      WHEN val-username.
*        r_result-username = lcl_help=>get_user_name( ).
*        r_result-val = CONV #( r_result-username ).
*      WHEN val-utc_current.
*        r_result-utc_current = lcl_help=>get_utc_current( ).
*        r_result-val = CONV #(  r_result-utc_current ).
*      WHEN val-uuid.
*        r_result-uuid = lcl_help=>get_uuid( ).
*        r_result-val = CONV #( r_result-uuid ).
*
*    ENDCASE.


  ENDMETHOD.


  METHOD msg.

*    r_result = COND #( WHEN val IS SUPPLIED
*                  THEN lcl_help_msg_mapper=>factory( )->get( val )
*                  ELSE lcl_help_msg_mapper=>factory( )->get_by_msg(
*                                               id   = msg-id
*                                               no   = msg-no
*                                               type = msg-ty
*                                               v1   = msg-v1
*                                               v2   = msg-v2
*                                               v3   = msg-v3
*                                               v4   = msg-v4
*                  ) ).

  ENDMETHOD.


  METHOD log.

  ENDMETHOD.


  METHOD log_sy.

  ENDMETHOD.


  METHOD log_factory.

  ENDMETHOD.
ENDCLASS.


CLASS hlp DEFINITION INHERITING FROM zcl_http_hlp.

  PUBLIC SECTION.
    CLASS-METHODS hlp_get_abap_as_json
      IMPORTING
        i_mo_app_row2_abap TYPE any
      RETURNING
        VALUE(r_result)    TYPE string.

    CLASS-METHODS hlp_get_json_as_abap
      IMPORTING
        i_mo_app_row2_abap TYPE REF TO data
      CHANGING
        co_data            TYPE data.

ENDCLASS.





CLASS lcl_app_error DEFINITION.

  PUBLIC SECTION.

    INTERFACES zif_http_painter.

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



CLASS hlp IMPLEMENTATION.


  METHOD hlp_get_json_as_abap.


    DATA(o_type_desc) = cl_abap_typedescr=>describe_by_data( co_data ).

    CASE o_type_desc->kind.
      WHEN cl_abap_typedescr=>kind_struct.

      WHEN cl_abap_typedescr=>kind_table.

        CLEAR co_data.
        FIELD-SYMBOLS <tab> TYPE table.
        FIELD-SYMBOLS <any> TYPE any.
        ASSIGN  i_mo_app_row2_abap->* TO <tab>.
        LOOP AT <tab> ASSIGNING <any>.

          INSERT INITIAL LINE INTO TABLE co_data ASSIGNING FIELD-SYMBOL(<row>).
          DO.
            DATA(lv_index) = sy-index.
            ASSIGN COMPONENT lv_index OF STRUCTURE <any>->* TO FIELD-SYMBOL(<field>).
            IF sy-subrc <> 0.
              EXIT.
            ENDIF.
            ASSIGN COMPONENT lv_index OF STRUCTURE <row> TO FIELD-SYMBOL(<field2>).
            <field2> = <field>->*.
          ENDDO.


        ENDLOOP.


      WHEN cl_abap_typedescr=>kind_class.

      WHEN cl_abap_typedescr=>kind_intf.

      WHEN cl_abap_typedescr=>kind_elem.

        co_data = i_mo_app_row2_abap->*.
*        IF  o_type_desc->get_relative_name( ) = 'ABAP_BOOL'.
*          DATA(lv_value) = COND #(  WHEN i_mo_app_row2_abap = abap_true THEN `true` ELSE `false` ).
*          r_result = lv_value.
*          RETURN.
*        ENDIF.
*
*        r_result =  '"' && i_mo_app_row2_abap && '"'.


      WHEN cl_abap_typedescr=>kind_ref.
    ENDCASE.


  ENDMETHOD.


  METHOD hlp_get_abap_as_json.




    DATA(o_type_desc) = cl_abap_typedescr=>describe_by_data( i_mo_app_row2_abap ).

    CASE o_type_desc->kind.
      WHEN cl_abap_typedescr=>kind_struct.
*      DATA(o_struct_desc) = CAST cl_abap_structdescr( o_type_desc ).
*      cl_demo_output=>write_data( o_struct_desc->components ).
      WHEN cl_abap_typedescr=>kind_table.

        r_result = /ui2/cl_json=>serialize( i_mo_app_row2_abap ).
        RETURN.

        r_result = escape( val    = /ui2/cl_json=>serialize( i_mo_app_row2_abap )
                           format = cl_abap_format=>e_json_string ) .

*      DATA(o_table_desc) = CAST cl_abap_tabledescr( o_type_desc ).
*      DATA(o_tl_struct_desc) = CAST cl_abap_structdescr( o_table_desc->get_table_line_type( ) ).
*      cl_demo_output=>write_data( o_tl_struct_desc->components ).
      WHEN cl_abap_typedescr=>kind_class.
*      DATA(o_class_desc) = CAST cl_abap_classdescr( o_type_desc ).
*      LOOP AT o_class_desc->methods ASSIGNING FIELD-SYMBOL(xxx<m>).
*        cl_demo_output=>write( <m>-name ).
*      ENDLOOP.
      WHEN cl_abap_typedescr=>kind_intf.
*      DATA(o_if_desc) = CAST cl_abap_intfdescr( o_type_desc ).
*      LOOP AT o_if_desc->methods ASSIGNING FIELD-SYMBOL(<i>).
*        cl_demo_output=>write( <i>-name ).
*      ENDLOOP.
      WHEN cl_abap_typedescr=>kind_elem.

        IF o_type_desc->get_relative_name( ) = 'ABAP_BOOL'.
          DATA(lv_value) = COND #( WHEN i_mo_app_row2_abap = abap_true THEN `true` ELSE `false` ).
          r_result = lv_value.
        ELSE.
          r_result = escape( val    = i_mo_app_row2_abap
                     format = cl_abap_format=>e_json_string ) .
          r_result =  '"' && r_result && '"'.

        ENDIF.

      WHEN cl_abap_typedescr=>kind_ref.
    ENDCASE.



  ENDMETHOD.

ENDCLASS.

CLASS zstc77_cl_xml_dom DEFINITION.

  PUBLIC SECTION.

    TYPES ty_o_me TYPE REF TO zstc77_cl_xml_dom.
    TYPES ty_T_me TYPE STANDARD TABLE OF ty_o_me WITH EMPTY KEY.
    TYPES:
      BEGIN OF TY_S_attributes,
        name   TYPE string,
        value  TYPE string,
        prefix TYPE string,
      END OF ty_s_attributes.
    TYPES: ty_T_attributes TYPE STANDARD TABLE OF ty_S_attributes WITH EMPTY KEY.
    TYPES ty_t_nodes TYPE STANDARD TABLE OF REF TO zstc77_cl_xml_dom WITH EMPTY KEY.
    DATA mv_name TYPE string.
    DATA mv_namespace TYPE string.
    DATA mt_attributes TYPE ty_t_attributes.

    CLASS-METHODS factory
      IMPORTING
        iv_xml          TYPE string
      RETURNING
        VALUE(r_result) TYPE REF TO zstc77_cl_xml_dom.
    METHODS get_child_all
      RETURNING
        VALUE(r_result) TYPE ty_t_nodes.

    METHODS get_child_first
      RETURNING
        VALUE(r_result) TYPE ty_o_me.

    DATA mv_xml TYPE string.

    METHODS _writer_insert_element
      IMPORTING
        io_xml_doc    TYPE REF TO  if_ixml_document
        io_xml_parent TYPE REF TO  if_ixml_element.

    METHODS get_name
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS get_attribute_all
      RETURNING
        VALUE(r_result) TYPE ty_T_attributes.

    METHODS get_attribute
      IMPORTING
        name            TYPE clike
      RETURNING
        VALUE(r_result) TYPE ty_s_attributes.

    METHODS get_namespace
      RETURNING
        VALUE(r_result) TYPE string.
    METHODS init.

    METHODS write
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS set_attribute
      IMPORTING
        n TYPE string
        v TYPE string
        p TYPE string OPTIONAL.

    DATA:
      mo_document TYPE REF TO if_ixml_document,
      mo_parser   TYPE REF TO if_ixml_parser_core,
      mo_node     TYPE REF TO if_ixml_node.



  PROTECTED SECTION.
    DATA mt_child TYPE ty_T_me.

    METHODS _read_childs.
ENDCLASS.

CLASS zstc77_cl_xml_dom IMPLEMENTATION.

  METHOD _writer_insert_element.

    DATA(lo_element) = io_xml_doc->create_element_ns(
         name   = mv_name "get_name( )
         prefix = mv_namespace
     ).

    io_xml_parent->append_child( lo_element  ).


    LOOP AT mt_attributes REFERENCE INTO DATA(lr_attri).
      lo_element->set_attribute_ns(
         name      = lr_attri->name
         prefix =  lr_attri->prefix
         value     = lr_attri->value
      ).
    ENDLOOP.

    LOOP AT mt_child INTO DATA(lo_child).
      lo_child->_writer_insert_element(
          io_xml_doc    = io_xml_doc
          io_xml_parent = lo_element
      ).
    ENDLOOP.

  ENDMETHOD.

  METHOD factory.


    DATA(lo_root) = NEW zstc77_cl_xml_dom(  ).
    lo_root->mv_xml = iv_xml.

    lo_root->init( ).

    r_result = lo_root.


  ENDMETHOD.

  METHOD write.

    DATA:
      mo_document TYPE REF TO if_ixml_document,
      mo_parser   TYPE REF TO if_ixml_parser_core,
      mo_node     TYPE REF TO if_ixml_node.

* XML-Interface
    DATA(o_ixml) = cl_ixml_core=>create( ).
* XML-Doc
    mo_document  = o_ixml->create_document( ).


    DATA(lo_element) = mo_document->create_element_ns(
         name   = 'root'
*       uri    = ''
     ).

    mo_document->append_child( lo_element ).


    _writer_insert_element(
          io_xml_doc    = mo_document
          io_xml_parent = lo_element
      ).



    DATA(lo_childs) = lo_element->get_children( ).


    DATA(lo_iterator) = lo_childs->create_iterator( ).
    DO.
      DATA(lo_node) = lo_iterator->get_next( ).
      "IF lo_node IS NOT BOUND.
      EXIT.
      "  ENDIF.
      " mo_document->append_child( lo_node ).
    ENDDO.

    mo_document->replace_child( old_child = lo_element new_child = lo_node ).

    TRY.
        " DATA(o_ixml) = cl_ixml=>create( ).
        DATA(o_sf) = o_ixml->create_stream_factory( ).
        DATA(o_encoding) = o_ixml->create_encoding( character_set =  'UTF-8'
                                                    byte_order = if_ixml_encoding=>co_none ).

        DATA: lv_xml TYPE xstring.

        DATA(o_ostream) = o_sf->create_ostream_xstring( lv_xml ).
        "  o_ostream->set_encoding( encoding = o_encoding ).
        " o_ostream->set_pretty_print( pretty_print = abap_true ).
        " o_ostream->s

        DATA(o_render) = o_ixml->create_renderer( ostream  = o_ostream
                                                  document = mo_document ).

* XML-String in lv_xml generieren
        DATA(lv_rc) = o_render->render( ).

* Dateigröße in Bytes
        DATA(lv_size) = o_ostream->get_num_written_raw( ).

* Stream schließen
        " o_ostream->close( ).

        IF lv_rc = 0 AND lv_size > 0.
          " data(ret_xml_str) = lv_xml.
          "2 data(lv_line_break) = lcl_help=>conv_string_2_xstring( CL_ABAP_CHAR_UTILITIES=>NEWLINE ).
          r_result = hlp=>conv_xstring_2_string( lv_xml ).
          " r_result = segment( val = r_result sep = `>` index = 2 ).
          r_result = substring_after( val = r_result sub = '>' ).
          "  REPLACE all OCCURRENCES OF `"` in r_result with `'`.
        ENDIF.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD get_attribute_all.


    TRY.
        DATA(lo_attri_iterator) = mo_node->get_attributes( )->create_iterator( ).

      CATCH cx_root.
        RETURN.
    ENDTRY.


    DO.
      DATA(node) = lo_attri_iterator->get_next( ).
      IF node IS INITIAL.
        EXIT.
      ENDIF.

      INSERT VALUE #(
          name   = node->get_name( )
          value  = node->get_value( )
          prefix = node->get_namespace_prefix( )
       ) INTO TABLE r_result.
    ENDDO.


  ENDMETHOD.
  METHOD get_namespace.


    r_result = mo_node->get_namespace( ).


  ENDMETHOD.
  METHOD get_child_all.

    _read_childs(  ).
    r_result = mt_child.

  ENDMETHOD.

  METHOD init.



    DATA(lv_xml) = hlp=>conv_string_2_xstring( mv_xml ).

* XML-Interface
    DATA(o_ixml) = cl_ixml_core=>create( ).
* XML-Doc
    mo_document  = o_ixml->create_document( ).
* Stream-Factory
    DATA(o_sf) = o_ixml->create_stream_factory( ).

* Stream
    DATA(o_stream) = o_sf->create_istream_xstring( string = lv_xml ).

* Parser-Objekt erzeugen
    mo_parser  = o_ixml->create_parser( document       = mo_document
                                       istream        = o_stream
                                       stream_factory = o_sf ).


    mo_parser->parse( ).


    mo_node = mo_document->get_root_element( ).

    mv_name = get_name(  ).
    mv_namespace = get_namespace(  ).

    mt_attributes = get_attribute_all( ).


    _read_childs( ).


*    data(lo_Node) =
*      data(lo_Node2) = mo_document->get_root( ).
*
*      data(lo_node3) = lo_node2->get_first_child(  ).
*        data(lv_test) = lo_node3->get_name(  ).
*      data(lv_val) = lo_node3->get_value(  ).
    " mo_node->get


  ENDMETHOD.
  METHOD get_name.

    r_result = mo_node->get_name(  ).

  ENDMETHOD.

  METHOD get_child_first.

    _read_childs( ).
    r_result = VALUE #( mt_child[ 1 ] OPTIONAL ).

  ENDMETHOD.


  METHOD _read_childs.

    IF mt_child IS NOT INITIAL.
      RETURN.
    ENDIF.


    DATA(lo_children_iterator) = mo_node->get_children( )->create_iterator( ).

    DO.
      DATA(node2) = lo_children_iterator->get_next( ).
      IF node2 IS INITIAL.
        EXIT.
      ENDIF.

      DATA(lo_node) = NEW zstc77_cl_xml_dom(  ).
      lo_node->mo_node = node2.
      lo_node->mo_parser = mo_parser.
      lo_node->mo_document = mo_document.
      lo_node->mv_name = lo_node->get_name(  ).
      lo_node->mv_namespace = lo_node->get_namespace(  ).
      lo_node->mt_attributes = lo_node->get_attribute_all( ).

      CHECK lo_node->mv_name NE `#text`.

      INSERT lo_node INTO TABLE mt_child.


      lo_node->_read_childs( ).
      "APPEND node->get_name( ) TO target_tab2.
      " APPEND node->get_value( ) TO target_tab.
    ENDDO.

  ENDMETHOD.



  METHOD get_attribute.

    r_result = mt_attributes[ name = name ].

  ENDMETHOD.


  METHOD set_attribute.

    DELETE mt_attributes WHERE name = n.
    INSERT VALUE #( name = n value = v prefix = p  ) INTO TABLE mt_attributes.
  ENDMETHOD.

ENDCLASS.


CLASS zstc77_cl_json_dom DEFINITION.
  PUBLIC SECTION.

    TYPES ty_o_me TYPE REF TO zstc77_cl_json_dom.
    TYPES ty_T_me TYPE STANDARD TABLE OF ty_o_me WITH EMPTY KEY.
    TYPES: BEGIN OF ty_S_name,
             n                    TYPE string,
             v                    TYPE string,
             apostrophe_deactived TYPE abap_bool,
           END OF ty_S_name.
    TYPES: ty_T_name_value TYPE STANDARD TABLE OF ty_S_name.

    CLASS-METHODS hlp_shrink
      IMPORTING
        iv_json         TYPE string
      RETURNING
        VALUE(r_result) TYPE string.

    CLASS-METHODS hlp_replace_apostr
      IMPORTING
        iv_json         TYPE string
      RETURNING
        VALUE(r_result) TYPE string.

    CLASS-METHODS factory
      IMPORTING
        iv_json         TYPE string
      RETURNING
        VALUE(r_result) TYPE  ty_o_me.

    METHODS constructor.

    METHODS get_root
      RETURNING
        VALUE(r_result) TYPE  ty_o_me.

    METHODS get_attribute
      IMPORTING
        name            TYPE string
      RETURNING
        VALUE(r_result) TYPE  ty_o_me.
    METHODS get_data
      RETURNING VALUE(r_result) TYPE REF TO data.

    METHODS get_attribute_all
      RETURNING
        VALUE(r_result) TYPE ty_T_me.

    METHODS get_parent
      RETURNING
        VALUE(r_result) TYPE ty_o_me.

    METHODS add_attribute
      IMPORTING
        n               TYPE clike
        v               TYPE clike
        apos_active     TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(r_result) TYPE  ty_o_me.

    METHODS add_attributes_name_value_tab
      IMPORTING
        it_name_value   TYPE ty_T_name_value
      RETURNING
        VALUE(r_result) TYPE  ty_o_me.

    METHODS add_attribute_object
      IMPORTING
        name            TYPE clike
      RETURNING
        VALUE(r_result) TYPE  ty_o_me.

    METHODS add_list_object
      RETURNING
        VALUE(r_result) TYPE  ty_o_me.

    METHODS add_attribute_list
      IMPORTING
        name            TYPE clike
      RETURNING
        VALUE(r_result) TYPE  ty_o_me.

    METHODS add_attribute_instance
      IMPORTING
        val             TYPE ty_o_me
      RETURNING
        VALUE(r_result) TYPE  ty_o_me.

    METHODS write_result
      RETURNING VALUE(r_result) TYPE string.

    METHODS get_name
      RETURNING VALUE(r_result) TYPE string.



  PROTECTED SECTION.

    DATA mo_root TYPE ty_o_me.
    DATA mo_parent TYPE ty_o_me.

    DATA mv_name   TYPE string.

    DATA mv_value  TYPE string.
    DATA mt_values TYPE ty_t_me.
    DATA mv_check_list TYPE abap_bool.

    DATA mo_value TYPE ty_o_me.
    DATA mr_actual TYPE REF TO data.
    DATA mv_apost_active TYPE abap_bool.


ENDCLASS.

CLASS zstc77_cl_json_dom IMPLEMENTATION.

  METHOD factory.

*    DATA(x) = COND i( WHEN iv_json IS INITIAL THEN THROW zstc77_cx( `ZCX_TREE_JSON_READER-FACTORY-JSON_INPUT_IS_EMPTY`) ).

    r_result = NEW #(  ).
    r_result->mo_root = r_result.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json         = iv_json
        assoc_arrays = abap_true
      CHANGING
        data         = r_result->mr_actual
     ).

*    x = COND i( WHEN r_result->mr_actual IS NOT BOUND THEN THROW zstc77_cx( `ZCX_TREE_JSON_READER-FACTORY-JSON_NOT_VALID`) ).

  ENDMETHOD.

  METHOD get_attribute.

  ENDMETHOD.
  METHOD get_data.


  ENDMETHOD.
  METHOD get_parent.

    r_result = COND #( WHEN mo_parent IS NOT BOUND THEN me ELSE mo_parent ).

  ENDMETHOD.
  METHOD add_attribute.

    DATA(lo_attri) = NEW zstc77_cl_json_dom(  ).
    lo_attri->mo_root = mo_root.
    lo_attri->mv_name = n.

    IF apos_active = abap_false.
      lo_attri->mv_value = v.
    ELSE.
      lo_attri->mv_value = escape( val    = v
             format = cl_abap_format=>e_json_string ) .
    ENDIF.
    lo_attri->mv_apost_active = apos_active.
    lo_attri->mo_parent = me.

    INSERT lo_attri INTO TABLE mt_values.

    r_result = me.

  ENDMETHOD.
  METHOD write_result.

    r_result &&= COND #( WHEN mv_check_list = abaP_true THEN `[` ELSE `{` ).

    LOOP AT mt_values INTO DATA(lo_attri).
      DATA(lv_index) = sy-tabix.

      r_result  &&= COND #( WHEN mv_check_list = abaP_false THEN |"{ lo_attri->mv_name }":| ).


      IF lo_attri->mt_values IS NOT INITIAL.

        r_result &&= lo_attri->write_result(  ).

      ELSE.

        r_result &&= COND #( WHEN lo_attri->mv_apost_active = abap_true OR lo_attri->mv_value IS INITIAL THEN |"| )
         && lo_attri->mv_value
         && COND #( WHEN lo_attri->mv_apost_active = abap_true OR lo_attri->mv_value IS INITIAL THEN |"| ).

      ENDIF.

      r_result &&= COND #( WHEN lv_index < lines( mt_values )  THEN |,| ).

    ENDLOOP.

    r_result &&= COND #( WHEN mv_check_list = abaP_true THEN `]` ELSE `}` ).


  ENDMETHOD.

  METHOD add_attribute_list.

    r_result = add_attribute_object( name = name ).
    r_result->mv_check_list = abap_true.

  ENDMETHOD.

  METHOD add_list_object.

    r_result = add_attribute_object( name =  CONV string( lines( mt_values ) ) ).

  ENDMETHOD.

  METHOD add_attribute_object.

    DATA(lo_attri) = NEW zstc77_cl_json_dom(  ).
    lo_attri->mv_name = name.
    " lo_attri->mv_apost_active = apostrophe_active.

    mt_values = VALUE #( BASE mt_values  ( lo_attri ) ).
    " INSERT lo_attri INTO TABLE mt_values.

    lo_attri->mo_root = mo_root.
    lo_attri->mo_parent = me.

    r_result = lo_attri.

  ENDMETHOD.

  METHOD get_attribute_all.

    r_result = mt_values.

  ENDMETHOD.

  METHOD get_name.

    r_result = mv_name.

  ENDMETHOD.

  METHOD add_attributes_name_value_tab.

    LOOP AT it_name_value INTO DATA(ls_value).

      add_attribute(
           n              = ls_value-n
           v             = ls_value-v
           apos_active = xsdbool( ls_value-apostrophe_deactived = abap_false )
       ).

    ENDLOOP.

    r_result = me.
  ENDMETHOD.

  METHOD get_root.
    r_result = mo_root.
  ENDMETHOD.

  METHOD constructor.

    mo_root = me.

  ENDMETHOD.

  METHOD hlp_shrink.

    "leerzeichen weg
    "zeilenumbrüche


  ENDMETHOD.

  METHOD hlp_replace_apostr.

  ENDMETHOD.

  METHOD add_attribute_instance.

    val->mo_root = mo_root.
    val->mo_parent = me.

    INSERT val INTO TABLE mt_values.

    r_result = val.

  ENDMETHOD.

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
        io_abap TYPE REF TO zstc77_cl_xml_dom
      CHANGING
        co_ui5  TYPE REF TO zstc77_cl_xml_dom.
    METHODS xml_ui5_new
      IMPORTING
        check_no_bind TYPE abap_bool DEFAULT abap_false
        i_lo_abap     TYPE REF TO zstc77_cl_xml_dom.

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
    ENDIF.

    DATA(lo_abap) = zstc77_cl_xml_dom=>factory( iv_xml = r_result ).
    DATA(lo_ui5)  = zstc77_cl_xml_dom=>factory( iv_xml = `` ).

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

CLASS lcl_cntrl_frontend_app DEFINITION.

  PUBLIC SECTION.

    METHODS load
      RETURNING
        VALUE(r_result) TYPE string.

  PROTECTED SECTION.

    METHODS html_header
      RETURNING VALUE(r_result) TYPE string.

    METHODS html_js_main
      RETURNING VALUE(r_result) TYPE string.

    METHODS html_footer
      RETURNING VALUE(r_result) TYPE string.

ENDCLASS.

CLASS lcl_cntrl_frontend_app IMPLEMENTATION.


  METHOD load.

    r_result  = `` &&
        html_header(  ) &&
        html_js_main(  ) &&
        html_footer(  ) &&
        ``.

  ENDMETHOD.

  METHOD html_header.

    r_result = `` &&
         '<!DOCTYPE html><html><head><meta charset="utf-8"><title>Fiori-Abap Painter</title> ' &&
       '    <script src="https://ui5.sap.com/resources/sap-ui-core.js" id="sap-ui-bootstrap" ' &&
       '    data-sap-ui-theme="sap_fiori_3" data-sap-ui-libs="sap.m" data-sap-ui-bindingSyntax="complex" ' &&
       '    data-sap-ui-compatVersion="edge" data-sap-ui-preload="async"></script> ' &&
       ' <script> ' &&
       ``.

  ENDMETHOD.

  METHOD html_footer.

    r_result =  `` &&
       '  </script>  ' &&
  ' </head><body class="sapUiBody">  <div id="content"></div></body></html>  ' &&
                 ''.

  ENDMETHOD.

  METHOD html_js_main.

    r_result = `` &&
        '    sap.ui.getCore().attachInit(function () { ' &&
         '        "use strict"; ' &&
         '    sap.ui.define([ ' &&
         '        "sap/ui/core/mvc/Controller", ' &&
         '        "sap/ui/model/odata/v2/ODataModel", ' &&
         '       "sap/ui/model/json/JSONModel", ' &&
         '    ], function (Controller, ODataModel, JSONModel) { ' &&
             '        "use strict"; ' &&
             '     return Controller.extend("MyController", { ' &&
                 ' ' &&
                 '      onEventBackend: function (oEvent) { ' &&
                     '        this.oBody = this.oView.getModel( ).oData; ' &&
                     '      this.oBody.oEvent = oEvent; ' &&
                     '    this.Roundtrip( ); ' &&
                     ' }, ' &&
                     ' ' &&
                "  get_function_prettify(  )  &&
                     ' ' &&
                     '     Roundtrip: function () { ' &&
                         '        this.oView.destroy(); ' &&
                         '        var xhr = new XMLHttpRequest(); ' &&
                         '         xhr.open("POST", window.location.pathname, true); ' &&
                    "     '        xhr.timeout = 2000; ' &&
                         '          ' &&
                         '        if (!this.oBody){ ' &&
                             '     ;       ' &&
                             '       this.oBody = { };       ' &&
                             '      this.oBody.window = window.location;       ' &&
                             '         } ' &&
                             '          ' &&
                             '        xhr.onload = function (that) { ' &&
                                 '             var oResponse = JSON.parse(that.target.response); ' &&
                                 '             console.log( oResponse.vModel );   ' &&
                                 '             console.log( oResponse.vView );  ' &&
                                 '           var oModel =  new JSONModel(oResponse.vModel); ' &&
                              "   '             var oView = sap.ui.xmlview({       ' &&
                                 '        var oView = new sap.ui.core.mvc.XMLView.create({   ' &&
*                                '                    viewContent: oResponse.vView,     ' &&
                                  '                    definition: oResponse.vView,     ' &&
                                 '                     preprocessors: {             '  &&
                                 '                          xml: {          ' &&
                                  '                              models: { meta: oModel }       ' &&
                                  '                               }         ' &&
                                 '                   },         ' &&
*                                '          }); ' &&
                                  '         }).then(oView => { ' &&
                                   '           oView.setModel(oModel); ' &&
                                 '            oView.placeAt("content"); ' &&
                                 '         });    ' &&
                                 '        }.bind(this); ' &&
                                 ' ' &&
                                 '         console.log( this.oBody ); ' &&
                                 '        xhr.send( JSON.stringify( this.oBody )); ' &&
                                 '   }, ' &&
                                 '  }); ' &&
                                 '    }); ' &&
                                 `   var oView = sap.ui.xmlview({ viewContent: "<mvc:View controllerName='MyController' xmlns:mvc='sap.ui.core.mvc' />" }); ` &&
                                 '    oView.getController( ).Roundtrip( );  ' &&
                                 '    });  '.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_cntrl_backend_http DEFINITION.
  PUBLIC SECTION.

    INTERFACES if_serializable_object.
    interfaces if_abap_parallel.
    DATA mv_xml TYPE string.
    DATA mo_view TYPE REF TO lcl_view.

    DATA mi_app TYPE REF TO zif_http_painter.

    DATA:
      BEGIN OF ms_config,
        id      TYPE string,
        id_prev TYPE string,
      END OF ms_config.


    CLASS-METHODS roundtrip
      IMPORTING
        iv_body        TYPE string
      RETURNING
        VALUE(rv_resp) TYPE string.

    METHODS db_save.

    METHODS db_load
      IMPORTING
        id              TYPE string
      RETURNING
        VALUE(r_result) TYPE REF TO lcl_cntrl_backend_http.

  PROTECTED SECTION.

    METHODS execute_init
      IMPORTING
        iv_body         TYPE string
      RETURNING
        VALUE(ro_model) TYPE REF TO lcl_cntrl_backend_http.
    METHODS execute_roundtrip.
    METHODS execute_finish
      RETURNING
        VALUE(rv_resp) TYPE string.
    METHODS init_prev.
    METHODS init_app.
    METHODS go_app_change.
    METHODS go_restart.
    METHODS go_back
      RETURNING VALUE(r_result) TYPE REF TO lcl_cntrl_backend_http.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_cntrl_backend_http IMPLEMENTATION.

  METHOD roundtrip.

    DATA(lo_model) = NEW lcl_cntrl_backend_http(  ).

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

    DATA(lo_model) = NEW zstc77_cl_json_dom( )->add_attribute(
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


    DATA(lv_app) = VALUE #( lt_fields[ name = `APP` ]-value DEFAULT `LCL_APP_START` ).

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

ENDCLASS.


CLASS lcl_app_error IMPLEMENTATION.

  METHOD zif_http_painter~roundtrip.

    ui_on_init( view ).
    ui_on_event( view ).
    ui_set_screen( view ).

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

ENDCLASS.

CLASS lcl_app_demo DEFINITION.

  PUBLIC SECTION.

    INTERFACES zif_http_painter.

    DATA mv_xml TYPE string.
ENDCLASS.

CLASS lcl_app_demo IMPLEMENTATION.

  METHOD zif_http_painter~roundtrip.

    view->mv_xml_abap = view->db_read( 'LCL_APP_DEMO' ).
    " mv_xml =  view->db_read( 'LCL' ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_view_editor DEFINITION.

  PUBLIC SECTION.

    INTERFACES zif_http_painter.
    DATA mv_xml TYPE string.

ENDCLASS.

CLASS lcl_view_editor IMPLEMENTATION.

  METHOD zif_http_painter~roundtrip.

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

ENDCLASS.

CLASS lcl_app_start DEFINITION.

  PUBLIC SECTION.

    INTERFACES zif_http_painter.

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

CLASS lcl_app_start IMPLEMENTATION.

  METHOD zif_http_painter~roundtrip.

    ui_on_init( view ).
    ui_on_event( view ).
    ui_set_screen( view ).

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

    ms_screen-app_name = 'LCL_APP_demo'.
    ms_screen-s_msg_stripe-text = 'Kein URL Parameter übergeben. Bitte Klassennamen übergeben, z.B. app=zcl_test'.
    ms_screen-s_msg_stripe-visible = abap_true.
    i_view->mv_xml_abap = i_view->db_read( 'test_test7' ).

  ENDMETHOD.

  METHOD ui_on_init.

    IF ms_screen-check_initialized = abap_true.
      RETURN.
    ENDIF.

    ms_screen-check_initialized = abap_true.

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

    methods delete.

    methods create.

    methods _clear_buffer.

PROTECTED SECTION.

    data mt_vbak type string_table.
    data mt_vbap type string_table.

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
