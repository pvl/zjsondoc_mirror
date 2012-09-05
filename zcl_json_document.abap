*----------------------------------------------------------------------*
*       CLASS ZCL_JSON_DOCUMENT DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class zcl_json_document definition
  public
  create public .

  public section.
*"* public components of class ZCL_JSON_DOCUMENT
*"* do not include other source files here!!!
    type-pools abap .

    class-methods create
      returning
        value(json_document) type ref to zcl_json_document .
    class-methods create_with_json
      importing
        !json type string
      returning
        value(json_document) type ref to zcl_json_document .
    class-methods create_with_data
      importing
        !data type any
        !suppress_itab type boolean optional
      returning
        value(json_document) type ref to zcl_json_document .
    methods set_json
      importing
        !json type string .
    methods get_json
      returning
        value(json) type string .
    methods set_data
      importing
        !data type any
        !suppress_itab type boolean optional .
    methods get_data
      importing
        !json type string optional
      exporting
        !data type any .
    methods append_data
      importing
        !data type any
        !iv_name type string .
    methods get_next
      returning
        value(data_found) type boolean .
    methods reset_cursor .
    methods get_value
      importing
        !key type string
      returning
        value(value) type string .
    methods get_value_int
      importing
        !key type string
      returning
        value(value) type i .
    methods dumps
      importing
        !json type string optional
        !current_intend type i optional
      exporting
        !result type string_table .
    methods set_suppress_itab
      importing
        !suppress_itab type boolean .
  protected section.
*"* protected components of class ZCL_JSON_DOCUMENT
*"* do not include other source files here!!!
  private section.
*"* private components of class ZCL_JSON_DOCUMENT
*"* do not include other source files here!!!

    data json type string .
    data data type zjson_key_value_t .
    data data_t type string_table .
    data array_cursor type i .
    data suppress_itab type boolean .

    class-methods copyright .
    methods parse
      importing
        !json type string optional .
    methods parse_object .
    methods parse_array .
    methods get_offset_close
      importing
        !json type string
        !offset_open type i default 0
      returning
        value(offset_close) type i .
    methods escapechar
      importing
        !json type string
        !offset type i
      changing
        !match_result type match_result_tab .
    methods add_data
      importing
        !data type any .
    methods add_table
      importing
        !table type any table .
    methods add_stru
      importing
        !line type any .
    methods add_string
      importing
        !string type any .
    methods add_xstring
      importing
        !xstring type any .
    methods add_number
      importing
        !number type any .
    methods add_date
      importing
        !date type d .
    methods add_time
      importing
        !time type t .
    methods get_stru
      changing
        !line type any .
    methods get_table
      changing
        !table type any table .
endclass.                    "zcl_json_document DEFINITION



*----------------------------------------------------------------------*
*       CLASS ZCL_JSON_DOCUMENT IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class zcl_json_document implementation.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_JSON_DOCUMENT->ADD_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method add_data.

    data: data_descr type ref to cl_abap_datadescr.

    data_descr ?= cl_abap_typedescr=>describe_by_data( data ).

    case data_descr->type_kind.
      when data_descr->typekind_table.       "table

        add_table( data ).

      when data_descr->typekind_struct1     "flat strcuture
      or   data_descr->typekind_struct2.     "deep strcuture

        add_stru( data ).

      when data_descr->typekind_char
      or   data_descr->typekind_string
      or   data_descr->typekind_clike
      or   data_descr->typekind_csequence.

        add_string( data ).

      when data_descr->typekind_int
      or   data_descr->typekind_int1
      or   data_descr->typekind_int2
      or   data_descr->typekind_packed
      or   data_descr->typekind_num.          "charlike incl. NUMC.

        add_number( data ).

      when data_descr->typekind_date.

        add_date( data ).

      when data_descr->typekind_time.

        add_time( data ).

      when data_descr->typekind_xstring.

        add_xstring( data ).

      when data_descr->typekind_dref.
        field-symbols <any> type data.
        assign data->* to <any>.
        add_data( <any> ).

*    WHEN data_descr->typekind_hex.
*    WHEN data_descr->typekind_float.
*    WHEN data_descr->typekind_w.
*    WHEN data_descr->typekind_oref.
*    WHEN data_descr->typekind_class.
*    WHEN data_descr->typekind_intf.
*    WHEN data_descr->typekind_any.
*    WHEN data_descr->typekind_data.
*    WHEN data_descr->typekind_simple.
*    WHEN data_descr->typekind_xsequence.
*    WHEN data_descr->typekind_numeric.
*    WHEN data_descr->typekind_table.
*    WHEN data_descr->typekind_iref.

*    WHEN OTHERS.

    endcase.

  endmethod.                    "ADD_DATA


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_JSON_DOCUMENT->ADD_DATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATE                           TYPE        D
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method add_date.

    data: lv_date_c type c length 10.

*  lv_date_c = |{ date DATE = raw }|.   ">= 7.02 only
    lv_date_c = date.                                         "<= 7.01

    concatenate
      json
      '"'
      lv_date_c
      '"'
    into json.

  endmethod.                    "ADD_DATE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_JSON_DOCUMENT->ADD_NUMBER
* +-------------------------------------------------------------------------------------------------+
* | [--->] NUMBER                         TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method add_number.

    data: lv_num_c type c length 20.

    lv_num_c = number.

    "*--- sign on the left ---*
    if lv_num_c cs '-'.
      shift lv_num_c right up to '-'.
      shift lv_num_c circular right.
    endif.

    "*--- store NUMC without leading zero (sapcodexch #issue 17) ---*
    shift lv_num_c left deleting leading '0'.

    condense lv_num_c no-gaps.

    concatenate
      json
      lv_num_c
    into json.

  endmethod.                    "ADD_NUMBER


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_JSON_DOCUMENT->ADD_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] STRING                         TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method add_string.

    data: lv_string type string.

    "*--- JSON conform conversion ---*
    "*--- sapcodexch issue #4 ---*
    lv_string = string.   "convert to string
    lv_string = cl_http_utility=>if_http_utility~escape_javascript( lv_string ).

    "*--- don't escape single quotes ---*
    "*--- sapcodexch issue #11 ---*
    replace all occurrences of '\''' in lv_string with ''''.

    concatenate
      json
      '"'
      lv_string
      '"'
    into json.

  endmethod.                    "ADD_STRING


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_JSON_DOCUMENT->ADD_STRU
* +-------------------------------------------------------------------------------------------------+
* | [--->] LINE                           TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method add_stru.

    data: stru_descr   type ref to cl_abap_structdescr
        , lv_tabix     type sy-tabix
        , comp_name    type abap_compname
        .

    field-symbols: <value> type any
                 , <component> type abap_compdescr
                 .

    data lv_parameter_id type string.

    stru_descr ?= cl_abap_typedescr=>describe_by_data( line ).

    concatenate
      json
      '{'
    into json.

    loop at stru_descr->components
      assigning <component>.

      lv_tabix = sy-tabix.

      assign component <component>-name of structure line to <value>.

      comp_name = <component>-name.
      translate comp_name to lower case.

      if comp_name = 'parameter_id'.
*      lv_parameter_id = |{ <value> }|.   ">= 7.02
        lv_parameter_id = <value>.                            "<= 7.01
      elseif comp_name = 'data'.
        comp_name = lv_parameter_id.
      endif.

      concatenate
        json
        '"'
        comp_name
        '" :'
      into json.

      add_data( <value> ).

      if lv_tabix <> lines( stru_descr->components ).
        concatenate
          json
          ','
        into json.
      endif.

    endloop.

    concatenate
      json
      '}'
    into json.

  endmethod.                    "ADD_STRU


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_JSON_DOCUMENT->ADD_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] TABLE                          TYPE        ANY TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method add_table.

    data: lv_tabix type sytabix.
    field-symbols: <line> type any.

    data lv_end type boolean.

    if strlen( json ) > 3
    or suppress_itab = abap_true. "sapcodexch issue #13
      concatenate
        json
        ' ['
      into json.
    else.
      lv_end = abap_true.
      concatenate
        json
        '{ "itab" : ['
      into json.
    endif.

    loop at table
      assigning <line>.

      lv_tabix = sy-tabix.

      add_data( <line> ).

      if lv_tabix <> lines( table ).
        concatenate
          json
          ','
        into json.
      endif.

    endloop.

    if lv_end = abap_true.
      concatenate
        json
        '] }'
      into json.
    else.
      concatenate
        json
        ']'
      into json.
    endif.
  endmethod.                    "ADD_TABLE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_JSON_DOCUMENT->ADD_TIME
* +-------------------------------------------------------------------------------------------------+
* | [--->] TIME                           TYPE        T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method add_time.

    data: lv_time_c type c length 8.

    concatenate
      time(2)
      ':'
      time+2(2)
      ':'
      time+4(2)
    into lv_time_c.

    concatenate
      json
      '"'
      lv_time_c
      '"'
    into json.

  endmethod.                    "ADD_TIME


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_JSON_DOCUMENT->ADD_XSTRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] XSTRING                        TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method add_xstring.

    data: lv_string type string.

*  lv_string = cl_http_utility=>encode_x_base64( xstring ) .  ">= 7.02

    "*--- <= 7.01 ---*
    data: c_last_error type i.
    data: ihttp_scid_base64_escape_x type i value 86.

    system-call ict
      did
        ihttp_scid_base64_escape_x
      parameters
        xstring                            " >
        lv_string                          " <
        c_last_error.                      " < return code

    concatenate
      json
      '"'
      lv_string
      '"'
    into json.

  endmethod.                    "ADD_XSTRING


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_JSON_DOCUMENT->APPEND_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        ANY
* | [--->] IV_NAME                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method append_data.

    if json is initial.
*    json =  | "{ iv_name }":|.                             ">= 7.02

      concatenate                                             "<= 7.01
        '"'
        iv_name
        '":'
      into json.

    else.
*    json = json && |, "{ iv_name }":|.                     ">= 7.02

      concatenate                                             "<= 7.01
        json
        ', "'
        iv_name
        '":'
      into json.

    endif.

    add_data( data ).

  endmethod.                    "APPEND_DATA


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_JSON_DOCUMENT=>COPYRIGHT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method copyright.

*--------------------------------------------------------------------*
*
* The JSON document class
* Copyright (C) 2010 Uwe Fetzer + SAP Developer Network members
*
* Project home: https://cw.sdn.sap.com/cw/groups/zjson
*
* Published under the Terms of use of SAP Code Exchange:
* http://www.sdn.sap.com/irj/sdn/code-exchange
*
*--------------------------------------------------------------------*

  endmethod.                    "COPYRIGHT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_JSON_DOCUMENT=>CREATE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] JSON_DOCUMENT                  TYPE REF TO ZCL_JSON_DOCUMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create.

    create object json_document.

  endmethod.                    "CREATE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_JSON_DOCUMENT=>CREATE_WITH_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        ANY
* | [--->] SUPPRESS_ITAB                  TYPE        BOOLEAN(optional)
* | [<-()] JSON_DOCUMENT                  TYPE REF TO ZCL_JSON_DOCUMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create_with_data.

    create object json_document.
    json_document->set_data(
      data          = data
      suppress_itab = suppress_itab
      ).

  endmethod.                    "CREATE_WITH_DATA


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_JSON_DOCUMENT=>CREATE_WITH_JSON
* +-------------------------------------------------------------------------------------------------+
* | [--->] JSON                           TYPE        STRING
* | [<-()] JSON_DOCUMENT                  TYPE REF TO ZCL_JSON_DOCUMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create_with_json.

    create object json_document.
    json_document->set_json( json ).

  endmethod.                    "CREATE_WITH_JSON


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_JSON_DOCUMENT->DUMPS
* +-------------------------------------------------------------------------------------------------+
* | [--->] JSON                           TYPE        STRING(optional)
* | [--->] CURRENT_INTEND                 TYPE        I(optional)
* | [<---] RESULT                         TYPE        STRING_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method dumps.

    data: json_doc   type ref to zcl_json_document
        , json_tmp   type string
        , data_tmp   type zjson_key_value_t
        , data_t_tmp type string_table
        , intend     type i
        , tabix      type sytabix
        , dump       type string_table
        .

    field-symbols: <data_line>   type zjson_key_value
                 , <data_t_line> type string
                 , <result_line> type string
                 .

    if json is not initial.
      json_tmp = json.
    else.
      json_tmp = me->json.
    endif.

    shift json_tmp left deleting leading space.
    me->json = json_tmp.

    intend = current_intend.

    case json_tmp(1).
      when '{'.
        parse_object( ).

        insert initial line into table result assigning <result_line>.
        do intend times.
          <result_line> = <result_line> && ` `.
        enddo.
        <result_line> = <result_line> && `{`.
        add 4 to intend.

        clear tabix.

        data_tmp = me->data.

        loop at data_tmp
          assigning <data_line>.

          add 1 to tabix.          "sy-tabix doesn't work here

          insert initial line into table result assigning <result_line>.
          do intend times.
            <result_line> = <result_line> && ` `.
          enddo.

          <result_line> = |{ <result_line> }"{ <data_line>-key }" : |.

          if <data_line>-value is initial.

            <result_line> = |{ <result_line> }""|.

          elseif <data_line>-value(1) cn '{['.

            if <data_line>-value co '0123456789.'
            and <data_line>-value(1) <> '0'.        "no leading zero (else asume a string)
              <result_line> = |{ <result_line> }{ <data_line>-value }|.
            else.
              <result_line> = |{ <result_line> }"{ <data_line>-value }"|.
            endif.

          else.
            clear dump.
            json_doc = zcl_json_document=>create_with_json( <data_line>-value ).
            json_doc->dumps( exporting current_intend = intend
                             importing result = dump ).
            insert lines of dump into table result.
            read table result index lines( result ) assigning <result_line>.

          endif.

          if tabix < lines( data_tmp ).
            <result_line> = <result_line> && `,`.
          endif.

        endloop.

        subtract 4 from intend.
        insert initial line into table result assigning <result_line>.
        do intend times.
          <result_line> = <result_line> && ` `.
        enddo.
        <result_line> = <result_line> && `}`.

      when '['.
        parse_array( ).

        insert initial line into table result assigning <result_line>.
        do intend times.
          <result_line> = <result_line> && ` `.
        enddo.
        <result_line> = <result_line> && `[`.
        add 4 to intend.

        clear tabix.

        data_t_tmp = me->data_t.

        loop at data_t_tmp
          assigning <data_t_line>.

          add 1 to tabix.          "sy-tabix doesn't work here

          if <data_t_line>(1) cn '{['.
            insert initial line into table result assigning <result_line>.
            do intend times.
              <result_line> = <result_line> && ` `.
            enddo.

            <result_line> = |{ <result_line> }"{ <data_t_line> }"|.
          else.
            clear dump.
            json_doc = zcl_json_document=>create_with_json( <data_t_line> ).
            json_doc->dumps( exporting current_intend = intend
                             importing result = dump ).
            insert lines of dump into table result.
            read table result index lines( result ) assigning <result_line>.
          endif.
          if tabix < lines( data_t_tmp ).
            <result_line> = <result_line> && `,`.
          endif.

        endloop.

        subtract 4 from intend.
        insert initial line into table result assigning <result_line>.
        do intend times.
          <result_line> = <result_line> && ` `.
        enddo.
        <result_line> = <result_line> && `]`.

    endcase.

  endmethod.                    "DUMPS


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_JSON_DOCUMENT->ESCAPECHAR
* +-------------------------------------------------------------------------------------------------+
* | [--->] JSON                           TYPE        STRING
* | [--->] OFFSET                         TYPE        I
* | [<-->] MATCH_RESULT                   TYPE        MATCH_RESULT_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method escapechar.

    data lv_tab type line of match_result_tab.
    data lv_len type i.
    data lt_result_tabguillemet type match_result_tab.
    data lv_result_tabguillemet type line of match_result_tab.
    data lv_pos_echap type i.
    data lv_count type i.
    data lv_parite type p decimals 1.

    constants : c_echap type c value '\'.

    check json cs c_echap.      "codexch issue #21

    loop at match_result into lv_tab.
      find all occurrences of '"' in json+offset(lv_tab-offset) results lt_result_tabguillemet.
      clear lv_count.
      loop at lt_result_tabguillemet into lv_result_tabguillemet where offset lt lv_tab-offset.
        lv_pos_echap = offset + lv_result_tabguillemet-offset - 1.
        check json+lv_pos_echap(1) ne c_echap.
        lv_count = lv_count + 1.
      endloop.
      lv_parite = frac( lv_count / 2 ).
      check lv_parite is not initial.
      delete match_result.
    endloop.

  endmethod.                    "ESCAPECHAR


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_JSON_DOCUMENT->GET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] JSON                           TYPE        STRING(optional)
* | [<---] DATA                           TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_data.

    data: data_descr  type ref to cl_abap_datadescr.
    data: lr_json_doc type ref to zcl_json_document.
    data: lv_json     type string.
    data: tmp         type c length 10.
    data: lv_submatch type string.

    if json is not initial.
      lv_json = json.
    else.
      lv_json = me->json.
    endif.

    clear data.

    "*--- create new JSON document (recursive!) ---*
    lr_json_doc = zcl_json_document=>create_with_json( lv_json ).
    lr_json_doc->parse( ).

    data_descr ?= cl_abap_typedescr=>describe_by_data( data ).

    case data_descr->type_kind.

      when data_descr->typekind_char         "charlike
      or   data_descr->typekind_string
      or   data_descr->typekind_clike
      or   data_descr->typekind_csequence.

        data = lr_json_doc->get_json( ).

        "*--- eliminate surrounding " ---*
        find regex '"(.{1,})"' in data     "get 1-n chars surrounded by "
          submatches lv_submatch.

        if sy-subrc = 0.
          data = lv_submatch.
        endif.

      when data_descr->typekind_num          "NUM + integer + packed (auto conversion)
      or   data_descr->typekind_int
      or   data_descr->typekind_int1
      or   data_descr->typekind_int2
      or   data_descr->typekind_packed

      or   data_descr->typekind_date
      or   data_descr->typekind_xstring.     "(should work, not tested yet) #uf

        data = lr_json_doc->get_json( ).

      when data_descr->typekind_time.

        tmp = lr_json_doc->get_json( ).
        if tmp cs ':'.
          replace all occurrences of ':' in tmp with ``.
        endif.

        data =  tmp.

      when data_descr->typekind_struct1     "flat strcuture
      or   data_descr->typekind_struct2.     "deep strcuture

        lr_json_doc->get_stru( changing line = data ).

      when data_descr->typekind_table.       "table

        lr_json_doc->get_table( changing table = data ).

*    WHEN data_descr->typekind_dref.
*    WHEN data_descr->typekind_hex.
*    WHEN data_descr->typekind_float.
*    WHEN data_descr->typekind_w.
*    WHEN data_descr->typekind_oref.
*    WHEN data_descr->typekind_class.
*    WHEN data_descr->typekind_intf.
*    WHEN data_descr->typekind_any.
*    WHEN data_descr->typekind_data.
*    WHEN data_descr->typekind_simple.
*    WHEN data_descr->typekind_xsequence.
*    WHEN data_descr->typekind_numeric.
*    WHEN data_descr->typekind_iref.

*    WHEN OTHERS.

    endcase.

  endmethod.                    "GET_DATA


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_JSON_DOCUMENT->GET_JSON
* +-------------------------------------------------------------------------------------------------+
* | [<-()] JSON                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_json.

    if me->json is initial.
    else.
      if  me->json+0(1) ne `{`
      and me->json+0(1) ne `[`.    "sapcodexch issue #7

        "*--- key/value pair only (sapcodexch issue #3) ---*
        find regex '"*":' in me->json.
        if sy-subrc = 0.
*        me->json = `{` && me->json .            ">= 7.02
          concatenate '{' me->json into me->json.             "<= 7.01
        endif.
      endif.

      data len type i.
      len = strlen( me->json ) - 1.

      if  me->json+len(1) ne `}`
      and me->json+len(1) ne `]`.  "sapcodexch issue #7

        "*--- key/value pair only (sapcodexch issue #3) ---*
        find regex '"*":' in me->json.
        if sy-subrc = 0.
*      me->json =   me->json && `}`.           ">= 7.02
          concatenate me->json '}'  into me->json.            "<= 7.01
        endif.

      endif.
    endif.
    json = me->json.

    shift json left deleting leading space.
  endmethod.                    "GET_JSON


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_JSON_DOCUMENT->GET_NEXT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] DATA_FOUND                     TYPE        BOOLEAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_next.

    data lv_json   type string.
    data lt_data   like me->data_t.
    data lv_cursor like me->array_cursor.

    add 1 to me->array_cursor.

    "*--- get next entry ---*
    read table me->data_t index me->array_cursor into lv_json.

    if sy-subrc = 0.
      lt_data = me->data_t.    "save data_t (nasted for tables)    codexch issue #20
      lv_cursor = me->array_cursor.

      set_json( lv_json ).

      me->data_t = lt_data.    "restore data_t (nasted for tables) codexch issue #20
      me->array_cursor = lv_cursor.

      data_found = abap_true.
    endif.

  endmethod.                    "GET_NEXT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_JSON_DOCUMENT->GET_OFFSET_CLOSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] JSON                           TYPE        STRING
* | [--->] OFFSET_OPEN                    TYPE        I (default =0)
* | [<-()] OFFSET_CLOSE                   TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_offset_close.

    data: lv_offset          type i
        , lv_copen           type c
        , lv_cclose          type c
        , lv_pos_echap       type i
        , lt_result_tabopen  type match_result_tab
        , lv_result_tabopen  type line of match_result_tab
        , lt_result_tabclose type match_result_tab
        , lv_result_tabclose type line of match_result_tab
        , lv_offsetclose_old type i
        .

    constants : c_echap type c value '\'.

    lv_copen = json+offset_open(1).
    case lv_copen.
      when '"'. lv_cclose = '"'.
      when '{'. lv_cclose = '}'.
      when '['. lv_cclose = ']'.
    endcase.
    lv_offset = offset_open + 1.
    if lv_copen eq '"'.
      find all occurrences of lv_cclose in json+lv_offset results lt_result_tabclose.
      loop at lt_result_tabclose into lv_result_tabclose.
        lv_pos_echap = lv_offset + lv_result_tabclose-offset - 1.
        check json+lv_pos_echap(1) ne c_echap.
        exit.
      endloop.
      offset_close = lv_offset + lv_result_tabclose-offset + 1. "CBO due to change in the else statement
    else.
      find all occurrences of lv_copen in json+lv_offset results lt_result_tabopen.
      escapechar(
        exporting
          json = json
          offset = lv_offset
        changing
          match_result = lt_result_tabopen
        ).

      find all occurrences of lv_cclose in json+lv_offset results lt_result_tabclose.
      escapechar(
        exporting
          json = json
          offset = lv_offset
        changing
          match_result = lt_result_tabclose
        ).

*   CHANGING CBO : We look to the first close where no open is set before
*                by removing each open corresponding of each close
      data lv_last_idx like sy-tabix.
      loop at lt_result_tabclose into lv_result_tabclose.
        clear: lv_result_tabopen.
        lv_last_idx = -1.
        loop at lt_result_tabopen into lv_result_tabopen where offset between 0 and lv_result_tabclose-offset.
          lv_last_idx = sy-tabix.
        endloop.
        if not lv_last_idx = -1 .
          delete lt_result_tabopen index lv_last_idx.
        else.
          offset_close = lv_offset + lv_result_tabclose-offset + 1.
          exit.
        endif.
      endloop.

    endif.

  endmethod.                    "GET_OFFSET_CLOSE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_JSON_DOCUMENT->GET_STRU
* +-------------------------------------------------------------------------------------------------+
* | [<-->] LINE                           TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_stru.

    data: stru_descr   type ref to cl_abap_structdescr
        , comp_name    type string
        , lv_json      type string
        .

    field-symbols: <value> type any
                 , <component> type abap_compdescr
                 .

    stru_descr ?= cl_abap_typedescr=>describe_by_data( line ).

    loop at stru_descr->components
      assigning <component>.

      assign component <component>-name of structure line to <value>.

      comp_name = <component>-name.
      translate comp_name to lower case.
      lv_json = me->get_value( comp_name ).

      check lv_json is not initial.    "value found?  "sapcodexch issue #6

      "*--- and again -> recursive! ---*
      me->get_data(
        exporting json = lv_json
        importing data = <value>
        ).

    endloop.

  endmethod.                    "GET_STRU


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_JSON_DOCUMENT->GET_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [<-->] TABLE                          TYPE        ANY TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_table.

    data: table_descr  type ref to cl_abap_tabledescr
        , data_descr   type ref to cl_abap_datadescr
        , stru_descr   type ref to cl_abap_structdescr
        , comp_name    type string
        , lv_json      type string
        .

    field-symbols: <value> type any
                 , <line>  type any
                 , <component> type abap_compdescr
                 .

    table_descr ?= cl_abap_typedescr=>describe_by_data( table ).

    "*--- currently only standard tables possible (no hashed/sorted) ---*
    check table_descr->table_kind = table_descr->tablekind_std.

    data_descr ?= table_descr->get_table_line_type( ).

    "*--- check structure or simple ---*
    if data_descr->type_kind = data_descr->typekind_struct1     "flat strcuture
    or data_descr->type_kind = data_descr->typekind_struct2.    "deep strcuture
      stru_descr ?= data_descr.
    endif.

    while me->get_next( ) is not initial.

      insert initial line into table table assigning <line>.

      if stru_descr is not bound.    "table line is not a structure

        me->get_data(
          exporting json = lv_json
          importing data = <line>
          ).

      else.

        loop at stru_descr->components
          assigning <component>.

          assign component <component>-name of structure <line> to <value>.

          comp_name = <component>-name.
          translate comp_name to lower case.
          lv_json = me->get_value( comp_name ).

          check lv_json is not initial.    "value found?  "sapcodexch issue #6

          "*--- and again -> recursive! ---*
          me->get_data(
            exporting json = lv_json
            importing data = <value>
            ).

        endloop.

      endif.

    endwhile.

  endmethod.                    "GET_TABLE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_JSON_DOCUMENT->GET_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] KEY                            TYPE        STRING
* | [<-()] VALUE                          TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_value.

    field-symbols: <data> type zjson_key_value.

    read table me->data
      assigning <data>
      with table key
        key = key.

    if sy-subrc = 0.
      value = <data>-value.
    endif.

  endmethod.                    "GET_VALUE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_JSON_DOCUMENT->GET_VALUE_INT
* +-------------------------------------------------------------------------------------------------+
* | [--->] KEY                            TYPE        STRING
* | [<-()] VALUE                          TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_value_int.

    data: lv_value_string type string.
    field-symbols: <data> type zjson_key_value.

    read table me->data
      assigning <data>
      with table key
      key = key.

    if sy-subrc = 0.
      lv_value_string = <data>-value.
    endif.

    if lv_value_string co ' 1234567890-'.
      value = lv_value_string.
    endif.

  endmethod.                    "GET_VALUE_INT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_JSON_DOCUMENT->PARSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] JSON                           TYPE        STRING(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method parse.

    "*--- new data given ---*
    if json is not initial.

      set_json( json ).

    else.

      check me->json is not initial.  "Codexch issue #1 CX_SY_RANGE_OUT_OF_BOUNDS

      case me->json(1).
        when '['.
          parse_array( ).
        when '{'.
          parse_object( ).
        when others.
          return.
      endcase.

    endif.

  endmethod.                    "PARSE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_JSON_DOCUMENT->PARSE_ARRAY
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method parse_array.

    data: lv_json      type string
        , lv_json_part type string
        , lv_close     type i
        , data         type zjson_key_value_t
        .

    lv_json = me->json.
    clear me->data_t.
    clear me->array_cursor.

    replace regex '^\[' in lv_json with ``.   "codexch issue #20
    replace regex '\]$' in lv_json with ``.   "codexch issue #20

    while not lv_json co space.

      case lv_json(1).

        when '{' or '['.          "object or array

          lv_close = get_offset_close( lv_json ).

          "*--- get object ---*
          lv_json_part = lv_json(lv_close).
          insert lv_json_part into table me->data_t.

          lv_json = lv_json+lv_close.

        when '"'.          "string

          lv_close = get_offset_close( lv_json ) - 2.  "w/o "

          "*--- get object ---*
          if lv_close > 0.
            lv_json_part = lv_json+1(lv_close).
          else.
            clear lv_json_part.
          endif.

          insert lv_json_part into table me->data_t.

          add 2 to lv_close.
          lv_json = lv_json+lv_close.

        when others.       "numbers, boolean, NULL

          split lv_json at ',' into lv_json_part lv_json.
          shift lv_json_part left deleting leading space.
          insert lv_json_part into table me->data_t.

      endcase.

      shift lv_json left deleting leading space.
      shift lv_json left deleting leading ','.
      shift lv_json left deleting leading space.

    endwhile.

  endmethod.                    "PARSE_ARRAY


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_JSON_DOCUMENT->PARSE_OBJECT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method parse_object.

    data: lv_json type string
        , lv_close type i
        , ls_key_value type zjson_key_value
        .

    lv_json = me->json.
    clear me->data.

    while not lv_json co '{} '.

      "*--- get key ---*
      shift lv_json left up to '"'.
      lv_close = get_offset_close( lv_json ).

      subtract 2 from lv_close.
      ls_key_value-key = lv_json+1(lv_close).
      translate ls_key_value-key to lower case.   "sapcodexch ticket #5

      "*--- get value ---*
      shift lv_json left up to ':'.
      shift lv_json left.
      shift lv_json left deleting leading space.

      case lv_json(1).
        when '"'.
          lv_close = get_offset_close( lv_json ).
          subtract 2 from lv_close.
          ls_key_value-value = lv_json+1(lv_close).
          add 2 to lv_close.
          lv_json = lv_json+lv_close.
        when '{' or '['.
          lv_close = get_offset_close( lv_json ).
          ls_key_value-value = lv_json+0(lv_close).
          add 1 to lv_close.
          lv_json = lv_json+lv_close.
        when others.     "boolean, numbers
          split lv_json at ',' into ls_key_value-value lv_json.
          replace '}' with `` into ls_key_value-value.   "last one of the list
      endcase.

      insert ls_key_value into table me->data.
    endwhile.

  endmethod.                    "PARSE_OBJECT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_JSON_DOCUMENT->RESET_CURSOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method reset_cursor.

    clear me->array_cursor.

  endmethod.                    "RESET_CURSOR


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_JSON_DOCUMENT->SET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        ANY
* | [--->] SUPPRESS_ITAB                  TYPE        BOOLEAN(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method set_data.

    set_suppress_itab( suppress_itab ).

    clear json.
    add_data( data ).

    parse( ).

  endmethod.                    "SET_DATA


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_JSON_DOCUMENT->SET_JSON
* +-------------------------------------------------------------------------------------------------+
* | [--->] JSON                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method set_json.

    me->json = json.

    "*--- esp. for CouchDB ---*
    replace all occurrences of cl_abap_char_utilities=>cr_lf in me->json with ``.
    replace all occurrences of cl_abap_char_utilities=>newline in me->json with ``.

    shift me->json left deleting leading space.

    parse( ).

  endmethod.                    "SET_JSON


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_JSON_DOCUMENT->SET_SUPPRESS_ITAB
* +-------------------------------------------------------------------------------------------------+
* | [--->] SUPPRESS_ITAB                  TYPE        BOOLEAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method set_suppress_itab.

    me->suppress_itab = suppress_itab.

  endmethod.                    "SET_SUPPRESS_ITAB
endclass.                    "ZCL_JSON_DOCUMENT IMPLEMENTATION