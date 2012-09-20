*"* local class implementation for public class
*"* use this source file for the implementation part of
*"* local helper classes
types: begin of t_int,
         i type i,
       end of t_int,
       begin of t_packed,
         p type p length 10 decimals 2,
       end of t_packed,
       begin of t_numc,
         nc type n length 4,
       end of t_numc,
       begin of t_string,
         s type string,
       end of t_string,
       begin of t_struc1,
         i   type i,
         nc  type n length 4,
         p   type p length 10 decimals 2,
         s   type string,
         c1  type c length 1,
         c20 type c length 20,
       end of t_struc1.

*----------------------------------------------------------------------*
*       CLASS lcl_zjson DEFINITION
*----------------------------------------------------------------------*
class lcl_zjson definition final for testing
  duration short
  risk level harmless.

  private section.
    data: json_doc  type ref to zcl_json_document,
          json_doc2 type ref to zcl_json_document,
          json_str  type string.

    methods: test_number              for testing,
             test_string_number       for testing,
             test_string_escape       for testing,
             test_string_number_struc for testing,
             test_number_struct       for testing,
             test_append_data         for testing,
             test_string_table        for testing,
             test_stru_table          for testing,
             test_stru_table_named    for testing,
             test_parse_list_strings  for testing,
             test_parse_flat_object   for testing.

endclass.                    "lcl_zjson DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_zjson IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_zjson implementation.

  method test_number.
    data: tpacked  type p length 10 decimals 2,
          tpacked2 type p length 10 decimals 2,
          tfloat   type f,
          tint     type i,
          tint2    type i,
          tnumc    type n length 4,
          tnumc2   type n length 4.

*   packed number
    tfloat = '10.5'.
    tpacked = tfloat.  "conversion to packed
    json_doc = zcl_json_document=>create_with_data( tpacked ).
    json_str = json_doc->get_json( ).
    cl_aunit_assert=>assert_equals( exp = '10.50'
                                    act = json_str ).
    json_doc2 = zcl_json_document=>create_with_json( json_str ).
    json_doc2->get_data( importing data = tpacked2 ).
    cl_aunit_assert=>assert_equals( exp = tpacked
                                    act = tpacked2 ).

*   packed negative
    tfloat = '-999.55'.
    tpacked = tfloat.  "conversion to packed
    json_doc = zcl_json_document=>create_with_data( tpacked ).
    json_str = json_doc->get_json( ).
    cl_aunit_assert=>assert_equals( exp = '-999.55'
                                    act = json_str ).
    json_doc2 = zcl_json_document=>create_with_json( json_str ).
    json_doc2->get_data( importing data = tpacked2 ).
    cl_aunit_assert=>assert_equals( exp = tpacked
                                    act = tpacked2 ).


*   integer
    tint = 10.
    json_doc = zcl_json_document=>create_with_data( tint ).
    json_str = json_doc->get_json( ).
    cl_aunit_assert=>assert_equals( exp = '10'
                                    act = json_str ).
    json_doc2 = zcl_json_document=>create_with_json( json_str ).
    json_doc2->get_data( importing data = tint2 ).
    cl_aunit_assert=>assert_equals( exp = tint
                                    act = tint2 ).

*   numc
    tnumc = '00010'.
    json_doc = zcl_json_document=>create_with_data( tnumc ).
    json_str = json_doc->get_json( ).
    cl_aunit_assert=>assert_equals( exp = '10'
                                    act = json_str ).
    json_doc2 = zcl_json_document=>create_with_json( json_str ).
    json_doc2->get_data( importing data = tnumc2 ).
    cl_aunit_assert=>assert_equals( exp = tnumc
                                    act = tnumc2 ).

*   numc with just zeros
    tnumc = '00000'.
    json_doc = zcl_json_document=>create_with_data( tnumc ).
    json_str = json_doc->get_json( ).
    cl_aunit_assert=>assert_equals( exp = '0'
                                    act = json_str ).
    json_doc2 = zcl_json_document=>create_with_json( json_str ).
    json_doc2->get_data( importing data = tnumc2 ).
    cl_aunit_assert=>assert_equals( exp = tnumc
                                    act = tnumc2 ).

  endmethod.                    "test_number


  method test_string_number.
    data: t_str  type string,
          t_str2 type string.

    t_str = '0010'.
    json_doc = zcl_json_document=>create_with_data( t_str ).
    json_str = json_doc->get_json( ).
    cl_aunit_assert=>assert_equals( exp = '"0010"'
                                    act = json_str ).
    json_doc2 = zcl_json_document=>create_with_json( json_str ).
    json_doc2->get_data( importing data = t_str2 ).
    cl_aunit_assert=>assert_equals( exp = t_str
                                    act = t_str2 ).

  endmethod.                    "test_string_number

  method test_string_escape.

    data: begin of t_struc,
              abc type string value 'def:"123}',
           end of t_struc.
    data t_str type string.

    json_doc = zcl_json_document=>create_with_data( t_struc ).
    json_str = json_doc->get_json( ).
    cl_aunit_assert=>assert_equals( exp = '{"abc" :"def:\"123}"}'
                                    act = json_str ).

    json_doc = zcl_json_document=>create_with_json( json_str ).
    t_str = json_doc->get_value( 'abc' ).

    cl_aunit_assert=>assert_equals( exp = 'def:\"123}'
                                    act = t_str ).

  endmethod.                    "test_string_number


  method test_string_number_struc.
    data: s_str  type t_string,
          s_str2 type t_string.

    s_str-s = '0010'.
    json_doc = zcl_json_document=>create_with_data( s_str ).
    json_str = json_doc->get_json( ).
    cl_aunit_assert=>assert_equals( exp = '{"s" :"0010"}'
                                    act = json_str ).
    json_doc2 = zcl_json_document=>create_with_json( json_str ).
    json_doc2->get_data( importing data = s_str2 ).
    cl_aunit_assert=>assert_equals( exp = s_str
                                    act = s_str2 ).
  endmethod.                    "test_string_number_struc


  method test_string_table.
    data: str     type string,
          strtab  type table of string,
          strtab2 type table of string.

    str = '0010'. append str to strtab.
    str = '0020'. append str to strtab.
    str = '0030'. append str to strtab.

    json_doc = zcl_json_document=>create_with_data( data = strtab suppress_itab = 'X' ).
    json_str = json_doc->get_json( ).
    cl_aunit_assert=>assert_equals( exp = '["0010","0020","0030"]'
                                    act = json_str ).
    json_doc2 = zcl_json_document=>create_with_json( json_str ).
    json_doc2->get_data( importing data = strtab2 ).
    cl_aunit_assert=>assert_equals( exp = strtab
                                    act = strtab2 ).

  endmethod.                    "test_string_table


  method test_number_struct.
    data: tfloat    type f,
          s_int     type t_int,
          s_int2    type t_int,
          s_packed  type t_packed,
          s_packed2 type t_packed,
          s_numc    type t_numc,
          s_numc2   type t_numc.

*   Integer
    s_int-i = 10.
    json_doc = zcl_json_document=>create_with_data( s_int ).
    json_str = json_doc->get_json( ).
    cl_aunit_assert=>assert_equals( exp = '{"i" :10}'
                                    act = json_str ).
    json_doc2 = zcl_json_document=>create_with_json( json_str ).
    json_doc2->get_data( importing data = s_int2 ).
    cl_aunit_assert=>assert_equals( exp = s_int
                                    act = s_int2 ).

*   Packed number
    tfloat = '10.5'.
    s_packed-p = tfloat.  "conversion
    json_doc = zcl_json_document=>create_with_data( s_packed ).
    json_str = json_doc->get_json( ).

    cl_aunit_assert=>assert_equals( exp = '{"p" :10.50}'
                                    act = json_str ).
    json_doc2 = zcl_json_document=>create_with_json( json_str ).
    json_doc2->get_data( importing data = s_packed2 ).
    cl_aunit_assert=>assert_equals( exp = s_packed
                                    act = s_packed2 ).

*   NUMC without leading zeros
    s_numc-nc = '10'.
    json_doc = zcl_json_document=>create_with_data( s_numc ).
    json_str = json_doc->get_json( ).

    cl_aunit_assert=>assert_equals( exp = '{"nc" :10}'
                                    act = json_str ).
    json_doc2 = zcl_json_document=>create_with_json( json_str ).
    json_doc2->get_data( importing data = s_numc2 ).
    cl_aunit_assert=>assert_equals( exp = s_numc
                                    act = s_numc2 ).

*   NUMC with leading zeros
    s_numc-nc = '0010'.
    json_doc = zcl_json_document=>create_with_data( s_numc ).
    json_str = json_doc->get_json( ).

    cl_aunit_assert=>assert_equals( exp = '{"nc" :10}'
                                    act = json_str ).
    json_doc2 = zcl_json_document=>create_with_json( json_str ).
    json_doc2->get_data( importing data = s_numc2 ).
    cl_aunit_assert=>assert_equals( exp = s_numc
                                    act = s_numc2 ).

  endmethod.                    "test_number_struct


  method test_append_data.
    data: s_int     type t_int,
          s_string  type t_string.

    s_int-i = 10.
    s_string-s = 'abc'.

    json_doc = zcl_json_document=>create( ).
    json_doc->append_data( data = s_int iv_name = 's_int' ).
    json_doc->append_data( data = s_string iv_name = 's_string' ).
    json_str = json_doc->get_json( ).
    cl_aunit_assert=>assert_equals(
        exp = '{"s_int":{"i" :10}, "s_string":{"s" :"abc"}}'
        act = json_str ).

  endmethod.                    "test_append_data

  method test_stru_table.

    data: str     type t_string,
          strtab  type table of t_string,
          strtab2 type table of t_string.

    str-s = '0010'. append str to strtab.
    str-s = '00xx'. append str to strtab.
    str-s = '0030'. append str to strtab.

    json_doc = zcl_json_document=>create_with_data( data = strtab suppress_itab = 'X' ).
    json_str = json_doc->get_json( ).
    cl_aunit_assert=>assert_equals( exp = '[{"s" :"0010"},{"s" :"00xx"},{"s" :"0030"}]'
                                    act = json_str ).
    json_doc2 = zcl_json_document=>create_with_json( json_str ).
    json_doc2->get_data( importing data = strtab2 ).
    cl_aunit_assert=>assert_equals( exp = strtab
                                    act = strtab2 ).

  endmethod.                    "test_stru_table

  method test_stru_table_named.

    data: str     type t_string,
          strtab  type table of t_string,
          strtab2 type table of t_string.

    str-s = '0010'. append str to strtab.
    str-s = '00xx'. append str to strtab.
    str-s = '0030'. append str to strtab.

    json_doc = zcl_json_document=>create( ).
    json_doc->append_data( data = strtab
                           iv_name = 'dataname' ).
    json_str = json_doc->get_json( ).
    cl_aunit_assert=>assert_equals( exp = '{"dataname": [{"s" :"0010"},{"s" :"00xx"},{"s" :"0030"}]}'
                                    act = json_str ).

  endmethod.                    "test_stru_table_named

  method test_parse_list_strings.
    data: json_input type string,
          has_next   type boolean.

    json_input = '["value1","value2","value3"]'.
    json_doc = zcl_json_document=>create_with_json( json_input ).
    json_doc->get_next( ).
    json_str = json_doc->get_json( ).
    cl_aunit_assert=>assert_equals( exp = 'value1'
                                    act = json_str ).
    json_doc->get_next( ).
    json_str = json_doc->get_json( ).
    cl_aunit_assert=>assert_equals( exp = 'value2'
                                    act = json_str ).
    json_doc->get_next( ).
    json_str = json_doc->get_json( ).
    cl_aunit_assert=>assert_equals( exp = 'value3'
                                    act = json_str ).
    has_next = json_doc->get_next( ).
    cl_aunit_assert=>assert_equals( exp = ''
                                    act = has_next ).
  endmethod.                    "test_parse_list_strings

  method test_parse_flat_object.
    data: json_input type string,
          input_stru type t_struc1,
          ref_stru type t_struc1.

    json_input = '{"i":22,"nc":20,"c1":"X","c20":"test","s":"string test","p":20.5}'.
    ref_stru-i = 22.
    ref_stru-nc = 20.
    ref_stru-c1 = 'X'.
    ref_stru-c20 = 'test'.
    ref_stru-s = 'string test'.
    ref_stru-p = '20.5'.

    json_doc = zcl_json_document=>create_with_json( json_input ).
    json_doc->get_data( importing data = input_stru ).

    cl_aunit_assert=>assert_equals( exp = ref_stru
                                    act = input_stru ).

    "test starting from a structure and getting the structure in the end
    clear input_stru.
    json_doc = zcl_json_document=>create_with_data( ref_stru ).
    json_doc->get_data( importing data = input_stru ).

    cl_aunit_assert=>assert_equals( exp = ref_stru
                                    act = input_stru ).

  endmethod.                    "test_parse_flat_object
endclass.                    "lcl_zjson IMPLEMENTATION